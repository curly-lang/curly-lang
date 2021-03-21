use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::collections::HashMap;
use std::env;
use std::env::Args;
use std::fs;
use std::path::Path;
use std::process::Command;
use std::result::Result;

use parser::AST;

use curlyc::backends::c::codegen;
use curlyc::frontend::ir;
use curlyc::frontend::ir::{DuplicateModuleInfo, IRError, IR};
use curlyc::frontend::parser;

pub static DEBUG: bool = false;

#[cfg(target_os = "macos")]
static COMPILER: &str = "gcc-10";
#[cfg(not(target_os = "macos"))]
static COMPILER: &str = "gcc";

#[derive(PartialEq)]
enum CompileMode {
    Executable,
    Dynamic,
    Static,
}

enum LibEntryType {
    Directory,
    LibraryPath,
}

struct CommandlineBuildOptions {
    output: String,
    inputs: Vec<String>,
    libraries: Vec<(String, LibEntryType)>,
    curly_libs: Vec<String>,
    compiler_options: Vec<String>,
    mode: CompileMode,
}

fn main() -> Result<(), ()> {
    let mut args = env::args();
    let name = args.next().unwrap();

    if args.len() == 0 {
        println!(
            "usage:
{} check [files]
{} build [options] [files]",
            &name, &name
        );
        Ok(())
    } else {
        match args.next().unwrap().as_str() {
            "build" => {
                let mut options = CommandlineBuildOptions {
                    output: String::with_capacity(0),
                    inputs: Vec::with_capacity(0),
                    libraries: Vec::with_capacity(0),
                    curly_libs: Vec::with_capacity(0),
                    compiler_options: Vec::with_capacity(0),
                    mode: CompileMode::Executable,
                };

                handle_args(&mut options, &mut args).expect("Failed to handle args.");
                post_process_args(&mut options, name).expect("Failed to post-process args.");

                make_dirs().expect("Failed to make dirs.");

                let mut ir = IR {
                    modules: HashMap::with_capacity(0),
                };

                // Get a list of (lib_path, mod_file_path)
                let curly_libs = get_curly_libs(&mut options).expect("Failed to get curly libs.");

                // Get contents of the mod files.
                let mod_files_contents: Vec<String> = curly_libs
                    .iter()
                    .map(|v| {
                        read_file(&v.1)
                            .unwrap_or_else(|_| panic!("Failed to read file {}.", v.1.clone()))
                    })
                    .collect();

                // Parse the mod file contents.
                let mod_files_asts: Vec<Vec<AST>> = mod_files_contents
                    .iter()
                    .map(|v| parser::parse_library(&v).expect("Failed to convert file to AST."))
                    .collect();

                // Try to add each module to the IR, 1 by 1, and log any errors.
                let mod_files_ir_errors: Vec<Vec<Vec<IRError>>> = mod_files_asts
                    .iter()
                    .enumerate()
                    .map(|v| {
                        let (a, b) = v;
                        b.iter()
                            .map(|w| ir::convert_module(&curly_libs[a].1, w.clone(), &mut ir))
                            .collect()
                    })
                    .collect();

                // Use the IR errors to make a map of which modules from which library corresponds to each module name.
                let module_references =
                    handle_module_ir_errors(&curly_libs, &mod_files_contents, mod_files_asts, mod_files_ir_errors)?;

                // Use the previous map to make a different map of what modules are being pulled from each library.
                let lib_modules_to_unpack =
                    module_refs_to_file_modules(&curly_libs, module_references);

                // Extract the selected modules from each .curlylib file
                lib_modules_to_unpack
                    .keys()
                    .map(|v| {
                        extract_modules_from_file(v, lib_modules_to_unpack.get(v).unwrap().to_vec())
                    })
                    .for_each(drop);

                // Move the extracted .o and .a files to the .build/.linker_files directory
                copy_o_and_a_files("../.linker_files");

                // cd into the .build/.libraries directory
                std::env::set_current_dir("../.libraries")
                    .expect("Failed to cd into .build/.libraries.");

                // Copy static libraries and object files into the .build/.libraries directory
                copy_lib_files(&mut options);

                // Move the extracted .o and .a files to the .build/.linker_files directory
                copy_o_and_a_files("../.linker_files");

                // cd into the .build/.linker_files directory
                std::env::set_current_dir("../.linker_files")
                    .expect("Failed to cd into .build/.linker_files.");

                // Get the files in the linker_files directory with ".linker_files/" appended to the beginning of them
                let linker_files = get_files_in_current_dir_with_prepend(".linker_files");

                // cd into . directory
                std::env::set_current_dir("../..").expect("Failed to cd into .");
                let contents: Vec<String> = options
                    .inputs
                    .iter()
                    .map(|v| match fs::read_to_string(v) {
                        Ok(v) => v,
                        Err(e) => {
                            eprintln!("Error reading {}: {}", v, e);
                            std::process::exit(1);
                        }
                    })
                    .collect();

                // cd into .build directory
                std::env::set_current_dir(".build").expect("failed to cd into .build");

                let c = compile(
                    &options
                        .inputs
                        .iter()
                        .map(|v| (v.clone(), false))
                        .collect::<Vec<(String, bool)>>(),
                    &contents,
                    &mut ir,
                    options.mode == CompileMode::Executable,
                )?;

                for c in c.iter() {
                    match fs::write(&c.0, &c.1) {
                        Ok(_) => (),
                        Err(e) => {
                            eprintln!("error writing {}: {}", c.0, e);
                            return Err(());
                        }
                    }
                }

                let mut command = Command::new(COMPILER);

                for i in linker_files.iter() {
                    command.arg(i);
                }

                for c in c.iter() {
                    command.arg(&c.0);
                }

                for entry in fs::read_dir(".").expect("failed to read current directory") {
                    let entry_name = entry
                        .expect("failed to read file")
                        .file_name()
                        .into_string()
                        .expect("failed to convert file name to String");
                    if entry_name.ends_with(".o") && entry_name != "curly.o" {
                        command.arg(&entry_name);
                    }
                }

                for arg in options.compiler_options {
                    command.arg(&arg);
                }

                match options.mode {
                    CompileMode::Executable => {
                        command.arg("-o");
                        command.arg(&options.output);
                    }

                    CompileMode::Static => {
                        command.arg("-c");
                    }

                    CompileMode::Dynamic => {
                        unimplemented!("Dynamic libs are borked lol.");
                        /*
                        command.arg("-c");
                        command.arg("-fPIC");
                        command.arg("-shared");
                        */
                    }
                }

                command
                    .spawn()
                    .expect("failed to execute cc")
                    .wait()
                    .expect("failed to wait for cc");

                // Archive
                if options.mode != CompileMode::Executable {
                    make_included_lib_archive(&options.output);
                    copy_o_and_a_files(".lib_archive_files");

                    let filename = format!(".lib_archive_files/{}.mod.curly", options.output);
                    let contents = codegen::generate_module_file(&ir);
                    if DEBUG {
                        println!(
                            "================ {} ================\n{}\n\n",
                            filename, contents
                        );
                    }
                    match fs::write(&filename, contents) {
                        Ok(_) => (),
                        Err(e) => {
                            eprintln!("Error writing {}: {}", filename, e);
                            return Err(());
                        }
                    }

                    let mut command = Command::new("ar");
                    command.arg("-qc");

                    match options.mode {
                        CompileMode::Static => {
                            command.arg(&format!("{}.curlylib", options.output));
                        }

                        CompileMode::Dynamic => {
                            unimplemented!("Dynamic libs are borked lol.");
                            /*
                            command.arg(&format!("lib{}.so", options.output));
                            */
                        }

                        CompileMode::Executable => unreachable!("no"),
                    }

                    std::env::set_current_dir(".lib_archive_files")
                        .expect("Failed to cd into .build/.lib_archive_files.");

                    let files_in_curlylib =
                        get_files_in_current_dir_with_prepend(".lib_archive_files");

                    std::env::set_current_dir("..").expect("Failed to cd back into .build.");

                    for file in files_in_curlylib.iter() {
                        command.arg(file);
                    }

                    command
                        .spawn()
                        .expect("failed to execute ar")
                        .wait()
                        .expect("failed to wait for ar");

                    // Generate module file
                }
            }

            "check" => {
                match args.next() {
                    Some(file) => {
                        // Get file contents
                        let contents = match fs::read_to_string(&file) {
                            Ok(v) => v,
                            Err(e) => {
                                eprintln!("Error reading {}: {}", file, e);
                                return Err(());
                            }
                        };

                        // Execute file
                        let mut ir = IR {
                            modules: HashMap::with_capacity(0),
                        };
                        match curlyc::check(&[(file, false)], &[contents], &mut ir, true, true) {
                            Ok(_) => println!("No errors found"),
                            Err(_) => return Err(()),
                        }
                    }

                    None => {
                        println!(
                            "usage:
{} check [options] [file]
options:
    -m file             - Adds file as a compiled module
",
                            &name,
                        );
                    }
                }
            }

            _ => {
                println!(
                    "usage:
{} check [files]
{} build [options] [files]
",
                    &name, &name,
                );
            }
        }

        Ok(())
    }
}

fn handle_args(options: &mut CommandlineBuildOptions, args: &mut Args) -> Result<(), ()> {
    while let Some(a) = args.next() {
        match a.as_str() {
            "-o" => {
                if let Some(v) = args.next() {
                    options.output = v;
                } else {
                    eprintln!("Must specify an output file");
                    return Err(());
                }
            }

            "--dyn" => {
                options.mode = CompileMode::Dynamic;
            }

            "--lib" => {
                options.mode = CompileMode::Static;
            }

            "-m" => {
                if let Some(v) = args.next() {
                    options.curly_libs.push(v);
                } else {
                    eprintln!("must specify a curly library");
                    return Err(());
                }
            }

            "-L" => {
                if let Some(v) = args.next() {
                    options.libraries.push((v, LibEntryType::Directory));
                } else {
                    eprintln!("must specify a c library location");
                    return Err(());
                }
            }

            "-l" => {
                if let Some(v) = args.next() {
                    options.libraries.push((v, LibEntryType::LibraryPath));
                } else {
                    eprintln!("must specify a c library");
                    return Err(());
                }
            }

            "-O0" | "-O1" | "-O2" | "-O3" => {
                options.compiler_options.push(a);
            }

            _ => {
                if a.starts_with("-l") || a.starts_with("-L") {
                    options.compiler_options.push(a);
                } else {
                    options.inputs.push(a);
                }
            }
        }
    }

    Ok(())
}

fn post_process_args(options: &mut CommandlineBuildOptions, name: String) -> Result<(), ()> {
    if options.inputs.is_empty() {
        println!(
            "usage:
{} build [options] [file]
options:
    -o output           - Sets the output file
    -m file             - Adds file as a compiled module (must include corresponding compiled library)
    --lib               - Compiles as a static library
    --dyn               - Compiles as a dynamic library
    -On                 - Compiles with the given optimisation level; n can be 0, 1, 2, or 3
    -L /path/to/dir     - Adds /path/to/dir as a directory where cc can look for libraries
    -l lib              - adds lib as a library
",
            &name
        );
        return Err(());
    }

    if options.output.is_empty() {
        options.output = String::from("main");
    }
    Ok(())
}

fn make_dirs() -> Result<(), ()> {
    // Remove .build directory
    let _ = fs::remove_dir_all(".build");

    // Create .build directory
    match fs::create_dir(".build") {
        Ok(_) => (),
        Err(e) => {
            eprintln!("error creating .build directory: {}", e);
            return Err(());
        }
    }

    // cd into .build directory
    std::env::set_current_dir(".build").expect("failed to cd into .build");

    // Create .build/.libraries directory
    match fs::create_dir(".libraries") {
        Ok(_) => (),
        Err(e) => {
            eprintln!("error creating .build/.libraries directory: {}", e);
            return Err(());
        }
    }

    // Create .build/.linker_files directory
    match fs::create_dir(".linker_files") {
        Ok(_) => (),
        Err(e) => {
            eprintln!("error creating .build/.linker_files directory: {}", e);
            return Err(());
        }
    }

    // Create .build/.lib_archive_files directory
    match fs::create_dir(".lib_archive_files") {
        Ok(_) => (),
        Err(e) => {
            eprintln!("error creating .build/.lib_archive_files directory: {}", e);
            return Err(());
        }
    }

    // Create .build/.curly_libs directory
    match fs::create_dir(".curly_libs") {
        Ok(_) => (),
        Err(e) => {
            eprintln!("error creating curly .build/.curly_libs directory: {}", e);
            return Err(());
        }
    }

    // cd into .curly_libs/.build directory
    std::env::set_current_dir(".curly_libs").expect("failed to cd into .curly_libs");

    Ok(())
}

fn get_curly_libs(options: &mut CommandlineBuildOptions) -> Result<Vec<(String, String)>, ()> {
    let mut curly_libs = Vec::new();

    for (i, cur_lib) in options.curly_libs.iter().enumerate() {
        let path_to_file = if let Some(rel) = cur_lib.strip_prefix("~") {
            format!("{}/{}", dirs::home_dir().unwrap().to_str().unwrap(), rel)
        } else if cur_lib.starts_with('/') {
            String::from(cur_lib)
        } else {
            format!("../../{}", cur_lib)
        };

        let file_name = format!(
            "{}_{}",
            i,
            Path::new(&path_to_file)
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string(),
        );

        match fs::copy(&path_to_file, &file_name) {
            Ok(_) => (),
            Err(e) => {
                eprintln!(
                    "error copying {:?} to {:?}: {}",
                    &path_to_file, &file_name, e
                );
                return Err(());
            }
        }

        curly_libs.push(file_name);
    }

    let mut curly_mod_files: Vec<String> = Vec::new();

    for (i, cur_lib) in curly_libs.iter().enumerate() {
        let mut command = Command::new("ar");

        let mut mod_file_name = Path::new(&cur_lib)
            .file_stem()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();

        mod_file_name = mod_file_name[mod_file_name.find('_').unwrap() + 1..].to_string();

        mod_file_name = format!("{}.mod.curly", mod_file_name);

        command.arg("-x");
        command.arg(&cur_lib);
        command.arg(&mod_file_name);

        command
            .spawn()
            .expect("failed to execute ar")
            .wait()
            .expect("failed to wait for ar");

        let unique_mod_file_name = format!("{}_{}", i, &mod_file_name,);

        match fs::rename(&mod_file_name, &unique_mod_file_name) {
            Ok(_) => (),
            Err(e) => {
                eprintln!(
                    "error renaming {:?} to {:?}: {}",
                    &mod_file_name, &unique_mod_file_name, e
                );
                return Err(());
            }
        }

        curly_mod_files.push(unique_mod_file_name);
    }

    let mut lib_paths: Vec<(String, String)> = Vec::new();

    for (curly_lib, curly_mod) in curly_libs.iter().zip(curly_mod_files.iter()) {
        lib_paths.push((curly_lib.clone(), curly_mod.clone()));
    }

    Ok(lib_paths)
}

fn read_file(file_name: &str) -> Result<String, ()> {
    match fs::read_to_string(&file_name) {
        Ok(t) => Ok(t),
        Err(e) => {
            eprintln!("Error reading {}: {}", &file_name, e);
            Err(())
        }
    }
}

fn get_module_name_from_ast(ast: AST) -> Result<String, ()> {
    // Deal with the header
    if let AST::LibHeader(_, name, _) = ast {
        // Get module name
        let mut full_name = vec![];
        let mut top = *name;
        while let AST::Infix(_, _, l, r) = top {
            if let AST::Symbol(_, v) = *r {
                full_name.push(v);
            }

            top = *l;
        }

        if let AST::Symbol(_, v) = top {
            full_name.push(v);
        }

        full_name.reverse();
        let module_name = full_name.join("::");

        Ok(module_name)
    } else {
        Err(())
    }
}

fn handle_module_ir_errors(
    curly_libs: &[(String, String)],
    contents: &[String],
    mod_files_asts: Vec<Vec<AST>>,
    mod_files_ir_errors: Vec<Vec<Vec<IRError>>>,
) -> Result<HashMap<String, (usize, usize)>, ()> {
    let mut without_error = true;

    let mut module_references: HashMap<String, (usize, usize)> = HashMap::new();
    let mut files = SimpleFiles::new();
    let mut file_hash = HashMap::new();

    for file in curly_libs
        .iter()
        .map(|v| (v.1.clone(), true))
        .enumerate()
    {
        file_hash.insert(file.1.0.clone(), files.add(file.1.0.clone(), contents[file.0].clone()));
    }

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

    // skyler please learn more rust and fix this aaaaaaaaaaaaaaaaaaaaa
    for file_num in 0..mod_files_ir_errors.len() {
        for module_num in 0..mod_files_ir_errors[file_num].len() {
            if mod_files_ir_errors[file_num][module_num].is_empty() {
                module_references.insert(
                    get_module_name_from_ast(mod_files_asts[file_num][module_num].clone())
                        .expect("Failed to get module name from AST"),
                    (file_num, module_num),
                );
            } else {
                for error in mod_files_ir_errors[file_num][module_num].iter() {
                    match error {
                        IRError::DuplicateModule(v, t) => {
                            let mut diagnostic: Diagnostic<usize>;
                            match t {
                                DuplicateModuleInfo::NoSuperset => {
                                    diagnostic = Diagnostic::error();
                                    diagnostic = diagnostic.with_message(format!(
                                        "Duplicate module `{}`. Duplicates are incompatible.",
                                        v
                                    ));
                                    term::emit(&mut writer.lock(), &config, &files, &diagnostic)
                                        .unwrap();
                                    without_error = false;
                                }

                                DuplicateModuleInfo::NewSupersetOld
                                | DuplicateModuleInfo::OldSupersetNew => {
                                    diagnostic = Diagnostic::warning();
                                    diagnostic = diagnostic.with_message(format!("Duplicate module `{}`. Duplicates are compatible, but may still cause issues.", v));
                                    term::emit(&mut writer.lock(), &config, &files, &diagnostic)
                                        .unwrap();
                                    match t {
                                        DuplicateModuleInfo::OldSupersetNew => (),
                                        DuplicateModuleInfo::NewSupersetOld => {
                                            module_references
                                                .insert(v.to_string(), (file_num, module_num));
                                        }
                                        _ => (),
                                    }
                                }

                                DuplicateModuleInfo::BothSuperset => {
                                    module_references.insert(v.to_string(), (file_num, module_num));
                                }
                            }
                        }
                        _ => {
                            let mut diagnostic = Diagnostic::error();
                            match error {
                                IRError::InvalidType(s) => {
                                    diagnostic = diagnostic
                                        .with_message("Invalid type used")
                                        .with_labels(vec![Label::primary(
                                            *file_hash.get(&s.filename).unwrap(),
                                            s.span.clone(),
                                        )
                                        .with_message("Undeclared type")])
                                }

                                IRError::DuplicateTypeInUnion(s1, s2, t) => {
                                    diagnostic = diagnostic
                                        .with_message("Duplicate type in union type declaration")
                                        .with_labels(vec![
                                            Label::secondary(
                                                *file_hash.get(&s1.filename).unwrap(),
                                                s1.span.clone(),
                                            )
                                            .with_message("Type used here first"),
                                            Label::primary(
                                                *file_hash.get(&s2.filename).unwrap(),
                                                s2.span.clone(),
                                            )
                                            .with_message(format!(
                                                "Type `{}` used a second time here",
                                                t
                                            )),
                                        ])
                                }

                                IRError::DoubleExport(s1, s2, e) => {
                                    diagnostic = diagnostic
                                        .with_message("Value exported twice")
                                        .with_labels(vec![
                                            Label::secondary(
                                                *file_hash.get(&s1.filename).unwrap(),
                                                s1.span.clone(),
                                            )
                                            .with_message("Value exported here first"),
                                            Label::primary(
                                                *file_hash.get(&s2.filename).unwrap(),
                                                s2.span.clone(),
                                            )
                                            .with_message(format!(
                                                "Value {} exported a second time here",
                                                e
                                            )),
                                        ])
                                }

                                IRError::RedefineImportAlias(s1, s2, a) => {
                                    diagnostic = diagnostic
                                        .with_message("Alias defined twice")
                                        .with_labels(vec![
                                            Label::secondary(
                                                *file_hash.get(&s1.filename).unwrap(),
                                                s1.span.clone(),
                                            )
                                            .with_message("Alias defined here first"),
                                            Label::primary(
                                                *file_hash.get(&s2.filename).unwrap(),
                                                s2.span.clone(),
                                            )
                                            .with_message(format!(
                                                "Alias {} defined a second time here",
                                                a
                                            )),
                                        ])
                                }

                                IRError::UnsupportedAnnotation(s, a) => {
                                    diagnostic = diagnostic
                                        .with_message("Unsupported annotation used")
                                        .with_labels(vec![Label::primary(
                                            *file_hash.get(&s.filename).unwrap(),
                                            s.span.clone(),
                                        )
                                        .with_message(format!("Annotation {} is unsupported", a))])
                                }

                                IRError::InvalidFFIType(s, t) => {
                                    diagnostic = diagnostic
                                        .with_message("Unsupported type used for FFI")
                                        .with_labels(vec![Label::primary(
                                            *file_hash.get(&s.filename).unwrap(),
                                            s.span.clone(),
                                        )
                                        .with_message(format!("Type {} is unsupported by FFI", t))])
                                }

                                IRError::DuplicateModule(v, _) => {
                                    diagnostic =
                                        diagnostic.with_message(format!("Duplicate module `{}`", v))
                                }
                            }
                            term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
                            without_error = false;
                        }
                    }
                }
            }
        }
    }

    if !without_error {
        return Err(());
    }

    return Ok(module_references);
}

fn module_refs_to_file_modules(
    curly_libs: &[(String, String)],
    module_refs: HashMap<String, (usize, usize)>,
) -> HashMap<String, Vec<String>> {
    let mut file_module_names = HashMap::new();

    for lib in curly_libs {
        file_module_names.insert(lib.0.clone(), Vec::new());
    }

    for key in module_refs.keys() {
        let file_name = &curly_libs[module_refs.get(key).unwrap().0].0;
        let mut file_vector = file_module_names[file_name].clone();
        file_vector.push(key.to_string());
        file_module_names.insert(file_name.to_string(), file_vector);
    }

    file_module_names
}

fn extract_modules_from_file(file_name: &str, modules_to_unpack: Vec<String>) {
    for module in modules_to_unpack.iter() {
        let stem_name = get_c_name_from_curly_module_name(module);
        let module_object_file_name = format!("{}.o", &stem_name);
        let included_libs_file_name = format!("{}included_libs.a", &stem_name);

        let mut command = Command::new("ar");
        command.arg("-x");
        command.arg(&file_name);
        command.arg(&module_object_file_name);

        command
            .spawn()
            .expect("Failed to execute ar.")
            .wait()
            .expect("Failed to wait for ar.");

        let mut command = Command::new("ar");
        command.arg("-x");
        command.arg(&file_name);
        command.arg(&included_libs_file_name);

        command
            .spawn()
            .expect("Failed to execute ar.")
            .wait()
            .expect("Failed to wait for ar.");
    }

    copy_o_and_a_files("../.lib_archive_files");

    for module in &modules_to_unpack {
        let stem_name = get_c_name_from_curly_module_name(module);
        let included_libs_file_name = format!("{}included_libs.a", &stem_name);

        let mut command = Command::new("ar");
        command.arg("-x");
        command.arg(&included_libs_file_name);

        command
            .spawn()
            .expect("Failed to execute ar.")
            .wait()
            .expect("Failed to wait for ar.");

        match fs::remove_file(&included_libs_file_name) {
            Err(e) => eprintln!("Failed to remove file {} with error \"{:?}\"", &included_libs_file_name, e),
            _ => ()
        };
    }
}

fn get_c_name_from_curly_module_name(curly_module_name: &str) -> String {
    return format!("{}$", curly_module_name.replace("::", "$$DOUBLECOLON$$"));
}

fn copy_o_and_a_files(relative_path: &str) {
    for entry in fs::read_dir(".").expect("Failed to read current directory.") {
        let entry_name = entry
            .expect("Failed to read file.")
            .file_name()
            .into_string()
            .expect("Failed to convert file name to String.");

        let entry_extension = &entry_name[entry_name.rfind('.').unwrap()..];

        if entry_extension == ".o" || entry_extension == ".a" {
            match fs::copy(&entry_name, format!("{}/{}", relative_path, &entry_name)) {
                Ok(_) => (),
                Err(e) => {
                    println!("failed to move file {}: {}", &entry_name, e);
                    std::process::exit(1);
                }
            }
        }
    }
}

fn copy_lib_files(options: &mut CommandlineBuildOptions) {
    let target_dir = std::env::current_dir()
        .expect("Failed to get path of current dir.")
        .to_str()
        .expect("Failed to convert file extension to &str.")
        .to_string();
    std::env::set_current_dir("../..").expect("Failed to cd into .");

    for library_param in &options.libraries {
        match &library_param.1 {
            LibEntryType::Directory => {
                let error_message = &format!("Failed to cd into {}.", &library_param.0);

                if let Some(rel) = library_param.0.strip_prefix("~") {
                    std::env::set_current_dir(format!(
                        "{}{}",
                        dirs::home_dir().unwrap().to_str().unwrap(),
                        rel
                    ))
                    .expect(error_message);
                } else {
                    std::env::set_current_dir(&target_dir)
                        .expect("Failed to cd into .build/.libraries.");
                    std::env::set_current_dir("../..").expect("Failed to cd into .");

                    std::env::set_current_dir(String::from(&library_param.0)).expect(error_message);
                }
            }

            LibEntryType::LibraryPath => {
                let mut file_path =
                    std::env::current_dir().expect("Failed to get path of current dir.");
                if let Some(rel) = library_param.0.strip_prefix("~") {
                    file_path.push(format!(
                        "{}{}",
                        dirs::home_dir().unwrap().to_str().unwrap(),
                        rel
                    ));
                } else {
                    file_path.push(String::from(&library_param.0));
                }
                let file_name = file_path
                    .file_name()
                    .expect("Failed to convert to extension")
                    .to_str()
                    .expect("Failed to convert file extension to &str.");
                let file_type = file_path
                    .extension()
                    .expect("Failed to convert to extension")
                    .to_str()
                    .expect("Failed to convert file extension to &str.");

                if file_type == "o" || file_type == "a" {
                    fs::copy(&file_path, format!("{}/{}", target_dir, file_name))
                        .unwrap_or_else(|_| panic!("Failed to copy {:?} to file \"{}/{}\".", &file_path, target_dir, file_name));
                } else {
                    options.compiler_options.push(
                        file_path
                            .to_str()
                            .expect("Failed to convert to &str.")
                            .to_string(),
                    );
                }
            }
        }
    }

    std::env::set_current_dir(target_dir).expect("Failed to set dir back to .build/.linker_files.");
}

fn get_files_in_current_dir_with_prepend(prepend_string: &str) -> Vec<String> {
    let mut entry_list = Vec::new();
    for entry in fs::read_dir(".").expect("Failed to read current directory.") {
        let entry_name = entry
            .expect("Failed to read file.")
            .file_name()
            .into_string()
            .expect("Failed to convert file name to String.");

        entry_list.push(format!("{}/{}", prepend_string, &entry_name));
    }
    entry_list
}

fn make_included_lib_archive(module_name: &str) {
    let archive_name = format!(
        "{}included_libs.a",
        get_c_name_from_curly_module_name(module_name)
    );

    std::env::set_current_dir(".libraries").expect("Failed to cd into .build/.libraries.");

    let files_to_include = get_files_in_current_dir_with_prepend("../.libraries");

    std::env::set_current_dir("../.lib_archive_files")
        .expect("Failed to cd into .build/.lib_archive_files.");

    let mut command = Command::new("ar");
    command.arg("-qc");
    command.arg(&archive_name);
    for i in files_to_include.iter() {
        command.arg(i);
    }
    command.arg("/dev/null");
    command
        .spawn()
        .expect("failed to execute ar")
        .wait()
        .expect("failed to wait for ar");

    let mut command = Command::new("ar");
    command.arg("-d");
    command.arg(&archive_name);
    command.arg("null");
    command
        .spawn()
        .expect("failed to execute ar")
        .wait()
        .expect("failed to wait for ar");

    std::env::set_current_dir("..").expect("Failed to cd back into .build");
}

// compile(&Vec<(String, bool)>, &Vec<(String, bool)>, &mut IR, bool) -> Result<String, ()>
// Compiles curly into C code.
fn compile(
    filenames: &[(String, bool)],
    codes: &[String],
    ir: &mut IR,
    require_main: bool,
) -> Result<Vec<(String, String)>, ()> {
    // Check the file
    match curlyc::check(filenames, codes, ir, require_main, true) {
        Ok(_) => (),
        Err(_) => return Err(()),
    }

    // Generate C code
    let c = codegen::convert_ir_to_c(&ir);
    if DEBUG {
        for (filename, contents) in c.iter() {
            println!(
                "================ {} ================\n{}\n\n",
                filename, contents
            );
        }
    }

    Ok(c)
}
