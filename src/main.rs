use std::collections::HashMap;
use std::env;
use std::fs;
use std::process::Command;
use std::path::PathBuf;

use curlyc::backends::c::codegen;
use curlyc::frontend::ir::IR;

pub static DEBUG: bool = false;

#[derive(PartialEq)]
enum CompileMode {
    Executable,
    Dynamic,
    Static,
}

struct CommandlineBuildOptions {
    output: String,
    inputs: Vec<String>,
    libraries: Vec<String>,
    compiled_mods: Vec<String>,
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
{} build [options] [files]
",
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
                    compiled_mods: Vec::with_capacity(0),
                    compiler_options: Vec::with_capacity(0),
                    mode: CompileMode::Executable,
                };

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
                                options.compiled_mods.push(v);
                            } else {
                                eprintln!("must specify a module file");
                                return Err(());
                            }
                        }
                        "-O0" | "-O1" | "-O2" | "-O3" => {
                            options.compiler_options.push(a);
                        }
                        _ => {
                            if a.starts_with("-L") || a.starts_with("-l") {
                                options.libraries.push(a);
                            } else {
                                options.inputs.push(a);
                            }
                        }
                    }
                }

                if options.inputs.is_empty() {
                    println!("usage:
{} build [options] [file]
options:
    -o output           - Sets the output file
    -m file             - Adds file as a compiled module (must include corresponding compiled library)
    --lib               - Compiles as a static library
    --dyn               - Compiles as a dynamic library
    -On                 - Compiles with the given optimisation level; n can be 0, 1, 2, or 3
    -L/path/to/dir      - Adds /path/to/dir as a directory where cc can look for libraries
    -llib               - adds lib as a library
", &name);
                    return Err(());
                }

                if options.output.is_empty() {
                    options.output = String::from("main");
                }

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
                    .chain(
                        options
                            .compiled_mods
                            .iter()
                            .map(|v| match fs::read_to_string(v) {
                                Ok(v) => v,
                                Err(e) => {
                                    eprintln!("Error reading {}: {}", v, e);
                                    std::process::exit(1);
                                }
                            }),
                    )
                    .collect();

                let mut ir = IR {
                    modules: HashMap::with_capacity(0),
                };
                let c = compile(
                    &options
                        .inputs
                        .iter()
                        .map(|v| (v.clone(), false))
                        .chain(options.compiled_mods.into_iter().map(|v| (v, true)))
                        .collect::<Vec<(String, bool)>>(),
                    &contents,
                    &mut ir,
                    options.mode == CompileMode::Executable,
                )?;

                // Create .build
                let _ = fs::remove_dir_all(".build");

                match fs::create_dir(".build") {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("error creating build directory: {}", e);
                        return Err(());
                    }
                }

                std::env::set_current_dir(".build").expect("failed to cd into .build");

                match fs::create_dir(".libraries") {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("error creating archive directory: {}", e);
                        return Err(());
                    }
                }

                let return_location = PathBuf::from(&std::env::current_dir().unwrap());
                let mut current_path = String::with_capacity(0);
                let mut copy_to_location = return_location.clone();
                copy_to_location.push(".libraries");
                for lib in options.libraries {
                    if let Some(lib) = lib.strip_prefix("-L") {
                        current_path = if let Some(rel) = lib.strip_prefix("~") {
                            format!("{}/{}", dirs::home_dir().unwrap().to_str().unwrap(), rel)
                        } else {
                            String::from(lib)
                        };
                    } else {
                        match std::env::set_current_dir(&current_path) {
                            Ok(_) => (),
                            Err(e) => {
                                eprintln!("failed to cd into {:?}: {}", &current_path, e);
                                return Err(())
                            }
                        }

                        let sub = format!("lib{}.a", &lib[2..]);
                        let mut target_file = copy_to_location.clone();
                        target_file.push(&sub);

                        match fs::copy(&sub, &target_file) {
                            Ok(_) => (),
                            Err(e) => {
                                eprintln!("error copying {} to {:?}/.libraries: {}", &sub, &target_file, e);
                                return Err(());
                            }
                        }

                        std::env::set_current_dir(&copy_to_location).expect("failed to cd to .build/.libraries");
                        
                        let dirname : String = (&sub).chars().into_iter().take((&sub).len() - 2).collect();
                        match fs::create_dir(&dirname) {
                            Ok(_) => (),
                            Err(e) => {
                                eprintln!("error creating {} directory: {}", &dirname, e);
                                return Err(());
                            }
                        }
                        match std::env::set_current_dir(&dirname) {
                            Ok(_) => (),
                            Err(e) => {
                                eprintln!("failed to cd into {}: {}", &dirname, e);
                                return Err(());
                            }
                        }
                        
                        let mut command = Command::new("ar");
                        command.arg("-x");
                        command.arg(&format!("../{}", sub));

                        command
                            .spawn()
                            .expect("failed to execute ar")
                            .wait()
                            .expect("failed to wait for ar");
                        
                        for entry in fs::read_dir(".").expect("failed to read current directory") {
                            let entry_name = 
                                entry
                                .expect("failed to read file")
                                .file_name()
                                .into_string().expect("failed to convert file name to String");
                            if entry_name.ends_with(".o") {
                                match fs::copy(&entry_name, format!("../../{}", &entry_name)) {
                                    Ok(_) => (),
                                    Err(e) => {
                                        eprintln!("failed to cd into {}: {}", &dirname, e);
                                        return Err(());
                                    }
                                }
                            }
                        }
                    }
                }
                std::env::set_current_dir(return_location).expect("failed to return to .build");

                for c in c.iter() {
                    match fs::write(&c.0, &c.1) {
                        Ok(_) => (),
                        Err(e) => {
                            eprintln!("error writing {}: {}", c.0, e);
                            return Err(());
                        }
                    }
                }

                let mut command = Command::new("gcc");

                for c in c.iter() {
                    command.arg(&c.0);
                }

                for entry in fs::read_dir(".").expect("failed to read current directory") {
                    let entry_name = 
                        entry
                        .expect("failed to read file")
                        .file_name()
                        .into_string().expect("failed to convert file name to String");
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
                        command.arg("-c");
                        command.arg("-fPIC");
                        command.arg("-shared");
                    }
                }

                command
                    .spawn()
                    .expect("failed to execute cc")
                    .wait()
                    .expect("failed to wait for cc");

                // Archive
                if options.mode != CompileMode::Executable {
                    let mut command = Command::new("ar");
                    command.arg("-rc");

                    match options.mode {
                        CompileMode::Static => {
                            command.arg(&format!("lib{}.a", options.output));
                        }

                        CompileMode::Dynamic => {
                            command.arg(&format!("lib{}.so", options.output));
                        }

                        CompileMode::Executable => unreachable!("no"),
                    }
                    for entry in fs::read_dir(".").expect("failed to read current directory") {
                        let entry_name = 
                            entry
                            .expect("failed to read file")
                            .file_name()
                            .into_string().expect("failed to convert file name to String");
                        if entry_name.ends_with(".o") {
                            command.arg(&entry_name);
                        }
                    }
                    command
                        .spawn()
                        .expect("failed to execute ar")
                        .wait()
                        .expect("failed to wait for ar");

                    // Generate module file
                    let filename = format!("{}.mod.curly", options.output);
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
                            &name
                        );
                    }
                }
            }

            _ => {
                println!(
                    "usage:
{} check [options] [files]
{} build [options] [files]
",
                    &name, &name
                );
            }
        }

        Ok(())
    }
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
        Err(_) => return Err(())
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
