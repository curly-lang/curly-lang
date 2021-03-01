use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::process::Command;

use curlyc::backends::c::codegen;
use curlyc::frontend::correctness;
use curlyc::frontend::correctness::CorrectnessError;
use curlyc::frontend::ir;
use curlyc::frontend::ir::{IR, IRError};
use curlyc::frontend::parser;

static DEBUG: bool = true;

#[derive(PartialEq)]
enum CompileMode
{
    Executable,
    Dynamic,
    Static
}

struct CommandlineBuildOptions
{
    output: String,
    inputs: Vec<String>,
    compiler_options: Vec<String>,
    mode: CompileMode
}

fn main() -> Result<(), ()>
{
    let args = env::args();
    let mut args = args.into_iter();
    let name = args.next().unwrap();

    if args.len() == 0
    {
        println!("usage:
{} check [files]
{} build [options] [files]
", &name, &name);
        Ok(())
    } else
    {
        match args.next().unwrap().as_str()
        {
            "build" => {
                let mut options = CommandlineBuildOptions {
                    output: String::with_capacity(0),
                    inputs: Vec::with_capacity(0),
                    compiler_options: Vec::with_capacity(0),
                    mode: CompileMode::Executable
                };

                while let Some(a) = args.next()
                {
                    match a.as_str()
                    {
                        "-o" => {
                            if let Some(v) = args.next()
                            {
                                options.output = v;
                            } else
                            {
                                println!("Must specify an output file");
                                return Err(());
                            }
                        }

                        "--dyn" => {
                            options.mode = CompileMode::Dynamic;
                        }

                        "--lib" => {
                            options.mode = CompileMode::Static;
                        }

                        _ => {
                            if a.starts_with("-L") || a.starts_with("-l")
                            {
                                options.compiler_options.push(a);
                            } else
                            {
                                options.inputs.push(a);
                            }
                        }
                    }
                }

                if options.inputs.len() == 0
                {
                    println!("usage:\n{} build [options] [file]\noptions:\n-o - Sets the output file", &name);
                    return Err(());
                }

                if options.output == ""
                {
                    options.output = String::from("main");
                }

                let contents: Vec<String> = options.inputs.iter().map(
                    |v| match fs::read_to_string(v)
                        {
                            Ok(v) => v,
                            Err(e) => {
                                eprintln!("Error reading {}: {}", v, e);
                                std::process::exit(1);
                            }
                        }
                ).collect();

                let mut ir = IR {
                    modules: HashMap::with_capacity(0)
                };
                let c = compile(&options.inputs, &contents, &mut ir, options.mode == CompileMode::Executable)?;

                // Create .build
                match fs::remove_dir_all(".build")
                {
                    _ => ()
                }

                match fs::create_dir(".build")
                {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("error creating build directory: {}", e);
                        return Err(());
                    }
                }

                std::env::set_current_dir(".build").expect("failed to move to .build");
                for c in c.iter()
                {
                    match fs::write(&c.0, &c.1)
                    {
                        Ok(_) => (),
                        Err(e) => {
                            eprintln!("error writing {}: {}", c.0, e);
                            return Err(());
                        }
                    }
                }

                let mut command = Command::new("gcc");

                for c in c.iter()
                {
                    command.arg(&c.0);
                }

                for arg in options.compiler_options
                {
                    command.arg(&arg);
                }

                match options.mode
                {
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
                    }
                }

                command.spawn().expect("failed to execute cc").wait().expect("failed to wait for cc");

                // Archive
                if options.mode != CompileMode::Executable
                {
                    let mut command = Command::new("ar");
                    command.arg("-rc");

                    match options.mode
                    {
                        CompileMode::Static => {
                            command.arg(&format!("lib{}.a", options.output));
                        }

                        CompileMode::Dynamic => {
                            command.arg(&format!("lib{}.so", options.output));
                        }

                        CompileMode::Executable => unreachable!("no")
                    }
                    
                    for c in c
                    {
                        if c.0.ends_with(".c")
                        {
                            command.arg(&format!("{}o", &c.0[..c.0.len() - 1]));
                        }
                    }
                    command.spawn().expect("failed to execute ar").wait().expect("failed to wait for ar");
                }
            }

            "check" => {
                match args.next()
                {
                    Some(file) => {
                        // Get file contents
                        let contents = match fs::read_to_string(&file)
                        {
                            Ok(v) => v,
                            Err(e) => {
                                eprintln!("Error reading {}: {}", file, e);
                                return Err(());
                            }
                        };

                        // Execute file
                        let mut ir = IR {
                            modules: HashMap::with_capacity(0)
                        };
                        match check(&vec![file], &vec![contents], &mut ir, true)
                        {
                            Ok(_) => println!("No errors found"),
                            Err(_) => return Err(())
                        }
                    }

                    None => {
                        println!("usage:\n{} check [file]", &name);
                    }
                }
            }

            _ => {
                println!("usage:
{} check [files]
{} build [options] [files]
", &name, &name);
            }
        }

        Ok(())
    }
}

// check(&Vec<String>, &Vec<String>, &mut IR, bool) -> Result<(), ()>
// Checks whether given code is valid.
fn check(filenames: &Vec<String>, codes: &Vec<String>, ir: &mut IR, require_main: bool) -> Result<(), ()>
{
    // Set up codespan
    let mut files = SimpleFiles::new();
    let mut file_hash = HashMap::new();
    for file in filenames.iter().enumerate()
    {
        file_hash.insert(file.1, files.add(file.1, codes[file.0].clone()));
    }
    let file_hash = file_hash;

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

    let mut fail = false;
    for file in filenames.iter().enumerate()
    {
        let code = &codes[file.0];
        let file_id = *file_hash.get(file.1).unwrap();

        // Generate the ast
        let ast = match parser::parse(code)
        {
            Ok(v) => v,
            Err(e) => {
                let diagnostic = Diagnostic::error()
                                    .with_message(&e.msg)
                                    .with_labels(vec![
                                        Label::primary(file_id, e.span)
                                    ]);
                term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
                return Err(());
            }
        };

        // Print out the ast
        if DEBUG { println!("{:#?}", &ast); }
        match ir::convert_ast_to_ir(&file.1, ast, ir)
        {
            Ok(_) if DEBUG => { dbg!(&ir); }
            Ok(_) => (),
            Err(e) => {
                for e in e
                {
                    let mut diagnostic = Diagnostic::error();
                    match e
                    {
                        IRError::InvalidType(s) => {
                            diagnostic = diagnostic
                                .with_message("Invalid type used")
                                .with_labels(vec![
                                    Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                    .with_message("Undeclared type")
                                ])
                        }

                        IRError::DuplicateTypeInUnion(s1, s2, t) => {
                            diagnostic = diagnostic
                                .with_message("Duplicate type in union type declaration")
                                .with_labels(vec![
                                    Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                    .with_message("Type used here first"),
                                    Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                    .with_message(format!("Type `{}` used a second time here", t))
                                ])
                        }

                        IRError::DoubleExport(s1, s2, e) => {
                            diagnostic = diagnostic
                                .with_message("Value exported twice")
                                .with_labels(vec![
                                    Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                    .with_message("Value exported here first"),
                                    Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                    .with_message(format!("Value {} exported a second time here", e)),
                                ])
                        }

                        IRError::RedefineImportAlias(s1, s2, a) => {
                            diagnostic = diagnostic
                                .with_message("Alias defined twice")
                                .with_labels(vec![
                                    Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                    .with_message("Alias defined here first"),
                                    Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                    .with_message(format!("Alias {} defined a second time here", a)),
                                ])
                        }

                        IRError::UnsupportedAnnotation(s, a) => {
                            diagnostic = diagnostic
                                .with_message("Unsupported annotation used")
                                .with_labels(vec![
                                    Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                    .with_message(format!("Annotation {} is unsupported", a)),
                                ])
                        }

                        IRError::InvalidFFIType(s, t) => {
                            diagnostic = diagnostic
                                .with_message("Unsupported type used for FFI")
                                .with_labels(vec![
                                    Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                    .with_message(format!("Type {} is unsupported by FFI", t))
                                ])
                        }
                    }
                    term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
                }
                fail = true;
            }
        }
    }

    if fail
    {
        return Err(());
    }

    // Check correctness
    let err = correctness::check_correctness(ir, require_main);

    // Print out the ir or the error
    match err
    {
        Ok(_) if DEBUG => {
            dbg!(ir);
            Ok(())
        }

        Ok(_) => Ok(()),

        Err(e) => {
            for e in e
            {
                let mut diagnostic = Diagnostic::error();
                match e
                {
                    CorrectnessError::UndefinedPrefixOp(s, _, t) =>
                        diagnostic = diagnostic
                            .with_message("Undefined prefix operator")
                            .with_labels(vec![
                                Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                .with_message(format!("`-` is undefined on `{}`", t))
                            ]),

                    CorrectnessError::UndefinedInfixOp(s, op, l, r) =>
                        diagnostic = diagnostic
                            .with_message("Undefined infix operator")
                            .with_labels(vec![
                                Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                .with_message(format!("`{:?}` is undefined on `{}` and `{}`", op, l, r))
                            ]),

                    CorrectnessError::NonboolInBoolExpr(s, t) =>
                        diagnostic = diagnostic
                            .with_message("Nonboolean in boolean expression")
                            .with_labels(vec![
                                Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                .with_message(format!("Expected `Bool`, got `{}`", t))
                            ]),

                    CorrectnessError::NonboolInIfCond(s, t) =>
                        diagnostic = diagnostic
                            .with_message("Nonboolean in if condition")
                            .with_labels(vec![
                                Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                .with_message(format!("Expected `Bool`, got `{}`", t))
                            ]),

                    CorrectnessError::NonmatchingAssignTypes(s1, t1, s2, t2) =>
                        diagnostic = diagnostic
                            .with_message("Nonmatching types in assignment")
                            .with_labels(vec![
                                Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                .with_message(format!("Assignment is declared with type `{}`", t1)),
                                Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                .with_message(format!("Expected `{}`, got `{}`", t1, t2))
                            ]),

                    CorrectnessError::SymbolNotFound(s, v) =>
                        diagnostic = diagnostic
                            .with_message("Symbol not found")
                            .with_labels(vec![
                                Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                .with_message(format!("Could not find symbol `{}`", v))
                            ]),

                    CorrectnessError::Reassignment(s1, s2, v) =>
                        diagnostic = diagnostic
                            .with_message("Redefinition of previously declared variable")
                            .with_labels(vec![
                                Label::primary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                .with_message(format!("`{}` is already defined and not declared as mutable", v)),
                                Label::secondary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                .with_message(format!("`{}` previously defined here", v))
                            ]),

                    CorrectnessError::InvalidType(s) =>
                        diagnostic = diagnostic
                            .with_message("Invalid type used")
                            .with_labels(vec![
                                Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                .with_message("Undeclared type")
                            ]),

                    CorrectnessError::DuplicateTypeInUnion(s1, s2, t) =>
                        diagnostic = diagnostic
                            .with_message("Duplicate type in union type declaration")
                            .with_labels(vec![
                                Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                .with_message("Type used here first"),
                                Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                .with_message(format!("Type `{}` used a second time here", t))
                            ]),

                    CorrectnessError::UnknownFunctionReturnType(s, v) =>
                        diagnostic = diagnostic
                            .with_message("Could not determine the return type of the function")
                            .with_labels(vec![
                                Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                .with_message(format!("Could not determine return type for `{}`", v))
                            ]),

                    CorrectnessError::MismatchedFunctionArgType(s, t1, t2) =>
                        diagnostic = diagnostic
                            .with_message("Wrong type passed as an argument")
                            .with_labels(vec![
                                Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                .with_message(format!("Expected `{}`, got `{}`", t1, t2))
                            ]),

                    CorrectnessError::InvalidApplication(s, t) => {
                        diagnostic = diagnostic
                            .with_message("Invalid application")
                            .with_labels(vec![
                                Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                .with_message(format!("Expected function, got `{}`", t))
                            ]);
                    }

                    CorrectnessError::InvalidCast(s1, t1, s2, t2) => {
                        diagnostic = diagnostic
                            .with_message("Invalid cast")
                            .with_labels(vec![
                                Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                .with_message(format!("Value has type `{}`", t1)),
                                Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                .with_message(format!("Cannot convert `{}` to `{}`", t1, t2))
                            ]);
                    }

                    CorrectnessError::NonSubtypeOnMatch(s1, t1, s2, t2) => {
                        diagnostic = diagnostic
                            .with_message("Nonsubtype checked for in match arm")
                            .with_labels(vec![
                                Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                .with_message(format!("Value has type `{}`", t1)),
                                Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                .with_message(format!("`{}` is not a subtype of `{}`", t2, t1))
                            ]);
                    }

                    CorrectnessError::InfiniteSizedType(s, t) => {
                        diagnostic = diagnostic
                            .with_message("Type has infinite size")
                            .with_labels(vec![
                                Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                .with_message(format!("Type `{}` has infinite size", t))
                            ]);
                    }

                    CorrectnessError::NonmemberAccess(s, a, b) => {
                        diagnostic = diagnostic
                            .with_message("Attempted to access a member that does not exist")
                            .with_labels(vec![
                                Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                .with_message(format!("`{}` has no member `{}`", a, b))
                            ]);
                    }

                    CorrectnessError::MismatchedDeclarationAssignmentTypes(s1, t1, s2, t2) => {
                        diagnostic = diagnostic
                            .with_message("Assignment of variable declared with incompatible type")
                            .with_labels(vec![
                                Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                .with_message(format!("Variable declared here with type {}", t1)),
                                Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                .with_message(format!("Assignment of value with type {} to variable of type {}", t2, t1)),
                            ]);
                    }

                    CorrectnessError::VariableImportedTwice(s1, s2) => {
                        diagnostic = diagnostic
                            .with_message("Variable imported twice")
                            .with_labels(vec![
                                Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                .with_message("Variable declared here at first"),
                                Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                .with_message("Variable redeclared here"),
                            ]);
                    }

                    CorrectnessError::ImportedValueNotExported(s, v, i) => {
                        diagnostic = diagnostic
                            .with_message("Imported value not exported")
                            .with_labels(vec![
                                Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                .with_message(format!("{} not exported by {}", v, i))
                            ]);
                    }

                    CorrectnessError::NoMainFunction => {
                        diagnostic = diagnostic
                            .with_message("No main function found");
                    }

                    CorrectnessError::CurriedExternalFunc(s) => {
                        diagnostic = diagnostic
                            .with_message("Cannot curry external function")
                            .with_labels(vec![
                                Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                .with_message("Curried function found here")
                            ])
                    }

                    CorrectnessError::ImpureInPure(s1, s2) => {
                        diagnostic = diagnostic
                            .with_message("Cannot use impure function in pure function")
                            .with_labels(vec![
                                Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                .with_message("Function defined as pure"),
                                Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                .with_message("Impurity found here")
                            ])
                    }

                    CorrectnessError::UnnecessaryImpure(s) => {
                        diagnostic = diagnostic
                            .with_message("Unnecessary impurity")
                            .with_labels(vec![
                                Label::secondary(*file_hash.get(&s.filename).unwrap(), s.span)
                                .with_message("Function defined as impure but has no impurities")
                            ])
                    }

                    CorrectnessError::AppliedImpureToPure(s) => {
                        diagnostic = diagnostic
                            .with_message("Applied impure function to pure function")
                            .with_labels(vec![
                                Label::primary(*file_hash.get(&s.filename).unwrap(), s.span)
                                .with_message("Function defined as impure but passed to pure function")
                            ])
                    }
                }
                term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
            }
            Err(())
        }
    }
}

// compile(&str, &str, &mut IR, bool) -> Result<String, ()>
// Compiles curly into C code.
fn compile(filenames: &Vec<String>, codes: &Vec<String>, ir: &mut IR, require_main: bool) -> Result<Vec<(String, String)>, ()>
{
    // Check the file
    check(filenames, codes, ir, require_main)?;

    // Generate C code
    let c = codegen::convert_ir_to_c(&ir);
    if DEBUG
    {
        for (filename, contents) in c.iter()
        {
            println!("================ {} ================\n{}\n\n", filename, contents);
        }
    }

    Ok(c)
}

