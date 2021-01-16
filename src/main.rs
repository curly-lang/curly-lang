use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::collections::HashMap;
use std::env;
use std::ffi::CString;
use std::fs;
use std::process::{Command, Stdio};
use libtcc::{Context, Guard, OutputType};

use curlyc::backends::c::codegen;
use curlyc::frontend::correctness;
use curlyc::frontend::correctness::CorrectnessError;
use curlyc::frontend::ir;
use curlyc::frontend::ir::IR;
use curlyc::frontend::parser;
use curlyc::frontend::types::Type;

enum CBackendCompiler
{
    TCC,
    GCC,
    Clang
}

struct CommandlineBuildOptions
{
    compiler: CBackendCompiler,
    output: String,
    input: String
}

fn main() -> Result<(), ()>
{
    let args = env::args();

    if args.len() == 1
    {
        repl();
        Ok(())
    } else
    {
        let mut args = args.into_iter();
        let name = args.next().unwrap();

        match args.next().unwrap().as_str()
        {
            "build" => {
                let mut options = CommandlineBuildOptions {
                    compiler: CBackendCompiler::TCC,
                    output: String::with_capacity(0),
                    input: String::with_capacity(0)
                };

                while let Some(a) = args.next()
                {
                    match a.as_str()
                    {
                        "--compiler" => {
                            if let Some(v) = args.next()
                            {
                                match v.as_str()
                                {
                                    "gcc" => options.compiler = CBackendCompiler::GCC,
                                    "tcc" => options.compiler = CBackendCompiler::TCC,
                                    "clang" => options.compiler = CBackendCompiler::Clang,
                                    _ => {
                                        println!("Supported C compilers are gcc, tcc, and clang");
                                        return Err(());
                                    }
                                }
                            } else
                            {
                                println!("Must specify a compiler to use");
                                return Err(());
                            }
                        }

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

                        _ => {
                            options.input = a;
                        }
                    }
                }

                if options.input == ""
                {
                    println!("usage:\n{} build [options] [file]\noptions:\n--compiler - Sets the C compiler for the backend; supported compilers are gcc, tcc, and clang\n-o - Sets the output file", &name);
                    return Err(());
                }

                if options.output == ""
                {
                    options.output = String::from(options.input.split(".").into_iter().next().unwrap());
                }

                let contents = match fs::read_to_string(&options.input)
                {
                    Ok(v) => v,
                    Err(e) => {
                        eprintln!("Error reading file: {}", e);
                        return Err(());
                    }
                };

                let mut ir = IR::new();
                let c = compile(&options.input, &contents, &mut ir, false)?;

                let mut echo = Command::new("echo")
                        .arg(&c)
                        .stdout(Stdio::piped())
                        .spawn()
                        .expect("Failed to execute echo");
                echo.wait().expect("Failed to wait for echo");

                match options.compiler
                {
                    CBackendCompiler::TCC => {
                        Command::new("tcc")
                                .arg("-o")
                                .arg(&options.output)
                                .arg("-")
                                .stdin(Stdio::from(echo.stdout.expect("Failed to get stdout")))
                                .spawn()
                                .expect("Failed to execute tcc")
                                .wait()
                                .expect("Failed to wait for tcc");

                    }

                    CBackendCompiler::GCC => {
                        Command::new("gcc")
                                .arg("-x")
                                .arg("c")
                                .arg("-o")
                                .arg(&options.output)
                                .arg("-")
                                .stdin(Stdio::from(echo.stdout.expect("Failed to get stdout")))
                                .spawn()
                                .expect("Failed to execute gcc")
                                .wait()
                                .expect("Failed to wait for gcc");

                    }

                    CBackendCompiler::Clang => {
                        Command::new("clang")
                                .arg("-x")
                                .arg("c")
                                .arg("-o")
                                .arg(&options.output)
                                .arg("-")
                                .stdin(Stdio::from(echo.stdout.expect("Failed to get stdout")))
                                .spawn()
                                .expect("Failed to execute clang")
                                .wait()
                                .expect("Failed to wait for clang");
                    }
                }

            }

            "run" => {
                match args.next()
                {
                    Some(file) => {
                        // Get file contents
                        let contents = match fs::read_to_string(&file)
                        {
                            Ok(v) => v,
                            Err(e) => {
                                eprintln!("Error reading file: {}", e);
                                return Err(());
                            }
                        };

                        // Execute file
                        let mut ir = IR::new();
                        let mut guard = Guard::new()
                            .expect("unable to initialise tcc guard");

                        execute(&file, &contents, &mut ir, None, &mut guard);
                    }

                    None => {
                        println!("usage:\n{} run [file]", &name);
                    }
                }
            }

            _ => {
                println!("usage:
{} run [file]
{} build [options] [file]
 ", &name, &name);
            }
        }

        Ok(())
    }
}

#[derive(Debug)]
#[repr(C)]
struct REPLFunc
{
    refc: u32,
    func: fn(),
    wrapper: fn(),
    arity: u32,
    argc: u32,
    args: fn()
}

#[derive(Debug)]
enum REPLValue
{
    Int(i64),
    Float(f64),
    Bool(bool),
    Func(REPLFunc)
}

// repl() -> ()
// Executes the REPL.
fn repl()
{
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err()
    {
        println!("No previous history.");
    }

    // Set up tcc jit
    let mut guard = Guard::new()
        .expect("unable to initialise tcc guard");
    let mut ir = IR::new();
    let mut vars: HashMap<String, REPLValue> = HashMap::new();

    loop
    {
        // Get line
        let readline = rl.readline(">>> ");
        match readline
        {
            Ok(line) => {
                // Quitting
                if line == ":q" || line == ":quit"
                {
                    break;
                }

                rl.add_history_entry(line.as_str());
                execute("<stdin>", &line, &mut ir, Some(&mut vars), &mut guard);
            }

            // Errors
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }

            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }

            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    rl.save_history("history.txt").unwrap();
}

// compile(&str, &str, &mut IR, bool) -> Result<String, ()>
// Compiles curly into C code.
fn compile(filename: &str, code: &str, ir: &mut IR, repl_mode: bool) -> Result<String, ()>
{
    // Set up codespan
    let mut files = SimpleFiles::new();
    let file_id = files.add(filename, code);
    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

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
    println!("{:#?}", &ast);
    ir.clear();
    ir::convert_ast_to_ir(ast, ir);
    dbg!(&ir);

    // Check correctness
    let err = correctness::check_correctness(ir);

    // Print out the ir or the error
    match err
    {
        Ok(_) => {
            dbg!(&ir);
        }

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
                                Label::primary(file_id, s)
                                .with_message(format!("`-` is undefined on `{:?}`", t))
                            ]),

                    CorrectnessError::UndefinedInfixOp(s, op, l, r) =>
                        diagnostic = diagnostic
                            .with_message("Undefined infix operator")
                            .with_labels(vec![
                                Label::primary(file_id, s)
                                .with_message(format!("`{:?}` is undefined on `{:?}` and `{:?}`", op, l, r))
                            ]),

                    CorrectnessError::NonboolInBoolExpr(s, t) =>
                        diagnostic = diagnostic
                            .with_message("Nonboolean in boolean expression")
                            .with_labels(vec![
                                Label::primary(file_id, s)
                                .with_message(format!("Expected `Bool`, got `{:?}`", t))
                            ]),

                    CorrectnessError::NonboolInIfCond(s, t) =>
                        diagnostic = diagnostic
                            .with_message("Nonboolean in if condition")
                            .with_labels(vec![
                                Label::primary(file_id, s)
                                .with_message(format!("Expected `Bool`, got `{:?}`", t))
                            ]),

                    CorrectnessError::NonmatchingIfBodies(s1, t1, s2, t2) =>
                        diagnostic = diagnostic
                            .with_message("Nonmatching if expression clauses")
                            .with_labels(vec![
                                Label::secondary(file_id, s1)
                                .with_message(format!("Then clause has type `{:?}`", t1)),
                                Label::primary(file_id, s2)
                                .with_message(format!("Expected `{:?}`, got `{:?}`", t1, t2))
                            ]),

                    CorrectnessError::NonmatchingAssignTypes(s1, t1, s2, t2) =>
                        diagnostic = diagnostic
                            .with_message("Nonmatching types in assignment")
                            .with_labels(vec![
                                Label::secondary(file_id, s1)
                                .with_message(format!("Assignment is declared with type `{:?}`", t1)),
                                Label::primary(file_id, s2)
                                .with_message(format!("Expected `{:?}`, got `{:?}`", t1, t2))
                            ]),

                    CorrectnessError::SymbolNotFound(s, v) =>
                        diagnostic = diagnostic
                            .with_message("Symbol not found")
                            .with_labels(vec![
                                Label::primary(file_id, s)
                                .with_message(format!("Could not find symbol `{}`", v))
                            ]),

                    CorrectnessError::Reassignment(s1, s2, v) =>
                        diagnostic = diagnostic
                            .with_message("Redefinition of previously declared variable")
                            .with_labels(vec![
                                Label::primary(file_id, s1)
                                .with_message(format!("`{}` is already defined and not declared as mutable", v)),
                                Label::secondary(file_id, s2)
                                .with_message(format!("`{}` previously defined here", v))
                            ]),

                    CorrectnessError::InvalidType(s) =>
                        diagnostic = diagnostic
                            .with_message("Invalid type used")
                            .with_labels(vec![
                                Label::primary(file_id, s)
                                .with_message("Undeclared type")
                            ]),

                    CorrectnessError::UnknownFunctionReturnType(s, v) =>
                        diagnostic = diagnostic
                            .with_message("Could not determine the return type of the function")
                            .with_labels(vec![
                                Label::primary(file_id, s)
                                .with_message(format!("Could not determine return type for `{}`", v))
                            ]),

                    CorrectnessError::MismatchedFunctionArgType(s, t1, t2) =>
                        diagnostic = diagnostic
                            .with_message("Wrong type passed as an argument")
                            .with_labels(vec![
                                Label::primary(file_id, s)
                                .with_message(format!("Expected `{:?}`, got `{:?}`", t1, t2))
                            ]),

                    CorrectnessError::InvalidApplication(s, t) => {
                        diagnostic = diagnostic
                            .with_message("Invalid application")
                            .with_labels(vec![
                                Label::primary(file_id, s)
                                .with_message(format!("Expected function, got `{:?}`", t))
                            ]);

                        if t == Type::String
                        {
                            diagnostic = diagnostic
                                .with_notes(vec![String::from("String concatenation is not yet implemented")]);
                        }
                    }
                }
                term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
            }
            return Err(());
        }
    }

    // Generate C code
    let c = codegen::convert_ir_to_c(&ir, repl_mode);
    println!("{}", &c);

    Ok(c)
}

// execute(&str, &str, &mut IR, Option<&mut HashMap<String, REPLType>>, &mut Guard) -> ()
// Executes Curly code.
fn execute(filename: &str, code: &str, ir: &mut IR, repl_vars: Option<&mut HashMap<String, REPLValue>>, guard: &mut Guard)
{
    use std::mem::transmute;

    // Compile code
    let c = match compile(filename, code, ir, if let Some(_) = repl_vars { true } else { false })
    {
        Ok(v) => v,
        Err(_) => return
    };

    // Execute the C code
    let mut context = Context::new(guard).expect("unable to initialise context for tcc");
    context.set_output_type(OutputType::Memory)
        .compile_string(&CString::new(c.as_str()).unwrap())
        .expect("unable to compile the code");
    let mut relocated = context.relocate()
        .expect("unable to relocate context");

    let addr = unsafe
    {
        relocated.get_symbol(&CString::new("main").unwrap()).expect("unable to get symbol `main`")
    };

    if let Some(map) = repl_vars
    {
        if let Some(sexpr) = ir.sexprs.last()
        {
            match sexpr.get_metadata()._type
            {
                Type::Int => {
                    let main: fn() -> i64 = unsafe { transmute(addr) };
                    let v = REPLValue::Int(main());
                    dbg!(v);
                }

                Type::Float => {
                    let main: fn() -> f64 = unsafe { transmute(addr) };
                    let v = REPLValue::Float(main());
                    dbg!(v);
                }

                Type::Bool => {
                    let main: fn() -> u8 = unsafe { transmute(addr) };
                    let v = REPLValue::Bool(main() != 0);
                    dbg!(v);
                }

                Type::Func(_, _) => {
                    let main: fn() -> REPLFunc = unsafe { transmute(addr) };
                    let v = REPLValue::Func(main());
                    dbg!(v);
                }

                _ => {
                    panic!("unsupported type!");
                }
            }
        }
    } else
    {
        let main: fn() = unsafe { transmute(addr) };
        main();
    }
}

