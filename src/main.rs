use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::env;
use std::fs;
use std::process::{Command, exit, Stdio};

use curly_lang::backends::c::codegen;
use curly_lang::frontend::correctness;
use curly_lang::frontend::ir;
use curly_lang::frontend::ir::IR;
use curly_lang::frontend::parser;

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
                                        exit(1);
                                    }
                                }
                            } else
                            {
                                println!("Must specify a compiler to use");
                                exit(1);
                            }
                        }

                        "-o" => {
                            if let Some(v) = args.next()
                            {
                                options.output = v;
                            } else
                            {
                                println!("Must specify an output file");
                                exit(1);
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
                    exit(1);
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
                        exit(1);
                    }
                };

                let mut ir = IR::new();
                let c = compile(&contents, &mut ir, true)?;

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
                        exec_file(&file);
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

// exec_file(&str) -> ()
// Executes a file.
fn exec_file(file: &str)
{
    // Get file contents
    let contents = match fs::read_to_string(file)
    {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return;
        }
    };

    // Execute file
    let mut ir = IR::new();
    execute(&contents, &mut ir, false);
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

    let mut ir = IR::new();

    loop
    {
        // Get line
        let readline = rl.readline(">>> ");
        match readline
        {
            Ok(line) => {
                println!("Line: {}", line);

                // Quitting
                if line == ":q" || line == ":quit"
                {
                    break;
                }

                rl.add_history_entry(line.as_str());
                execute(&line, &mut ir, true);
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

// compile(&str, &mut IR, bool) -> Result<String, ()>
// Compiles curly into C code.
fn compile(code: &str, ir: &mut IR, repl_mode: bool) -> Result<String, ()>
{
   // Generate the ast
    let ast = match parser::parse(code)
    {
        Ok(v) => v,
        Err(e) => {
            dbg!("{:?}", e);
            return Err(());
        }
    };

    // Print out the ast
    println!("{:#?}", &ast);
    ir.clear();
    ir::convert_ast_to_ir(ast, ir);
    dbg!("{:#?}", &ir);

    // Check correctness
    let err = correctness::check_correctness(ir);

    // Print out the ir or the error
    match err
    {
        Ok(_) => {
            dbg!("{:#?}", &ir);
        }

        Err(e) => {
            eprintln!("{:?}", e);
            return Err(());
        }
    }

    // Generate C code
    let c = codegen::convert_ir_to_c(&ir, repl_mode);
    println!("{}", &c);

    Ok(c)
}

// execute(&str, &mut IRi, bool) -> ()
// Executes Curly code.
fn execute(code: &str, ir: &mut IR, repl_mode: bool)
{
    // Compile code
    let c = match compile(code, ir, repl_mode)
    {
        Ok(v) => v,
        Err(_) => return
    };

    // Execute the C code
    let mut echo = Command::new("echo")
            .arg(&c)
            .stdout(Stdio::piped())
            .spawn()
            .expect("Failed to execute echo");
    echo.wait().expect("Failed to wait for echo");
    Command::new("tcc")
            .arg("-run")
            .arg("-")
            .stdin(Stdio::from(echo.stdout.expect("Failed to get stdout")))
            .spawn()
            .expect("Failed to execute tcc")
            .wait()
            .expect("Failed to wait for tcc");
}

