use rustyline::Editor;
use rustyline::error::ReadlineError;
use std::env;
use std::fs;
use std::process::{Command, Stdio};

use curly_lang::backends::c::codegen;
use curly_lang::frontend::correctness;
use curly_lang::frontend::ir;
use curly_lang::frontend::ir::IR;
use curly_lang::frontend::parser;

fn main()
{
    let args = env::args();

    if args.len() == 1
    {
        repl();
    } else
    {
        let args: Vec<String> = args.into_iter().collect();
        exec_file(&args[1]);
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

// execute(&str, &mut IRi, bool) -> ()
// Executes Curly code.
fn execute(code: &str, ir: &mut IR, repl_mode: bool)
{
    // Generate the ast
    let ast = match parser::parse(code)
    {
        Ok(v) => v,
        Err(e) => {
            println!("{:?}", e);
            return;
        }
    };

    // Print out the ast
    println!("{:#?}", &ast);
    ir.clear();
    ir::convert_ast_to_ir(ast, ir);
    println!("{:#?}", &ir);

    // Check correctness
    let err = correctness::check_correctness(ir);

    // Print out the ir or the error
    match err
    {
        Ok(_) => println!("{:#?}", &ir),
        Err(e) => {
            println!("{:?}", e);
            return;
        }
    }

    // Generate C code
    let c = codegen::convert_ir_to_c(&ir, repl_mode);
    println!("{}", &c);

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

