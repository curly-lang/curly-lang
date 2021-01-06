use rustyline::Editor;
use rustyline::error::ReadlineError;

use curly_lang::frontend::parser;

fn main()
{
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err()
    {
        println!("No previous history.");
    }

    loop
    {
        let readline = rl.readline(">>> ");
        match readline
        {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                println!("Line: {}", line);

                if line == ":q" || line == ":quit"
                {
                    break;
                }

                execute(&line);
            }

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

fn execute(code: &str)
{
    let ast = parser::parse(code);
    println!("{:#?}", ast);
}
