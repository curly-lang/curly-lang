use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use owo_colors::OwoColorize;
use rustyline::{Editor, Helper};
use rustyline::completion::Completer;
use rustyline::config::{Builder, CompletionType, EditMode};
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::{Validator, ValidationContext, ValidationResult};
use rustyline::error::ReadlineError;
use std::borrow::Cow;
use std::collections::HashMap;
use std::env;
use std::fs;
use std::process::{Command, Stdio};
use libloading::{Library, Symbol};
use logos::{Lexer, Span};

use curlyc::backends::c::codegen;
use curlyc::frontend::correctness;
use curlyc::frontend::correctness::CorrectnessError;
use curlyc::frontend::ir;
use curlyc::frontend::ir::{IR, SExpr};
use curlyc::frontend::parser::{self, Token};
use curlyc::frontend::types::Type;

static DEBUG: bool = false;

enum CBackendCompiler
{
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
                    compiler: CBackendCompiler::Clang,
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
                let c = compile(&options.input, &contents, &mut ir, None)?;

                let mut echo = Command::new("echo")
                        .arg(&c)
                        .stdout(Stdio::piped())
                        .spawn()
                        .expect("Failed to execute echo");
                echo.wait().expect("Failed to wait for echo");

                match options.compiler
                {
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

            "check" => {
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
                        match check(&file, &contents, &mut ir)
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
                        execute(&file, &contents, &mut ir, None, 0);
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
    func: *const fn(),
    wrapper: *const fn(),
    arity: u32,
    argc: u32,
    cleaners: *const fn(),
    args: *const fn()
}

#[derive(Debug)]
#[repr(C)]
struct REPLSum
{
    tag: u32,
    value: u64
}

#[derive(Debug)]
#[repr(C)]
struct REPLSumFunc
{
    tag: u32,
    value: REPLFunc
}

#[derive(Debug)]
#[repr(C)]
enum REPLValue
{
    Int(i64),
    Float(f64),
    Bool(u8),
    Func(REPLFunc),
    Sum(REPLSum),
    SumFunc(REPLSumFunc)
}

struct CurlyREPLHelper
{
    libs: Vec<Library>,
    ir: IR,
    var_names: Vec<String>,
    vars: HashMap<String, REPLValue>,
}

impl CurlyREPLHelper
{
    fn new() -> CurlyREPLHelper
    {
        CurlyREPLHelper {
            libs: vec![],
            ir: IR::new(),
            var_names: vec![],
            vars: HashMap::new()
        }
    }

    fn _highlight(line: &str) -> String
    {
        use curlyc::frontend::parser::Token::*;

        let lexer = Lexer::<Token>::new(line);
        let mut new_line = std::string::String::new();
        let mut last = Span { start: 0, end: 0 };

        for t in lexer.spanned()
        {
            if t.1.start != last.end
            {
                new_line.push_str(&line[last.end..t.1.start]);
            }

            last = t.1.clone();

            match t.0
            {
                LParen
                    | RParen
                    | LBrack
                    | RBrack
                    | LBrace
                    | RBrace =>
                    new_line.push_str(&line[t.1]),

                Newline
                    | Whitespace =>
                    new_line.push_str(&line[t.1]),

                Comment =>
                    new_line.push_str(&format!("{}", line[t.1].to_owned().bright_black())),

                Error
                    | Unreachable =>
                    new_line.push_str(&format!("{}", line[t.1].to_owned().red())),

                Mul
                    | DivMod
                    | Add
                    | Sub
                    | BitShift
                    | Compare
                    | Ampersand
                    | Bar
                    | Caret
                    | Range
                    | Assign
                    | RightArrow
                    | ThiccArrow
                    | Colon
                    | ColonColon
                    | Comma
                    | Dot =>
                    new_line.push_str(&format!("{}", line[t.1].to_owned().bright_yellow())),

                Int(_)
                    | Float(_)
                    | String
                    | True
                    | False =>
                    new_line.push_str(&format!("{}", line[t.1].to_owned().purple())),

                Symbol if &line[t.1.start..t.1.end] == "uwu" =>
                    new_line.push_str(&format!("{}", line[t.1].to_owned().magenta())),

                Symbol if line[t.1.start..].chars().next().unwrap().is_uppercase() =>
                    new_line.push_str(&format!("{}", line[t.1].to_owned().green())),

                Symbol =>
                    new_line.push_str(&format!("{}", line[t.1].to_owned().cyan())),

                With
                    | For
                    | Some
                    | All
                    | If
                    | Then
                    | Else
                    | Where
                    | Pass
                    | Stop
                    | Type
                    | Enum
                    | Class
                    | Match
                    | Lambda
                    | To
                    | And
                    | Or
                    | Xor
                    | In =>
                    new_line.push_str(&format!("{}", line[t.1].to_owned().yellow().bold()))
            }
        }

        if line.contains("#")
        {
            new_line.push_str(&format!("{}", line[last.end..].to_owned().bright_black()));
        } else
        {
            new_line.push_str(&line[last.end..]);
        }

        new_line
    }
}

impl Completer for CurlyREPLHelper
{
    type Candidate = String;

    fn complete(&self, line: &str, pos: usize, _ctx: &rustyline::Context<'_>) -> rustyline::Result<(usize, Vec<Self::Candidate>)>
    {
        let mut token = (Token::Unreachable, Span { start: 0, end: 0 });
        let lexer = Lexer::new(line);

        for t in lexer.spanned()
        {
            if t.1.start <= pos && pos <= t.1.end
            {
                token = t;
                break;
            }
        }

        let mut choices = vec![];
        if token.0 == Token::Symbol || token.1.start == token.1.end
        {
            let typed = &line[token.1.clone()];
            for choice in self.var_names.iter()
            {
                if choice.starts_with(&typed)
                {
                    choices.push(String::from(choice));
                }
            }
        }

        Ok((token.1.start, choices))
    }
}

impl Hinter for CurlyREPLHelper
{
    type Hint = String;
}

impl Highlighter for CurlyREPLHelper
{
    fn highlight<'l>(&self, line: &'l str, _pos: usize) -> Cow<'l, str>
    {
        Cow::Owned(CurlyREPLHelper::_highlight(line))
    }

    fn highlight_char(&self, _: &str, _: usize) -> bool
    {
        true
    }
}

impl Validator for CurlyREPLHelper
{
    fn validate(&self, ctx: &mut ValidationContext) -> Result<ValidationResult, ReadlineError>
    {
        use ValidationResult::*;

        match parser::parse(ctx.input())
        {
            Ok(_) => Ok(Valid(None)),
            Err(e) if e.continuable => Ok(Incomplete),
            Err(_) if ctx.input().ends_with("\\") => Ok(Incomplete),
            Err(_) => Ok(Valid(None))
        }
    }
}

impl Helper for CurlyREPLHelper {}

// repl() -> ()
// Executes the REPL.
fn repl()
{
    // Set up
    let config = Builder::new()
        .edit_mode(EditMode::Vi)
        .completion_type(CompletionType::List)
        .tab_stop(4)
        .indent_size(4)
        .build();
    let mut rl = Editor::with_config(config);
    let helper = CurlyREPLHelper::new();
    let mut n = 0;
    rl.set_helper(Some(helper));
    if rl.load_history("history.txt").is_err()
    {
        println!("No previous history.");
    }

    loop
    {
        // Get line
        let readline = rl.readline(">>>\n");
        match readline
        {
            Ok(line) => {
                // Quitting
                if line == ":q" || line == ":quit"
                {
                    break;
                }

                rl.add_history_entry(line.as_str());
                let helper = rl.helper_mut().unwrap();
                if let Some(lib) = execute("<stdin>", &line, &mut helper.ir, Some((&mut helper.var_names, &mut helper.vars)), n)
                {
                    helper.libs.push(lib);
                }
                n += 1;
            }

            // Errors
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }

            Err(ReadlineError::Eof) => {
                println!("^D");
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

// check(&str, &str, &mut IR) -> Result<(), ()>
// Checks whether given code is valid.
fn check(filename: &str, code: &str, ir: &mut IR) -> Result<(), ()>
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
    if DEBUG { println!("{:#?}", &ast); }
    ir.clear();
    ir::convert_ast_to_ir(ast, ir);
    if DEBUG { dbg!(&ir); }

    // Check correctness
    let err = correctness::check_correctness(ir);

    // Print out the ir or the error
    match err
    {
        Ok(_) if DEBUG => {
            dbg!(&ir);
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
                                Label::primary(file_id, s)
                                .with_message(format!("`-` is undefined on `{}`", t))
                            ]),

                    CorrectnessError::UndefinedInfixOp(s, op, l, r) =>
                        diagnostic = diagnostic
                            .with_message("Undefined infix operator")
                            .with_labels(vec![
                                Label::primary(file_id, s)
                                .with_message(format!("`{:?}` is undefined on `{}` and `{}`", op, l, r))
                            ]),

                    CorrectnessError::NonboolInBoolExpr(s, t) =>
                        diagnostic = diagnostic
                            .with_message("Nonboolean in boolean expression")
                            .with_labels(vec![
                                Label::primary(file_id, s)
                                .with_message(format!("Expected `Bool`, got `{}`", t))
                            ]),

                    CorrectnessError::NonboolInIfCond(s, t) =>
                        diagnostic = diagnostic
                            .with_message("Nonboolean in if condition")
                            .with_labels(vec![
                                Label::primary(file_id, s)
                                .with_message(format!("Expected `Bool`, got `{}`", t))
                            ]),

                    CorrectnessError::NonmatchingAssignTypes(s1, t1, s2, t2) =>
                        diagnostic = diagnostic
                            .with_message("Nonmatching types in assignment")
                            .with_labels(vec![
                                Label::secondary(file_id, s1)
                                .with_message(format!("Assignment is declared with type `{}`", t1)),
                                Label::primary(file_id, s2)
                                .with_message(format!("Expected `{}`, got `{}`", t1, t2))
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
                                .with_message(format!("Expected `{}`, got `{}`", t1, t2))
                            ]),

                    CorrectnessError::InvalidApplication(s, t) => {
                        diagnostic = diagnostic
                            .with_message("Invalid application")
                            .with_labels(vec![
                                Label::primary(file_id, s)
                                .with_message(format!("Expected function, got `{}`", t))
                            ]);

                        if t == Type::String
                        {
                            diagnostic = diagnostic
                                .with_notes(vec![String::from("String concatenation is not yet implemented")]);
                        }
                    }

                    CorrectnessError::InvalidCast(s1, t1, s2, t2) => {
                        diagnostic = diagnostic
                            .with_message("Invalid cast")
                            .with_labels(vec![
                                Label::secondary(file_id, s1)
                                .with_message(format!("Value has type `{}`", t1)),
                                Label::primary(file_id, s2)
                                .with_message(format!("Cannot convert `{}` to `{}`", t1, t2))
                            ]);
                    }

                    CorrectnessError::NonSubtypeOnMatch(s1, t1, s2, t2) => {
                        diagnostic = diagnostic
                            .with_message("Nonsubtype checked for in match arm")
                            .with_labels(vec![
                                Label::secondary(file_id, s1)
                                .with_message(format!("Value has type `{}`", t1)),
                                Label::primary(file_id, s2)
                                .with_message(format!("`{}` is not a subtype of `{}`", t2, t1))
                            ]);
                    }

                    CorrectnessError::InfiniteSizedType(s, t) => {
                        diagnostic = diagnostic
                            .with_message("Type has infinite size")
                            .with_labels(vec![
                                Label::primary(file_id, s)
                                .with_message(format!("Type `{}` has infinite size", t))
                            ]);
                    }

                    CorrectnessError::NonmemberAccess(s, a, b) => {
                        diagnostic = diagnostic
                            .with_message("Attempted to access a member that does not exist")
                            .with_labels(vec![
                                Label::primary(file_id, s)
                                .with_message(format!("`{}` has no member `{}`", a, b))
                            ])
                    }
                }
                term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
            }
            return Err(());
        }
    }
}

// compile(&str, &str, &mut IR, Option<&mut Vec<String>>) -> Result<String, ()>
// Compiles curly into C code.
fn compile(filename: &str, code: &str, ir: &mut IR, repl_vars: Option<&Vec<String>>) -> Result<String, ()>
{
    // Check the file
    check(filename, code, ir)?;

    // Remove empty sexpressions
    use std::mem::swap;
    let mut sexprs = Vec::with_capacity(0);
    swap(&mut sexprs, &mut ir.sexprs);
    ir.sexprs = sexprs.into_iter().filter(|v| if let SExpr::TypeAlias(_, _) = v { false } else { true }).collect();

    // Generate C code
    let c = codegen::convert_ir_to_c(&ir, repl_vars);
    if DEBUG { println!("{}", &c); }

    Ok(c)
}

// execute(&str, &str, &mut IR, Option<(&mut Vec<String>, &mut HashMap<String, REPLType>)>, &mut Guard) -> Option<Library>
// Executes Curly code.
fn execute(filename: &str, code: &str, ir: &mut IR, repl_vars: Option<(&mut Vec<String>, &mut HashMap<String, REPLValue>)>, n: usize) -> Option<Library>
{
    // Compile code
    let c = match compile(filename, code, ir, if let Some((v, _)) = &repl_vars { Some(v) } else { None })
    {
        Ok(v) => v,
        Err(_) => return None
    };

    // Compile the C code
    let mut echo = Command::new("echo")
                    .arg(&c)
                    .stdout(Stdio::piped())
                    .spawn()
                    .expect("Failed to execute echo");
    echo.wait().expect("Failed to wait for echo");

    let so_file = format!("./.curly_temp_{}.so", n);
    Command::new("clang")
        .arg("-shared")
        .arg("-fPIC")
        .arg("-o")
        .arg(&so_file)
        .arg("-x")
        .arg("c")
        .arg("-")
        .stdin(Stdio::from(echo.stdout.expect("Failed to get stdout")))
        .spawn()
        .expect("Failed to execute clang")
        .wait()
        .expect("Failed to wait for clang");

    // Execute the C code
    let lib = Library::new(&so_file).unwrap();
    if let Some((names, map)) = repl_vars
    {
        if let Some(sexpr) = ir.sexprs.last()
        {
            let v;
            let values: Vec<&REPLValue> = names.iter().map(|v| map.get(v).unwrap()).collect();

            let mut _type = &sexpr.get_metadata()._type;
            while let Type::Symbol(v) = &_type
            {
                _type = ir.types.get(v).unwrap()
            }

            unsafe 
            {
                match _type
                {
                    Type::Int => {
                        let main: Symbol<fn(*const &REPLValue) -> i64> = lib.get("__repl_line".as_bytes()).unwrap();
                        v = REPLValue::Int(main(values.as_ptr()));
                    }

                    Type::Float => {
                        let main: Symbol<fn(*const &REPLValue) -> f64> = lib.get("__repl_line".as_bytes()).unwrap();
                        v = REPLValue::Float(main(values.as_ptr()));
                    }

                    Type::Bool => {
                        let main: Symbol<fn(*const &REPLValue) -> u8> = lib.get("__repl_line".as_bytes()).unwrap();
                        v = REPLValue::Bool(main(values.as_ptr()));
                    }

                    Type::Func(_, _) => {
                        let main: Symbol<fn(*const &REPLValue) -> REPLFunc> = lib.get("__repl_line".as_bytes()).unwrap();
                        v = REPLValue::Func(main(values.as_ptr()));
                    }

                    Type::Sum(fields) => {
                        let mut func = false;
                        for v in fields.0.iter()
                        {
                            if let Type::Func(_, _) = v
                            {
                                func = true;
                                break;
                            }
                        }

                        if func
                        {
                            let main: Symbol<fn(*const &REPLValue) -> REPLSumFunc> = lib.get("__repl_line".as_bytes()).unwrap();
                            v = REPLValue::SumFunc(main(values.as_ptr()));
                        } else
                        {
                            let main: Symbol<fn(*const &REPLValue) -> REPLSum> = lib.get("__repl_line".as_bytes()).unwrap();
                            v = REPLValue::Sum(main(values.as_ptr()));
                        }
                    }

                    _ => {
                        panic!("unsupported type!");
                    }
                }
            }

            if let Some(SExpr::Assign(_, a, _)) = ir.sexprs.last()
            {
                names.push(a.clone());
                map.insert(a.clone(), v);
            }
        }
    } else
    {
        unsafe
        {
            let main: Symbol<fn()> = lib.get("main".as_bytes()).unwrap();
            main();
        }
    }

    // Remove .curly_repl_temp.so
    std::fs::remove_file(&so_file).expect("unable to remove temporary file");

    Some(lib)
}

