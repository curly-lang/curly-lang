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
use std::process::Command;
use libloading::{Library, Symbol};
use logos::{Lexer, Span};

use curlyc::backends::c::codegen;
use curlyc::frontend::correctness;
use curlyc::frontend::correctness::CorrectnessError;
use curlyc::frontend::ir;
use curlyc::frontend::ir::{IR, IRError, IRModule, SExpr};
use curlyc::frontend::parser::{self, Token};
use curlyc::frontend::types::Type;

static DEBUG: bool = false;

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
                let c = compile(&options.inputs, &contents, &mut ir, None, options.mode == CompileMode::Executable)?;

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
            ir: IR {
                modules: HashMap::with_capacity(0)
            },
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
                    | Walrus
                    | RightArrow
                    | ThiccArrow
                    | Colon
                    | ColonColon
                    | Comma
                    | Dot
                    | Semicolon =>
                    new_line.push_str(&format!("{}", line[t.1].to_owned().bright_yellow())),

                Int(_)
                    | Float(_)
                    | Word(_)
                    | Char(_)
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

                Annotation =>
                    new_line.push_str(&format!("{}", line[t.1].to_owned().blue())),

                With
                    | For
                    | Some
                    | All
                    | If
                    | Then
                    | Else
                    | Where
                    | Pass
                    | Module
                    | Import
                    | Qualified
                    | Extern
                    | As
                    | Stop
                    | Type
                    | Enum
                    | Pointer
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

        if ctx.input().ends_with("\n")
        {
            Ok(Valid(None))
        } else
        {
            Ok(Incomplete)
        }
    }
}

impl Helper for CurlyREPLHelper { }

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
    let mut _n = 0;
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
                /*let helper = rl.helper_mut().unwrap();
                if let Some(lib) = execute(&vec![String::from("<stdin>")], &vec![line], &mut helper.ir, Some((&mut helper.var_names, &mut helper.vars)), n)
                {
                    helper.libs.push(lib);
                }*/
                //n += 1;
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
                        println!("thing: {:?}", s2);
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
                }
                term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
            }
            Err(())
        }
    }
}

// compile(&str, &str, &mut IR, Option<&mut Vec<String>>, bool) -> Result<String, ()>
// Compiles curly into C code.
fn compile(filenames: &Vec<String>, codes: &Vec<String>, ir: &mut IR, repl_vars: Option<&Vec<String>>, require_main: bool) -> Result<Vec<(String, String)>, ()>
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

// execute(&str, &str, &mut IR, Option<(&mut Vec<String>, &mut HashMap<String, REPLType>)>, &mut Guard) -> Option<Library>
// Executes Curly code.
/*
fn execute(filenames: &Vec<String>, codes: &Vec<String>, ir: &mut IR, repl_vars: Option<(&mut Vec<String>, &mut HashMap<String, REPLValue>)>, n: usize) -> Option<Library>
{
    // Compile code
    let c = match compile(filenames, codes, ir, if let Some((v, _)) = &repl_vars { Some(v) } else { None })
    {
        Ok(v) => v,
        Err(_) => return None
    };

    // Compile the C code
    let c_file = format!("./.curly_temp_{}.c", n);
    let so_file = format!("./.curly_temp_{}.so", n);
    match fs::write(&c_file, c)
    {
        Ok(_) => (),
        Err(e) => {
            eprintln!("Error writing temporary file: {}", e);
            return None;
        }
    }

    Command::new("gcc")
        .arg("-shared")
        .arg("-fPIC")
        .arg("-o")
        .arg(&so_file)
        .arg(&c_file)
        .spawn()
        .expect("Failed to execute gcc")
        .wait()
        .expect("Failed to wait for gcc");

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
    fs::remove_file(&c_file).expect("unable to remove temporary file");
    fs::remove_file(&so_file).expect("unable to remove temporary file");

    Some(lib)
}
*/
