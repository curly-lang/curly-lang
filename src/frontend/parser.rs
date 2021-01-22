use logos::{Logos, Lexer, Span};

// The tokens parsed by the lexer.
#[derive(Logos, PartialEq, Debug, Copy, Clone)]
pub enum Token
{
    // Brackets
    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("[")]
    LBrack,

    #[token("]")]
    RBrack,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    // Whitespace
    #[token("\n")]
    Newline,

    #[regex(r"([ \t\f\r]|\\\n)+", logos::skip)]
    Whitespace,

    #[regex(r"#[^\n]*\n?", logos::skip)]
    Comment,

    // Error
    #[error]
    Error,

    // Punctuation and symbols
    #[token(":")]
    Colon,
    
    #[token(",")]
    Comma,

    #[token(".")]
    Dot,
    
    #[token("..")]
    Range,
    
    #[token("=")]
    Assign,
    
    #[token("*")]
    Mul,
    
    #[regex(r"/|%")]
    DivMod,
    
    #[token("+")]
    Add,

    #[token("-")]
    Sub,
    
    #[regex(r"<<|>>")]
    BitShift,
    
    #[regex(r"<|>|<=|>=|==|!=")]
    Compare,
    
    #[token("&")]
    Ampersand,
    
    #[token("|")]
    Bar,
    
    #[token("^")]
    Caret,
    
    // Numbers
    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    #[regex(r"0x[0-9a-fA-F]+", |lex| i64::from_str_radix(&lex.slice()[2..], 16))]
    #[regex(r"0b[01]+", |lex| i64::from_str_radix(&lex.slice()[2..], 2))]
    Int(i64),
    
    #[regex(r"[0-9]+(\.[0-9]*([eE][+-]?[0-9]+)?|[eE][+-]?[0-9]+)", |lex| lex.slice().parse())]
    Float(f64),

    // Symbols (variables and stuff)
    #[regex(r"[$@a-zA-Z_][a-zA-Z0-9_']*")]
    Symbol,

    // Strings
    #[regex(r#""([^\\"]|\\.)*""#)]
    String,

    // Booleans
    #[token("true")]
    True,

    #[token("false")]
    False,

    // Arrows
    #[token("->")]
    RightArrow,
    
    #[token("=>")]
    ThiccArrow,
    
    // Keywords
    #[token("with")]
    With,
    
    #[token("for")]
    For,
    
    #[token("some")]
    Some,
    
    #[token("all")]
    All,
    
    #[token("if")]
    If,
    
    #[token("then")]
    Then,
    
    #[token("else")]
    Else,
    
    #[token("where")]
    Where,
    
    #[token("pass")]
    Pass,
    
    #[token("stop")]
    Stop,
    
    #[token("type")]
    Type,
    
    #[token("enum")]
    Enum,
    
    #[token("class")]
    Class,
    
    #[token("match")]
    Match,

    #[token("lambda")]
    Lambda,
    
    #[token("to")]
    To,
    
    #[token("and")]
    And,
    
    #[token("or")]
    Or,
    
    #[token("xor")]
    Xor,

    #[token("in")]
    In,

    #[token("as")]
    As,

    Unreachable
}

// Represents a parser.
struct Parser<'a>
{
    // The lexer the parser uses internally.
    lexer: Lexer<'a, Token>,

    // The tokens already parsed
    tokens: Vec<(Token, Span)>,

    // The current position of the parser.
    token_pos: usize
}

impl<'a> Parser<'a>
{
    // new(&str) -> Parser
    // Creates a new parser
    fn new(s: &str) -> Parser
    {
        Parser {
            lexer: Token::lexer(s),
            tokens: vec![],
            token_pos: 0
        }
    }

    // next(&mut self) -> Option<&(Token, Span)>
    // Gets the next token.
    fn next<'b>(&'b mut self) -> Option<&'b (Token, Span)>
    {
        // Get token from list of already parsed tokens if it exists
        if self.token_pos < self.tokens.len()
        {
            let token = &self.tokens[self.token_pos];
            self.token_pos += 1;
            Some(token)

        // Otherwise get token from the lexer
        } else
        {
            self.tokens.push((self.lexer.next()?, self.lexer.span()));
            self.token_pos += 1;
            self.tokens.last()
        }
    }

    // peek(&mut self) -> Option<&(Token, Span)>
    // Peeks at the next token.
    fn peek<'b>(&'b mut self) -> Option<(&'b Token, Span)>
    {
        // Get token from list of already parsed tokens if it exists
        if self.token_pos < self.tokens.len()
        {
            let token = &self.tokens[self.token_pos];
            Some((&token.0, token.1.clone()))

        // Otherwise get token from lexer
        } else
        {
            self.tokens.push((self.lexer.next()?, self.lexer.span()));
            let token = self.tokens.last()?;
            Some((&token.0, token.1.clone()))
        }
    }

    // slice(&self) -> String
    // Returns the slice corresponding to the current token.
    fn slice(&mut self) -> String
    {
        if self.token_pos >= self.tokens.len()
        {
            self.peek();
        }

        if self.token_pos < self.tokens.len()
        {
            let range = &self.tokens[self.token_pos].1;
            String::from(&self.lexer.source()[range.start..range.end])
        } else
        {
            String::with_capacity(0)
        }
    }

    // span(&self) -> Span
    // Returns the current span.
    fn span(&mut self) -> Span
    {
        if let Some((_, s)) = self.peek()
        {
            s
        } else
        {
            self.lexer.span()
        }
    }

    // save_state(&self) -> usize
    // Saves the current token position by returning it.
    fn save_state(&self) -> usize
    {
        self.token_pos
    }

    // return_state(&mut self, usize) -> ()
    // Returns to a given state.
    fn return_state(&mut self, state: usize)
    {
        self.token_pos = state;
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AST
{
    // Numbers
    Int(Span, i64),
    Float(Span, f64),

    // Booleans
    True(Span),
    False(Span),

    // String
    String(Span, String),

    // Symbol (variables and stuff)
    Symbol(Span, String),

    // Lists
    List(Span, Vec<AST>),

    // Function Application
    Application(Span, Box<AST>, Box<AST>),

    // Prefix expressions
    Prefix(Span, String, Box<AST>),

    // Infix expressions
    Infix(Span, String, Box<AST>, Box<AST>),

    // Casting
    As(Span, Box<AST>, Box<AST>),

    // If expressions
    If(Span, Box<AST>, Box<AST>, Box<AST>),

    // Assignments
    Assign(Span, String, Box<AST>),

    // Assignments with types
    AssignTyped(Span, String, Box<AST>, Box<AST>),

    // Assignment of types
    AssignType(Span, String, Box<AST>),

    // Assignment of functions
    AssignFunction(Span, String, Vec<(String, AST)>, Box<AST>),

    // Lambda functions
    Lambda(Span, Vec<(String, AST)>, Box<AST>),

    // Scoping
    With(Span, Vec<AST>, Box<AST>)
}

impl AST
{
    pub fn get_span(&self) -> Span
    {
        match self
        {
            Self::Int(s, _)
                | Self::Float(s, _)
                | Self::True(s)
                | Self::False(s)
                | Self::String(s, _)
                | Self::List(s, _)
                | Self::Symbol(s, _)
                | Self::Application(s, _, _)
                | Self::Prefix(s, _, _)
                | Self::Infix(s, _, _, _)
                | Self::As(s, _, _)
                | Self::If(s, _, _, _)
                | Self::Assign(s, _, _)
                | Self::AssignTyped(s, _, _, _)
                | Self::AssignType(s, _, _)
                | Self::AssignFunction(s, _, _, _)
                | Self::Lambda(s, _, _)
                | Self::With(s, _, _)
                => s.clone(),
        }
    }
}

#[derive(Debug)]
pub struct ParseError
{
    pub span: Span,
    pub msg: String,
    pub continuable: bool,
    fatal: bool,
}

impl ParseError
{
    // empty<T>() -> Result<T, ParseError>
    // Creates an empty ParseError.
    fn empty<T>() -> Result<T, ParseError>
    {
        Err(ParseError {
            span: Span { start: 0, end: 0 },
            msg: String::with_capacity(0),
            continuable: false,
            fatal: false
        })
    }
}

// call_func(ident, ident, ident) -> Result<AST, ParseError>
// Calls a function and returns if an error was encountered.
macro_rules! call_func
{
    ($func: ident, $parser: ident, $state: ident) => {
        match $func($parser)
        {
            Ok(v) => v,
            Err(e) => {
                $parser.return_state($state);
                return Err(e);
            }
        }
    }
}

// call_func_fatal(ident, ident, literal, literal, expr*) -> Result<AST, ParseError>
// Calls a function and returns a fatal error if unsuccessful.
macro_rules! call_func_fatal
{
    ($func: ident, $parser: ident, $cont: literal, $format: literal $(,$vs: expr),*) => {
        match $func($parser)
        {
            Ok(v) => v,
            Err(e) if e.fatal => return Err(e),
            Err(_) => return Err(ParseError {
                span: $parser.span(),
                msg: format!($format $(,$vs),*),
                continuable: $cont,
                fatal: true
            })
        }
    }
}

// call_optional(ident, ident) => Result<AST, ParseError>
// Calls a function and only returns if a fatal error is encountered.
macro_rules! call_optional
{
    ($func: ident, $parser: ident) => {
        match $func($parser)
        {
            Ok(v) => Ok(v),
            Err(e) if e.fatal => return Err(e),
            Err(e) => Err(e)
        }
    }
}

// consume_nosave(ident, ident, ident, literal, literal, literal, expr*) -> Result<AST, ParseError>
// Consumes a token without saving it, returning if an error was encountered.
macro_rules! consume_nosave
{
    ($parser: ident, $token: ident, $state: ident, $cont: literal, $fatal: literal, $format: literal $(,$vs: expr),*) => {
        match $parser.peek()
        {
            Some((Token::$token, _)) => {
                $parser.next();
            }

            _ => {
                let span = $parser.span();
                $parser.return_state($state);
                return Err(ParseError {
                    span,
                    msg: format!($format $(,$vs),*),
                    continuable: $cont,
                    fatal: $fatal
                });
            }
        }
    }
}

// consume_save(ident, ident, ident, literal, literal, literal, expr*) -> Result<AST, ParseError>
// Consumes a token and saves it, returning if an error was encountered.
macro_rules! consume_save
{
    ($parser: ident, $token: ident, $state: ident, $cont: literal, $fatal: literal, $format: literal $(,$vs: expr),*) => {
        match $parser.peek()
        {
            Some((Token::$token, s)) => {
                let v = ($parser.slice(), s);
                $parser.next();
                v
            }

            _ => {
                let span = $parser.span();
                $parser.return_state($state);
                return Err(ParseError {
                    span,
                    msg: format!($format $(,$vs),*),
                    continuable: $cont,
                    fatal: $fatal
                })
            }
        };
    }
}

// newline(&mut Parser) -> ()
// Optionally parses newlines.
fn newline(parser: &mut Parser)
{
    while let Some((Token::Newline, _)) = parser.peek()
    {
        parser.next();
    }
}

// value(&mut Parser) -> Result<AST, ParseError>
// Gets the next value.
fn value(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Get token
    let (token, span) = match parser.peek()
    {
        Some(v) => v,
        None => return ParseError::empty()
    };

    // Check for int
    if let Token::Int(n) = token
    {
        let n = *n;
        parser.next();
        Ok(AST::Int(span, n))

    // Check for float
    } else if let Token::Float(n) = token
    {
        let n = *n;
        parser.next();
        Ok(AST::Float(span, n))

    // Check for string
    } else if let Token::String = token
    {
        let s = parser.slice();
        parser.next();
        Ok(AST::String(span, String::from(s)))

    // True
    } else if let Token::True = token
    {
        parser.next();
        Ok(AST::True(span))

    // False
    } else if let Token::False = token
    {
        parser.next();
        Ok(AST::False(span))

    // Check for symbol
    } else if let Token::Symbol = token
    {
        let s = parser.slice();
        parser.next();
        Ok(AST::Symbol(span, String::from(s)))

    // Parenthesised expressions
    } else if let Token::LParen = token
    {
        // Get value
        let state = parser.save_state();
        parser.next();
        newline(parser);
        let value = match expression(parser) {
            Ok(v) => v,
            Err(e) => {
                parser.return_state(state);
                return Err(e);
            }
        };

        // Get right parenthesis
        newline(parser);
        consume_nosave!(parser, RParen, state, true, true, "");
        Ok(value)

    // Not a value
    } else
    {
        ParseError::empty()
    }
}

fn _as(parser: &mut Parser) -> Result<AST, ParseError>
{
    let value = value(parser)?;

    if let Some((Token::As, _)) = parser.peek()
    {
        parser.next();
        let _type = call_func_fatal!(type_expr, parser, false, "Expected type after `as`");

        Ok(AST::As(Span {
            start: value.get_span().start,
            end: _type.get_span().end
        }, Box::new(value), Box::new(_type)))
    } else
    {
        Ok(value)
    }
}

// application(&mut Parser) -> Result<AST, ParseError>
// Parses function application.
fn application(parser: &mut Parser) -> Result<AST, ParseError>
{
    let mut left = _as(parser)?;

    loop
    {
        let right = match _as(parser)
        {
            Ok(v) => v,
            Err(e) if e.fatal => break Err(e),
            Err(_) => break Ok(left)
        };

        left = AST::Application(Span {
            start: left.get_span().start,
            end: right.get_span().end
        }, Box::new(left), Box::new(right));
    }
}

// prefix(&mut Parser) -> Result<AST, ParseError>
// Gets the next prefix expression.
fn prefix(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Set up
    let (token, span) = match parser.peek()
    {
        Some(v) => v,
        None => return ParseError::empty()
    };

    // Unary minus
    if let Token::Sub = token
    {
        parser.next();

        // Get value
        let value = call_func_fatal!(application, parser, false, "Expected value after prefix operator");

        Ok(AST::Prefix(Span {
            start: span.start,
            end: value.get_span().end
        }, String::from("-"), Box::new(value)))

    // Span
    } else if let Token::Mul = token
    {
        parser.next();

        // Get value
        let value = call_func_fatal!(application, parser, false, "Expected value after prefix operator");

        Ok(AST::Prefix(Span {
            start: span.start,
            end: value.get_span().end
        }, String::from("*"), Box::new(value)))

    // Default to regular value
    } else
    {
        application(parser)
    }
}

// infix_op(ident, ident, pat, pat) -> Result<AST, ParseError>
// Parses an infix operator.
macro_rules! infix_op
{
    ($parser: ident, $subfunc: ident, $op1: pat, $op2: pat) => {{
        // Set up
        let state = $parser.save_state();
        let mut left = call_func!($subfunc, $parser, state);

        loop
        {
            // Save current state
            let state2 = $parser.save_state();

            // Check for operator
            if let Some(op) = $parser.peek()
            {
                // Get operator
                let op = match op.0
                {
                    $op1 | $op2 => String::from($parser.slice()),
                    _ => {
                        $parser.return_state(state2);
                        break;
                    }
                };
                $parser.next();

                // Get right hand side
                let right = call_func_fatal!($subfunc, $parser, false, "Expected value after infix operator");

                // Build ast
                left = AST::Infix(Span {
                    start: left.get_span().start,
                    end: right.get_span().end
                }, op, Box::new(left), Box::new(right));

            // If there's no operator, break
            } else
            {
                break;
            }
        }

        Ok(left)
    }}
}

// muldivmod(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next multiplication/division/modulus expression.
fn muldivmod(parser: &mut Parser) -> Result<AST, ParseError>
{
    infix_op!(parser, prefix, Token::Mul, Token::DivMod)
}

// addsub(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next addition/subtraction expression.
fn addsub(parser: &mut Parser) -> Result<AST, ParseError>
{
    
    infix_op!(parser, muldivmod, Token::Add, Token::Sub)
}

// bitshift(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next bitshift expression.
fn bitshift(parser: &mut Parser) -> Result<AST, ParseError>
{
    infix_op!(parser, addsub, Token::BitShift, Token::Unreachable)
}

// compare(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next comparison expression.
fn compare(parser: &mut Parser) -> Result<AST, ParseError>
{
    infix_op!(parser, bitshift, Token::Compare, Token::In)
}

// and(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next bitwise and expression.
fn and(parser: &mut Parser) -> Result<AST, ParseError>
{
    infix_op!(parser, compare, Token::Ampersand, Token::Unreachable)
}

// or(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next bitwise or expression.
fn or(parser: &mut Parser) -> Result<AST, ParseError>
{
    infix_op!(parser, and, Token::Bar, Token::Unreachable) 
}

// xor(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next bitwise xor expression.
fn xor(parser: &mut Parser) -> Result<AST, ParseError>
{
    infix_op!(parser, or, Token::Caret, Token::Unreachable) 
}

// bool_and(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next logical and expression.
fn bool_and(parser: &mut Parser) -> Result<AST, ParseError>
{
    infix_op!(parser, xor, Token::And, Token::Or) 
}

// bool_or(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next logical or expression.
fn bool_or(parser: &mut Parser) -> Result<AST, ParseError>
{
    infix_op!(parser, bool_and, Token::Or, Token::Unreachable)
}

// bool_xor(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next logical xor expression.
fn bool_xor(parser: &mut Parser) -> Result<AST, ParseError>
{
    infix_op!(parser, bool_or, Token::Xor, Token::Unreachable)
}

// if_expr(&mut Parser) -> Result<AST, ParseError>
// Parses an if expression.
fn if_expr(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Get if keyword
    if let Some((Token::If, span)) = parser.peek()
    {
        let state = parser.save_state();
        parser.next();
        newline(parser);

        // Get condition
        let cond = call_func_fatal!(expression, parser, false, "Expected condition after if");

        // Get then keyword
        newline(parser);
        let slice = parser.slice();
        consume_nosave!(parser, Then, state, true, true, "Expected `then`, got `{}`", slice);

        // Get body
        newline(parser);
        let then = call_func_fatal!(expression, parser, true, "Expected body after then");

        // Get else keyword
        newline(parser);
        let slice = parser.slice();
        consume_nosave!(parser, Else, state, true, true, "Expected `else`, got `{}`", slice);

        // Get else clause
        newline(parser);
        let elsy = call_func_fatal!(expression, parser, true, "Expected body after else");

        // Return success
        Ok(AST::If(Span {
            start: span.start,
            end: elsy.get_span().end
        }, Box::new(cond), Box::new(then), Box::new(elsy)))

    // Not an if expression
    } else
    {
        ParseError::empty()
    }
}

// list(&mut Parser) -> Result<AST, ParseError>
// Parses a list.
fn list(parser: &mut Parser) -> Result<AST, ParseError>
{
    let state = parser.save_state();
    let (_, start) = consume_save!(parser, LBrack, state, false, false, "");
    let mut list = vec![];

    loop
    {
        if list.len() > 0
        {
            match parser.peek()
            {
                Some((Token::Comma, _)) => {
                    parser.next();
                }

                _ => break
            }
        }

        newline(parser);
        list.push(match expression(parser)
        {
            Ok(v) => v,
            Err(e) if e.fatal => return Err(e),
            Err(_) => break
        });
    }

    newline(parser);
    let (_, end) = consume_save!(parser, RBrack, state, true, true, "Expected `[` after end of list");

    Ok(AST::List(Span {
        start: start.start,
        end: end.end
    }, list))
}

// lambda(&mut Parser) -> Result<AST, ParseError>
// Parses a lambda function.
fn lambda(parser: &mut Parser) -> Result<AST, ParseError>
{
    let state = parser.save_state();
    let mut args = vec![];
    let (_, span) = consume_save!(parser, Lambda, state, false, false, "");

    // Get arguments
    loop
    {
        // Get comma
        if args.len() != 0
        {
            match parser.peek()
            {
                Some((Token::Comma, _)) => {
                    parser.next();
                }

                _ => break
            }
        }

        let arg = match declaration(parser)
        {
            Ok(v) => (v.1, v.2),
            Err(e) => {
                parser.return_state(state);
                return Err(e);
            }
        };

        args.push(arg);
    }

    // Check that there is at least one argument
    if args.len() == 0
    {
        parser.return_state(state);
        return Err(ParseError {
            span: parser.span(),
            msg: String::from("Expected argument after `lambda`"),
            continuable: false,
            fatal: true
        })
    }

    // Get the assign operator
    let slice = parser.slice();
    consume_nosave!(parser, Assign, state, false, true, "Expected `=`, got `{}`", slice);

    // Get the value
    newline(parser);
    let body = call_func_fatal!(expression, parser, true, "Expected function body after `=`");

    Ok(AST::Lambda(Span {
        start: span.start,
        end: body.get_span().end
    }, args, Box::new(body)))
}

// expression(&mut Parser) -> Result<AST, ParseError>
// Parses an expression.
fn expression(parser: &mut Parser) -> Result<AST, ParseError>
{

    if let Ok(iffy) = call_optional!(if_expr, parser)
    {
        Ok(iffy)
    } else if let Ok(withy) = call_optional!(with, parser)
    {
        Ok(withy)
    } else if let Ok(lambda) = call_optional!(lambda, parser)
    {
        Ok(lambda)
    } else if let Ok(list) = call_optional!(list, parser)
    {
        Ok(list)
    } else
    {
        bool_xor(parser)
    }
}

// assignment_raw(&mut Parser) -> Result<AST, ParseError>
// Parses an assignment without any types or arguments.
fn assignment_raw(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Get the variable name
    let state = parser.save_state();
    let (name, span) = consume_save!(parser, Symbol, state, false, false, "");

    // Get the assign operator
    consume_nosave!(parser, Assign, state, false, false, "");

    // Get the value
    newline(parser);
    let value = call_func_fatal!(expression, parser, true, "Expected value after `=`");

    Ok(AST::Assign(Span {
        start: span.start,
        end: value.get_span().end
    }, name, Box::new(value)))
}

// type_symbol(&mut Parser) -> Result<AST, ParseError>
// Parses a type symbol or parenthesised type.
fn type_symbol(parser: &mut Parser) -> Result<AST, ParseError>
{
    let (token, span) = match parser.peek()
    {
        Some(v) => v,
        None => return ParseError::empty()
    };

    // Symbols
    if let Token::Symbol = token
    {
        let value = AST::Symbol(span, parser.slice());
        parser.next();
        Ok(value)

    // Enums
    } else if let Token::Enum = token
    {
        let state = parser.save_state();
        parser.next();
        let (value, span2) = consume_save!(parser, Symbol, state, false, true, "Expected symbol after `enum`");

        Ok(AST::Prefix(Span {
            start: span.start,
            end: span2.end
        }, String::from("enum"), Box::new(AST::Symbol(span2, value))))

    // Parenthesised types
    } else if let Token::LParen = token
    {
        // Get value
        let state = parser.save_state();
        parser.next();
        newline(parser);

        let value = match type_expr(parser) {
            Ok(v) => v,
            Err(e) => {
                parser.return_state(state);
                return Err(e);
            }
        };

        // Get right parenthesis
        newline(parser);
        consume_nosave!(parser, RParen, state, true, true, "");
        Ok(AST::Prefix(span, String::with_capacity(0), Box::new(value)))

    // Not a value
    } else
    {
        ParseError::empty()
    }
}

// type_union(&mut Parser) -> Result<AST, ParseError>
// Parses a union type declaration.
fn type_union(parser: &mut Parser) -> Result<AST, ParseError>
{
    infix_op!(parser, type_symbol, Token::Bar, Token::Unreachable)
}

// type_expr(&mut Parser) -> Result<AST, ParseError>
// Parses a type.
fn type_expr(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Set up
    use std::mem::swap;
    let state = parser.save_state();
    let mut top = call_func!(type_union, parser, state);
    let mut acc = &mut top;

    loop
    {
        // Save current state
        let state2 = parser.save_state();

        // Check for operator
        if let Some(op) = parser.peek()
        {
            // Get operator
            match op.0
            {
                Token::RightArrow => (),

                _ => {
                    parser.return_state(state2);
                    break;
                }
            }
            parser.next();

            // Get right hand side
            let mut right = Some(call_func_fatal!(type_union, parser, false, "Expected value after infix operator"));

            // Build ast
            match &mut acc
            {
                AST::Infix(_, op, _, r) if op.as_str() == "->" => {
                    let mut left = Box::new(AST::False(Span { start: 0, end: 0 }));
                    swap(r, &mut left);
                    let right_unwrapped = right.unwrap();
                    right = None;
                    let mut new = Box::new(AST::Infix(Span {
                        start: r.get_span().start,
                        end: right_unwrapped.get_span().end
                    }, String::from("->"), left, Box::new(right_unwrapped)));
                    swap(r, &mut new);
                }

                _ => ()
            }

            // Adjust top node
            if let AST::Prefix(span, s, v) = acc
            {
                #[allow(unused_assignments)]
                if s == ""
                {
                    let mut temp = AST::True(span.clone());
                    acc = &mut temp;
                    top = *v.clone();
                    acc = &mut top;
                }
            }

            if right.is_some()
            {
                let right_unwrapped = right.unwrap();
                top = AST::Infix(Span {
                    start: top.get_span().start,
                    end: right_unwrapped.get_span().end
                }, String::from("->"), Box::new(top), Box::new(right_unwrapped));
                acc = &mut top;
            } else if let AST::Infix(_, _, _, r) = acc
            {
                acc = r;
            }

        // If there's no operator, break
        } else
        {
            break;
        }
    }

    Ok(top)
}

// type_assignment(&mut Parser) -> Result<AST, ParseError>
// Parses an assignment of a type.
fn type_assignment(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Get type keyword
    let state = parser.save_state();
    let (_, span) = consume_save!(parser, Type, state, false, false, "");

    // Get name of type
    let (name, _) = consume_save!(parser, Symbol, state, false, true, "Expected symbol after type");

    // Get assignment operator
    consume_nosave!(parser, Assign, state, false, true, "Expected `=` after type name");
    newline(parser);

    // Get type
    let _type = call_func_fatal!(type_expr, parser, true, "Expected type after `=`");

    // Successfully return
    Ok(AST::AssignType(Span {
        start: span.start,
        end: _type.get_span().end
    }, name, Box::new(_type)))
}

// declaration(&mut Parser) -> Result<(Span, String, AST), ParseError>
// Parses a declaration.
fn declaration(parser: &mut Parser) -> Result<(Span, String, AST), ParseError>
{
    // Get the variable name
    let state = parser.save_state();
    let (name, span) = consume_save!(parser, Symbol, state, false, false, "");

    // Get the colon
    consume_nosave!(parser, Colon, state, false, false, "");

    // Get the type
    let type_val = call_func_fatal!(type_expr, parser, false, "Expected type after `:`");

    Ok((span, name, type_val))
}

// assignment_typed(&mut Parser) -> Result<AST, ParseError>
// Parses an assignment annotated with a type.
fn assignment_typed(parser: &mut Parser) -> Result<AST, ParseError>
{
    let state = parser.save_state();
    let (span, name, type_val) = declaration(parser)?;

    // Get the assign operator
    let slice = parser.slice();
    consume_nosave!(parser, Assign, state, false, true, "Expected `=`, got `{}`", slice);

    // Get the value
    newline(parser);
    let value = call_func_fatal!(expression, parser, true, "Expected value after `=`");

    Ok(AST::AssignTyped(Span {
        start: span.start,
        end: value.get_span().end
    }, name, Box::new(type_val), Box::new(value)))
}

// assignment_func(&mut Parser) -> Result<AST, ParseError>
// Parses an assignment for a function.
fn assignment_func(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Get the variable name
    let state = parser.save_state();
    let mut args = vec![];
    let (name, span) = consume_save!(parser, Symbol, state, false, false, "");

    // Get arguments
    loop
    {
        // Get comma
        if args.len() != 0
        {
            match parser.peek()
            {
                Some((Token::Comma, _)) => {
                    parser.next();
                }

                _ => break
            }
        }

        let arg = match declaration(parser)
        {
            Ok(v) => (v.1, v.2),
            Err(e) => {
                parser.return_state(state);
                return Err(e);
            }
        };

        args.push(arg);
    }

    // Check that there is at least one argument
    if args.len() == 0
    {
        parser.return_state(state);
        return ParseError::empty();
    }

    // Get the assign operator
    let slice = parser.slice();
    consume_nosave!(parser, Assign, state, false, true, "Expected `=`, got `{}`", slice);

    // Get the value
    newline(parser);
    let value = call_func_fatal!(expression, parser, true, "Expected function body after `=`");

    Ok(AST::AssignFunction(Span {
        start: span.start,
        end: value.get_span().end
    }, name, args, Box::new(value)))
}

// assignment(&mut Parser) -> Result<AST, ParseError>
// Parses an assignment.
fn assignment(parser: &mut Parser) -> Result<AST, ParseError>
{
    if let Ok(typed) = call_optional!(assignment_raw, parser)
    {
        Ok(typed)
    } else if let Ok(func) = call_optional!(assignment_typed, parser)
    {
        Ok(func)
    } else
    {
        assignment_func(parser)
    }
}

// with(&mut Parser) -> Result<AST, ParseError>
// Parses a with expression (scoping).
fn with(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Get the with keyword
    let state = parser.save_state();
    let span = parser.span();
    consume_nosave!(parser, With, state, false, false, "");

    // Get assignments
    let mut assigns = vec![];
    loop
    {
        let assign = match assignment(parser)
        {
            Ok(v) => v,
            Err(e) if e.fatal => return Err(e),
            Err(_) => break
        };
        assigns.push(assign);

        // Comma
        let slice = parser.slice();
        consume_nosave!(parser, Comma, state, false, true, "Expected `,`, got `{}`", slice);
        newline(parser);
    }

    // Check that there is at least one assignment
    if assigns.len() == 0
    {
        parser.return_state(state);
        return ParseError::empty();
    }

    // Get the body
    let body = call_func!(expression, parser, state);

    Ok(AST::With(Span {
        start: span.start,
        end: body.get_span().end
    }, assigns, Box::new(body)))
}

// parse(&str) -> Result<AST, ParseError>
// Parses curly code.
pub fn parse(s: &str) -> Result<Vec<AST>, ParseError>
{
    let mut parser = Parser::new(s);
    let mut lines = vec![];

    while let Some(_) = parser.peek()
    {
        // Parse one line
        let p = &mut parser;
        if let Ok(assign) = call_optional!(assignment, p)
        {
            lines.push(assign);
        } else if let Ok(_type) = call_optional!(type_assignment, p)
        {
            lines.push(_type);
        } else
        {
            lines.push(match expression(&mut parser)
            {
                Ok(v) => v,
                Err(e) if e.fatal => return Err(e),
                Err(_) => {
                    let peeked = if let Some(_) = parser.peek() { parser.slice() } else { String::from("eof") };
                    return Err(ParseError {
                        span: parser.span(),
                        msg: format!("Unexpected `{}`", peeked),
                        continuable: false,
                        fatal: true
                    })
                }
            });
        }

        // Skip newlines
        loop
        {
            match parser.peek()
            {
                Some((Token::Newline, _)) => { parser.next(); }
                _ => break
            }
        }
    }

    Ok(lines)
}

