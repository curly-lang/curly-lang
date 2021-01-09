use logos::{Logos, Lexer, Span};

// The tokens parsed by the lexer.
#[derive(Logos, PartialEq, Debug, Copy, Clone)]
enum Token
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
   
    #[regex(r"([ \t\f]|\\\n)+", logos::skip)]
    Whitespace,

    // Error
    #[error]
    Error,

    // Punctuation and symbols
    #[token(":")]
    Colon,
    
    #[token(",")]
    Comma,
    
    #[token("\\")]
    Backslash,
    
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
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_']*")]
    Symbol,

    // Strings
    #[regex(r#""([^\\]|\\.)*""#)]
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
    fn slice(&self) -> String
    {
        if self.token_pos < self.tokens.len()
        {
            let range = &self.tokens[self.token_pos].1;
            String::from(&self.lexer.source()[range.start..range.end])
        } else
        {
            panic!("you aren't supposed to be here!");
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

#[derive(Debug, PartialEq)]
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

    // Function Application
    Application(Span, Box<AST>, Box<AST>),

    // Prefix expressions
    Prefix(Span, String, Box<AST>),

    // Infix expressions
    Infix(Span, String, Box<AST>, Box<AST>),

    // If expressions
    If(Span, Box<AST>, Box<AST>, Box<AST>),

    // Assignments
    Assign(Span, String, Box<AST>),

    // Assignments with types
    AssignTyped(Span, String, Box<AST>, Box<AST>),

    // Assignment of functions
    AssignFunction(Span, String, Vec<(String, AST)>, Box<AST>),

    // Scoping
    With(Span, Vec<AST>, Box<AST>),
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
                | Self::Symbol(s, _)
                | Self::Application(s, _, _)
                | Self::Prefix(s, _, _)
                | Self::Infix(s, _, _, _)
                | Self::If(s, _, _, _)
                | Self::Assign(s, _, _)
                | Self::AssignTyped(s, _, _, _)
                | Self::AssignFunction(s, _, _, _)
                | Self::With(s, _, _)
                => s.clone(),
        }
    }
}

#[derive(Debug)]
pub struct ParseError
{
    pub span: Span,
    // pub msg: String,
    // pub continuable: bool,
    // fatal: bool,
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

// consume_nosave(ident, ident, ident) -> Result<AST, ParseError>
// Consumes a token without saving it, returning if an error was encountered.
macro_rules! consume_nosave
{
    ($parser: ident, $token: ident, $state: ident) => {
        match $parser.peek()
        {
            Some((Token::$token, _)) => {
                $parser.next();
            }
            _ => {
                $parser.return_state($state);
                return Err(ParseError {
                    span: $parser.span(),
                });
            }
        }
    }
}

macro_rules! consume_save
{
    ($parser: ident, $token: ident) => {
        match $parser.peek()
        {
            Some((Token::$token, s)) => {
                let v = ($parser.slice(), s);
                $parser.next();
                v
            }

            _ => return Err(ParseError {
                span: $parser.span(),
            })
        };
    }
}

// newline(&mut Parser) -> ()
// Optionally parses a newline.
fn newline(parser: &mut Parser)
{
    if let Some((Token::Newline, _)) = parser.peek()
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
        None => return Err(ParseError {
            span: parser.span(),
        })
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
        if let Some((Token::RParen, _)) = parser.peek()
        {
            parser.next();
            Ok(value)
        } else
        {
            parser.return_state(state);
            Err(ParseError {
                span,
            })
        }

    // Not a value
    } else
    {
        Err(ParseError {
            span,
        })
    }
}

// application(&mut Parser) -> Result<AST, ParseError>
// Parses function application.
fn application(parser: &mut Parser) -> Result<AST, ParseError>
{
    let mut left = value(parser)?;

    loop
    {
        let right = match value(parser)
        {
            Ok(v) => v,
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
    let state = parser.save_state();
    let (token, span) = match parser.peek()
    {
        Some(v) => v,
        None => return Err(ParseError {
            span: parser.span(),
        })
    };

    // Unary minus
    if let Token::Sub = token
    {
        parser.next();

        // Get value
        let value = call_func!(application, parser, state);

        Ok(AST::Prefix(Span {
            start: span.start,
            end: value.get_span().end
        }, String::from("-"), Box::new(value)))

    // Span
    } else if let Token::Mul = token
    {
        parser.next();

        // Get value
        let value = call_func!(application, parser, state);

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
                let right = call_func!($subfunc, $parser, state);

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
        let cond = call_func!(expression, parser, state);

        // Get then keyword
        newline(parser);
        consume_nosave!(parser, Then, state);

        // Get body
        newline(parser);
        let then = call_func!(expression, parser, state);

        // Get else keyword
        newline(parser);
        consume_nosave!(parser, Else, state);

        // Get else clause
        newline(parser);
        let elsy = call_func!(expression, parser, state);

        // Return success
        Ok(AST::If(Span {
            start: span.start,
            end: elsy.get_span().end
        }, Box::new(cond), Box::new(then), Box::new(elsy)))

    // Not an if expression
    } else
    {
        Err(ParseError {
            span: parser.span(),
        })
    }
}

// expression(&mut Parser) -> Result<AST, ParseError>
// Parses an expression.
fn expression(parser: &mut Parser) -> Result<AST, ParseError>
{
    if let Ok(iffy) = if_expr(parser)
    {
        Ok(iffy)
    } else if let Ok(withy) = with(parser)
    {
        Ok(withy)
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
    let (name, span) = consume_save!(parser, Symbol);

    // Get the assign operator
    parser.next();
    consume_nosave!(parser, Assign, state);

    // Get the value
    newline(parser);
    let value = call_func!(expression, parser, state);

    Ok(AST::Assign(Span {
        start: span.start,
        end: value.get_span().end
    }, name, Box::new(value)))
}

// type_expr(&mut Parser) -> Result<AST, ParseError>
// Parses a type (currently just a symbol).
fn type_expr(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Symbol
    if let Some((Token::Symbol, span)) = parser.peek()
    {
        let s = parser.slice();
        parser.next();
        Ok(AST::Symbol(span, String::from(s)))
    } else
    {
        Err(ParseError {
            span: parser.span(),
        })
    }
}

// declaration(&mut Parser) -> Result<(Span, String, AST), ParseError>
// Parses a declaration.
fn declaration(parser: &mut Parser) -> Result<(Span, String, AST), ParseError>
{
    // Get the variable name
    let state = parser.save_state();
    let (name, span) = consume_save!(parser, Symbol);

    // Get the colon
    parser.next();
    consume_nosave!(parser, Colon, state);

    // Get the type
    let type_val = call_func!(type_expr, parser, state);

    Ok((span, name, type_val))
}

// assignment_typed(&mut Parser) -> Result<AST, ParseError>
// Parses an assignment annotated with a type.
fn assignment_typed(parser: &mut Parser) -> Result<AST, ParseError>
{
    let state = parser.save_state();
    let (span, name, type_val) = declaration(parser)?;

    // Get the assign operator
    consume_nosave!(parser, Assign, state);

    // Get the value
    newline(parser);
    let value = call_func!(expression, parser, state);

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
    let (name, span) = consume_save!(parser, Symbol);

    // Get arguments
    parser.next();
    loop
    {
        let arg = match declaration(parser)
        {
            Ok(v) => (v.1, v.2),
            Err(_) => break
        };

        args.push(arg);
    }

    // Check that there is at least one argument
    if args.len() == 0
    {
        parser.return_state(state);
        return Err(ParseError {
            span: parser.span()
        });
    }

    // Get the assign operator
    consume_nosave!(parser, Assign, state);

    // Get the value
    newline(parser);
    let value = call_func!(expression, parser, state);

    Ok(AST::AssignFunction(Span {
        start: span.start,
        end: value.get_span().end
    }, name, args, Box::new(value)))
}

// assignment(&mut Parser) -> Result<AST, ParseError>
// Parses an assignment.
fn assignment(parser: &mut Parser) -> Result<AST, ParseError>
{
    if let Ok(typed) = assignment_typed(parser)
    {
        Ok(typed)
    } else if let Ok(func) = assignment_func(parser)
    {
        Ok(func)
    } else
    {
        assignment_raw(parser)
    }
}

// with(&mut Parser) -> Result<AST, ParseError>
// Parses a with expression (scoping).
fn with(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Get the with keyword
    let state = parser.save_state();
    let span = parser.span();
    consume_nosave!(parser, With, state);

    // Get assignments
    let mut assigns = vec![];
    loop
    {
        let assign = match assignment(parser)
        {
            Ok(v) => v,
            Err(_) => break
        };
        assigns.push(assign);

        // Comma
        consume_nosave!(parser, Comma, state);
        newline(parser);
    }

    // Check that there is at least one assignment
    if assigns.len() == 0
    {
        parser.return_state(state);
        return Err(ParseError {
            span: parser.span()
        });
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
        if let Ok(assign) = assignment(&mut parser)
        {
            lines.push(assign)
        } else
        {
            lines.push(expression(&mut parser)?)
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

