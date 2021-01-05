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
   
    #[regex(r"[ \t\f]+", logos::skip)]
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
    
    #[regex(r"<|>|<=|>=|==|!=|in", priority=3)]
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
    #[regex(r"[a-zA-Z_\$][a-zA-Z0-9_']")]
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
            let span = self.lexer.span();
            self.tokens.push((self.lexer.next()?, span));
            self.tokens.last()
        }
    }

    // peek(&mut self) -> Option<&(Token, Span)>
    // Peeks at the next token.
    fn peek<'b>(&'b mut self) -> Option<&'b (Token, Span)>
    {
        // Get token from list of already parsed tokens if it exists
        if self.token_pos < self.tokens.len()
        {
            let token = &self.tokens[self.token_pos];
            Some(token)

        // Otherwise get token from lexer
        } else
        {
            let span = self.lexer.span();
            self.tokens.push((self.lexer.next()?, span));
            self.tokens.last()
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
            String::from(self.lexer.slice())
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
    Int(i64),
    Float(f64),

    // Booleans
    True,
    False,

    // String
    String(String),

    // Symbol (variables and stuff)
    Symbol(String),

    // Prefix expressions
    Prefix(String, Box<AST>),

    // Infix expressions
    Infix(String, Box<AST>, Box<AST>),

    // If expressions
    If(Box<AST>, Box<AST>, Box<AST>),
}

#[derive(Debug)]
pub struct ParseError
{
    
}

// value(&mut Parser) -> Result<AST, ParseError>
// Gets the next value.
fn value(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Get token
    let token = match parser.peek()
    {
        Some(v) => v.0,
        None => return Err(ParseError {

        })
    };

    // Check for int
    if let Token::Int(n) = token
    {
        parser.next();
        Ok(AST::Int(n))

    // Check for float
    } else if let Token::Float(n) = token
    {
        parser.next();
        Ok(AST::Float(n))

    // Check for string
    } else if let Token::String = token
    {
        let s = parser.slice();
        parser.next();
        Ok(AST::String(String::from(s)))
    
    // True
    } else if let Token::True = token
    {
        parser.next();
        Ok(AST::True)

    // False
    } else if let Token::False = token
    {
        parser.next();
        Ok(AST::False)

    // Check for symbol
    } else if let Token::Symbol = token
    {
        let s = parser.slice();
        parser.next();
        Ok(AST::Symbol(String::from(s)))

    // Parenthesised expressions
    } else if let Token::LParen = token
    {
        // Get value
        let state = parser.save_state();
        parser.next();
        let value = match expression(parser) {
            Ok(v) => v,
            Err(e) => {
                parser.return_state(state);
                return Err(e);
            }
        };

        // Get right parenthesis
        if let Some((Token::RParen, _)) = parser.peek()
        {
            parser.next();
            Ok(value)
        } else
        {
            parser.return_state(state);
            Err(ParseError {

            })
        }

    // Not a value
    } else
    {
        Err(ParseError {

        })
    }
}

// prefix(&mut Parser) -> Result<AST, ParseError>
// Gets the next prefix expression.
fn prefix(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Set up
    let state = parser.save_state();
    let token = match parser.peek()
    {
        Some(v) => v,
        None => return Err(ParseError {

        })
    };

    // Unary minus
    if let Token::Sub = token.0
    {
        parser.next();

        // Get value
        let value = match value(parser)
        {
            Ok(v) => v,
            Err(e) => {
                parser.return_state(state);
                return Err(e);
            }
        };
        Ok(AST::Prefix(String::from("-"), Box::new(value)))

    // Span
    } else if let Token::Mul = token.0
    {
        parser.next();

        // Get value
        let value = match value(parser)
        {
            Ok(v) => v,
            Err(e) => {
                parser.return_state(state);
                return Err(e);
            }
        };
        Ok(AST::Prefix(String::from("-"), Box::new(value)))
    
    // Default to regular value
    } else
    {
        value(parser)
    }
}

// muldivmod(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next multiplication/division/modulus expression.
fn muldivmod(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Set up
    let mut left = prefix(parser)?;
    let state = parser.save_state();

    loop
    {
        // Save current state
        let state2 = parser.save_state();

        // Check for operator
        if let Some(op) = parser.peek()
        {
            // Get operator
            let op = match op.0
            {
                Token::Mul => String::from("*"),
                Token::DivMod => String::from(parser.slice()),
                _ => {
                    parser.return_state(state2);
                    break;
                }
            };
            parser.next();

            // Get right hand side
            let right = match prefix(parser)
            {
                Ok(v) => v,
                Err(_) => {
                    parser.return_state(state);
                    return Err(ParseError {

                    });
                }
            };

            // Build ast
            left = AST::Infix(op, Box::new(left), Box::new(right));

        // If there's no operator, break
        } else
        {
            break;
        }
    }

    Ok(left)
}

// addsub(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next addition/subtraction expression.
fn addsub(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Set up
    let mut left = muldivmod(parser)?;
    let state = parser.save_state();

    loop
    {
        // Save current state
        let state2 = parser.save_state();

        // Check for operator
        if let Some(op) = parser.peek()
        {
            // Get operator
            let op = match op.0
            {
                Token::Add => String::from("+"),
                Token::Sub => String::from("-"),
                _ => {
                    parser.return_state(state2);
                    break;
                }
            };
            parser.next();

            // Get right hand side
            let right = match muldivmod(parser)
            {
                Ok(v) => v,
                Err(_) => {
                    parser.return_state(state);
                    return Err(ParseError {

                    });
                }
            };

            // Build ast
            left = AST::Infix(op, Box::new(left), Box::new(right));

        // If there's no operator, break
        } else
        {
            break;
        }
    }

    Ok(left)
}

// bitshift(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next bitshift expression.
fn bitshift(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Set up
    let mut left = addsub(parser)?;
    let state = parser.save_state();

    loop
    {
        // Save current state
        let state2 = parser.save_state();

        // Check for operator
        if let Some(op) = parser.peek()
        {
            // Get operator
            let op = match op.0
            {
                Token::BitShift => String::from(parser.slice()),
                _ => {
                    parser.return_state(state2);
                    break;
                }
            };
            parser.next();

            // Get right hand side
            let right = match addsub(parser)
            {
                Ok(v) => v,
                Err(_) => {
                    parser.return_state(state);
                    return Err(ParseError {

                    });
                }
            };

            // Build ast
            left = AST::Infix(op, Box::new(left), Box::new(right));

        // If there's no operator, break
        } else
        {
            break;
        }
    }

    Ok(left)
}

// compare(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next comparison expression.
fn compare(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Set up
    let mut left = bitshift(parser)?;
    let state = parser.save_state();

    loop
    {
        // Save current state
        let state2 = parser.save_state();

        // Check for operator
        if let Some(op) = parser.peek()
        {
            // Get operator
            let op = match op.0
            {
                Token::Compare => String::from(parser.slice()),
                _ => {
                    parser.return_state(state2);
                    break;
                }
            };
            parser.next();

            // Get right hand side
            let right = match bitshift(parser)
            {
                Ok(v) => v,
                Err(_) => {
                    parser.return_state(state);
                    return Err(ParseError {

                    });
                }
            };

            // Build ast
            left = AST::Infix(op, Box::new(left), Box::new(right));

        // If there's no operator, break
        } else
        {
            break;
        }
    }

    Ok(left)
}

// and(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next logical and expression.
fn and(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Set up
    let mut left = compare(parser)?;
    let state = parser.save_state();

    loop
    {
        // Save current state
        let state2 = parser.save_state();

        // Check for operator
        if let Some(op) = parser.peek()
        {
            // Get operator
            let op = match op.0
            {
                Token::And => String::from(parser.slice()),
                _ => {
                    parser.return_state(state2);
                    break;
                }
            };
            parser.next();

            // Get right hand side
            let right = match compare(parser)
            {
                Ok(v) => v,
                Err(_) => {
                    parser.return_state(state);
                    return Err(ParseError {

                    });
                }
            };

            // Build ast
            left = AST::Infix(op, Box::new(left), Box::new(right));

        // If there's no operator, break
        } else
        {
            break;
        }
    }

    Ok(left)
}

// or(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next logical or expression.
fn or(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Set up
    let mut left = and(parser)?;
    let state = parser.save_state();

    loop
    {
        // Save current state
        let state2 = parser.save_state();

        // Check for operator
        if let Some(op) = parser.peek()
        {
            // Get operator
            let op = match op.0
            {
                Token::Or => String::from(parser.slice()),
                _ => {
                    parser.return_state(state2);
                    break;
                }
            };
            parser.next();

            // Get right hand side
            let right = match and(parser)
            {
                Ok(v) => v,
                Err(_) => {
                    parser.return_state(state);
                    return Err(ParseError {

                    });
                }
            };

            // Build ast
            left = AST::Infix(op, Box::new(left), Box::new(right));

        // If there's no operator, break
        } else
        {
            break;
        }
    }

    Ok(left)
}

// xor(&mut Parser) -> Option<AST::Infix, ParseError>
// Gets the next logical xor expression.
fn xor(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Set up
    let mut left = or(parser)?;
    let state = parser.save_state();

    loop
    {
        // Save current state
        let state2 = parser.save_state();

        // Check for operator
        if let Some(op) = parser.peek()
        {
            // Get operator
            let op = match op.0
            {
                Token::Xor => String::from(parser.slice()),
                _ => {
                    parser.return_state(state2);
                    break;
                }
            };
            parser.next();

            // Get right hand side
            let right = match or(parser)
            {
                Ok(v) => v,
                Err(_) => {
                    parser.return_state(state);
                    return Err(ParseError {

                    });
                }
            };

            // Build ast
            left = AST::Infix(op, Box::new(left), Box::new(right));

        // If there's no operator, break
        } else
        {
            break;
        }
    }

    Ok(left)
}

fn if_expr(parser: &mut Parser) -> Result<AST, ParseError>
{
    // Get if keyword
    if let Some((Token::If, _)) = parser.peek()
    {
        let state = parser.save_state();
        parser.next();

        // Get condition
        let cond = match expression(parser)
        {
            Ok(v) => v,
            Err(e) => {
                parser.return_state(state);
                return Err(e);
            }
        };

        // Get then keyword
        match parser.peek()
        {
            Some((Token::Then, _)) => {
                parser.next();
            }
            _ => {
                parser.return_state(state);
                return Err(ParseError {

                });
            }
        }

        // Get body
        let then = match expression(parser)
        {
            Ok(v) => v,
            Err(e) => {
                parser.return_state(state);
                return Err(e);
            }
        };

        // Get else keyword
        match parser.peek()
        {
            Some((Token::Else, _)) => {
                parser.next();
            }
            _ => {
                parser.return_state(state);
                return Err(ParseError {

                });
            }
        }

        // Get else clause
        let elsy = match expression(parser)
        {
            Ok(v) => v,
            Err(e) => {
                parser.return_state(state);
                return Err(e);
            }
        };

        Ok(AST::If(Box::new(cond), Box::new(then), Box::new(elsy)))

    // Not an if expression
    } else
    {
        Err(ParseError {

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
    } else
    {
        xor(parser)
    }
}

#[cfg(test)]
mod tests
{
    use super::*;

    #[test]
    fn ints()
    {
        let mut parser = Parser::new("32 0x123abc 0b010101");
        assert_eq!(value(&mut parser).unwrap(), AST::Int(32));
        assert_eq!(value(&mut parser).unwrap(), AST::Int(0x123abc));
        assert_eq!(value(&mut parser).unwrap(), AST::Int(0b010101));
    }

    #[test]
    fn floats()
    {
        let mut parser = Parser::new("0.25 2e3 2e-3");
        assert_eq!(value(&mut parser).unwrap(), AST::Float(0.25));
        assert_eq!(value(&mut parser).unwrap(), AST::Float(2e3));
        assert_eq!(value(&mut parser).unwrap(), AST::Float(2e-3))
    }

    #[test]
    fn prefix()
    {
        let mut parser = Parser::new("-2");
        assert_eq!(super::prefix(&mut parser).unwrap(), AST::Prefix(String::from("-"), Box::new(AST::Int(2))));
    }

    #[test]
    fn mul()
    {
        let mut parser = Parser::new("2*3 2*3*4");
        assert_eq!(muldivmod(&mut parser).unwrap(), AST::Infix(String::from("*"), Box::new(AST::Int(2)), Box::new(AST::Int(3))));
        assert_eq!(muldivmod(&mut parser).unwrap(), AST::Infix(String::from("*"), Box::new(AST::Infix(String::from("*"), Box::new(AST::Int(2)), Box::new(AST::Int(3)))), Box::new(AST::Int(4))));
    }

    #[test]
    fn parentheses()
    {
        let mut parser = Parser::new("2*(3+4)");
        assert_eq!(expression(&mut parser).unwrap(), AST::Infix(String::from("*"), Box::new(AST::Int(2)), Box::new(AST::Infix(String::from("+"), Box::new(AST::Int(3)), Box::new(AST::Int(4))))));

    }

    #[test]
    fn iffy()
    {
        let mut parser = Parser::new("if true then 1 else 0");
        assert_eq!(if_expr(&mut parser).unwrap(), AST::If(Box::new(AST::True), Box::new(AST::Int(1)), Box::new(AST::Int(0))));
    }
}
