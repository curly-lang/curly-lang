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
    
    #[regex(r"\+|-")]
    AddSub,
    
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
    #[regex("true|false")]
    Bool,
    
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

    // String
    String(String),

    // Symbol (variables and stuff)
    Symbol(String),

    // Infix expressions
    Infix(String, Box<AST>, Box<AST>)
}

// value(&mut Lexer<Token>) -> Option<AST>
// Gets the next value.
fn value(parser: &mut Parser) -> Option<AST>
{
    // Get token
    let token = parser.peek()?.0;

    // Check for int
    if let Token::Int(n) = token
    {
        parser.next();
        Some(AST::Int(n))

    // Check for float
    } else if let Token::Float(n) = token
    {
        parser.next();
        Some(AST::Float(n))

    // Check for string
    } else if let Token::String = token
    {
        let s = parser.slice();
        parser.next();
        Some(AST::String(String::from(s)))

    // Check for symbol
    } else if let Token::Symbol = token
    {
        let s = parser.slice();
        parser.next();
        Some(AST::Symbol(String::from(s)))

    // Not a value
    } else
    {
        None
    }
}

// muldivmod(&mut Lexer<Token>) -> Option<AST::Infix>
// Gets the next multiplication/division/modulus expression.
fn muldivmod(parser: &mut Parser) -> Option<AST>
{
    // Set up
    let mut left = value(parser)?;
    let state = parser.save_state();
    
    loop
    {
        // Save current state
        let mut state2 = parser.save_state();

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
            let right = match value(parser)
            {
                Some(v) => v,
                None => {
                    parser.return_state(state);
                    return None;
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

    Some(left)
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
    fn mul()
    {
        let mut parser = Parser::new("2*3 2*3*4");
        assert_eq!(muldivmod(&mut parser).unwrap(), AST::Infix(String::from("*"), Box::new(AST::Int(2)), Box::new(AST::Int(3))));
        assert_eq!(muldivmod(&mut parser).unwrap(), AST::Infix(String::from("*"), Box::new(AST::Infix(String::from("*"), Box::new(AST::Int(2)), Box::new(AST::Int(3)))), Box::new(AST::Int(4))));
    }
}
