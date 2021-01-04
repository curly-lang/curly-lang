use logos::{Logos, Lexer, Span};

#[derive(Logos, PartialEq, Debug, Copy, Clone)]
pub enum Token
{
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
   
    #[token("\n")]
    Newline,
   
    #[regex(r"[ \t\f]+", logos::skip)]
    Whitespace,
   
    #[error]
    Error,

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
    
    #[regex(r"[0-9]+", |lex| lex.slice().parse())]
    #[regex(r"0x[0-9a-fA-F]+", |lex| i64::from_str_radix(&lex.slice()[2..], 16))]
    #[regex(r"0b[01]+", |lex| i64::from_str_radix(&lex.slice()[2..], 2))]
    Int(i64),
    
    #[regex(r"[0-9]+(\.[0-9]*([eE][+-]?[0-9]+)?|[eE][+-]?[0-9]+)", |lex| lex.slice().parse())]
    Float(f64),
    
    #[regex(r"[a-zA-Z_\$][a-zA-Z0-9_']")]
    Symbol,
    
    #[regex(r#""([^\\]|\\.)*""#)]
    String,
    
    #[regex("true|false")]
    Bool,
    
    #[token("->")]
    RightArrow,
    
    #[token("=>")]
    ThiccArrow,
    
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

struct Parser<'a>
{
    lexer: Lexer<'a, Token>,
    tokens: Vec<(Span, Token)>,
    token_pos: usize
}

impl<'a> Iterator for Parser<'a>
{
    type Item = (Span, Token);

    fn next(&mut self) -> Option<(Span, Token)>
    {
        if self.token_pos < self.tokens.len()
        {
            let token = self.tokens[self.token_pos].clone();
            self.token_pos += 1;
            Some(token)
        } else
        {
            let token = (self.lexer.span(), self.lexer.next()?);
            self.tokens.push(token.clone());
            Some(token)
        }
    }
}

#[derive(Debug)]
pub enum AST
{
    Int(i64),
    Float(f64),
    String(String),
    Symbol(String),
    Infix(String, Box<AST>, Box<AST>)
}

// value(&mut Lexer<Token>) -> Option<AST>
// Gets the next value.
fn value(parser: &mut Parser) -> Option<AST>
{
    let mut peekable = parser.peekable();
    let token = peekable.peek()?.1;

    if let Token::Int(n) = token
    {
        parser.next();
        Some(AST::Int(n))
    } else if let Token::Float(n) = token
    {
        parser.next();
        Some(AST::Float(n))
    } else if let Token::String = token
    {
        let s = parser.lexer.slice();
        parser.next();
        Some(AST::String(String::from(s)))
    } else if let Token::Symbol = token
    {
        let s = parser.lexer.slice();
        parser.next();
        Some(AST::Symbol(String::from(s)))
    } else
    {
        None
    }
}

// muldivmod(&mut Lexer<Token>) -> Option<AST::Infix>
// Gets the next multiplication/division/modulus expression.
fn muldivmod(parser: &mut Parser) -> Option<AST>
{

    let left = value(parser)?;
    let mut peekable = parser.peekable();
    let op = match peekable.peek()?.1
    {
        Token::Mul => String::from("*"),
        Token::DivMod => String::from(parser.lexer.slice()),
        _ => return None
    };
    let right = value(parser)?;
    Some(AST::Infix(op, Box::new(left), Box::new(right)))
}

