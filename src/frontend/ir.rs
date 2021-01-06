use super::parser::AST;
use super::types::Type;

// Represents a prefix operator.
pub enum PrefixOp
{
    Neg,
    Span,
}

// Represents an infix operator.
pub enum BinOp
{
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    BSL,
    BSR,
    LT,
    GT,
    LEQ,
    GEQ,
    EQ,
    NEQ,
    In,
    And,
    Or,
    Xor,
    BoolXor,
}

// Represents metadata associated with sexpressions.
pub struct SExprMetadata
{
    _type: Type,
}

// Represents an s expression
pub enum SExpr
{
    // Ints
    Int(SExprMetadata, i64),

    // Floats
    Float(SExprMetadata, f64),

    // Booleans
    True(SExprMetadata),
    False(SExprMetadata),

    // Symbols
    Symbol(SExprMetadata, String),

    // Strings
    String(SExprMetadata, String),

    // Functions
    Function(SExprMetadata, usize),

    // Prefix expression
    Prefix(SExprMetadata, PrefixOp, Box<SExpr>),

    // Infix expression
    Infix(SExprMetadata, BinOp, Box<SExpr>, Box<SExpr>),

    // Boolean and
    And(SExprMetadata, Box<SExpr>, Box<SExpr>),

    // Boolean or
    Or(SExprMetadata, Box<SExpr>, Box<SExpr>),

    // If expression
    If(SExprMetadata, Box<SExpr>, Box<SExpr>, Box<SExpr>),

    // Function application
    Application(SExprMetadata, Box<SExpr>, Box<SExpr>),

    // Assignment
    Assign(SExprMetadata, String, Box<SExpr>),
}

impl SExpr
{
    // get_metadata(&SExpr) -> &mut SExprMetadata
    // Returns an immutable reference to the metadata.
    pub fn get_metadata(&self) -> &SExprMetadata
    {
        match self
        {
            Self::Int(m, _) => m,
            Self::Float(m, _) => m,
            Self::True(m) => m,
            Self::False(m) => m,
            Self::Symbol(m, _) => m,
            Self::String(m, _) => m,
            Self::Function(m, _) => m,
            Self::Prefix(m, _, _) => m,
            Self::Infix(m, _, _, _) => m,
            Self::And(m, _, _) => m,
            Self::Or(m, _, _) => m,
            Self::If(m, _, _, _) => m,
            Self::Application(m, _, _) => m,
            Self::Assign(m, _, _) => m
        }
    }

    // get_mutable_metadata(&mut SExpr) -> &mut SExprMetadata
    // Returns a mutable reference to the metadata.
    pub fn get_mutable_metadata(&mut self) -> &mut SExprMetadata
    {
        match self
        {
            Self::Int(m, _) => m,
            Self::Float(m, _) => m,
            Self::True(m) => m,
            Self::False(m) => m,
            Self::Symbol(m, _) => m,
            Self::String(m, _) => m,
            Self::Function(m, _) => m,
            Self::Prefix(m, _, _) => m,
            Self::Infix(m, _, _, _) => m,
            Self::And(m, _, _) => m,
            Self::Or(m, _, _) => m,
            Self::If(m, _, _, _) => m,
            Self::Application(m, _, _) => m,
            Self::Assign(m, _, _) => m
        }
    }
}

// Represents the ir.
pub struct IR
{
    pub sexprs: Vec<SExpr>
}

// convert_node(AST) -> SExpr
// Converts an ast node into an sexpression.
fn convert_node(ast: AST) -> SExpr
{
    match ast
    {
        // Int
        AST::Int(n) => SExpr::Int(SExprMetadata {
            _type: Type::Int
        }, n),

        // Float
        AST::Float(n) => SExpr::Float(SExprMetadata {
            _type: Type::Float
        }, n),

        // True
        AST::True => SExpr::True(SExprMetadata {
            _type: Type::Bool
        }),
 
        // False
        AST::False => SExpr::False(SExprMetadata {
            _type: Type::Bool
        }),

        // Symbol
        AST::Symbol(s) => SExpr::Symbol(SExprMetadata {
            _type: Type::Unknown
        }, s),

        // String
        AST::String(s) => SExpr::String(SExprMetadata {
            _type: Type::String
        }, s),

        // Prefix
        AST::Prefix(op, v) => {
            let op = match op.as_str()
            {
                "-" => PrefixOp::Neg,
                "*" => PrefixOp::Span,
                _ => panic!("Invalid operator"),
            };

            SExpr::Prefix(SExprMetadata {
                _type: Type::Bool
            }, op, Box::new(convert_node(*v)))
        }

        // Infix
        AST::Infix(op, l, r) => {
            // Separate sexpressions for and and or
            if op == "and"
            {
                SExpr::And(SExprMetadata {
                    _type: Type::Bool
                }, Box::new(convert_node(*l)), Box::new(convert_node(*r)))
            } else if op == "or"
            {
                 SExpr::Or(SExprMetadata {
                    _type: Type::Bool
                }, Box::new(convert_node(*l)), Box::new(convert_node(*r)))
           } else
            {
                // Get operator
                let op = match op.as_str()
                {
                    "*" => BinOp::Mul,
                    "/" => BinOp::Div,
                    "%" => BinOp::Mod,
                    "+" => BinOp::Add,
                    "-" => BinOp::Sub,
                    "<<" => BinOp::BSL,
                    ">>" => BinOp::BSR,
                    "&" => BinOp::And,
                    "|" => BinOp::Or,
                    "^" => BinOp::Xor,
                    "xor" => BinOp::BoolXor,
                    _ => panic!("Invalid operator"),
                };

                // Return
                SExpr::Infix(SExprMetadata {
                    _type: Type::Unknown
                }, op, Box::new(convert_node(*l)), Box::new(convert_node(*r)))
            }
        }

        // If expression
        AST::If { cond, then, elsy } => SExpr::If(SExprMetadata {
            _type: Type::Unknown
        }, Box::new(convert_node(*cond)), Box::new(convert_node(*then)), Box::new(convert_node(*elsy))),

        AST::Application(l, r) => SExpr::Application(SExprMetadata {
            _type: Type::Unknown
        }, Box::new(convert_node(*l)), Box::new(convert_node(*r))),

        _ => panic!("Unimplemented AST node!")
    }
}

// convert_ast_to_ir(Vec<AST>) -> IR
// Converts a list of asts into ir.
pub fn convert_ast_to_ir(asts: Vec<AST>) -> IR
{
    let mut sexprs = vec![];

    for ast in asts
    {
        sexprs.push(convert_node(ast));
    }

    IR {
        sexprs
    }
}

