use std::collections::HashMap;

use super::parser::AST;
use super::scopes::Scope;
use super::types;
use super::types::Type;

// Represents a prefix operator.
#[derive(Debug, Hash, PartialEq, Eq)]
pub enum PrefixOp
{
    Neg,
    Span,
}

// Represents an infix operator.
#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
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
#[derive(Debug)]
pub struct SExprMetadata
{
    pub _type: Type,
}

// Represents an s expression
#[derive(Debug)]
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

    // Scoping
    With(SExprMetadata, Vec<SExpr>, Box<SExpr>),
}

impl SExpr
{
    // get_metadata(&SExpr) -> &SExprMetadata
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
            Self::Assign(m, _, _) => m,
            Self::With(m, _, _) => m
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
            Self::Assign(m, _, _) => m,
            Self::With(m, _, _) => m
        }
    }
}

#[derive(Debug)]
pub struct IRFunction
{
    pub args: Vec<(String, Type)>,
    pub body: SExpr
}

#[derive(Debug)]
pub struct IRMetadata
{
    pub funcs: Vec<IRFunction>,
    pub scope: Scope,
}

// Represents the ir.
#[derive(Debug)]
pub struct IR
{
    pub metadata: IRMetadata,
    pub sexprs: Vec<SExpr>
}

impl IR
{
    // new() -> IR
    // Creates a new root IR.
    pub fn new() -> IR
    {
        IR {
            metadata: IRMetadata {
                funcs: vec![],
                scope: Scope {
                    variables: HashMap::new(),
                    funcs: HashMap::new(),
                    func_ret_types: HashMap::new(),
                    parent: None
                }
            },
            sexprs: vec![]
        }
    }

    // clear(&mut self) -> ()
    // Clears the root of any sexpressions.
    pub fn clear(&mut self)
    {
        self.sexprs.clear();
    }

    // push_scope(&mut self) -> ()
    // Pushes a new scope to the top of the scope stack.
    pub fn push_scope(&mut self)
    {
        use std::mem::swap;

        let mut scope = Scope {
            variables: HashMap::new(),
            funcs: HashMap::new(),
            func_ret_types: HashMap::new(),
            parent: None
        };

        swap(&mut scope, &mut self.metadata.scope);
        self.metadata.scope.parent = Some(Box::new(scope));
    }

    // pop_scop(&mut self) -> ()
    // Pops a scope from the stack if a parent scope exists.
    pub fn pop_scope(&mut self)
    {
        use std::mem::swap;

        if let Some(v) = &mut self.metadata.scope.parent
        {
            let mut scope = Scope {
                variables: HashMap::with_capacity(0),
                funcs: HashMap::with_capacity(0),
                func_ret_types: HashMap::with_capacity(0),
                parent: None
            };

            swap(&mut scope, v);
            swap(&mut self.metadata.scope, &mut scope);
        }
    }
}

// convert_node(AST) -> SExpr
// Converts an ast node into an sexpression.
fn convert_node(ast: AST, funcs: &mut Vec<IRFunction>) -> SExpr
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
                _type: Type::Unknown
            }, op, Box::new(convert_node(*v, funcs)))
        }

        // Infix
        AST::Infix(op, l, r) => {
            // Separate sexpressions for and and or
            if op == "and"
            {
                SExpr::And(SExprMetadata {
                    _type: Type::Bool
                }, Box::new(convert_node(*l, funcs)), Box::new(convert_node(*r, funcs)))
            } else if op == "or"
            {
                 SExpr::Or(SExprMetadata {
                    _type: Type::Bool
                }, Box::new(convert_node(*l, funcs)), Box::new(convert_node(*r, funcs)))
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
                }, op, Box::new(convert_node(*l, funcs)), Box::new(convert_node(*r, funcs)))
            }
        }

        // If expression
        AST::If { cond, then, elsy } => SExpr::If(SExprMetadata {
            _type: Type::Unknown
        }, Box::new(convert_node(*cond, funcs)), Box::new(convert_node(*then, funcs)), Box::new(convert_node(*elsy, funcs))),

        // Application
        AST::Application(l, r) => SExpr::Application(SExprMetadata {
            _type: Type::Unknown
        }, Box::new(convert_node(*l, funcs)), Box::new(convert_node(*r, funcs))),

        // Assignment
        AST::Assign { name, val } => SExpr::Assign(SExprMetadata {
            _type: Type::Unknown
        }, name, Box::new(convert_node(*val, funcs))),

        AST::AssignTyped { name, _type, val } => SExpr::Assign(SExprMetadata {
            _type: types::convert_ast_to_type(*_type)
        }, name, Box::new(convert_node(*val, funcs))),

        // Assigning functions
        AST::AssignFunction { name, args, val } => {
            // Get function id
            let func_id = SExpr::Function(SExprMetadata {
                _type: Type::Unknown
            }, funcs.len());

            // Create the function
            let func = IRFunction {
                args: args.into_iter().map(|v| (v.0, types::convert_ast_to_type(v.1))).collect(),
                body: convert_node(*val, funcs)
            };
            funcs.push(func);
            let func = funcs.last().unwrap();

            // Get the function type
            let mut type_acc = func.body.get_metadata()._type.clone();
            for arg in func.args.iter().rev()
            {
                type_acc = Type::Func(Box::new(arg.1.clone()), Box::new(type_acc));
            }

            // Return assigning to the function id
            SExpr::Assign(SExprMetadata {
                _type: type_acc
            }, name, Box::new(func_id))
        }

        // With expressions
        AST::With(a, v) => {
            let v = convert_node(*v, funcs);
            SExpr::With(SExprMetadata {
                _type: v.get_metadata()._type.clone()
            }, a.into_iter().map(|a| convert_node(a, funcs)).collect(), Box::new(v))
        }
    }
}

// convert_ast_to_ir(Vec<AST>) -> IR
// Converts a list of asts into ir.
pub fn convert_ast_to_ir(asts: Vec<AST>, ir: &mut IR)
{
    for ast in asts
    {
        ir.sexprs.push(convert_node(ast, &mut ir.metadata.funcs));
    }
}

