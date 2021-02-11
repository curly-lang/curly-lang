use logos::Span;
use std::collections::HashMap;
use std::iter::FromIterator;

use super::parser::AST;
use super::scopes::Scope;
use super::types;
use super::types::Type;

// Represents a location
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Location
{
    pub span: Span,
    pub filename: String
}

impl Location
{
    // new(Span, &str) -> Location
    // Creates a new Location.
    pub fn new(span: Span, filename: &str) -> Location
    {
        Location {
            span,
            filename: String::from(filename)
        }
    }

    // empty() -> Location
    // Creates an empty Location.
    pub fn empty() -> Location
    {
        Location {
            span: Span { start: 0, end: 0 },
            filename: String::with_capacity(0)
        }
    }

}

// Represents a prefix operator.
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
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
#[derive(Debug, Clone)]
pub struct SExprMetadata
{
    pub loc: Location,
    pub loc2: Location,
    pub _type: Type,
    pub arity: usize,
    pub saved_argc: Option<usize>,
    pub tailrec: bool
}

impl SExprMetadata
{
    // empty() -> SExprMetadata
    // Creates empty metadata.
    pub fn empty() -> SExprMetadata
    {
        SExprMetadata {
            loc: Location::empty(),
            loc2: Location::empty(),
            _type: Type::Error,
            arity: 0,
            saved_argc: None,
            tailrec: false

        }
    }
}

// Represents an s expression
#[derive(Debug, Clone)]
pub enum SExpr
{
    // Empty
    TypeAlias(SExprMetadata, String),

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

    // Lists
    List(SExprMetadata, Vec<SExpr>),

    // Functions
    Function(SExprMetadata, String),

    // Prefix expression
    Prefix(SExprMetadata, PrefixOp, Box<SExpr>),

    // Infix expression
    Infix(SExprMetadata, BinOp, Box<SExpr>, Box<SExpr>),

    // Casting
    As(SExprMetadata, Box<SExpr>),

    // Boolean operators
    And(SExprMetadata, Box<SExpr>, Box<SExpr>),
    Or(SExprMetadata, Box<SExpr>, Box<SExpr>),

    // If expression
    If(SExprMetadata, Box<SExpr>, Box<SExpr>, Box<SExpr>),

    // Function application
    Application(SExprMetadata, Box<SExpr>, Box<SExpr>),

    // Assignment
    Assign(SExprMetadata, String, Box<SExpr>),

    // Scoping
    With(SExprMetadata, Vec<SExpr>, Box<SExpr>),

    // Match expressions
    Match(SExprMetadata, Box<SExpr>, Vec<(Type, SExpr)>),

    // Member access
    MemberAccess(SExprMetadata, Vec<String>)
}

impl SExpr
{
    // get_metadata(&SExpr) -> &SExprMetadata
    // Returns an immutable reference to the metadata.
    pub fn get_metadata(&self) -> &SExprMetadata
    {
        match self
        {
            Self::TypeAlias(m, _)
                | Self::Int(m, _)
                | Self::Float(m, _)
                | Self::True(m)
                | Self::False(m)
                | Self::Symbol(m, _)
                | Self::String(m, _)
                | Self::List(m, _)
                | Self::Function(m, _)
                | Self::Prefix(m, _, _)
                | Self::Infix(m, _, _, _)
                | Self::As(m, _)
                | Self::And(m, _, _)
                | Self::Or(m, _, _)
                | Self::If(m, _, _, _)
                | Self::Application(m, _, _)
                | Self::Assign(m, _, _)
                | Self::With(m, _, _)
                | Self::Match(m, _, _)
                | Self::MemberAccess(m, _)
                => m
        }
    }

    // get_mutable_metadata(&mut SExpr) -> &mut SExprMetadata
    // Returns a mutable reference to the metadata.
    pub fn get_mutable_metadata(&mut self) -> &mut SExprMetadata
    {
        match self
        {
            Self::TypeAlias(m, _)
                | Self::Int(m, _)
                | Self::Float(m, _)
                | Self::True(m)
                | Self::False(m)
                | Self::Symbol(m, _)
                | Self::String(m, _)
                | Self::List(m, _)
                | Self::Function(m, _)
                | Self::Prefix(m, _, _)
                | Self::Infix(m, _, _, _)
                | Self::As(m, _)
                | Self::And(m, _, _)
                | Self::Or(m, _, _)
                | Self::If(m, _, _, _)
                | Self::Application(m, _, _)
                | Self::Assign(m, _, _)
                | Self::With(m, _, _)
                | Self::Match(m, _, _)
                | Self::MemberAccess(m, _)
                => m
        }
    }
}

// Represents a function in the IR.
#[derive(Debug)]
pub struct IRFunction
{
    pub loc: Location,
    pub args: Vec<(String, Type)>,
    pub captured: HashMap<String, Type>,
    pub captured_names: Vec<String>,
    pub body: SExpr,
    pub global: bool,
    pub checked: bool,
    pub written: bool
}

// Represents the ir.
#[derive(Debug)]
pub struct IR
{
    pub scope: Scope,
    pub funcs: HashMap<String, IRFunction>,
    pub types: HashMap<String, Type>,
    pub sexprs: HashMap<String, Vec<SExpr>>
}

impl IR
{
    // new() -> IR
    // Creates a new root IR.
    pub fn new() -> IR
    {
        IR {
            scope: Scope::new().init_builtins(),
            funcs: HashMap::with_capacity(0),
            types: HashMap::with_capacity(0),
            sexprs: HashMap::with_capacity(0)
        }
    }

    // clear(&mut self) -> ()
    // Clears the root of any sexpressions.
    pub fn clear(&mut self)
    {
        use std::mem::swap;

        self.sexprs.clear();
        let mut funcs = HashMap::with_capacity(0);
        swap(&mut funcs, &mut self.funcs);
        self.funcs = HashMap::from_iter(funcs.into_iter().filter(|v| v.1.global));
        for f in self.funcs.iter_mut()
        {
            f.1.written = true;
        }
        let mut vars = HashMap::with_capacity(0);
        swap(&mut vars, &mut self.scope.variables);
        self.scope.variables = HashMap::from_iter(vars.into_iter().filter(|v| v.1.4));
    }
}

// convert_node(AST, &str, bool, &mut HashMap<String, IRFunction>, &mut HashMap<String, HashMap>) -> SExpr
// Converts an ast node into an sexpression.
fn convert_node(ast: AST, filename: &str, funcs: &mut HashMap<String, IRFunction>, global: bool, seen_funcs: &mut HashMap<String, usize>, types: &mut HashMap<String, Type>) -> SExpr
{
    match ast
    {
        // Int
        AST::Int(span, n) => SExpr::Int(SExprMetadata {
            loc: Location::new(span, filename),
            loc2: Location::empty(),
            _type: Type::Int,
            arity: 0,
            saved_argc: None,
            tailrec: false
        }, n),

        // Float
        AST::Float(span, n) => SExpr::Float(SExprMetadata {
            loc: Location::new(span, filename),
            loc2: Location::empty(),
            _type: Type::Float,
            arity: 0,
            saved_argc: None,
            tailrec: false
        }, n),

        // True
        AST::True(span) => SExpr::True(SExprMetadata {
            loc: Location::new(span, filename),
            loc2: Location::empty(),
            _type: Type::Bool,
            arity: 0,
            saved_argc: None,
            tailrec: false
        }),
 
        // False
        AST::False(span) => SExpr::False(SExprMetadata {
            loc: Location::new(span, filename),
            loc2: Location::empty(),
            _type: Type::Bool,
            arity: 0,
            saved_argc: None,
            tailrec: false
        }),

        AST::List(span, list) => SExpr::List(SExprMetadata {
            loc: Location::new(span, filename),
            loc2: Location::empty(),
            _type: Type::Error,
            arity: 0,
            saved_argc: None,
            tailrec: false
        }, list.into_iter().map(|v| convert_node(v, filename, funcs, global, seen_funcs, types)).collect()),

        // Symbol
        AST::Symbol(span, s) => SExpr::Symbol(SExprMetadata {
            loc: Location::new(span, filename),
            loc2: Location::empty(),
            _type: Type::Error,
            arity: 0,
            saved_argc: None,
            tailrec: false
        }, s),

        AST::Annotation(_, _)
            | AST::Import(_, _, _)
            | AST::QualifiedImport(_, _, _)
            | AST::Header(_, _, _, _)
            => {
            unreachable!("annotations, imports, and headers are already handled!");
        }

        // String
        AST::String(span, s) => SExpr::String(SExprMetadata {
            loc: Location::new(span, filename),
            loc2: Location::empty(),
            _type: Type::String,
            arity: 0,
            saved_argc: None,
            tailrec: false
        }, s),

        // Prefix
        AST::Prefix(span, op, v) => {
            let op = match op.as_str()
            {
                "-" => PrefixOp::Neg,
                "*" => PrefixOp::Span,
                _ => panic!("Invalid operator"),
            };

            SExpr::Prefix(SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                _type: Type::Error,
                arity: 0,
                saved_argc: None,
                tailrec: false
            }, op, Box::new(convert_node(*v, filename, funcs, global, seen_funcs, types)))
        }

        // Infix
        AST::Infix(span, op, l, r) => {
            // Deal with boolean operators
            if op == "and"
            {
                SExpr::And(SExprMetadata {
                    loc: Location::new(span, filename),
                    loc2: Location::empty(),
                    _type: Type::Error,
                    arity: 0,
                    saved_argc: None,
                    tailrec: false
                }, Box::new(convert_node(*l, filename, funcs, global, seen_funcs, types)), Box::new(convert_node(*r, filename, funcs, global, seen_funcs, types)))
            } else if op == "or"
            {
                SExpr::Or(SExprMetadata {
                    loc: Location::new(span, filename),
                    loc2: Location::empty(),
                    _type: Type::Error,
                    arity: 0,
                    saved_argc: None,
                    tailrec: false
                }, Box::new(convert_node(*l, filename, funcs, global, seen_funcs, types)), Box::new(convert_node(*r, filename, funcs, global, seen_funcs, types)))

            // Deal with accessing members
            } else if op == "::"
            {
                let mut accesses = vec![];
                if let AST::Symbol(_, s) = *r
                {
                    accesses.push(s);
                }

                let mut l = *l;
                while let AST::Infix(_, _, l_, r) = l
                {
                    if let AST::Symbol(_, s) = *r
                    {
                        accesses.insert(0, s);
                    }

                    l = *l_;
                }

                if let AST::Symbol(_, s) = l
                {
                    accesses.insert(0, s);
                }

                SExpr::MemberAccess(SExprMetadata {
                    loc: Location::new(span, filename),
                    loc2: Location::empty(),
                    _type: Type::Error,
                    arity: 0,
                    saved_argc: None,
                    tailrec: false
                }, accesses)
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
                    "<" => BinOp::LT,
                    ">" => BinOp::GT,
                    "<=" => BinOp::LEQ,
                    ">=" => BinOp::GEQ,
                    "==" => BinOp::EQ,
                    "!=" => BinOp::NEQ,
                    "in" => BinOp::In,
                    "&" => BinOp::And,
                    "|" => BinOp::Or,
                    "^" => BinOp::Xor,
                    "xor" => BinOp::BoolXor,
                    _ => panic!("Invalid operator"),
                };

                // Return
                SExpr::Infix(SExprMetadata {
                    loc: Location::new(span, filename),
                    loc2: Location::empty(),
                    _type: Type::Error,
                    arity: 0,
                    saved_argc: None,
                    tailrec: false
                }, op, Box::new(convert_node(*l, filename, funcs, global, seen_funcs, types)), Box::new(convert_node(*r, filename, funcs, global, seen_funcs, types)))
            }
        }

        AST::As(span, value, _type) => SExpr::As(SExprMetadata {
            loc: Location::new(span, filename),
            loc2: Location::new(_type.get_span(), filename),
            _type: types::convert_ast_to_type(*_type, filename, types),
            arity: 0,
            saved_argc: None,
            tailrec: false
        }, Box::new(convert_node(*value, filename, funcs, global, seen_funcs, types))),

        // If expression
        AST::If(span, cond, then, elsy) => SExpr::If(SExprMetadata {
            loc: Location::new(span, filename),
            loc2: Location::empty(),
            _type: Type::Error,
            arity: 0,
            saved_argc: None,
            tailrec: false
        }, Box::new(convert_node(*cond, filename, funcs, global, seen_funcs, types)), Box::new(convert_node(*then, filename, funcs, global, seen_funcs, types)), Box::new(convert_node(*elsy, filename, funcs, global, seen_funcs, types))),

        // Application
        AST::Application(span, l, r) => SExpr::Application(SExprMetadata {
            loc: Location::new(span, filename),
            loc2: Location::empty(),
            _type: Type::Error,
            arity: 0,
            saved_argc: None,
            tailrec: false
        }, Box::new(convert_node(*l, filename, funcs, global, seen_funcs, types)), Box::new(convert_node(*r, filename, funcs, global, seen_funcs, types))),

        // Assignment
        AST::Assign(span, name, val) => SExpr::Assign(SExprMetadata {
            loc: Location::new(span, filename),
            loc2: Location::empty(),
            _type: Type::Error,
            arity: 0,
            saved_argc: None,
            tailrec: false
        }, name, Box::new(convert_node(*val, filename, funcs, global, seen_funcs, types))),

        // Assignment with types
        AST::AssignTyped(span, name, _type, val) => {
            SExpr::Assign(SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::new(_type.get_span().clone(), filename),
                _type: types::convert_ast_to_type(*_type, filename, types),
                arity: 0,
                saved_argc: None,
                tailrec: false
            }, name, Box::new(convert_node(*val, filename, funcs, global, seen_funcs, types)))
        }

        AST::AssignType(span, name, _type) => {
            let span2 = _type.get_span();
            let _type = types::convert_ast_to_type(*_type, filename, types);
            types.insert(name.clone(), _type.clone());
            SExpr::TypeAlias(SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::new(span2, filename),
                _type,
                arity: 0,
                saved_argc: None,
                tailrec: false
            }, name)
        }

        // Assigning functions
        AST::AssignFunction(span, name, args, val) => {
            // Get function id
            let func_name = if seen_funcs.contains_key(&name)
            {
                let seen = seen_funcs.get_mut(&name).unwrap();
                *seen += 1;
                format!("{}${}", name, seen)
            } else
            {
                seen_funcs.insert(name.clone(), 0);
                name.clone()
            };

            let arity = args.len();
            let func_id = SExpr::Function(SExprMetadata {
                loc: Location::new(span.clone(), filename),
                loc2: Location::empty(),
                _type: Type::Error,
                arity,
                saved_argc: None,
                tailrec: false
            }, func_name.clone());

            // Create the function
            let func = IRFunction {
                loc: Location::new(Span {
                    start: span.start,
                    end: func_id.get_metadata().loc.span.start - 1
                }, filename),
                args: args.into_iter().map(|v| (v.0, types::convert_ast_to_type(v.1, filename, types))).collect(),
                captured: HashMap::with_capacity(0),
                captured_names: Vec::with_capacity(0),
                body: convert_node(*val, filename, funcs, false, seen_funcs, types),
                global,
                checked: false,
                written: false
            };

            let mut _type = Type::Error;
            for a in func.args.iter()
            {
                if let Type::UndeclaredTypeError(_) = a.1
                {
                    _type = a.1.clone();
                    break;
                } else if let Type::DuplicateTypeError(_, _, _) = a.1
                {
                    _type = a.1.clone();
                    break;
                }
            }

            // Return assigning to the function id
            funcs.insert(func_name, func);
            SExpr::Assign(SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                _type,
                arity,
                saved_argc: None,
                tailrec: false
            }, name, Box::new(func_id))
        }

        AST::Lambda(span, args, val) => {
            // Get function id
            let func_name = {
                let seen = seen_funcs.get_mut("").unwrap();
                *seen += 1;
                format!("$${}", seen)
            };

            let arity = args.len();
            let mut func_id = SExpr::Function(SExprMetadata {
                loc: Location::new(span.clone(), filename),
                loc2: Location::empty(),
                _type: Type::Error,
                arity,
                saved_argc: None,
                tailrec: false
            }, func_name.clone());

            // Create the function
            let func = IRFunction {
                loc: Location::new(span, filename),
                args: args.into_iter().map(|v| (v.0, types::convert_ast_to_type(v.1, filename, types))).collect(),
                captured: HashMap::with_capacity(0),
                captured_names: Vec::with_capacity(0),
                body: convert_node(*val, filename, funcs, false, seen_funcs, types),
                global: false,
                checked: false,
                written: false
            };

            let mut _type = Type::Error;
            for a in func.args.iter()
            {
                if let Type::UndeclaredTypeError(_) = a.1
                {
                    _type = a.1.clone();
                    break;
                } else if let Type::DuplicateTypeError(_, _, _) = a.1
                {
                    _type = a.1.clone();
                    break;
                }
            }

            // Return the function id
            funcs.insert(func_name, func);
            func_id.get_mutable_metadata()._type = _type;
            func_id
        }

        AST::Match(span, v, a) =>
            SExpr::Match(SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                _type: Type::Error,
                arity: 0,
                saved_argc: None,
                tailrec: false
            }, Box::new(convert_node(*v, filename, funcs, global, seen_funcs, types)),
                a.into_iter().map(|a| {
                    let span2 = a.0.get_span().clone();
                    let mut v = (types::convert_ast_to_type(a.0, filename, types), convert_node(a.1, filename, funcs, global, seen_funcs, types));
                    v.1.get_mutable_metadata().loc2.span = span2;
                    v
                }).collect()
            ),

        // With expressions
        AST::With(span, a, v) => {
            let v = convert_node(*v, filename, funcs, false, seen_funcs, types);
            SExpr::With(SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                _type: v.get_metadata()._type.clone(),
                arity: 0,
                saved_argc: None,
                tailrec: false
            }, a.into_iter().map(|a| convert_node(a, filename, funcs, false, seen_funcs, types)).collect(), Box::new(v))
        }
    }
}

// extract_types_to_ir(&Vec<AST>, &mut IR) -> ()
// Extracts types and inserts them into the IR's list of types.
fn extract_types_to_ir(asts: &Vec<AST>, ir: &mut IR)
{
    for ast in asts
    {
        if let AST::AssignType(_, v, _) = ast
        {
            ir.types.insert(v.clone(), Type::Unknown);
        }
    }
}

// convert_ast_to_ir(Vec<AST>) -> IR
// Converts a list of asts into ir.
pub fn convert_ast_to_ir(filename: &str, asts: Vec<AST>, ir: &mut IR)
{
    extract_types_to_ir(&asts, ir);
    let mut seen_funcs = HashMap::from_iter(ir.funcs.iter().map(|v| (v.0.clone(), 0usize)));
    seen_funcs.insert(String::with_capacity(0), 0);
    let mut sexprs = vec![];
    let mut module_name = String::with_capacity(0);
    for ast in asts
    {
        if let AST::Header(_, name, exports, imports) = ast
        {
            if let AST::Symbol(_, v) = *name
            {
                module_name = v;
            } else
            {
                unreachable!("header module name is always a symbol");
            }
        } else if let AST::Annotation(_span, a) = ast
        {
            if a == "@debug"
            {
                let last = sexprs.pop().unwrap();
                let called = if let SExpr::Assign(_, v, _) = &last
                {
                    let v = v.clone();
                    sexprs.push(last);
                    SExpr::Application(
                        SExprMetadata::empty(),
                        Box::new(SExpr::Symbol(
                            SExprMetadata::empty(),
                            String::from("debug"))),
                            Box::new(SExpr::Symbol(
                                SExprMetadata::empty(),
                                v
                            )
                        )
                    )
                } else
                {
                    SExpr::Application(
                        SExprMetadata::empty(),
                        Box::new(SExpr::Symbol(
                            SExprMetadata::empty(), 
                            String::from("debug"))
                        ),
                        Box::new(last)
                    )
                };
                sexprs.push(called);
            } else
            {
                panic!("unsupported annotation!");
            }
        } else
        {
            sexprs.push(convert_node(ast, filename, &mut ir.funcs, true, &mut seen_funcs, &mut ir.types));
        }
    }

    if module_name == ""
    {
        module_name = String::from(filename);
    }

    ir.sexprs.insert(module_name, sexprs);
}

