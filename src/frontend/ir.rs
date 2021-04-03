use logos::Span;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

use super::parser::AST;
use super::scopes::Scope;
use super::types;
use super::types::{arc, Type, TypeRc};

// Represents a location
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Location {
    pub span: Span,
    pub filename: String,
}

impl Location {
    // new(Span, &str) -> Location
    // Creates a new Location.
    pub fn new(span: Span, filename: &str) -> Location {
        Location {
            span,
            filename: String::from(filename),
        }
    }

    // empty() -> Location
    // Creates an empty Location.
    pub fn empty() -> Location {
        Location {
            span: Span { start: 0, end: 0 },
            filename: String::with_capacity(0),
        }
    }
}

// Represents an error in IR
pub enum IRError {
    InvalidType(Location),
    DuplicateTypeInUnion(Location, Location, TypeRc),
    DoubleExport(Location, Location, String),
    RedefineImportAlias(Location, Location, String),
    UnsupportedAnnotation(Location, String),
    InvalidFFIType(Location, TypeRc),
    DuplicateModule(String, DuplicateModuleInfo),
}

pub enum DuplicateModuleInfo {
    NoSuperset,
    NewSupersetOld,
    OldSupersetNew,
    BothSuperset,
}

// Represents a prefix operator.
#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum PrefixOp {
    Neg,
    Span,
}

// Represents an infix operator.
#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
pub enum BinOp {
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
    And,
    Or,
    Xor,
    BoolXor,
}

// Represents metadata associated with sexpressions.
#[derive(Debug, Clone)]
pub struct SExprMetadata {
    pub loc: Location,
    pub loc2: Location,
    pub origin: String,
    pub _type: TypeRc,
    pub arity: usize,
    pub saved_argc: Option<usize>,
    pub tailrec: bool,
    pub impure: bool,
}

impl SExprMetadata {
    // empty() -> SExprMetadata
    // Creates empty metadata.
    pub fn empty() -> SExprMetadata {
        SExprMetadata {
            loc: Location::empty(),
            loc2: Location::empty(),
            origin: String::with_capacity(0),
            _type: arc::new(Type::Error),
            arity: 0,
            saved_argc: None,
            tailrec: false,
            impure: false,
        }
    }
}

// Represents an s expression
#[derive(Debug, Clone)]
pub enum SExpr {
    // Empty
    TypeAlias(SExprMetadata, String),

    // Ints
    Int(SExprMetadata, i64),

    // Floats
    Float(SExprMetadata, f64),

    // Words
    Word(SExprMetadata, u64),

    // Chars
    Char(SExprMetadata, u8),

    // Booleans
    True(SExprMetadata),
    False(SExprMetadata),

    // Symbols
    Symbol(SExprMetadata, String),

    // Enums
    Enum(SExprMetadata, String),

    // Strings
    String(SExprMetadata, String),

    // Lists
    List(SExprMetadata, Vec<SExpr>),

    // Functions
    Function(SExprMetadata, String),

    // External function application
    ExternalFunc(SExprMetadata, String, Vec<SExpr>),

    // Prefix expression
    Prefix(SExprMetadata, PrefixOp, Box<SExpr>),

    // Infix expression
    Infix(SExprMetadata, BinOp, Box<SExpr>, Box<SExpr>),

    // Chain operator
    Chain(SExprMetadata, Box<SExpr>, Box<SExpr>),

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
    Walrus(SExprMetadata, String, Box<SExpr>),

    // Match expressions
    Match(SExprMetadata, Box<SExpr>, Vec<(TypeRc, SExpr, Location)>),

    // Member access
    MemberAccess(SExprMetadata, Vec<String>),
}

impl SExpr {
    // get_metadata(&SExpr) -> &SExprMetadata
    // Returns an immutable reference to the metadata.
    pub fn get_metadata(&self) -> &SExprMetadata {
        match self {
            Self::TypeAlias(m, _)
            | Self::Int(m, _)
            | Self::Float(m, _)
            | Self::Word(m, _)
            | Self::Char(m, _)
            | Self::True(m)
            | Self::False(m)
            | Self::Symbol(m, _)
            | Self::Enum(m, _)
            | Self::String(m, _)
            | Self::List(m, _)
            | Self::Function(m, _)
            | Self::ExternalFunc(m, _, _)
            | Self::Prefix(m, _, _)
            | Self::Infix(m, _, _, _)
            | Self::Chain(m, _, _)
            | Self::As(m, _)
            | Self::And(m, _, _)
            | Self::Or(m, _, _)
            | Self::If(m, _, _, _)
            | Self::Application(m, _, _)
            | Self::Assign(m, _, _)
            | Self::With(m, _, _)
            | Self::Walrus(m, _, _)
            | Self::Match(m, _, _)
            | Self::MemberAccess(m, _) => m,
        }
    }

    // get_mutable_metadata(&mut SExpr) -> &mut SExprMetadata
    // Returns a mutable reference to the metadata.
    pub fn get_mutable_metadata(&mut self) -> &mut SExprMetadata {
        match self {
            Self::TypeAlias(m, _)
            | Self::Int(m, _)
            | Self::Float(m, _)
            | Self::Word(m, _)
            | Self::Char(m, _)
            | Self::True(m)
            | Self::False(m)
            | Self::Symbol(m, _)
            | Self::Enum(m, _)
            | Self::String(m, _)
            | Self::List(m, _)
            | Self::Function(m, _)
            | Self::ExternalFunc(m, _, _)
            | Self::Prefix(m, _, _)
            | Self::Infix(m, _, _, _)
            | Self::Chain(m, _, _)
            | Self::As(m, _)
            | Self::And(m, _, _)
            | Self::Or(m, _, _)
            | Self::If(m, _, _, _)
            | Self::Application(m, _, _)
            | Self::Assign(m, _, _)
            | Self::With(m, _, _)
            | Self::Walrus(m, _, _)
            | Self::Match(m, _, _)
            | Self::MemberAccess(m, _) => m,
        }
    }
}

// Represents a function in the IR.
#[derive(Debug)]
pub struct IRFunction {
    pub loc: Location,
    pub name: String,
    pub args: Vec<(String, TypeRc)>,
    pub captured: HashMap<String, TypeRc>,
    pub captured_names: Vec<String>,
    pub body: SExpr,
    pub global: bool,
    pub checked: bool,
    pub written: bool,
    pub impure: bool,
}

#[derive(Debug)]
pub struct IRImport {
    pub name: String,
    pub loc: Location,
    pub qualified: bool,
    pub imports: HashMap<String, (TypeRc, usize, bool)>,
}

#[derive(Debug)]
pub struct IRExtern {
    pub loc: Location,
    pub extern_name: String,
    pub arg_types: Vec<TypeRc>,
    pub ret_type: TypeRc,
    pub impure: bool,
}

// Represents a module of the ir.
#[derive(Debug)]
pub struct IRModule {
    pub name: String,
    pub filename: String,
    pub contents: String,
    pub lib: bool,
    pub imports: HashMap<String, IRImport>,
    pub exports: HashMap<String, (Location, TypeRc)>,
    pub externals: HashMap<String, IRExtern>,
    pub scope: Scope,
    pub funcs: HashMap<String, IRFunction>,
    pub types: HashMap<String, TypeRc>,
    pub sexprs: Vec<SExpr>,
}

#[derive(Debug)]
pub struct IR {
    pub modules: HashMap<String, IRModule>,
}

impl IRModule {
    // new() -> IRModule
    // Creates a new IRModule.
    pub fn new(filename: &str, contents: &str) -> IRModule {
        IRModule {
            name: String::with_capacity(0),
            filename: String::from(filename),
            contents: String::from(contents),
            lib: false,
            imports: HashMap::with_capacity(0),
            exports: HashMap::with_capacity(0),
            externals: HashMap::with_capacity(0),

            // TODO: make init_builtins() be called in only a prelude
            scope: Scope::new().init_builtins(),
            funcs: HashMap::with_capacity(0),
            types: HashMap::with_capacity(0),
            sexprs: Vec::with_capacity(0),
        }
    }
}

// convert_node(AST, &str, bool, &mut HashMap<String, IRFunction>, &mut HashMap<String, HashMap>) -> SExpr
// Converts an ast node into an sexpression.
fn convert_node(
    ast: AST,
    filename: &str,
    funcs: &mut HashMap<String, IRFunction>,
    global: bool,
    seen_funcs: &mut HashMap<String, usize>,
    types: &mut HashMap<String, TypeRc>,
) -> SExpr {
    match ast {
        AST::Empty => unreachable!("never empty"),

        // Int
        AST::Int(span, n) => SExpr::Int(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Int),
                arity: 0,
                saved_argc: None,
                tailrec: false,
                impure: false,
            },
            n,
        ),

        // Float
        AST::Float(span, n) => SExpr::Float(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Float),
                arity: 0,
                saved_argc: None,
                tailrec: false,
                impure: false,
            },
            n,
        ),

        // Word
        AST::Word(span, n) => SExpr::Word(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Word),
                arity: 0,
                saved_argc: None,
                tailrec: false,
                impure: false,
            },
            n,
        ),

        // Char
        AST::Char(span, c) => SExpr::Char(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Char),
                arity: 0,
                saved_argc: None,
                tailrec: false,
                impure: false,
            },
            c,
        ),

        // True
        AST::True(span) => SExpr::True(SExprMetadata {
            loc: Location::new(span, filename),
            loc2: Location::empty(),
            origin: String::with_capacity(0),
            _type: arc::new(Type::Bool),
            arity: 0,
            saved_argc: None,
            tailrec: false,
            impure: false,
        }),

        // False
        AST::False(span) => SExpr::False(SExprMetadata {
            loc: Location::new(span, filename),
            loc2: Location::empty(),
            origin: String::with_capacity(0),
            _type: arc::new(Type::Bool),
            arity: 0,
            saved_argc: None,
            tailrec: false,
            impure: false,
        }),

        AST::List(span, list) => SExpr::List(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Error),
                arity: 0,
                saved_argc: None,
                tailrec: false,
                impure: false,
            },
            list.into_iter()
                .map(|v| convert_node(v, filename, funcs, global, seen_funcs, types))
                .collect(),
        ),

        // Symbol
        AST::Symbol(span, s) => SExpr::Symbol(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Error),
                arity: 0,
                saved_argc: None,
                tailrec: false,
                impure: false,
            },
            s,
        ),

        // Enum
        AST::Enum(span, e) => SExpr::Enum(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Enum(e.clone())),
                arity: 0,
                saved_argc: None,
                tailrec: false,
                impure: false,
            },
            e,
        ),

        AST::Annotation(_, _)
        | AST::Import(_, _, _)
        | AST::QualifiedImport(_, _, _)
        | AST::Header(_, _, _, _)
        | AST::LibHeader(_, _, _)
        | AST::Extern(_, _, _, _) => {
            unreachable!(
                "annotations, imports, headers, and external declarations are already handled!"
            );
        }

        // String
        AST::String(span, s) => {
            let loc = Location::new(span, filename);
            let mut cons = SExpr::Application(
                SExprMetadata {
                    loc: loc.clone(),
                    loc2: Location::empty(),
                    _type: arc::new(Type::Error),
                    origin: String::with_capacity(0),
                    arity: 0,
                    saved_argc: None,
                    tailrec: false,
                    impure: false,
                },
                Box::new(SExpr::Application(
                    SExprMetadata {
                        loc: loc.clone(),
                        loc2: Location::empty(),
                        _type: arc::new(Type::Error),
                        origin: String::with_capacity(0),
                        arity: 0,
                        saved_argc: None,
                        tailrec: false,
                        impure: false,
                    },
                    Box::new(SExpr::Symbol(
                        SExprMetadata {
                            loc: loc.clone(),
                            loc2: Location::empty(),
                            _type: arc::new(Type::Error),
                            origin: String::with_capacity(0),
                            arity: 0,
                            saved_argc: None,
                            tailrec: false,
                            impure: false,
                        },
                        String::from("cons_S"),
                    )),
                    Box::new(SExpr::Char(
                        SExprMetadata {
                            loc: loc.clone(),
                            loc2: Location::empty(),
                            _type: arc::new(Type::Char),
                            origin: String::with_capacity(0),
                            arity: 0,
                            saved_argc: None,
                            tailrec: false,
                            impure: false,
                        },
                        if s.is_empty() {
                            0
                        } else {
                            s.bytes().last().unwrap()
                        },
                    )),
                )),
                Box::new(SExpr::Char(
                    SExprMetadata {
                        loc: loc.clone(),
                        loc2: Location::empty(),
                        _type: arc::new(Type::Char),
                        origin: String::with_capacity(0),
                        arity: 0,
                        saved_argc: None,
                        tailrec: false,
                        impure: false,
                    },
                    0,
                )),
            );
            if s.is_empty() {
                cons
            } else {
                for c in s.bytes().rev().skip(1) {
                    cons = SExpr::Application(
                        SExprMetadata {
                            loc: loc.clone(),
                            loc2: Location::empty(),
                            _type: arc::new(Type::Error),
                            origin: String::with_capacity(0),
                            arity: 0,
                            saved_argc: None,
                            tailrec: false,
                            impure: false,
                        },
                        Box::new(SExpr::Application(
                            SExprMetadata {
                                loc: loc.clone(),
                                loc2: Location::empty(),
                                _type: arc::new(Type::Error),
                                origin: String::with_capacity(0),
                                arity: 0,
                                saved_argc: None,
                                tailrec: false,
                                impure: false,
                            },
                            Box::new(SExpr::Symbol(
                                SExprMetadata {
                                    loc: loc.clone(),
                                    loc2: Location::empty(),
                                    _type: arc::new(Type::Error),
                                    origin: String::with_capacity(0),
                                    arity: 0,
                                    saved_argc: None,
                                    tailrec: false,
                                    impure: false,
                                },
                                String::from("cons_S"),
                            )),
                            Box::new(SExpr::Char(
                                SExprMetadata {
                                    loc: loc.clone(),
                                    loc2: Location::empty(),
                                    _type: arc::new(Type::Char),
                                    origin: String::with_capacity(0),
                                    arity: 0,
                                    saved_argc: None,
                                    tailrec: false,
                                    impure: false,
                                },
                                c,
                            )),
                        )),
                        Box::new(cons),
                    )
                }
                cons
            }
        }

        // Prefix
        AST::Prefix(span, op, v) => {
            let op = match op.as_str() {
                "-" => PrefixOp::Neg,
                "*" => PrefixOp::Span,
                _ => panic!("Invalid operator"),
            };

            SExpr::Prefix(
                SExprMetadata {
                    loc: Location::new(span, filename),
                    loc2: Location::empty(),
                    origin: String::with_capacity(0),
                    _type: arc::new(Type::Error),
                    arity: 0,
                    saved_argc: None,
                    tailrec: false,
                    impure: false,
                },
                op,
                Box::new(convert_node(*v, filename, funcs, global, seen_funcs, types)),
            )
        }

        // Infix
        AST::Infix(span, op, l, r) => {
            // Deal with boolean operators
            if op == "and" {
                SExpr::And(
                    SExprMetadata {
                        loc: Location::new(span, filename),
                        loc2: Location::empty(),
                        origin: String::with_capacity(0),
                        _type: arc::new(Type::Error),
                        arity: 0,
                        saved_argc: None,
                        tailrec: false,
                        impure: false,
                    },
                    Box::new(convert_node(*l, filename, funcs, global, seen_funcs, types)),
                    Box::new(convert_node(*r, filename, funcs, global, seen_funcs, types)),
                )
            } else if op == "or" {
                SExpr::Or(
                    SExprMetadata {
                        loc: Location::new(span, filename),
                        loc2: Location::empty(),
                        origin: String::with_capacity(0),
                        _type: arc::new(Type::Error),
                        arity: 0,
                        saved_argc: None,
                        tailrec: false,
                        impure: false,
                    },
                    Box::new(convert_node(*l, filename, funcs, global, seen_funcs, types)),
                    Box::new(convert_node(*r, filename, funcs, global, seen_funcs, types)),
                )

            // Deal with accessing members
            } else if op == "::" {
                let mut accesses = vec![];
                if let AST::Symbol(_, s) = *r {
                    accesses.push(s);
                }

                let mut l = *l;
                while let AST::Infix(_, _, l_, r) = l {
                    if let AST::Symbol(_, s) = *r {
                        accesses.insert(0, s);
                    }

                    l = *l_;
                }

                if let AST::Symbol(_, s) = l {
                    accesses.insert(0, s);
                }

                SExpr::MemberAccess(
                    SExprMetadata {
                        loc: Location::new(span, filename),
                        loc2: Location::empty(),
                        origin: String::with_capacity(0),
                        _type: arc::new(Type::Error),
                        arity: 0,
                        saved_argc: None,
                        tailrec: false,
                        impure: false,
                    },
                    accesses,
                )
            } else if op == ";" {
                SExpr::Chain(
                    SExprMetadata {
                        loc: Location::new(span, filename),
                        loc2: Location::empty(),
                        origin: String::with_capacity(0),
                        _type: arc::new(Type::Error),
                        arity: 0,
                        saved_argc: None,
                        tailrec: false,
                        impure: false,
                    },
                    Box::new(convert_node(*l, filename, funcs, global, seen_funcs, types)),
                    Box::new(convert_node(*r, filename, funcs, global, seen_funcs, types)),
                )
            } else if op == "$" {
                SExpr::Application(
                    SExprMetadata {
                        loc: Location::new(span, filename),
                        loc2: Location::empty(),
                        origin: String::with_capacity(0),
                        _type: arc::new(Type::Error),
                        arity: 0,
                        saved_argc: None,
                        tailrec: false,
                        impure: false,
                    },
                    Box::new(convert_node(*l, filename, funcs, global, seen_funcs, types)),
                    Box::new(convert_node(*r, filename, funcs, global, seen_funcs, types)),
                )
            } else {
                // Get operator
                let op = match op.as_str() {
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
                    "&" => BinOp::And,
                    "|" => BinOp::Or,
                    "^" => BinOp::Xor,
                    "xor" => BinOp::BoolXor,
                    _ => panic!("Invalid operator"),
                };

                // Return
                SExpr::Infix(
                    SExprMetadata {
                        loc: Location::new(span, filename),
                        loc2: Location::empty(),
                        origin: String::with_capacity(0),
                        _type: arc::new(Type::Error),
                        arity: 0,
                        saved_argc: None,
                        tailrec: false,
                        impure: false,
                    },
                    op,
                    Box::new(convert_node(*l, filename, funcs, global, seen_funcs, types)),
                    Box::new(convert_node(*r, filename, funcs, global, seen_funcs, types)),
                )
            }
        }

        AST::As(span, value, _type) => SExpr::As(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::new(_type.get_span(), filename),
                origin: String::with_capacity(0),
                _type: arc::new(types::convert_ast_to_type(*_type, filename)),
                arity: 0,
                saved_argc: None,
                tailrec: false,
                impure: false,
            },
            Box::new(convert_node(
                *value, filename, funcs, global, seen_funcs, types,
            )),
        ),

        // If expression
        AST::If(span, cond, then, elsy) => SExpr::If(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Error),
                arity: 0,
                saved_argc: None,
                tailrec: false,
                impure: false,
            },
            Box::new(convert_node(
                *cond, filename, funcs, global, seen_funcs, types,
            )),
            Box::new(convert_node(
                *then, filename, funcs, global, seen_funcs, types,
            )),
            Box::new(convert_node(
                *elsy, filename, funcs, global, seen_funcs, types,
            )),
        ),

        // Application
        AST::Application(span, l, r) => SExpr::Application(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Error),
                arity: 0,
                saved_argc: None,
                tailrec: false,
                impure: false,
            },
            Box::new(convert_node(*l, filename, funcs, global, seen_funcs, types)),
            Box::new(convert_node(*r, filename, funcs, global, seen_funcs, types)),
        ),

        // Assignment
        AST::Assign(span, name, val) => {
            let sexpr = convert_node(*val, filename, funcs, false, seen_funcs, types);
            if global && name != "_" {
                let func_name = if seen_funcs.contains_key(&name) {
                    let seen = seen_funcs.get_mut(&name).unwrap();
                    *seen += 1;
                    format!("{}${}", name, seen)
                } else {
                    seen_funcs.insert(name.clone(), 0);
                    name.clone()
                };
                funcs.insert(
                    func_name.clone(),
                    IRFunction {
                        loc: Location::new(span.clone(), filename),
                        name: name.clone(),
                        args: Vec::with_capacity(0),
                        captured: HashMap::with_capacity(0),
                        captured_names: Vec::with_capacity(0),
                        body: sexpr,
                        global: true,
                        checked: false,
                        written: false,
                        impure: false,
                    },
                );

                SExpr::Assign(
                    SExprMetadata {
                        loc: Location::new(span.clone(), filename),
                        loc2: Location::empty(),
                        origin: String::with_capacity(0),
                        _type: arc::new(Type::Error),
                        arity: 0,
                        saved_argc: None,
                        tailrec: false,
                        impure: false,
                    },
                    name,
                    Box::new(SExpr::Function(
                        SExprMetadata {
                            loc: Location::new(span, filename),
                            loc2: Location::empty(),
                            origin: String::with_capacity(0),
                            _type: arc::new(Type::Error),
                            arity: 0,
                            saved_argc: None,
                            tailrec: false,
                            impure: false,
                        },
                        func_name,
                    )),
                )
            } else {
                SExpr::Assign(
                    SExprMetadata {
                        loc: Location::new(span, filename),
                        loc2: Location::empty(),
                        origin: String::with_capacity(0),
                        _type: arc::new(Type::Error),
                        arity: 0,
                        saved_argc: None,
                        tailrec: false,
                        impure: false,
                    },
                    name,
                    Box::new(sexpr),
                )
            }
        }

        // Assignment with types
        AST::AssignTyped(span, name, _type, val) => {
            let sexpr = convert_node(*val, filename, funcs, false, seen_funcs, types);
            if global && name != "_" {
                let func_name = if seen_funcs.contains_key(&name) {
                    let seen = seen_funcs.get_mut(&name).unwrap();
                    *seen += 1;
                    format!("{}${}", name, seen)
                } else {
                    seen_funcs.insert(name.clone(), 0);
                    name.clone()
                };
                funcs.insert(
                    func_name.clone(),
                    IRFunction {
                        loc: Location::new(span.clone(), filename),
                        name: name.clone(),
                        args: Vec::with_capacity(0),
                        captured: HashMap::with_capacity(0),
                        captured_names: Vec::with_capacity(0),
                        body: sexpr,
                        global: true,
                        checked: false,
                        written: false,
                        impure: false,
                    },
                );

                let ts = _type.get_span();
                let _type = arc::new(types::convert_ast_to_type(*_type, filename));
                SExpr::Assign(
                    SExprMetadata {
                        loc: Location::new(span.clone(), filename),
                        loc2: Location::new(ts, filename),
                        origin: String::with_capacity(0),
                        _type: _type.clone(),
                        arity: 0,
                        saved_argc: None,
                        tailrec: false,
                        impure: false,
                    },
                    name,
                    Box::new(SExpr::Function(
                        SExprMetadata {
                            loc: Location::new(span, filename),
                            loc2: Location::empty(),
                            origin: String::with_capacity(0),
                            _type,
                            arity: 0,
                            saved_argc: None,
                            tailrec: false,
                            impure: false,
                        },
                        func_name,
                    )),
                )
            } else {
                SExpr::Assign(
                    SExprMetadata {
                        loc: Location::new(span, filename),
                        loc2: Location::new(_type.get_span(), filename),
                        origin: String::with_capacity(0),
                        _type: arc::new(types::convert_ast_to_type(*_type, filename)),
                        arity: 0,
                        saved_argc: None,
                        tailrec: false,
                        impure: false,
                    },
                    name,
                    Box::new(sexpr),
                )
            }
        }

        AST::AssignType(span, name, _type) => {
            let span2 = _type.get_span();
            let _type = arc::new(types::convert_ast_to_type(*_type, filename));
            types.insert(name.clone(), _type.clone());
            SExpr::TypeAlias(
                SExprMetadata {
                    loc: Location::new(span, filename),
                    loc2: Location::new(span2, filename),
                    origin: String::with_capacity(0),
                    _type,
                    arity: 0,
                    saved_argc: None,
                    tailrec: false,
                    impure: false,
                },
                name,
            )
        }

        // Assigning functions
        AST::AssignFunction(span, name, args, val) => {
            // Get function id
            let func_name = if seen_funcs.contains_key(&name) {
                let seen = seen_funcs.get_mut(&name).unwrap();
                *seen += 1;
                format!("{}${}", name, seen)
            } else {
                seen_funcs.insert(name.clone(), 0);
                name.clone()
            };

            let arity = args.len();
            let func_id = SExpr::Function(
                SExprMetadata {
                    loc: Location::new(span.clone(), filename),
                    loc2: Location::empty(),
                    origin: String::with_capacity(0),
                    _type: arc::new(Type::Error),
                    arity,
                    saved_argc: None,
                    tailrec: false,
                    impure: false,
                },
                func_name.clone(),
            );

            // Create the function
            let func = IRFunction {
                loc: Location::new(
                    Span {
                        start: span.start,
                        end: func_id.get_metadata().loc.span.start,
                    },
                    filename,
                ),
                name: name.clone(),
                args: args
                    .into_iter()
                    .map(|v| (v.0, arc::new(types::convert_ast_to_type(v.1, filename))))
                    .collect(),
                captured: HashMap::with_capacity(0),
                captured_names: Vec::with_capacity(0),
                body: convert_node(*val, filename, funcs, false, seen_funcs, types),
                global,
                checked: false,
                written: false,
                impure: false,
            };

            let mut _type = arc::new(Type::Error);
            for a in func.args.iter() {
                if let Type::UndeclaredTypeError(_) = *a.1 {
                    _type = a.1.clone();
                    break;
                } else if let Type::DuplicateTypeError(_, _, _) = *a.1 {
                    _type = a.1.clone();
                    break;
                }
            }

            // Return assigning to the function id
            funcs.insert(func_name, func);
            SExpr::Assign(
                SExprMetadata {
                    loc: Location::new(span, filename),
                    loc2: Location::empty(),
                    origin: String::with_capacity(0),
                    _type,
                    arity,
                    saved_argc: None,
                    tailrec: false,
                    impure: false,
                },
                name,
                Box::new(func_id),
            )
        }

        AST::Lambda(span, args, val) => {
            // Get function id
            let func_name = {
                let seen = seen_funcs.get_mut("").unwrap();
                *seen += 1;
                format!("$${}", seen)
            };

            let arity = args.len();
            let mut func_id = SExpr::Function(
                SExprMetadata {
                    loc: Location::new(span.clone(), filename),
                    loc2: Location::empty(),
                    origin: String::with_capacity(0),
                    _type: arc::new(Type::Error),
                    arity,
                    saved_argc: None,
                    tailrec: false,
                    impure: false,
                },
                func_name.clone(),
            );

            // Create the function
            let func = IRFunction {
                loc: Location::new(span, filename),
                name: String::with_capacity(0),
                args: args
                    .into_iter()
                    .map(|v| (v.0, arc::new(types::convert_ast_to_type(v.1, filename))))
                    .collect(),
                captured: HashMap::with_capacity(0),
                captured_names: Vec::with_capacity(0),
                body: convert_node(*val, filename, funcs, false, seen_funcs, types),
                global: false,
                checked: false,
                written: false,
                impure: false,
            };

            let mut _type = arc::new(Type::Error);
            for a in func.args.iter() {
                if let Type::UndeclaredTypeError(_) = *a.1 {
                    _type = a.1.clone();
                    break;
                } else if let Type::DuplicateTypeError(_, _, _) = *a.1 {
                    _type = a.1.clone();
                    break;
                }
            }

            // Return the function id
            funcs.insert(func_name, func);
            func_id.get_mutable_metadata()._type = _type;
            func_id
        }

        // With expressions
        AST::With(span, a, v) => SExpr::With(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Error),
                arity: 0,
                saved_argc: None,
                tailrec: false,
                impure: false,
            },
            a.into_iter()
                .map(|a| convert_node(a, filename, funcs, false, seen_funcs, types))
                .collect(),
            Box::new(convert_node(*v, filename, funcs, false, seen_funcs, types)),
        ),

        AST::Walrus(span, a, v) => SExpr::Walrus(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Error),
                arity: 0,
                saved_argc: None,
                tailrec: false,
                impure: false,
            },
            a,
            Box::new(convert_node(*v, filename, funcs, false, seen_funcs, types)),
        ),

        AST::Match(span, v, a) => SExpr::Match(
            SExprMetadata {
                loc: Location::new(span, filename),
                loc2: Location::empty(),
                origin: String::with_capacity(0),
                _type: arc::new(Type::Error),
                arity: 0,
                saved_argc: None,
                tailrec: false,
                impure: false,
            },
            Box::new(convert_node(*v, filename, funcs, global, seen_funcs, types)),
            a.into_iter()
                .map(|a| {
                    let span2 = a.0.get_span();
                    (
                        arc::new(types::convert_ast_to_type(a.0, filename)),
                        convert_node(a.1, filename, funcs, global, seen_funcs, types),
                        Location::new(span2, filename),
                    )
                })
                .collect(),
        ),
    }
}

// extract_types_to_ir(&Vec<AST>, &mut IRModule) -> ()
// Extracts types and inserts them into the IR's list of types.
fn extract_types_to_ir(asts: &[AST], module: &mut IRModule) {
    for ast in asts {
        if let AST::AssignType(_, v, _) = ast {
            module.types.insert(v.clone(), arc::new(Type::Unknown));
        }
    }
}

// Represents the purity of the next function.
enum Purity {
    Pure,
    Impure,
    Default,
}

// convert_ast_to_ir(Vec<AST>) -> IR
// Converts a list of asts into ir.
pub fn convert_ast_to_ir(
    filename: &str,
    contents: &str,
    asts: Vec<AST>,
    ir: &mut IR,
) -> Result<(), Vec<IRError>> {
    // Set up
    let mut module = IRModule::new(filename, contents);
    extract_types_to_ir(&asts, &mut module);
    let mut seen_funcs = HashMap::new();
    seen_funcs.insert(String::with_capacity(0), 0);
    let mut sexprs = vec![];
    let mut module_name = String::with_capacity(0);
    let mut errors = vec![];
    let mut purity = Purity::Default;

    // Iterate over every ast node
    for ast in asts {
        // Deal with the header
        if let AST::Header(_, name, exports, imports) = ast {
            // Get module name
            let mut full_name = vec![];
            let mut top = *name;
            while let AST::Infix(_, _, l, r) = top {
                if let AST::Symbol(_, v) = *r {
                    full_name.push(v);
                }

                top = *l;
            }
            if let AST::Symbol(_, v) = top {
                full_name.push(v);
            }
            full_name.reverse();
            module_name = full_name.join("::");

            // Deal with exports
            for export in exports {
                // Check exported variable type
                if let AST::Empty = export.2 {
                    unimplemented!("nya :(");
                } else {
                    let _type = types::convert_ast_to_type(export.2, filename);
                    if let Type::UndeclaredTypeError(s) = _type {
                        errors.push(IRError::InvalidType(s));
                    } else if let Type::DuplicateTypeError(s1, s2, t) = _type {
                        errors.push(IRError::DuplicateTypeInUnion(s1, s2, t));

                    // Check export is unique
                    } else if module.exports.contains_key(&export.1) {
                        errors.push(IRError::DoubleExport(
                            module.exports.get(&export.1).unwrap().0.clone(),
                            Location::new(export.0, filename),
                            export.1,
                        ));
                    } else {
                        // Add export to list of exports
                        module
                            .exports
                            .insert(export.1, (Location::new(export.0, filename), arc::new(_type)));
                    }
                }
            }

            // Deal with imports
            for import in imports {
                let imp_mod;
                let alias;
                if let AST::QualifiedImport(s, m, a) = import {
                    let mut name = vec![];
                    let mut m = m;
                    while let AST::Infix(_, _, l, r) = *m {
                        if let AST::Symbol(_, v) = *r {
                            name.push(v);
                        } else {
                            unreachable!("always a symbol");
                        }
                        m = l;
                    }
                    if let AST::Symbol(_, v) = *m {
                        name.push(v);
                    }
                    name.reverse();

                    if !a.is_empty() {
                        alias = a;
                    } else {
                        alias = name.join("::");
                    }

                    imp_mod = IRImport {
                        name: name.join("::"),
                        loc: Location::new(s, filename),
                        qualified: true,
                        imports: HashMap::with_capacity(0),
                    };
                } else if let AST::Import(s, m, imports) = import {
                    let mut name = vec![];
                    let mut m = m;
                    while let AST::Infix(_, _, l, r) = *m {
                        if let AST::Symbol(_, v) = *r {
                            name.push(v);
                        } else {
                            unreachable!("always a symbol");
                        }
                        m = l;
                    }
                    if let AST::Symbol(_, v) = *m {
                        name.push(v);
                    }
                    name.reverse();

                    alias = name.join("::");
                    imp_mod = IRImport {
                        name: name.join("::"),
                        loc: Location::new(s, filename),
                        qualified: false,
                        imports: imports
                            .into_iter()
                            .map(|v| (v, (arc::new(Type::Unknown), 0, false)))
                            .collect(),
                    };
                } else {
                    unreachable!("always either a QualifiedImport or an Import");
                }

                match module.imports.entry(alias) {
                    Entry::Occupied(e) => {
                        errors.push(IRError::RedefineImportAlias(
                            e.get().loc.clone(),
                            imp_mod.loc,
                            e.key().clone(),
                        ));
                    }

                    Entry::Vacant(e) => {
                        e.insert(imp_mod);
                    }
                }
            }
        } else if let AST::Annotation(span, a) = ast {
            // Purity tags
            if a == "@pure" {
                purity = Purity::Pure;
            } else if a == "@impure" {
                purity = Purity::Impure;
            } else {
                errors.push(IRError::UnsupportedAnnotation(
                    Location::new(span, filename),
                    a,
                ));
            }
        } else if let AST::Extern(span, c, n, t) = ast {
            let ts = t.get_span().clone();
            let t = types::convert_ast_to_type(*t, &module.filename);

            // Check type
            if let Type::UndeclaredTypeError(s) = t {
                errors.push(IRError::InvalidType(s));
            } else if let Type::DuplicateTypeError(s1, s2, t2) = t {
                errors.push(IRError::DuplicateTypeInUnion(s1, s2, t2));
            } else if !t.is_ffi_compatible(&module.types) {
                errors.push(IRError::InvalidFFIType(
                    Location::new(ts, &module.filename),
                    arc::new(t),
                ));
            } else {
                // Get arg types and return function
                let mut arg_types = vec![];
                let mut ret_type = arc::new(t);

                while let Type::Func(f, a) = &*ret_type {
                    arg_types.push(f.clone());
                    ret_type = a.clone();
                }

                // Add external function
                module.externals.insert(
                    n,
                    IRExtern {
                        loc: Location::new(span, &module.filename),
                        extern_name: c,
                        arg_types,
                        ret_type,
                        impure: matches!(purity, Purity::Default | Purity::Impure)
                    },
                );
            }

            purity = Purity::Default;
        } else {
            let v = convert_node(
                ast,
                filename,
                &mut module.funcs,
                true,
                &mut seen_funcs,
                &mut module.types,
            );

            if let SExpr::Assign(_, _, v) = &v {
                if let SExpr::Function(_, f) = &**v {
                    module.funcs.get_mut(f).unwrap().impure = matches!(purity, Purity::Impure);
                }
            }
            sexprs.push(v);
            purity = Purity::Default;
        }
    }

    // Check module name
    if module_name.is_empty() {
        module_name = filename
            .split('/')
            .last()
            .unwrap()
            .split('.')
            .next()
            .unwrap()
            .to_string();
    }
    module.name = module_name.clone();
    module.sexprs = sexprs;

    // Add module to ir root and error if already exists
    match ir.modules.entry(module_name) {
        Entry::Occupied(e) => {
            if format!("{:?}", module) == format!("{:?}", e.get()) {
            } else {
                errors.push(IRError::DuplicateModule(
                    e.key().clone(),
                    DuplicateModuleInfo::NoSuperset,
                ));
            }
        }

        Entry::Vacant(e) => {
            e.insert(module);
        }
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

pub fn convert_library_header(
    filename: &str,
    asts: Vec<AST>,
    ir: &mut IR,
) -> Result<(), Vec<IRError>> {
    let mut errors = vec![];

    for ast in asts {
        errors.extend(convert_module(filename, ast, ir));
    }

    if errors.is_empty() {
        Ok(())
    } else {
        Err(errors)
    }
}

pub fn convert_module(filename: &str, ast: AST, ir: &mut IR) -> Vec<IRError> {
    let mut errors = vec![];

    if let AST::LibHeader(_, name, exports) = ast {
        // Get module name
        let mut full_name = vec![];
        let mut top = *name;
        while let AST::Infix(_, _, l, r) = top {
            if let AST::Symbol(_, v) = *r {
                full_name.push(v);
            }

            top = *l;
        }
        if let AST::Symbol(_, v) = top {
            full_name.push(v);
        }
        full_name.reverse();
        let module_name = full_name.join("::");
        let mut module = IRModule::new(filename, "");
        module.name = module_name.clone();
        module.lib = true;

        // Deal with exports
        for export in exports {
            // Check exported variable type
            if let AST::Empty = export.4 {
                unimplemented!("nya :(");
            } else {
                let _type = arc::new(types::convert_ast_to_type(export.4, filename));
                if let Type::DuplicateTypeError(s1, s2, t) = &*_type {
                    errors.push(IRError::DuplicateTypeInUnion(s1.clone(), s2.clone(), t.clone()));

                // Check export is unique
                } else {
                    let (span, export, arity, impure, _) = export;
                    match module.exports.entry(export) {
                        Entry::Occupied(e) => {
                            errors.push(IRError::DoubleExport(
                                e.get().0.clone(),
                                Location::new(span, filename),
                                e.key().clone(),
                            ));
                        }

                        Entry::Vacant(e) => {
                            // Add export to list of exports
                            let loc = Location::new(span, filename);
                            module.scope.put_var(
                                e.key(),
                                &_type,
                                arity,
                                Some(0),
                                &loc,
                                true,
                                &module_name,
                            );
                            let mut args = vec![];
                            let mut ret_type = _type.clone();
                            for i in 0..arity {
                                if let Type::Func(l, r) = &*ret_type {
                                    args.push((format!("${}", i), l.clone()));
                                    ret_type = r.clone();
                                }
                            }

                            module.funcs.insert(
                                e.key().clone(),
                                IRFunction {
                                    loc: Location::empty(),
                                    name: e.key().clone(),
                                    args,
                                    captured: HashMap::with_capacity(0),
                                    captured_names: Vec::with_capacity(0),
                                    body: SExpr::True(SExprMetadata {
                                        loc: Location::empty(),
                                        loc2: Location::empty(),
                                        origin: String::with_capacity(0),
                                        _type: ret_type.clone(),
                                        arity: 0,
                                        saved_argc: None,
                                        tailrec: false,
                                        impure,
                                    }),
                                    global: true,
                                    checked: true,
                                    written: true,
                                    impure,
                                },
                            );
                            module.sexprs.push(SExpr::Assign(
                                SExprMetadata::empty(),
                                e.key().clone(),
                                Box::new(SExpr::Function(SExprMetadata::empty(), e.key().clone())),
                            ));
                            e.insert((loc, _type));
                        }
                    }
                }
            }
        }

        // Add module to ir root and error if already exists
        match ir.modules.entry(module_name) {
            Entry::Occupied(mut e) => {
                let old_mod = e.get();
                let new_mod = &module;

                let mut new_superset_of_old = true;
                let mut old_superset_of_new = true;

                for i in old_mod.exports.keys() {
                    let old_mod_export = old_mod.exports.get(i).unwrap();

                    let new_mod_export = match new_mod.exports.get(i) {
                        Some(v) => v,
                        _ => {
                            new_superset_of_old = false;
                            break;
                        }
                    };

                    if old_mod_export.1 != new_mod_export.1 {
                        new_superset_of_old = false;
                        break;
                    }
                }

                for i in new_mod.exports.keys() {
                    let new_mod_export = new_mod.exports.get(i).unwrap();

                    let old_mod_export = match old_mod.exports.get(i) {
                        Some(v) => v,
                        _ => {
                            old_superset_of_new = false;
                            break;
                        }
                    };

                    if new_mod_export.1 != old_mod_export.1 {
                        old_superset_of_new = false;
                        break;
                    }
                }

                if new_superset_of_old && old_superset_of_new {
                    errors.push(IRError::DuplicateModule(
                        e.key().clone(),
                        DuplicateModuleInfo::BothSuperset,
                    ));
                } else if new_superset_of_old ^ old_superset_of_new {
                    //TODO: Duplicate module superset warning
                    if new_superset_of_old {
                        e.insert(module);
                        errors.push(IRError::DuplicateModule(
                            e.key().clone(),
                            DuplicateModuleInfo::NewSupersetOld,
                        ));
                    } else {
                        errors.push(IRError::DuplicateModule(
                            e.key().clone(),
                            DuplicateModuleInfo::OldSupersetNew,
                        ));
                    }
                } else {
                    errors.push(IRError::DuplicateModule(
                        e.key().clone(),
                        DuplicateModuleInfo::NoSuperset,
                    ));
                }
            }

            Entry::Vacant(e) => {
                e.insert(module);
            }
        }
    }

    errors
}
