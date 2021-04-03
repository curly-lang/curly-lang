use logos::Span;
use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Error, Formatter};
use std::hash::{Hash, Hasher};
use std::sync::Arc;

use super::ir::Location;
use super::parser::AST;

#[derive(Clone, Debug)]
pub struct HashSetWrapper<T>(pub HashSet<T>);

impl<T: Hash + Eq> PartialEq for HashSetWrapper<T> {
    fn eq(&self, other: &HashSetWrapper<T>) -> bool {
        self.0 == other.0
    }
}

impl<T: Hash + Eq> Eq for HashSetWrapper<T> {}

impl<T: Hash + Eq> Hash for HashSetWrapper<T> {
    fn hash<H: Hasher>(&self, h: &mut H) {
        let mut hash: u64 = 0;
        for v in self.0.iter() {
            let mut h_ = DefaultHasher::new();
            v.hash(&mut h_);
            hash ^= h_.finish();
        }
        hash.hash(h);
    }
}

pub type TypeRc = Arc<Type>;

// Represents a type.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Error,
    UndeclaredTypeError(Location),
    DuplicateTypeError(Location, Location, TypeRc),
    Unknown,
    Int,
    Float,
    Bool,
    Word,
    Char,
    Symbol(String),
    Func(TypeRc, TypeRc),
    Union(HashSetWrapper<TypeRc>),
    // Sum(HashMapWrapper<String, TypeRc>),
    Enum(String),
    Pointer(String),
    Tag(String, TypeRc),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            // Errors
            Type::Error => {
                write!(f, "TypeError")?;
            }
            Type::UndeclaredTypeError(_) => {
                write!(f, "UndeclaredTypeError")?;
            }
            Type::DuplicateTypeError(_, _, _) => {
                write!(f, "DuplicateTypeError")?;
            }
            Type::Unknown => {
                write!(f, "{{ unknown }}")?;
            }

            // Primitives
            Type::Int => {
                write!(f, "Int")?;
            }
            Type::Float => {
                write!(f, "Float")?;
            }
            Type::Bool => {
                write!(f, "Bool")?;
            }
            Type::Word => {
                write!(f, "Word")?;
            }
            Type::Char => {
                write!(f, "Char")?;
            }
            Type::Symbol(s) => {
                write!(f, "{}", s)?;
            }
            Type::Enum(e) => {
                write!(f, "enum {}", e)?;
            }
            Type::Pointer(p) => {
                write!(f, "ptr {}", p)?;
            }

            // Function types
            Type::Func(func, a) => {
                if let Type::Func(_, _) = **func {
                    write!(f, "({})", **func)?;
                } else {
                    write!(f, "{}", **func)?;
                }
                write!(f, " -> {}", a)?;
            }

            // Union types
            Type::Union(fields) => {
                let mut bar = false;
                for field in fields.0.iter() {
                    if bar {
                        write!(f, " | ")?;
                    } else {
                        bar = true;
                    }

                    if let Type::Func(_, _) = **field {
                        write!(f, "({})", field)?;
                    } else {
                        write!(f, "{}", field)?;
                    }
                }
            }

            // Tagged types
            Type::Tag(tag, field) => {
                write!(f, "{}: {}", tag, field)?;
            }
        }
        Ok(())
    }
}

impl Type {
    // sum_hash(&self) -> u64
    // Returns the hash value used by codegenned sum/union types.
    pub fn sum_hash(&self) -> u64 {
        let mut hash = DefaultHasher::new();
        self.hash(&mut hash);
        hash.finish()
    }

    // is_ffi_compatible(&self, &HashMap<String, Type>) -> bool
    // Returns true if function is ffi compatible.
    pub fn is_ffi_compatible(&self, types: &HashMap<String, TypeRc>) -> bool {
        match self {
            Type::Int => true,
            Type::Float => true,
            Type::Bool => true,
            Type::Word => true,
            Type::Char => true,
            Type::Symbol(s) => types.get(s).unwrap().is_ffi_compatible(types),
            Type::Func(f, a) => f.is_ffi_compatible(types) && a.is_ffi_compatible(types),
            Type::Enum(_) => true,
            Type::Pointer(_) => true,
            _ => false,
        }
    }

    // is_subtype(&self, &Type, &HashMap<String, Type>) -> bool
    // Returns true if self is a valid subtype in respect to the passed in type.
    pub fn is_subtype(&self, supertype: &Type, types: &HashMap<String, TypeRc>) -> bool {
        let _type = self;

        if _type == supertype {
            return true;
        }

        match supertype {
            // Primitives
            Type::Int => *_type == Type::Int,
            Type::Float => *_type == Type::Float,
            Type::Bool => *_type == Type::Bool,
            Type::Word => *_type == Type::Word,
            Type::Char => *_type == Type::Char,

            // Functions
            Type::Func(sf, sa) => {
                if let Type::Func(f, a) = _type {
                    f == sf && a == sa
                } else {
                    false
                }
            }

            // Union types
            Type::Union(fields) => {
                // Union types mean the subtype has fields over a subset of fields of the supertype
                if let Type::Union(sub) = _type {
                    for s in sub.0.iter() {
                        let mut is_subtype = false;
                        for f in fields.0.iter() {
                            if s.is_subtype(&f, types) {
                                is_subtype = true;
                                break;
                            }
                        }

                        if !is_subtype {
                            return false;
                        }
                    }

                    return true;
                }

                for t in fields.0.iter() {
                    if _type.is_subtype(t, types) {
                        return true;
                    }
                }

                false
            }

            // Enums
            Type::Enum(se) => {
                if let Type::Enum(e) = _type {
                    se == e
                } else {
                    false
                }
            }

            // Pointer
            Type::Pointer(sp) => {
                if let Type::Pointer(p) = _type {
                    sp == p
                } else {
                    false
                }
            }

            Type::Tag(s, t) => {
                if let Type::Tag(s2, t2) = _type {
                    s == s2 && t2.is_subtype(t, types)
                } else {
                    false
                }
            }

            // Everything else is to be ignored
            Type::Error
            | Type::UndeclaredTypeError(_)
            | Type::DuplicateTypeError(_, _, _)
            | Type::Unknown
            | Type::Symbol(_) => false,
        }
    }
}

// ast_sum_builder_helper(AST, &str, &mut HashMap<TypeRc, Span>, &mut HashMap<String, Span>) -> Type
// Helper function for building sum/union types.
fn ast_sum_builder_helper(ast: AST, filename: &str, fields: &mut HashMap<TypeRc, Span>, labels: &mut HashMap<String, (Span, TypeRc)>) -> Type {
    let s = ast.get_span();
    let v = convert_ast_to_type(ast, filename);
    if let Type::Union(v) = v {
        for v in v.0 {
            if let Some(s2) = fields.remove(&v) {
                return Type::DuplicateTypeError(
                    Location::new(s, filename),
                    Location::new(s2, filename),
                    v,
                );
            } else {
                match &*v {
                    Type::Enum(a) | Type::Tag(a, _) => {
                        if let Some((s2, _)) = labels.remove(a) {
                            return Type::DuplicateTypeError(
                                Location::new(s, filename),
                                Location::new(s2, filename),
                                arc::new(Type::Symbol(a.clone()))
                            );
                        }

                        labels.insert(a.clone(), (s.clone(), v.clone()));
                    }

                    _ => {
                        fields.insert(v, s.clone());
                    }
                }
            }
        }
    } else {
        let v = arc::new(v);
        if let Some(s2) = fields.remove(&v) {
            return Type::DuplicateTypeError(
                Location::new(s, filename),
                Location::new(s2, filename),
                v,
            );
        } else {
            match &*v {
                Type::Enum(a) | Type::Tag(a, _) => {
                    if let Some((s2, _)) = labels.remove(a) {
                        return Type::DuplicateTypeError(
                            Location::new(s, filename),
                            Location::new(s2, filename),
                            arc::new(Type::Symbol(a.clone()))
                        );
                    }

                    labels.insert(a.clone(), (s, v.clone()));
                }

                _ => {
                    fields.insert(v, s);
                }
            }
        }
    }

    Type::Unknown
}

// convert_ast_to_type(AST, &IR) -> Type
// Converts an ast node into a type.
pub fn convert_ast_to_type(ast: AST, filename: &str) -> Type {
    match ast {
        // Symbols
        AST::Symbol(_, v) => {
            match v.as_str() {
                // Primitives
                "Int" => Type::Int,
                "Float" => Type::Float,
                "Bool" => Type::Bool,
                "Word" => Type::Word,
                "Char" => Type::Char,

                // Symbol
                _ => Type::Symbol(v),
            }
        }

        // Enums
        AST::Prefix(_, op, v) if op == "enum" => {
            if let AST::Symbol(_, v) = *v {
                Type::Enum(v)
            } else {
                unreachable!("enum always has a symbol");
            }
        }

        // Pointer
        AST::Prefix(_, op, v) if op == "ptr" => {
            if let AST::Symbol(_, v) = *v {
                Type::Pointer(v)
            } else {
                unreachable!("ptr always has a symbol");
            }
        }

        // Sum types
        AST::Infix(_, op, l, r) if op == "|" => {
            let mut fields = HashMap::new();
            let mut labels = HashMap::new();
            let mut acc = *l;
            let t = ast_sum_builder_helper(*r, filename, &mut fields, &mut labels);
            if t != Type::Unknown {
                return t;
            }

            loop {
                match acc {
                    AST::Infix(_, op, l, r) if op == "|" => {
                        let t = ast_sum_builder_helper(*r, filename, &mut fields, &mut labels);
                        if t != Type::Unknown {
                            return t;
                        }

                        acc = *l;
                    }

                    _ => break,
                }
            }

            let t = ast_sum_builder_helper(acc, filename, &mut fields, &mut labels);
            if t != Type::Unknown {
                return t;
            }

            for f in fields.iter() {
                if let Type::UndeclaredTypeError(s) = &**f.0 {
                    return Type::UndeclaredTypeError(s.clone());
                }
            }

            if fields.len() == 1 {
                (*fields.into_iter().next().unwrap().0).clone()
            } else {
                Type::Union(HashSetWrapper(
                    fields.into_iter().map(|v| v.0).collect()
                ))
            }
        }

        // Function types
        AST::Infix(_, op, l, r) if op == "->" => {
            let l = convert_ast_to_type(*l, filename);
            let r = convert_ast_to_type(*r, filename);

            if let Type::UndeclaredTypeError(s) = l {
                Type::UndeclaredTypeError(s)
            } else if let Type::DuplicateTypeError(a, b, c) = l {
                Type::DuplicateTypeError(a, b, c)
            } else if let Type::UndeclaredTypeError(s) = r {
                Type::UndeclaredTypeError(s)
            } else if let Type::DuplicateTypeError(a, b, c) = r {
                Type::DuplicateTypeError(a, b, c)
            } else {
                Type::Func(arc::new(l), arc::new(r))
            }
        }

        AST::Infix(_, op, l, r) if op == ":" => {
            let s = r.get_span();
            let r = convert_ast_to_type(*r, filename);

            if let Type::UndeclaredTypeError(s) = r {
                Type::UndeclaredTypeError(s)
            } else if let Type::DuplicateTypeError(a, b, c) = r {
                Type::DuplicateTypeError(a, b, c)
            } else if let Type::Enum(_) = r {
                Type::UndeclaredTypeError(Location::new(s, filename))
            } else if let AST::Symbol(_, s) = *l {
                Type::Tag(s, arc::new(r))
            } else {
                unreachable!("Tag always has symbol as left operand");
            }
        }

        // Error
        _ => Type::UndeclaredTypeError(Location::new(ast.get_span(), filename)),
    }
}

pub mod arc {
    use std::sync::Arc;
    use super::Type;

    pub fn new(t: Type) -> Arc<Type> {
        Arc::new(t)
    }
}

