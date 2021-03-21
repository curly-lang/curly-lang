use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Error, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

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

pub type TypeRc = Rc<Type>;

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
    Sum(HashSetWrapper<TypeRc>),
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

            // Sum types
            Type::Sum(fields) => {
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
    // sum_hash(&self, &HashMap<String, TypeRc>) -> u64
    // Returns the hash value used by codegenned sum/union types.
    pub fn sum_hash(&self, types: &HashMap<String, TypeRc>) -> u64 {
        match self {
            Type::Symbol(_) => {
                let mut t = self;
                while let Type::Symbol(s) = t {
                    t = types.get(s).unwrap();
                }
                let mut hash = DefaultHasher::new();
                t.hash(&mut hash);
                hash.finish()
            }
            _ => {
                let mut hash = DefaultHasher::new();
                self.hash(&mut hash);
                hash.finish()
            }
        }
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

    // equals(&self, &Type, &HashMap<String, Type>) -> bool
    // Returns true if the two types are equal, accounting for type aliases.
    pub fn equals(&self, other: &Type, types: &HashMap<String, TypeRc>) -> bool {
        let mut _type = self;
        while let Type::Symbol(s) = _type {
            if let Some(v) = types.get(s) {
                _type = v;
            } else {
                break;
            }
        }

        let mut other = other;
        while let Type::Symbol(s) = other {
            if let Some(v) = types.get(s) {
                other = v;
            } else {
                break;
            }
        }

        match (_type, other) {
            (Type::Int, Type::Int)
            | (Type::Float, Type::Float)
            | (Type::Bool, Type::Bool)
            | (Type::Word, Type::Word)
            | (Type::Char, Type::Char) => true,

            (Type::Func(a1, f1), Type::Func(a2, f2)) => {
                a1.equals(a2, types) && f1.equals(f2, types)
            }

            (Type::Sum(v1), Type::Sum(v2)) => {
                /*
                if v1.0.len() != v2.0.len() {
                    return false;
                }

                'a: for v1 in v1.0.iter() {
                    for v2 in v2.0.iter() {
                        if v1.equals(v2, types) {
                            continue 'a;
                        }
                    }
                    return false;
                }
                true
                */
                v1 == v2
            }
            (Type::Enum(v1), Type::Enum(v2)) => v1 == v2,
            (Type::Pointer(v1), Type::Pointer(v2)) => v1 == v2,
            (Type::Tag(s1, t1), Type::Tag(s2, t2)) => s1 == s2 && t1.equals(t2, types),

            _ => false,
        }
    }

    // is_subtype(&self, &Type, &HashMap<String, Type>) -> bool
    // Returns true if self is a valid subtype in respect to the passed in type.
    pub fn is_subtype(&self, supertype: &Type, types: &HashMap<String, TypeRc>) -> bool {
        let mut _type = self;
        while let Type::Symbol(s) = _type {
            if let Some(t) = types.get(s) {
                _type = t;
            } else {
                break;
            }
        }

        let mut supertype = supertype;
        while let Type::Symbol(s) = supertype {
            if let Some(t) = types.get(s) {
                supertype = t;
            } else {
                break;
            }
        }

        if _type.equals(supertype, types) {
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
                    f.equals(sf, types) && a.equals(sa, types)
                } else {
                    false
                }
            }

            // Sum types
            Type::Sum(fields) => {
                // Sum types mean the subtype has fields over a subset of fields of the supertype
                if let Type::Sum(sub) = _type {
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
            let s = r.get_span();
            let v = convert_ast_to_type(*r, filename);
            if let Type::Sum(v) = v {
                for v in v.0 {
                    if let Some(s2) = fields.remove(&v) {
                        return Type::DuplicateTypeError(
                            Location::new(s, filename),
                            Location::new(s2, filename),
                            v,
                        );
                    }

                    fields.insert(v, s.clone());
                }
            } else {
                if let Some(s2) = fields.remove(&v) {
                    return Type::DuplicateTypeError(
                        Location::new(s, filename),
                        Location::new(s2, filename),
                        Rc::new(v),
                    );
                }

                fields.insert(Rc::new(v), s);
            }
            let mut acc = *l;

            loop {
                match acc {
                    AST::Infix(_, op, l, r) if op == "|" => {
                        let s = r.get_span().clone();
                        let v = convert_ast_to_type(*r, filename);
                        if let Type::Sum(v) = v {
                            for v in v.0 {
                                if let Some(s2) = fields.remove(&v) {
                                    return Type::DuplicateTypeError(
                                        Location::new(s, filename),
                                        Location::new(s2, filename),
                                        v,
                                    );
                                }

                                fields.insert(v, s.clone());
                            }
                        } else {
                            if let Some(s2) = fields.remove(&v) {
                                return Type::DuplicateTypeError(
                                    Location::new(s, filename),
                                    Location::new(s2, filename),
                                    Rc::new(v),
                                );
                            }

                            fields.insert(Rc::new(v), s);
                        }

                        acc = *l;
                    }

                    _ => break,
                }
            }

            let s = acc.get_span();
            let v = convert_ast_to_type(acc, filename);
            if let Type::Sum(v) = v {
                for v in v.0 {
                    if let Some(s2) = fields.remove(&*v) {
                        return Type::DuplicateTypeError(
                            Location::new(s, filename),
                            Location::new(s2, filename),
                            v,
                        );
                    }

                    fields.insert(v, s.clone());
                }
            } else {
                if let Some(s2) = fields.remove(&v) {
                    return Type::DuplicateTypeError(
                        Location::new(s, filename),
                        Location::new(s2, filename),
                        Rc::new(v),
                    );
                }

                fields.insert(Rc::new(v), s);
            }

            for f in fields.iter() {
                if let Type::UndeclaredTypeError(s) = &**f.0 {
                    return Type::UndeclaredTypeError(s.clone());
                }
            }

            if fields.len() == 1 {
                (*fields.into_iter().next().unwrap().0).clone()
            } else {
                Type::Sum(HashSetWrapper(
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
                Type::Func(Rc::new(l), Rc::new(r))
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
                Type::Tag(s, Rc::new(r))
            } else {
                unreachable!("Tag always has symbol as left operand");
            }
        }

        // Error
        _ => Type::UndeclaredTypeError(Location::new(ast.get_span(), filename)),
    }
}
