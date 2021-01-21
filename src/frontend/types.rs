use logos::Span;

use super::parser::AST;

// Represents a type.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type
{
    Error,
    ConversionError(Span),
    Unknown,
    Int,
    Float,
    Bool,
    String,
    Func(Box<Type>, Box<Type>),
    Sum(Vec<Type>),
    Enum(String)
}

impl Type
{
    pub fn is_subtype(&self, supertype: &Type) -> bool
    {
        match supertype
        {
            Type::Int => *self == Type::Int,
            Type::Float => *self == Type::Float,
            Type::Bool => *self == Type::Bool,
            Type::String => *self == Type::String,

            Type::Func(sf, sa) =>
                if let Type::Func(f, a) = self
                {
                    f.is_subtype(sf) && a.is_subtype(sa)
                } else
                {
                    false
                }

            Type::Sum(types) => {
                for t in types
                {
                    if self.is_subtype(t)
                    {
                        return true;
                    }
                }

                false
            }

            Type::Enum(se) =>
                if let Type::Enum(e) = self
                {
                    se == e
                } else
                {
                    false
                }

            _ => false
        }
    }
}

// convert_ast_to_type(AST) -> Type
// Converts an ast node into a type.
pub fn convert_ast_to_type(ast: AST) -> Type
{
    match ast
    {
        // Symbols
        AST::Symbol(s, v) => {
            match v.as_str()
            {
                // Primatives
                "Int" => Type::Int,
                "Float" => Type::Float,
                "Bool" => Type::Bool,

                // Error
                _ => Type::ConversionError(s)
            }
        }

        // Enums
        AST::Prefix(_, op, v) if op == "enum" =>
            if let AST::Symbol(_, v) = *v
            {
                Type::Enum(v)
            } else
            {
                unreachable!("enum always has a symbol");
            }

        // Sum types
        AST::Infix(_, op, l, r) if op == "|" => {
            let mut fields = vec![convert_ast_to_type(*r)];
            let mut acc = *l;

            loop 
            {
                match acc
                {
                    AST::Infix(_, op, l, r) if op == "|" => {
                        fields.insert(0, convert_ast_to_type(*r));
                        acc = *l;
                    }

                    _ => break
                }
            }

            fields.insert(0, convert_ast_to_type(acc));
            Type::Sum(fields)
        }

        // Function types
        AST::Infix(_, op, l, r) if op == "->" =>
            Type::Func(Box::new(convert_ast_to_type(*l)), Box::new(convert_ast_to_type(*r))),

        // Parenthesised types
        AST::Prefix(_, op, v) if op == "" =>
            convert_ast_to_type(*v),

        // Error
        _ => Type::ConversionError(ast.get_span())
    }
}
