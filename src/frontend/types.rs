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
}

// convert_ast_to_type(AST) -> Type
// Converts an ast node into a type.
pub fn convert_ast_to_type(ast: AST) -> Type
{
    match ast
    {
        AST::Symbol(s, v) => {
            match v.as_str()
            {
                "Int" => Type::Int,
                "Float" => Type::Float,
                "Bool" => Type::Bool,
                _ => Type::ConversionError(s)
            }
        }

        AST::Infix(_, op, l, r) if op.as_str() == "->" =>
            Type::Func(Box::new(convert_ast_to_type(*l)), Box::new(convert_ast_to_type(*r))),

        AST::Prefix(_, op, v) if op.as_str() == "" =>
            convert_ast_to_type(*v),

        _ => Type::ConversionError(ast.get_span())
    }
}
