use super::parser::AST;

// Represents a type.
#[derive(Debug, Clone)]
pub enum Type
{
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
        AST::Symbol(s) => {
            match s.as_str()
            {
                "Int" => Type::Int,
                "Float" => Type::Float,
                "Bool" => Type::Bool,
                _ => panic!("Invalid type!")
            }
        }

        _ => panic!("Invalid type!")
    }
}