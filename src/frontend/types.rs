// Represents a type.
pub enum Type
{
    Unknown,
    Int,
    Float,
    Bool,
    String,
    Func(Box<Type>, Box<Type>),
}
