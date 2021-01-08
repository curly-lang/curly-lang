use std::collections::HashMap;

use super::types::Type;
use super::ir::{BinOp};

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum FunctionName
{
    Symbol(String, Type),
    Infix(BinOp, Type, Type),
    Prefix(Type)
}

#[derive(Debug)]
pub struct Scope
{
    pub variables: HashMap<String, Type>,
    pub funcs: HashMap<FunctionName, usize>,
    pub func_ret_types: HashMap<FunctionName, Type>,
    pub parent: Option<Box<Scope>>
}

impl Scope
{
    // init_builtins(mut self) -> Scope
    // Initialises a scope with the builtin operators and returns itself.
    pub fn init_builtins(mut self) -> Scope
    {
        // Prefix operators
        self.func_ret_types.insert(FunctionName::Prefix(Type::Int), Type::Int);
        self.func_ret_types.insert(FunctionName::Prefix(Type::Float), Type::Float);

        // Infix operators
        self.func_ret_types.insert(FunctionName::Infix(BinOp::Mul, Type::Int, Type::Int), Type::Int);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::Mul, Type::Float, Type::Int), Type::Float);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::Mul, Type::Int, Type::Float), Type::Float);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::Mul, Type::Float, Type::Float), Type::Float);

        self.func_ret_types.insert(FunctionName::Infix(BinOp::Div, Type::Int, Type::Int), Type::Int);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::Div, Type::Float, Type::Int), Type::Float);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::Div, Type::Int, Type::Float), Type::Float);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::Div, Type::Float, Type::Float), Type::Float);

         self.func_ret_types.insert(FunctionName::Infix(BinOp::Mod, Type::Int, Type::Int), Type::Int);

        self.func_ret_types.insert(FunctionName::Infix(BinOp::Add, Type::Int, Type::Int), Type::Int);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::Add, Type::Float, Type::Int), Type::Float);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::Add, Type::Int, Type::Float), Type::Float);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::Add, Type::Float, Type::Float), Type::Float);

        self.func_ret_types.insert(FunctionName::Infix(BinOp::Sub, Type::Int, Type::Int), Type::Int);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::Sub, Type::Float, Type::Int), Type::Float);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::Sub, Type::Int, Type::Float), Type::Float);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::Sub, Type::Float, Type::Float), Type::Float);

         self.func_ret_types.insert(FunctionName::Infix(BinOp::BSL, Type::Int, Type::Int), Type::Int);
         self.func_ret_types.insert(FunctionName::Infix(BinOp::BSR, Type::Int, Type::Int), Type::Int);


         self.func_ret_types.insert(FunctionName::Infix(BinOp::And, Type::Int, Type::Int), Type::Int);
         self.func_ret_types.insert(FunctionName::Infix(BinOp::Or, Type::Int, Type::Int), Type::Int);
         self.func_ret_types.insert(FunctionName::Infix(BinOp::Xor, Type::Int, Type::Int), Type::Int);

        self
    }
}
