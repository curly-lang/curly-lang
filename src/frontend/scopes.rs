use std::collections::HashMap;

use super::types::Type;
use super::ir::{BinOp, PrefixOp};

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum FunctionName
{
    Symbol(String, Type),
    Infix(BinOp, Type, Type),
    Prefix(PrefixOp, Type)
}

#[derive(Debug)]
pub struct Scope
{
    pub variables: HashMap<String, Type>,
    pub funcs: HashMap<FunctionName, usize>,
    pub func_ret_types: HashMap<FunctionName, Type>,
    pub parent: Option<Box<Scope>>
}

