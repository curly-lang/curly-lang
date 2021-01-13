use std::collections::HashMap;

use super::types::Type;
use super::ir::BinOp;

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum FunctionName
{
    Infix(BinOp, Type, Type),
    Prefix(Type)
}

#[derive(Debug)]
pub struct Scope
{
    pub variables: HashMap<String, (Type, usize, usize)>,
    func_ret_types: HashMap<FunctionName, Type>,
    pub parent: Option<Box<Scope>>
}

impl Scope
{
    // new() -> Scope
    // Creates a new empty scope.
    pub fn new() -> Scope
    {
        Scope {
            variables: HashMap::with_capacity(0),
            func_ret_types: HashMap::with_capacity(0),
            parent: None
        }
    }

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

        self.func_ret_types.insert(FunctionName::Infix(BinOp::LT, Type::Int, Type::Int), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::LT, Type::Float, Type::Int), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::LT, Type::Int, Type::Float), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::LT, Type::Float, Type::Float), Type::Bool);

        self.func_ret_types.insert(FunctionName::Infix(BinOp::GT, Type::Int, Type::Int), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::GT, Type::Float, Type::Int), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::GT, Type::Int, Type::Float), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::GT, Type::Float, Type::Float), Type::Bool);

        self.func_ret_types.insert(FunctionName::Infix(BinOp::LEQ, Type::Int, Type::Int), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::LEQ, Type::Float, Type::Int), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::LEQ, Type::Int, Type::Float), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::LEQ, Type::Float, Type::Float), Type::Bool);

        self.func_ret_types.insert(FunctionName::Infix(BinOp::GEQ, Type::Int, Type::Int), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::GEQ, Type::Float, Type::Int), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::GEQ, Type::Int, Type::Float), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::GEQ, Type::Float, Type::Float), Type::Bool);

        self.func_ret_types.insert(FunctionName::Infix(BinOp::EQ, Type::Int, Type::Int), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::EQ, Type::Float, Type::Int), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::EQ, Type::Int, Type::Float), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::EQ, Type::Float, Type::Float), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::EQ, Type::String, Type::String), Type::Bool);

        self.func_ret_types.insert(FunctionName::Infix(BinOp::NEQ, Type::Int, Type::Int), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::NEQ, Type::Float, Type::Int), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::NEQ, Type::Int, Type::Float), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::NEQ, Type::Float, Type::Float), Type::Bool);
        self.func_ret_types.insert(FunctionName::Infix(BinOp::NEQ, Type::String, Type::String), Type::Bool);

         self.func_ret_types.insert(FunctionName::Infix(BinOp::And, Type::Int, Type::Int), Type::Int);
         self.func_ret_types.insert(FunctionName::Infix(BinOp::Or, Type::Int, Type::Int), Type::Int);
         self.func_ret_types.insert(FunctionName::Infix(BinOp::Xor, Type::Int, Type::Int), Type::Int);

         self.func_ret_types.insert(FunctionName::Infix(BinOp::BoolXor, Type::Bool, Type::Bool), Type::Bool);

        self
    }

    // put_var_raw(&mut self, String, Type, usize, usize) -> ()
    // Puts a variable in the current scope.
    pub fn put_var_raw(&mut self, name: String, _type: Type, arity: usize, saved_argc: usize)
    {
        self.variables.insert(name, (_type, arity, saved_argc));
    }

    // put_var(&mut self, &str, usize, usize) -> ()
    // Puts a variable in the current scope.
    pub fn put_var(&mut self, name: &str, _type: &Type, arity: usize, saved_argc: usize)
    {
        self.variables.insert(String::from(name), (_type.clone(), arity, saved_argc));
    }

    // get_var(&self, &str) -> Option<&(Type, usize, usize)>
    // Gets a variable from the stack of scopes.
    pub fn get_var(&self, name: &str) -> Option<&(Type, usize, usize)>
    {
        // Set up
        let name = String::from(name);
        let mut scope = self;

        loop
        {
            // Return success if found
            if let Some(v) = scope.variables.get(&name)
            {
                return Some(v);
            }

            // Get next scope
            scope = match &scope.parent
            {
                Some(v) => &**v,
                None => break None
            }
        }
    }

    // put_func_ret(&mut self, FunctionName, &type) -> ()
    // Puts a function and its return type in the current scope.
    pub fn put_func_ret(&mut self, name: FunctionName, _type: &Type)
    {
        self.func_ret_types.insert(name, _type.clone());
    }

    // get_func_ret(&self, FunctionName) -> Option<&Type>
    // Gets a function's return type from the stack of scopes.
    pub fn get_func_ret(&self, name: FunctionName) -> Option<&Type>
    {
        // Set up
        let mut scope = self;

        loop
        {
            // Return success if found
            if let Some(v) = scope.func_ret_types.get(&name)
            {
                return Some(v);
            }

            // Get next scope
            scope = match &scope.parent
            {
                Some(v) => &**v,
                None => break None
            }
        }
    }
}

