use std::collections::HashMap;

use super::ir::{BinOp, Location};
use super::types::{arc, Type, TypeRc};

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum FunctionName {
    Infix(BinOp, TypeRc, TypeRc),
    Prefix(TypeRc),
}

#[derive(Debug)]
pub struct Scope {
    pub variables: HashMap<String, (TypeRc, usize, Option<usize>, Location, bool, String)>,
    func_ret_types: HashMap<FunctionName, TypeRc>,
    pub parent: Option<Box<Scope>>,
    new_func: bool,
}

impl Scope {
    // new() -> Scope
    // Creates a new empty scope.
    pub fn new() -> Scope {
        Scope {
            variables: HashMap::with_capacity(0),
            func_ret_types: HashMap::with_capacity(0),
            parent: None,
            new_func: false,
        }
    }

    // init_builtins(mut self) -> Scope
    // Initialises a scope with the builtin operators and returns itself.
    pub fn init_builtins(mut self) -> Scope {
        // Prefix operators
        self.func_ret_types
            .insert(FunctionName::Prefix(arc::new(Type::Int)), arc::new(Type::Int));
        self.func_ret_types
            .insert(FunctionName::Prefix(arc::new(Type::Float)), arc::new(Type::Float));

        // Infix operators
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Mul, arc::new(Type::Word), arc::new(Type::Word)),
            arc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Mul, arc::new(Type::Int), arc::new(Type::Int)),
            arc::new(Type::Int),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Mul, arc::new(Type::Float), arc::new(Type::Int)),
            arc::new(Type::Float),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Mul, arc::new(Type::Int), arc::new(Type::Float)),
            arc::new(Type::Float),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Mul, arc::new(Type::Float), arc::new(Type::Float)),
            arc::new(Type::Float),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Div, arc::new(Type::Word), arc::new(Type::Word)),
            arc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Div, arc::new(Type::Int), arc::new(Type::Int)),
            arc::new(Type::Int),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Div, arc::new(Type::Float), arc::new(Type::Int)),
            arc::new(Type::Float),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Div, arc::new(Type::Int), arc::new(Type::Float)),
            arc::new(Type::Float),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Div, arc::new(Type::Float), arc::new(Type::Float)),
            arc::new(Type::Float),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Mod, arc::new(Type::Word), arc::new(Type::Word)),
            arc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Mod, arc::new(Type::Int), arc::new(Type::Int)),
            arc::new(Type::Int),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Add, arc::new(Type::Word), arc::new(Type::Word)),
            arc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Add, arc::new(Type::Int), arc::new(Type::Int)),
            arc::new(Type::Int),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Add, arc::new(Type::Float), arc::new(Type::Int)),
            arc::new(Type::Float),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Add, arc::new(Type::Int), arc::new(Type::Float)),
            arc::new(Type::Float),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Add, arc::new(Type::Float), arc::new(Type::Float)),
            arc::new(Type::Float),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Sub, arc::new(Type::Word), arc::new(Type::Word)),
            arc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Sub, arc::new(Type::Int), arc::new(Type::Int)),
            arc::new(Type::Int),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Sub, arc::new(Type::Float), arc::new(Type::Int)),
            arc::new(Type::Float),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Sub, arc::new(Type::Int), arc::new(Type::Float)),
            arc::new(Type::Float),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Sub, arc::new(Type::Float), arc::new(Type::Float)),
            arc::new(Type::Float),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::BSL, arc::new(Type::Word), arc::new(Type::Word)),
            arc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::BSL, arc::new(Type::Int), arc::new(Type::Int)),
            arc::new(Type::Int),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::BSR, arc::new(Type::Word), arc::new(Type::Word)),
            arc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::BSR, arc::new(Type::Int), arc::new(Type::Int)),
            arc::new(Type::Int),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LT, arc::new(Type::Char), arc::new(Type::Char)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LT, arc::new(Type::Word), arc::new(Type::Word)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LT, arc::new(Type::Int), arc::new(Type::Int)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LT, arc::new(Type::Float), arc::new(Type::Int)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LT, arc::new(Type::Int), arc::new(Type::Float)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LT, arc::new(Type::Float), arc::new(Type::Float)),
            arc::new(Type::Bool),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GT, arc::new(Type::Char), arc::new(Type::Char)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GT, arc::new(Type::Word), arc::new(Type::Word)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GT, arc::new(Type::Int), arc::new(Type::Int)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GT, arc::new(Type::Float), arc::new(Type::Int)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GT, arc::new(Type::Int), arc::new(Type::Float)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GT, arc::new(Type::Float), arc::new(Type::Float)),
            arc::new(Type::Bool),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LEQ, arc::new(Type::Char), arc::new(Type::Char)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LEQ, arc::new(Type::Word), arc::new(Type::Word)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LEQ, arc::new(Type::Int), arc::new(Type::Int)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LEQ, arc::new(Type::Float), arc::new(Type::Int)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LEQ, arc::new(Type::Int), arc::new(Type::Float)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LEQ, arc::new(Type::Float), arc::new(Type::Float)),
            arc::new(Type::Bool),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GEQ, arc::new(Type::Char), arc::new(Type::Char)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GEQ, arc::new(Type::Word), arc::new(Type::Word)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GEQ, arc::new(Type::Int), arc::new(Type::Int)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GEQ, arc::new(Type::Float), arc::new(Type::Int)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GEQ, arc::new(Type::Int), arc::new(Type::Float)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GEQ, arc::new(Type::Float), arc::new(Type::Float)),
            arc::new(Type::Bool),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::EQ, arc::new(Type::Char), arc::new(Type::Char)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::EQ, arc::new(Type::Word), arc::new(Type::Word)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::EQ, arc::new(Type::Int), arc::new(Type::Int)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::EQ, arc::new(Type::Float), arc::new(Type::Float)),
            arc::new(Type::Bool),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::NEQ, arc::new(Type::Char), arc::new(Type::Char)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::NEQ, arc::new(Type::Word), arc::new(Type::Word)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::NEQ, arc::new(Type::Int), arc::new(Type::Int)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::NEQ, arc::new(Type::Float), arc::new(Type::Int)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::NEQ, arc::new(Type::Int), arc::new(Type::Float)),
            arc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::NEQ, arc::new(Type::Float), arc::new(Type::Float)),
            arc::new(Type::Bool),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::And, arc::new(Type::Word), arc::new(Type::Word)),
            arc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::And, arc::new(Type::Int), arc::new(Type::Int)),
            arc::new(Type::Int),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Or, arc::new(Type::Word), arc::new(Type::Word)),
            arc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Or, arc::new(Type::Int), arc::new(Type::Int)),
            arc::new(Type::Int),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Xor, arc::new(Type::Word), arc::new(Type::Word)),
            arc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Xor, arc::new(Type::Int), arc::new(Type::Int)),
            arc::new(Type::Int),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::BoolXor, arc::new(Type::Bool), arc::new(Type::Bool)),
            arc::new(Type::Bool),
        );

        // Functions
        self.put_var_raw(
            String::from("debug"),
            arc::new(Type::Unknown),
            1,
            None,
            Location::empty(),
            true,
            String::with_capacity(0),
        );

        self
    }

    // put_var_raw(&mut self, String, TypeRc, usize, Option<usize>, Span, bool) -> ()
    // Puts a variable in the current scope.
    pub fn put_var_raw(
        &mut self,
        name: String,
        _type: TypeRc,
        arity: usize,
        saved_argc: Option<usize>,
        loc: Location,
        assigned: bool,
        origin: String,
    ) {
        self.variables
            .insert(name, (_type, arity, saved_argc, loc, assigned, origin));
    }

    // put_var(&mut self, &str, usize, Option<usize>, Span, bool) -> ()
    // Puts a variable in the current scope.
    pub fn put_var(
        &mut self,
        name: &str,
        _type: &TypeRc,
        arity: usize,
        saved_argc: Option<usize>,
        loc: &Location,
        assigned: bool,
        origin: &str,
    ) {
        self.variables.insert(
            String::from(name),
            (
                _type.clone(),
                arity,
                saved_argc,
                loc.clone(),
                assigned,
                String::from(origin),
            ),
        );
    }

    // get_var(&self, &str) -> Option<&(Type, usize, Option<usize>, Span)>
    // Gets a variable from the stack of scopes.
    pub fn get_var(
        &self,
        name: &str,
    ) -> Option<&(TypeRc, usize, Option<usize>, Location, bool, String)> {
        // Set up
        let name = String::from(name);
        let mut scope = self;

        loop {
            // Return success if found
            if let Some(v) = scope.variables.get(&name) {
                return Some(v);
            }

            // Get next scope
            scope = match &scope.parent {
                Some(v) => &**v,
                None => break None,
            }
        }
    }

    // put_func_ret(&mut self, FunctionName, &type) -> ()
    // Puts a function and its return type in the current scope.
    pub fn put_func_ret(&mut self, name: FunctionName, _type: &TypeRc) {
        self.func_ret_types.insert(name, _type.clone());
    }

    // get_func_ret(&self, FunctionName) -> Option<&Type>
    // Gets a function's return type from the stack of scopes.
    pub fn get_func_ret(&self, name: FunctionName) -> Option<&TypeRc> {
        // Set up
        let mut scope = self;

        loop {
            // Return success if found
            if let Some(v) = scope.func_ret_types.get(&name) {
                return Some(v);
            }

            // Get next scope
            scope = match &scope.parent {
                Some(v) => &**v,
                None => break None,
            }
        }
    }

    // push_scope(&mut self, bool) -> ()
    // Pushes a new scope to the top of the scope stack.
    pub fn push_scope(&mut self, new_func: bool) {
        use std::mem::swap;

        let mut scope = Scope::new();
        scope.new_func = new_func;

        swap(&mut scope, self);
        self.parent = Some(Box::new(scope));
    }

    // pop_scop(&mut self) -> ()
    // Pops a scope from the stack if a parent scope exists.
    pub fn pop_scope(&mut self) {
        use std::mem::swap;

        if let Some(v) = &mut self.parent {
            let mut scope = Scope::new();

            swap(&mut scope, v);
            swap(self, &mut scope);
        }
    }

    // is_captured(&self, &str) -> bool
    // Returns true if captured from a new function
    pub fn is_captured(&self, name: &str) -> bool {
        // Set up
        let mut scope = self;
        let mut last_new_func = false;
        let mut new_func;

        loop {
            // Global scope is not captured
            if scope.parent.is_none() {
                break false;
            }

            // Update new_func if in a new function
            new_func = last_new_func;
            if scope.new_func {
                last_new_func = true;
            }

            // Return success if found
            if let Some(v) = scope.variables.get(name) {
                break if let Type::Enum(_) = *v.0 {
                    false
                } else {
                    new_func
                };
            }

            // Get next scope
            scope = match &scope.parent {
                Some(v) => &**v,
                None => break false,
            }
        }
    }
}
