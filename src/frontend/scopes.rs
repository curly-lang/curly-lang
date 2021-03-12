use std::collections::HashMap;
use std::rc::Rc;

use super::ir::{BinOp, Location};
use super::types::{Type, TypeRc};

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
            .insert(FunctionName::Prefix(Rc::new(Type::Int)), Rc::new(Type::Int));
        self.func_ret_types
            .insert(FunctionName::Prefix(Rc::new(Type::Float)), Rc::new(Type::Float));

        // Infix operators
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Mul, Rc::new(Type::Word), Rc::new(Type::Word)),
            Rc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Mul, Rc::new(Type::Int), Rc::new(Type::Int)),
            Rc::new(Type::Int),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Mul, Rc::new(Type::Float), Rc::new(Type::Int)),
            Rc::new(Type::Float),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Mul, Rc::new(Type::Int), Rc::new(Type::Float)),
            Rc::new(Type::Float),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Mul, Rc::new(Type::Float), Rc::new(Type::Float)),
            Rc::new(Type::Float),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Div, Rc::new(Type::Word), Rc::new(Type::Word)),
            Rc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Div, Rc::new(Type::Int), Rc::new(Type::Int)),
            Rc::new(Type::Int),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Div, Rc::new(Type::Float), Rc::new(Type::Int)),
            Rc::new(Type::Float),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Div, Rc::new(Type::Int), Rc::new(Type::Float)),
            Rc::new(Type::Float),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Div, Rc::new(Type::Float), Rc::new(Type::Float)),
            Rc::new(Type::Float),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Mod, Rc::new(Type::Word), Rc::new(Type::Word)),
            Rc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Mod, Rc::new(Type::Int), Rc::new(Type::Int)),
            Rc::new(Type::Int),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Add, Rc::new(Type::Word), Rc::new(Type::Word)),
            Rc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Add, Rc::new(Type::Int), Rc::new(Type::Int)),
            Rc::new(Type::Int),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Add, Rc::new(Type::Float), Rc::new(Type::Int)),
            Rc::new(Type::Float),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Add, Rc::new(Type::Int), Rc::new(Type::Float)),
            Rc::new(Type::Float),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Add, Rc::new(Type::Float), Rc::new(Type::Float)),
            Rc::new(Type::Float),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Sub, Rc::new(Type::Word), Rc::new(Type::Word)),
            Rc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Sub, Rc::new(Type::Int), Rc::new(Type::Int)),
            Rc::new(Type::Int),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Sub, Rc::new(Type::Float), Rc::new(Type::Int)),
            Rc::new(Type::Float),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Sub, Rc::new(Type::Int), Rc::new(Type::Float)),
            Rc::new(Type::Float),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Sub, Rc::new(Type::Float), Rc::new(Type::Float)),
            Rc::new(Type::Float),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::BSL, Rc::new(Type::Word), Rc::new(Type::Word)),
            Rc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::BSL, Rc::new(Type::Int), Rc::new(Type::Int)),
            Rc::new(Type::Int),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::BSR, Rc::new(Type::Word), Rc::new(Type::Word)),
            Rc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::BSR, Rc::new(Type::Int), Rc::new(Type::Int)),
            Rc::new(Type::Int),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LT, Rc::new(Type::Char), Rc::new(Type::Char)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LT, Rc::new(Type::Word), Rc::new(Type::Word)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LT, Rc::new(Type::Int), Rc::new(Type::Int)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LT, Rc::new(Type::Float), Rc::new(Type::Int)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LT, Rc::new(Type::Int), Rc::new(Type::Float)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LT, Rc::new(Type::Float), Rc::new(Type::Float)),
            Rc::new(Type::Bool),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GT, Rc::new(Type::Char), Rc::new(Type::Char)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GT, Rc::new(Type::Word), Rc::new(Type::Word)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GT, Rc::new(Type::Int), Rc::new(Type::Int)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GT, Rc::new(Type::Float), Rc::new(Type::Int)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GT, Rc::new(Type::Int), Rc::new(Type::Float)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GT, Rc::new(Type::Float), Rc::new(Type::Float)),
            Rc::new(Type::Bool),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LEQ, Rc::new(Type::Char), Rc::new(Type::Char)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LEQ, Rc::new(Type::Word), Rc::new(Type::Word)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LEQ, Rc::new(Type::Int), Rc::new(Type::Int)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LEQ, Rc::new(Type::Float), Rc::new(Type::Int)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LEQ, Rc::new(Type::Int), Rc::new(Type::Float)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::LEQ, Rc::new(Type::Float), Rc::new(Type::Float)),
            Rc::new(Type::Bool),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GEQ, Rc::new(Type::Char), Rc::new(Type::Char)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GEQ, Rc::new(Type::Word), Rc::new(Type::Word)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GEQ, Rc::new(Type::Int), Rc::new(Type::Int)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GEQ, Rc::new(Type::Float), Rc::new(Type::Int)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GEQ, Rc::new(Type::Int), Rc::new(Type::Float)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::GEQ, Rc::new(Type::Float), Rc::new(Type::Float)),
            Rc::new(Type::Bool),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::EQ, Rc::new(Type::Char), Rc::new(Type::Char)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::EQ, Rc::new(Type::Word), Rc::new(Type::Word)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::EQ, Rc::new(Type::Int), Rc::new(Type::Int)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::EQ, Rc::new(Type::Float), Rc::new(Type::Float)),
            Rc::new(Type::Bool),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::NEQ, Rc::new(Type::Char), Rc::new(Type::Char)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::NEQ, Rc::new(Type::Word), Rc::new(Type::Word)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::NEQ, Rc::new(Type::Int), Rc::new(Type::Int)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::NEQ, Rc::new(Type::Float), Rc::new(Type::Int)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::NEQ, Rc::new(Type::Int), Rc::new(Type::Float)),
            Rc::new(Type::Bool),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::NEQ, Rc::new(Type::Float), Rc::new(Type::Float)),
            Rc::new(Type::Bool),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::And, Rc::new(Type::Word), Rc::new(Type::Word)),
            Rc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::And, Rc::new(Type::Int), Rc::new(Type::Int)),
            Rc::new(Type::Int),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Or, Rc::new(Type::Word), Rc::new(Type::Word)),
            Rc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Or, Rc::new(Type::Int), Rc::new(Type::Int)),
            Rc::new(Type::Int),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Xor, Rc::new(Type::Word), Rc::new(Type::Word)),
            Rc::new(Type::Word),
        );
        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::Xor, Rc::new(Type::Int), Rc::new(Type::Int)),
            Rc::new(Type::Int),
        );

        self.func_ret_types.insert(
            FunctionName::Infix(BinOp::BoolXor, Rc::new(Type::Bool), Rc::new(Type::Bool)),
            Rc::new(Type::Bool),
        );

        // Functions
        self.put_var_raw(
            String::from("debug"),
            Rc::new(Type::Unknown),
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
