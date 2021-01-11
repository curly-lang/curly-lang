use logos::Span;
use std::collections::{HashMap, HashSet};
use std::mem::swap;

use super::ir::{BinOp, IR, IRFunction, PrefixOp, SExpr, SExprMetadata};
use super::scopes::{FunctionName, Scope};
use super::types::Type;

#[derive(Debug)]
pub enum CorrectnessError
{
    UndefinedPrefixOp(Span, PrefixOp, Type),
    UndefinedInfixOp(Span, BinOp, Type, Type),
    NonboolInBoolExpr(Span, Type, Type),
    NonboolInIfCond(Span, Type),
    NonmatchingIfBodies(Span, Type, Span, Type),
    NonmatchingAssignTypes(Span, Type, Type),
    SymbolNotFound(Span, String),
    FunctionTypeNotFound(Span, String),
    Reassignment(Span, String),
    InvalidType(Span),
    UnknownFunctionReturnType(Span, String),
    MismatchedFunctionArgType(Span, Type, Type),
    InvalidApplication(Span, Type)
}

// check_sexpr(&mut SExpr, &mut SExprMetadata, &mut Vec<CorrectnessError>) -> ()
// Checks an s expression for type correctness and correct symbol usage.
fn check_sexpr(sexpr: &mut SExpr, root: &mut IR, errors: &mut Vec<CorrectnessError>)
{
    if let Type::ConversionError = sexpr.get_metadata()._type
    {
        errors.push(CorrectnessError::InvalidType(
            sexpr.get_metadata().span.clone()
        ));
        return;
    }

    match sexpr
    {
        // Values
        SExpr::Int(_, _) => (),
        SExpr::Float(_, _) => (),
        SExpr::String(_, _) => (),
        SExpr::True(_) => (),
        SExpr::False(_) => (),

        // Functions
        SExpr::Function(m, f) => {
            match root.metadata.scope.get_var(f)
            {
                Some(t) => m._type = t.clone(),
                None => errors.push(CorrectnessError::FunctionTypeNotFound(
                    m.span.clone(),
                    f.clone()
                ))
            }
        }

        // Symbols
        SExpr::Symbol(m, s) => {
            match root.metadata.scope.get_var(s)
            {
                Some(t) => m._type = t.clone(),
                None => errors.push(CorrectnessError::SymbolNotFound(
                    m.span.clone(),
                    s.clone()
                ))
            }
        }

        // Prefix operators
        SExpr::Prefix(m, op, v) => {
            match op
            {
                // Negative has operators defined in scope
                PrefixOp::Neg => {
                    // Check child node
                    check_sexpr(v, root, errors);

                    // Check if an error occured
                    if v.get_metadata()._type == Type::Error
                    {
                        return;
                    }

                    // Get type
                    if let Some(t) = root.metadata.scope.get_func_ret(FunctionName::Prefix(v.get_metadata()._type.clone()))
                    {
                        m._type = t.clone();
                    } else
                    {
                        errors.push(CorrectnessError::UndefinedPrefixOp(
                            m.span.clone(),
                            PrefixOp::Neg,
                            v.get_metadata()._type.clone()
                        ));
                    }
                }

                PrefixOp::Span => panic!("unsupported operator!")
            }
        }

        // Infix operators
        SExpr::Infix(m, op, left, right) => {
            // Check child nodes
            check_sexpr(left, root, errors);
            check_sexpr(right, root, errors);

            // Check if an error occured
            if left.get_metadata()._type == Type::Error || right.get_metadata()._type == Type::Error
            {
                return;
            }

            // Get type
            if let Some(t) = root.metadata.scope.get_func_ret(FunctionName::Infix(
                *op,
                left.get_metadata()._type.clone(),
                right.get_metadata()._type.clone()
            ))
            {
                m._type = t.clone();
            } else
            {
                errors.push(CorrectnessError::UndefinedInfixOp(m.span.clone(), *op, left.get_metadata()._type.clone(), right.get_metadata()._type.clone()));
            }
        }

        // Boolean and/or
        SExpr::And(m, left, right) | SExpr::Or(m, left, right) => {
            // Check child nodes
            check_sexpr(left, root, errors);
            check_sexpr(right, root, errors);

            // Check for error
            if left.get_metadata()._type == Type::Error || right.get_metadata()._type == Type::Error
            {
                return;
            }

            // Check the types of the child nodes are Bool
            if left.get_metadata()._type != Type::Bool || right.get_metadata()._type != Type::Bool
            {
                errors.push(CorrectnessError::NonboolInBoolExpr(
                    m.span.clone(),
                    left.get_metadata()._type.clone(),
                    right.get_metadata()._type.clone()
                ));
            }
        }

        // If expressions
        SExpr::If(m, cond, then, elsy) => {
            // Check child nodes
            check_sexpr(cond, root, errors);
            check_sexpr(then, root, errors);
            check_sexpr(elsy, root, errors);

            // Check if an error occured
            if cond.get_metadata()._type == Type::Error || then.get_metadata()._type == Type::Error || elsy.get_metadata()._type == Type::Error
            {
                return;
            }

            // Check that condition is a boolean
            if cond.get_metadata()._type != Type::Bool
            {
                errors.push(CorrectnessError::NonboolInIfCond(
                    cond.get_metadata().span.clone(),
                    cond.get_metadata()._type.clone()
                ));
            }

            // Check that the types of the then and else blocks match
            if then.get_metadata()._type != elsy.get_metadata()._type
            {
                errors.push(CorrectnessError::NonmatchingIfBodies(
                    then.get_metadata().span.clone(),
                    then.get_metadata()._type.clone(),
                    elsy.get_metadata().span.clone(),
                    elsy.get_metadata()._type.clone()
                ));
            } else
            {
                m._type = then.get_metadata()._type.clone();
            }
        }

        SExpr::Application(m, func, arg) => {
            // Check the function and arg types
            check_sexpr(func, root, errors);
            check_sexpr(arg, root, errors);

            // Check if either the function or argument resulted in an error
            if func.get_metadata()._type == Type::Error || arg.get_metadata()._type == Type::Error
            {
                return;
            }

            // Match the function
            match &func.get_metadata()._type
            {
                // Strings concatenate to other strings
                Type::String => {
                    m._type = Type::String;
                }

                // Functions apply their arguments
                Type::Func(l, r) => {
                    if **l == arg.get_metadata()._type
                    {
                        m._type = *r.clone();
                    } else
                    {
                        errors.push(CorrectnessError::MismatchedFunctionArgType(
                            arg.get_metadata().span.clone(),
                            *r.clone(),
                            arg.get_metadata()._type.clone()
                        ));
                    }
                }

                // Everything else is invalid
                _ => {
                    errors.push(CorrectnessError::InvalidApplication(
                        func.get_metadata().span.clone(),
                        func.get_metadata()._type.clone()
                    ));
                }
            }
        }

        // Assignments
        SExpr::Assign(m, name, value) => {
            // Check if variable already exists
            if let Some(_) = root.metadata.scope.get_var(name)
            {
                // Only error if the value is not a sexpression
                if let SExpr::Function(_, _) = **value {}
                else
                {
                    errors.push(CorrectnessError::Reassignment(
                            m.span.clone(),
                            name.clone()
                    ));
                    return;
                }
            }

            // Check child node
            check_sexpr(value, root, errors);

            // Check if an error occured
            if value.get_metadata()._type == Type::Error
            {
                return;
            }

            // Check that the types match
            match m._type
            {
                // No preassigned type
                Type::Error => m._type = value.get_metadata()._type.clone(),

                // Preassigned type
                _ => {
                    // Types do not match
                    if m._type != value.get_metadata()._type
                    {
                        errors.push(CorrectnessError::NonmatchingAssignTypes(
                            m.span.clone(),
                            m._type.clone(),
                            value.get_metadata()._type.clone()
                        ));
                        m._type = Type::Error;
                    }
                }
            }

            // Add variable to scope if no error occured
            if m._type != Type::Error
            {
                root.metadata.scope.put_var(name, &m._type);
            }
        }

        // With expressions
        SExpr::With(m, assigns, body) => {
            // Push a new scope
            root.metadata.push_scope();

            // Function iterator
            let iter = assigns.iter().filter_map(
                |a| if let SExpr::Assign(_, _, a) = &a
                {
                    if let SExpr::Function(_, v) = &**a
                    {
                        Some(v.clone())
                    } else
                    {
                        None
                    }
                } else
                {
                    None
                }
            );

            // Deal with functions
            check_function_group(iter, root, errors);

            // Check assignments
            for a in assigns
            {
                check_sexpr(a, root, errors);
            }

            // Check body
            check_sexpr(body, root, errors);
            m._type = body.get_metadata()._type.clone();

            // Pop scope
            root.metadata.pop_scope();
        }
    }
}

// convert_function_symbols(&mut SExpr, &mut HashSet<String>) -> ()
// Converts function symbols in a sexpression into function references.
fn convert_function_symbols(sexpr: &mut SExpr, scopes: &HashSet<String>)
{
    match sexpr
    {
        // Check symbol for functions
        SExpr::Symbol(m, s) => {
            if scopes.contains(s)
            {
                let mut meta = SExprMetadata {
                    span: Span {
                        start: 0,
                        end: 0
                    },
                    _type: Type::Error
                };

                swap(&mut meta, m);
                let mut id = String::with_capacity(0);
                swap(&mut id, s);
                *sexpr = SExpr::Function(meta, id);
            }
        }

        // Prefix operators
        SExpr::Prefix(_, _, v) => {
            convert_function_symbols(v, scopes);
        }

        // Infix operators
        SExpr::Infix(_, _, l, r)
            | SExpr::And(_, l, r)
            | SExpr::Or(_, l, r)
            | SExpr::Application(_, l, r)
            => {
            convert_function_symbols(l, scopes);
            convert_function_symbols(r, scopes);
        }

        // If expressions
        SExpr::If(_, c, b, e) => {
            convert_function_symbols(c, scopes);
            convert_function_symbols(b, scopes);
            convert_function_symbols(e, scopes);
        }

        // Check scope
        SExpr::With(_, assigns, body) => {
            // Check assigns
            for a in assigns
            {
                convert_function_symbols(a, scopes);
            }

            // Check body
            convert_function_symbols(body, scopes);
        }

        // Check assignments
        SExpr::Assign(_, _, value) => {
            convert_function_symbols(value, scopes);
        }

        // Ignore everything else
        _ => ()
    }
}

// get_function_type(&SExpr, &mut Scope, &HashMap<String, IRFunction>, &mut Vec<HashMap<String, Type>>, &mut Vec<CorrectnessError>) -> Type
// Gets the function type, returning Type::Unknown if a type cannot be found.
fn get_function_type(sexpr: &SExpr, scope: &mut Scope, funcs: &HashMap<String, IRFunction>, vars: &mut Vec<HashMap<String, Type>>, errors: &mut Vec<CorrectnessError>) -> Type
{
    match sexpr
    {
        // Values
        SExpr::Int(m, _)
            | SExpr::Float(m, _)
            | SExpr::String(m, _)
            | SExpr::True(m)
            | SExpr::False(m)
            => m._type.clone(),

        // Functions
        SExpr::Function(_, f) => {
            // Check scope
            match scope.get_var(f)
            {
                Some(v) => v.clone(),

                // Check child function
                None => {
                    check_function_body(f, funcs.get(f).unwrap(), scope, funcs, errors);
                    scope.get_var(f).unwrap().clone()
                }
            }
        }

        // Symbols
        SExpr::Symbol(_, s) => {
            // Check global scope
            match scope.get_var(s)
            {
                Some(v) => v.clone(),

                // Check local scopes
                None => {
                    // Get the last scope
                    let mut iter = vars.iter();
                    let mut v = match iter.next()
                    {
                        Some(v) => v,
                        None => return Type::Unknown
                    };

                    // Iterate over each scope
                    loop
                    {
                        match v.get(s)
                        {
                            // Break if variable is found
                            Some(v) => break v.clone(),

                            // Get next scope
                            None => {
                                v = match iter.next()
                                {
                                    Some(v) => v,
                                    None => break Type::Unknown
                                }
                            }
                        }
                    }
                }
            }
        }

        // Prefix operators
        SExpr::Prefix(_, op, v) => {
            match op
            {
                PrefixOp::Neg => {
                    let vt = get_function_type(v, scope, funcs, vars, errors).clone();

                    match scope.get_func_ret(FunctionName::Prefix(vt))
                    {
                        Some(v) => v.clone(),
                        None => Type::Unknown
                    }
                }

                PrefixOp::Span => panic!("unsupported operator")
            }
        }

        // Infix operators
        SExpr::Infix(_, op, l, r) => {
            let lt = get_function_type(l, scope, funcs, vars, errors).clone();
            let rt = get_function_type(r, scope, funcs, vars, errors).clone();

            match scope.get_func_ret(FunctionName::Infix(*op, lt, rt))
            {
                Some(v) => v.clone(),
                None => Type::Unknown
            }
        }

        // If expressions
        SExpr::If(_, _, body, elsy) => {
            let bt = get_function_type(body, scope, funcs, vars, errors);
            if bt != Type::Unknown
            {
                bt
            } else
            {
                get_function_type(elsy, scope, funcs, vars, errors)
            }
        }

        // Applications
        SExpr::Application(_, f, a) => {
            // Get function type
            let ft = get_function_type(f, scope, funcs, vars, errors);
            if let Type::Unknown = ft
            {
                return Type::Unknown;
            }

            // Get argument type
            let at = get_function_type(a, scope, funcs, vars, errors);
            if let Type::Unknown = at
            {
                return Type::Unknown;
            }

            match ft
            {
                // Strings concatenate to other strings
                Type::String => Type::String,

                // Functions apply their arguments
                Type::Func(l, r) => {
                    if *l == at
                    {
                        *r.clone()
                    } else
                    {
                        Type::Unknown
                    }
                }

                // Everything else is invalid
                _ => Type::Unknown
            }
        }

        // Assignments
        SExpr::Assign(_, name, value) => {
            if let Some(_) = vars.last()
            {
                let t = get_function_type(value, scope, funcs, vars, errors);
                vars.last_mut().unwrap().insert(name.clone(), t.clone());
                t
            } else
            {
                Type::Unknown
            }
        }

        // With expressions
        SExpr::With(_, assigns, body) => {
            // Push scope
            vars.push(HashMap::new());

            // Populate scope with variable types
            for a in assigns
            {
                get_function_type(a, scope, funcs, vars, errors);
            }

            // Get the function type
            let bt = get_function_type(body, scope, funcs, vars, errors);

            // Pop the scope and return type
            vars.pop();
            bt
        }

        // Everything else is unknown
        _ => Type::Unknown
    }
}

// check_function_body(&str, &IRFunction, &mut Scope, &HashMap<String, IRFunction>, &mut Vec<CorrectnessError>) -> ()
// Checks a function body and determines the return type of the function.
fn check_function_body(name: &str, func: &IRFunction, scope: &mut Scope, funcs: &HashMap<String, IRFunction>, errors: &mut Vec<CorrectnessError>)
{
    // Put arguments into scope
    let mut vars = vec![HashMap::new()];
    scope.put_var_raw(String::from(name), Type::Unknown);
    for arg in func.args.iter()
    {
        vars.last_mut().unwrap().insert(arg.0.clone(), arg.1.clone());
    }

    // Get the type
    let _type = get_function_type(&func.body, scope, funcs, &mut vars, errors);

    // Push an error if type is unknown
    if _type == Type::Unknown
    {
        errors.push(CorrectnessError::UnknownFunctionReturnType(
            func.body.get_metadata().span.clone(),
            String::from(name)
        ));
    } else
    {
        // Construct the type
        let mut acc = _type;

        for t in func.args.iter().rev()
        {
            acc = Type::Func(Box::new(t.1.clone()), Box::new(acc));
        }

        // Put function type in global scope
        scope.put_var_raw(String::from(name), acc);
    }
}

// check_function_group(T, &HashMap<String, IRFunction>, &mut IR, &mut Vec<CorrectnessError>) -> ()
// Checks the group of functions for return types.
fn check_function_group<T>(names: T, ir: &mut IR, errors: &mut Vec<CorrectnessError>)
    where T: Iterator<Item = String> + Clone
{
    // Generate function types
    for name in names.clone()
    {
        let func = ir.funcs.get(&name).unwrap();
        check_function_body(&name, func, &mut ir.metadata.scope, &ir.funcs, errors);
    }

    // Check all function bodies
    for name in names
    {
        // Remove function
        let mut func = ir.funcs.remove(&name).unwrap();

        // Push scope and add arguments
        ir.metadata.push_scope();
        for arg in &func.args
        {
            ir.metadata.scope.put_var(&arg.0, &arg.1);
        }

        // Check body
        check_sexpr(&mut func.body, ir, errors);

        // Pop scope
        ir.metadata.pop_scope();

        // Reinsert function
        ir.funcs.insert(name, func);
    }

}

// check_functions(&mut IR, &mut Vec<CorrectnessError>) -> ()
// Checks function return types.
fn check_functions(ir: &mut IR, errors: &mut Vec<CorrectnessError>)
{
    // Get the set of all global functions
    use std::iter::FromIterator;
    let globals = HashSet::from_iter(ir.funcs.iter().filter_map(|v| {
        if v.1.global
        {
            Some(v.0.clone())
        } else
        {
            None
        }
    }));

    // Iterate over every function
    for func in ir.funcs.iter_mut()
    {
        convert_function_symbols(&mut func.1.body, &globals);
    }

    // Check all global functions
    let v: Vec<String> = ir.funcs.iter().filter_map(
        |v| match v.1.global
        {
            true => Some(v.0.clone()),
            false => None
        }
    ).collect();
    check_function_group(v.into_iter(), ir, errors);
}

// check_correctness(&mut IR) -> ()
// Checks the correctness of ir.
pub fn check_correctness(ir: &mut IR) -> Result<(), Vec<CorrectnessError>>
{
    let mut errors = Vec::with_capacity(0);

    // Check functions
    check_functions(ir, &mut errors);

    // Check sexpressions
    let mut sexprs = Vec::with_capacity(0);
    swap(&mut ir.sexprs, &mut sexprs);
    for sexpr in sexprs.iter_mut()
    {
        check_sexpr(sexpr, ir, &mut errors);
    }
    swap(&mut ir.sexprs, &mut sexprs);

    // Return error if they exist, otherwise return success
    if errors.len() == 0
    {
        Ok(())
    } else
    {
        Err(errors)
    }
}

