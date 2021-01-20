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
    NonboolInBoolExpr(Span, Type),
    NonboolInIfCond(Span, Type),
    NonmatchingIfBodies(Span, Type, Span, Type),
    NonmatchingAssignTypes(Span, Type, Span, Type),
    SymbolNotFound(Span, String),
    Reassignment(Span, Span, String),
    InvalidType(Span),
    UnknownFunctionReturnType(Span, String),
    MismatchedFunctionArgType(Span, Type, Type),
    InvalidApplication(Span, Type)
}

// check_sexpr(&mut SExpr, &mut SExprMetadata, &mut Vec<CorrectnessError>) -> ()
// Checks an s expression for type correctness and correct symbol usage.
fn check_sexpr(sexpr: &mut SExpr, root: &mut IR, errors: &mut Vec<CorrectnessError>)
{
    if let Type::ConversionError(s) = &sexpr.get_metadata()._type
    {
        errors.push(CorrectnessError::InvalidType(
            s.clone()
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

        // Lists
        SExpr::List(_, _) => panic!("uwu"),

        // Functions
        SExpr::Function(m, f) => {
            match root.scope.get_var(f)
            {
                Some(t) => {
                    m._type = t.0.clone();
                    m.arity = t.1;
                    m.saved_argc = t.2;

                    if m._type == Type::Unknown
                    {
                        root.funcs.remove(f);
                    }
                }

                None => errors.push(CorrectnessError::SymbolNotFound(
                    m.span.clone(),
                    f.clone()
                ))
            }
        }

        // Symbols
        SExpr::Symbol(m, s) => {
            match root.scope.get_var(s)
            {
                Some(t) => {
                    m._type = t.0.clone();
                    m.arity = t.1;
                    m.saved_argc = t.2;
                }

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
                    if let Some(t) = root.scope.get_func_ret(FunctionName::Prefix(v.get_metadata()._type.clone()))
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
            if let Some(t) = root.scope.get_func_ret(FunctionName::Infix(
                *op,
                left.get_metadata()._type.clone(),
                right.get_metadata()._type.clone()
            ))
            {
                m._type = t.clone();
            } else
            {
                errors.push(CorrectnessError::UndefinedInfixOp(
                    m.span.clone(),
                    *op,
                    left.get_metadata()._type.clone(),
                    right.get_metadata()._type.clone()
                ));
            }
        }

        // And and or
        SExpr::And(m, left, right) | SExpr::Or(m, left, right) => {
            // Check child nodes
            check_sexpr(left, root, errors);
            check_sexpr(right, root, errors);

            // Check if an error occured
            if left.get_metadata()._type == Type::Error || right.get_metadata()._type == Type::Error
            {
                return;
            }

            // Assert the types are booleans.
            if left.get_metadata()._type != Type::Bool
            {
                errors.push(CorrectnessError::NonboolInBoolExpr(
                    left.get_metadata().span.clone(),
                    left.get_metadata()._type.clone(),
                ));
            }
            if right.get_metadata()._type != Type::Bool
            {
                errors.push(CorrectnessError::NonboolInBoolExpr(
                    right.get_metadata().span.clone(),
                    right.get_metadata()._type.clone(),
                ));
            }
            if left.get_metadata()._type == Type::Bool && right.get_metadata()._type == Type::Bool
            {
                m._type = Type::Bool;
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
                if then.get_metadata().saved_argc == elsy.get_metadata().saved_argc
                {
                    m.saved_argc = then.get_metadata().saved_argc;
                }
                if then.get_metadata().arity == elsy.get_metadata().arity
                {
                    m.arity = then.get_metadata().arity;
                }
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

            // Quick hack for debug functions
            // TODO: Generic types so this isn't implemented as a hack
            if let SExpr::Symbol(_, v) = &**func
            {
                if v == "debug"
                {
                    m._type = arg.get_metadata()._type.clone();
                    return;
                }
            }

            // Match the function
            match &func.get_metadata()._type
            {
                // Strings concatenate to other strings
                // Type::String => {
                //    m._type = Type::String;
                // }

                // Functions apply their arguments
                Type::Func(l, r) => {
                    if **l == arg.get_metadata()._type
                    {
                        m._type = *r.clone();
                        m.arity = if func.get_metadata().arity > 0
                        {
                            func.get_metadata().arity - 1
                        } else
                        {
                            0
                        };

                        if let Some(v) = func.get_metadata().saved_argc
                        {
                            m.saved_argc = Some(v + 1);
                        }
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
            if let Some(v) = root.scope.variables.get(name)
            {
                if value.get_metadata()._type == Type::Unknown
                {
                    root.scope.variables.remove(name);
                    return;
                }

                if v.4
                {
                    errors.push(CorrectnessError::Reassignment(
                        m.span.clone(),
                        v.3.clone(),
                        name.clone()
                    ));
                    if let SExpr::Function(_, v) = &**value
                    {
                        if v != name
                        {
                            root.scope.variables.remove(v);
                        }
                        root.funcs.remove(v);
                    }
                    return;
                }
            }

            // Check child node
            check_sexpr(value, root, errors);

            // Check if variable value is unknown type
            if value.get_metadata()._type == Type::Unknown
            {
                root.scope.variables.remove(name);
                return;
            }


            // Check if an error occured or the type is unknown
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
                            m.span2.clone(),
                            m._type.clone(),
                            value.get_metadata().span.clone(),
                            value.get_metadata()._type.clone()
                        ));
                        m._type = Type::Error;
                    }
                }
            }

            // Add variable to scope if no error occured
            if m._type != Type::Error
            {
                root.scope.put_var(name, &m._type, value.get_metadata().arity, value.get_metadata().saved_argc, Span { start: m.span.start, end: value.get_metadata().span.start }, true);
            }
        }

        // With expressions
        SExpr::With(m, assigns, body) => {
            // Push a new scope
            root.scope.push_scope(false);

            // Function iterator
            let iter = assigns.iter().filter_map(
                |a| if let SExpr::Assign(_, s, a) = &a
                {
                    if let SExpr::Function(_, v) = &**a
                    {
                        Some((v.clone(), s.clone()))
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
            m.arity = body.get_metadata().arity;

            // Pop scope
            root.scope.pop_scope();
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
                    span2: Span {
                        start: 0,
                        end: 0
                    },
                    _type: Type::Error,
                    arity: 0,
                    saved_argc: None
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

// get_function_type(&SExpr, &mut Scope, &HashMap<String, IRFunction>, &mut Vec<CorrectnessError>, &mut Vec<(String, Type)>) -> Type
// Gets the function type, returning Type::Unknown if a type cannot be found.
fn get_function_type(sexpr: &SExpr, scope: &mut Scope, funcs: &mut HashMap<String, IRFunction>, errors: &mut Vec<CorrectnessError>, captured: &mut HashMap<String, Type>, captured_names: &mut Vec<String>) -> Type
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

        // Lists
        SExpr::List(_, _) => panic!("uwu"),

        // Functions
        SExpr::Function(_, f) => {
            // Check scope
            match scope.get_var(f)
            {
                Some(v) => v.0.clone(),

                // Check child function
                None => {
                    let mut func = funcs.remove(f).unwrap();
                    check_function_body(f, f, &mut func, scope, funcs, errors);
                    funcs.insert(f.clone(), func);
                    scope.get_var(f).unwrap().0.clone()
                }
            }
        }

        // Symbols
        SExpr::Symbol(_, s) => {
            // Check global scope
            match scope.get_var(s)
            {
                Some(v) => {
                    // Check if captured
                    if scope.is_captured(s) && !captured.contains_key(s)
                    {
                        captured.insert(s.clone(), v.0.clone());
                        captured_names.push(s.clone());
                    }
                    v.0.clone()
                }

                // Check local scopes
                None => Type::Unknown
            }
        }

        // Prefix operators
        SExpr::Prefix(_, op, v) => {
            match op
            {
                PrefixOp::Neg => {
                    let vt = get_function_type(v, scope, funcs, errors, captured, captured_names).clone();

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
            let lt = get_function_type(l, scope, funcs, errors, captured, captured_names).clone();
            let rt = get_function_type(r, scope, funcs, errors, captured, captured_names).clone();

            match scope.get_func_ret(FunctionName::Infix(*op, lt, rt))
            {
                Some(v) => v.clone(),
                None => Type::Unknown
            }
        }

        // Boolean and/or
        SExpr::And(_, l, r) | SExpr::Or(_, l, r) => {
            get_function_type(l, scope, funcs, errors, captured, captured_names);
            get_function_type(r, scope, funcs, errors, captured, captured_names);
            Type::Bool
        }

        // If expressions
        SExpr::If(_, _, body, elsy) => {
            let bt = get_function_type(body, scope, funcs, errors, captured, captured_names);
            if bt != Type::Unknown
            {
                bt
            } else
            {
                get_function_type(elsy, scope, funcs, errors, captured, captured_names)
            }
        }

        // Applications
        SExpr::Application(_, f, a) => {
            // Get function type
            let ft = get_function_type(f, scope, funcs, errors, captured, captured_names);
            if let Type::Unknown = ft
            {
                return Type::Unknown;
            }

            // Get argument type
            let at = get_function_type(a, scope, funcs, errors, captured, captured_names);
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
            let t = get_function_type(value, scope, funcs, errors, captured, captured_names);
            scope.put_var(&name, &t, 0, None, Span { start: 0, end: 0 }, true);
            t
        }

        // With expressions
        SExpr::With(_, assigns, body) => {
            // Push scope
            scope.push_scope(false);

            // Populate scope with variable types
            for a in assigns
            {
                get_function_type(a, scope, funcs, errors, captured, captured_names);
            }

            // Get the function type
            let bt = get_function_type(body, scope, funcs, errors, captured, captured_names);

            // Pop the scope and return type
            scope.pop_scope();
            bt
        }
    }
}

// check_function_body(&str, &str, &IRFunction, &mut Scope, &HashMap<String, IRFunction>, &mut Vec<CorrectnessError>) -> Vec<(String, Type)>
// Checks a function body and determines the return type of the function.
fn check_function_body(name: &str, refr: &str, func: &mut IRFunction, scope: &mut Scope, funcs: &mut HashMap<String, IRFunction>, errors: &mut Vec<CorrectnessError>)
{
    if let None = scope.variables.get(name)
    {
        // Put function in scope
        scope.put_var_raw(String::from(name), Type::Unknown, func.args.len(), None, Span { start: 0, end: 0 }, false);
        if name != refr
        {
            scope.put_var_raw(String::from(refr), Type::Unknown, func.args.len(), None, Span { start: 0, end: 0 }, false);
        }

        // Put arguments into scope
        scope.push_scope(true);
        for arg in func.args.iter()
        {
            scope.put_var(&arg.0, &arg.1, 0, None, Span { start: 0, end: 0 }, true);
        }

        // Get the type
        let mut captured = HashMap::with_capacity(0);
        let mut captured_names = Vec::with_capacity(0);
        let _type = get_function_type(&func.body, scope, funcs, errors, &mut captured, &mut captured_names);
        func.captured = captured;
        func.captured_names = captured_names;
        scope.pop_scope();

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
            if name != refr
            {
                scope.put_var_raw(String::from(refr), acc.clone(), func.args.len(), Some(func.captured.len()), func.span.clone(), false);
            }
            scope.put_var_raw(String::from(name), acc, func.args.len(), Some(func.captured.len()), func.span.clone(), false);
        }
    }
}

// check_function_group(T, &HashMap<String, IRFunction>, &mut IR, &mut Vec<CorrectnessError>) -> ()
// Checks the group of functions for return types.
fn check_function_group<T>(names: T, ir: &mut IR, errors: &mut Vec<CorrectnessError>)
    where T: Iterator<Item = (String, String)> + Clone
{
    // Generate function types
    for name in names.clone()
    {
        let mut func = ir.funcs.remove(&name.0).unwrap();
        check_function_body(&name.0, &name.1, &mut func, &mut ir.scope, &mut ir.funcs, errors);
        ir.funcs.insert(name.0, func);
    }

    // Check all function bodies
    for name in names
    {
        // Remove function
        let mut func = ir.funcs.remove(&name.0).unwrap();

        // Push scope and add arguments
        ir.scope.push_scope(false);
        for arg in &func.args
        {
            ir.scope.put_var(&arg.0, &arg.1, 0, None, Span { start: 0, end: 0 }, true);
        }

        // Check body
        check_sexpr(&mut func.body, ir, errors);

        // Pop scope
        ir.scope.pop_scope();

        // Reinsert function
        ir.funcs.insert(name.0, func);
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
    let v: Vec<(String, String)> = ir.funcs.iter().filter_map(
        |v| match v.1.global
        {
            true => Some((v.0.clone(), v.0.clone())),
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

