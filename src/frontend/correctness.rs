use logos::Span;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use std::mem::swap;

use super::ir::{BinOp, IR, IRFunction, PrefixOp, SExpr, SExprMetadata};
use super::scopes::{FunctionName, Scope};
use super::types::{HashSetWrapper, Type};

#[derive(Debug)]
pub enum CorrectnessError
{
    UndefinedPrefixOp(Span, PrefixOp, Type),
    UndefinedInfixOp(Span, BinOp, Type, Type),
    NonboolInBoolExpr(Span, Type),
    NonboolInIfCond(Span, Type),
    NonmatchingAssignTypes(Span, Type, Span, Type),
    SymbolNotFound(Span, String),
    Reassignment(Span, Span, String),
    InvalidType(Span),
    DuplicateTypeInUnion(Span, Span, Type),
    UnknownFunctionReturnType(Span, String),
    MismatchedFunctionArgType(Span, Type, Type),
    InvalidApplication(Span, Type),
    InvalidCast(Span, Type, Span, Type),
    NonSubtypeOnMatch(Span, Type, Span, Type),
    InfiniteSizedType(Span, Type),
    NonmemberAccess(Span, String, String)
}

// check_sexpr(&mut SExpr, &mut SExprMetadata, &mut Vec<CorrectnessError>) -> ()
// Checks an s expression for type correctness and correct symbol usage.
fn check_sexpr(sexpr: &mut SExpr, root: &mut IR, errors: &mut Vec<CorrectnessError>)
{
    if let Type::UndeclaredTypeError(s) = &sexpr.get_metadata()._type
    {
        errors.push(CorrectnessError::InvalidType(
            s.clone()
        ));
        sexpr.get_mutable_metadata()._type = Type::Error;
        return;
    } else if let Type::DuplicateTypeError(s1, s2, t) = &sexpr.get_metadata()._type
    {
        errors.push(CorrectnessError::DuplicateTypeInUnion(
            s1.clone(),
            s2.clone(),
            *t.clone()
        ));
        sexpr.get_mutable_metadata()._type = Type::Error;
        return;
    }

    match sexpr
    {
        // Type alias
        SExpr::TypeAlias(_, _) => (),

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

                    if root.funcs.get(f).is_some() && !root.funcs.get(f).unwrap().checked
                    {
                        let mut func = root.funcs.remove(f).unwrap();
                        root.scope.push_scope(true);
                        for a in func.args.iter()
                        {
                            root.scope.put_var(&a.0, &a.1, 0, None, Span { start: 0, end: 0 }, true);
                        }
                        check_sexpr(&mut func.body, root, errors);
                        root.scope.pop_scope();
                        root.funcs.insert(f.clone(), func);
                    }

                    if m._type == Type::Unknown
                    {
                        root.funcs.remove(f);
                    }
                }

                None => {
                    let mut func = root.funcs.remove(f).unwrap();
                    check_function_body(f, f, &mut func, &mut root.scope, &mut root.funcs, errors, &root.types);

                    root.scope.push_scope(true);
                    for a in func.args.iter()
                    {
                        root.scope.put_var(&a.0, &a.1, 0, None, Span { start: 0, end: 0 }, true);
                    }
                    check_sexpr(&mut func.body, root, errors);
                    root.scope.pop_scope();

                    m._type = root.scope.get_var(f).unwrap().0.clone();
                    m.arity = func.args.len();
                    m.saved_argc = Some(func.captured.len());
                    root.funcs.insert(f.clone(), func);
                }
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

        SExpr::As(m, v) => {
            // Check child node
            check_sexpr(v, root, errors);

            // Check if an error occured
            if v.get_metadata()._type == Type::Error
            {
                return;
            }

            // Assert the types are valid
            if !v.get_metadata()._type.is_subtype(&m._type, &root.types)
            {
                errors.push(CorrectnessError::InvalidCast(
                    v.get_metadata().span.clone(),
                    v.get_metadata()._type.clone(),
                    m.span2.clone(),
                    m._type.clone()
                ));
                m._type = Type::Error;
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
                let mut set = Vec::with_capacity(0);
                if let Type::Sum(v) = &then.get_metadata()._type
                {
                    set = v.0.iter().collect();
                } else
                {
                    set.push(&then.get_metadata()._type);
                }

                if let Type::Sum(v) = &elsy.get_metadata()._type
                {
                    for v in v.0.iter()
                    {
                        set.push(v);
                    }
                } else
                {
                    set.push(&elsy.get_metadata()._type);
                }

                m._type = Type::Sum(HashSetWrapper(HashSet::from_iter(set.into_iter().cloned())));
            } else
            {
                m._type = then.get_metadata()._type.clone();
            }

            if then.get_metadata().saved_argc == elsy.get_metadata().saved_argc
            {
                m.saved_argc = then.get_metadata().saved_argc;
            }

            if then.get_metadata().arity == elsy.get_metadata().arity
            {
                m.arity = then.get_metadata().arity;
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

            let mut _type = &func.get_metadata()._type;
            while let Type::Symbol(s) = _type
            {
                _type = root.types.get(s).unwrap();
            }

            // Match the function
            match _type
            {
                // Strings concatenate to other strings
                // Type::String => {
                //    m._type = Type::String;
                // }

                // Functions apply their arguments
                Type::Func(l, r) => {
                    if arg.get_metadata()._type.is_subtype(&**l, &root.types)
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
                            *l.clone(),
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
                    if !value.get_metadata()._type.is_subtype(&m._type, &root.types)
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

        SExpr::Match(m, value, arms) => {
            // Check value
            check_sexpr(value, root, errors);
            let _type = &value.get_metadata()._type;
            if *_type == Type::Error
            {
                return;
            } else if let Type::UndeclaredTypeError(_) = *_type
            {
                return;
            }

            // Check match arms
            root.scope.push_scope(false);
            let mut set = Vec::with_capacity(0);
            for arm in arms.iter_mut()
            {
                // Get name of symbol
                let (name, atype) = if let Type::Tag(s, t) = &arm.0
                {
                    root.scope.put_var(s, t, 0, None, arm.1.get_metadata().span2.clone(), true);
                    (Some(s), &**t)
                } else
                {
                    (None, &arm.0)
                };

                if let Type::UndeclaredTypeError(s) = &arm.0
                {
                    errors.push(CorrectnessError::InvalidType(
                        s.clone()
                    ));
                    root.scope.pop_scope();
                    continue;
                }

                // Nonsubtypes are errors
                if !atype.is_subtype(&_type, &root.types)
                {
                    errors.push(CorrectnessError::NonSubtypeOnMatch(
                        value.get_metadata().span.clone(),
                        _type.clone(),
                        arm.1.get_metadata().span2.clone(),
                        arm.0.clone()
                    ));
                    root.scope.pop_scope();
                    return;
                }

                // Check body of match arm
                check_sexpr(&mut arm.1, root, errors);

                // Remove variable
                if let Some(s) = name
                {
                    root.scope.variables.remove(s);
                }

                // Check for error
                if arm.1.get_metadata()._type == Type::Error
                {
                    set.push(Type::Error);
                    continue;
                }

                // Unwrap symbol
                let mut _type = arm.1.get_metadata()._type.clone();
                while let Type::Symbol(s) = _type
                {
                    _type = root.types.get(&s).unwrap().clone();
                }

                // Add types
                if let Type::Sum(v) = _type
                {
                    for v in v.0
                    {
                        set.push(v);
                    }
                } else
                {
                    set.push(_type);
                }
            }

            root.scope.pop_scope();
            let set = HashSet::from_iter(set.into_iter());
            m._type = if set.len() == 1
            {
                set.into_iter().next().unwrap()
            } else if set.contains(&Type::Error)
            {
                Type::Error
            } else
            {
                Type::Sum(HashSetWrapper(set))
            };
        }

        SExpr::MemberAccess(m, a) => {
            if let Some(t) = root.types.get(&a[0])
            {
                if let Type::Sum(f) = t
                {
                    if f.0.contains(&Type::Enum(a[1].clone()))
                    {
                        if a.len() != 2
                        {
                            errors.push(CorrectnessError::NonmemberAccess(
                                m.span.clone(),
                                format!("{}::{}", a[0], a[1]),
                                a[2].clone()
                            ));
                        } else
                        {
                            m._type = Type::Symbol(a[0].clone());
                        }
                    } else if let Some(v) = f.0.iter().filter(|v| if let Type::Tag(t, _) = v { t == &a[1] } else { false }).next()
                    {
                        if a.len() != 2
                        {
                            errors.push(CorrectnessError::NonmemberAccess(
                                m.span.clone(),
                                format!("{}::{}", a[0], a[1]),
                                a[2].clone()
                            ));
                        } else if let Type::Tag(_, t) = v
                        {
                            let mut temp = SExprMetadata {
                                span: Span { start: 0, end: 0 },
                                span2: Span { start: 0, end: 0 },
                                _type: Type::Error,
                                arity: 0,
                                saved_argc: None,
                                tailrec: false
                            };

                            swap(&mut temp, m);
                            temp.arity = 1;
                            temp.saved_argc = Some(0);
                            temp._type = Type::Func(t.clone(), Box::new(Type::Symbol(a[0].clone())));

                            *sexpr = SExpr::Function(temp, a.join("::").to_string());
                        }
                    } else
                    {
                        errors.push(CorrectnessError::NonmemberAccess(
                            m.span.clone(),
                            a[0].clone(),
                            a[1].clone()
                        ));
                    }
                } else
                {
                    errors.push(CorrectnessError::NonmemberAccess(
                        m.span.clone(),
                        a[0].clone(),
                        a[1].clone()
                    ));
                }
            } else
            {
                errors.push(CorrectnessError::SymbolNotFound(
                    m.span.clone(),
                    a[0].clone(),
                ));
            }
        }
    }
}

// convert_function_symbols(&mut SExpr, &mut HashSet<String>) -> ()
// Converts function symbols in a sexpression into function references.
fn convert_function_symbols(sexpr: &mut SExpr, funcs: &mut HashSet<String>)
{
    match sexpr
    {
        // Check symbol for functions
        SExpr::Symbol(m, s) => {
            if funcs.contains(s)
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
                    saved_argc: None,
                    tailrec: false
                };

                swap(&mut meta, m);
                let mut id = String::with_capacity(0);
                swap(&mut id, s);
                *sexpr = SExpr::Function(meta, id);
            }
        }

        // Prefix operators
        SExpr::Prefix(_, _, v) => {
            convert_function_symbols(v, funcs);
        }

        // Infix operators
        SExpr::Infix(_, _, l, r)
            | SExpr::And(_, l, r)
            | SExpr::Or(_, l, r)
            | SExpr::Application(_, l, r)
            => {
            convert_function_symbols(l, funcs);
            convert_function_symbols(r, funcs);
        }

        // Casting
        SExpr::As(_, v) => convert_function_symbols(v, funcs),

        // If expressions
        SExpr::If(_, c, b, e) => {
            convert_function_symbols(c, funcs);
            convert_function_symbols(b, funcs);
            convert_function_symbols(e, funcs);
        }

        // Check scope
        SExpr::With(_, assigns, body) => {
            // Save removed funcs
            let mut removed = HashSet::with_capacity(0);

            // Check assigns
            for a in assigns.iter_mut()
            {
                convert_function_symbols(a, funcs);
            }

            // Remove assignments from function set
            for a in assigns
            {
                if let SExpr::Assign(_, a, _) = a
                {
                    if funcs.remove(a)
                    {
                        removed.insert(a.clone());
                    }
                }
            }

            // Check body
            convert_function_symbols(body, funcs);
            *funcs = HashSet::from_iter(funcs.union(&removed).cloned());
        }

        // Check assignments
        SExpr::Assign(_, _, value) => {
            convert_function_symbols(value, funcs);
        }

        SExpr::Match(_, v, a) => {
            convert_function_symbols(v, funcs);
            for a in a.iter_mut()
            {
                convert_function_symbols(&mut a.1, funcs);
            }
        }

        // Ignore everything else
        _ => ()
    }
}

// get_function_type(&SExpr, &mut Scope, &HashMap<String, IRFunction>, &mut Vec<CorrectnessError>, &mut Vec<(String, Type)>, &HashMap<String, Type>) -> Type
// Gets the function type, returning Type::Unknown if a type cannot be found.
fn get_function_type(sexpr: &SExpr, scope: &mut Scope, funcs: &mut HashMap<String, IRFunction>, errors: &mut Vec<CorrectnessError>, captured: &mut HashMap<String, Type>, captured_names: &mut Vec<String>, types: &HashMap<String, Type>) -> Type
{
    match sexpr
    {
        // Values
        SExpr::TypeAlias(m, _)
            | SExpr::Int(m, _)
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
                    check_function_body(f, f, &mut func, scope, funcs, errors, types);
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
                    if scope.is_captured(s) && !captured.contains_key(s) && s != "debug" && s != "putch"
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
                    let vt = get_function_type(v, scope, funcs, errors, captured, captured_names, types).clone();

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
            let lt = get_function_type(l, scope, funcs, errors, captured, captured_names, types).clone();
            let rt = get_function_type(r, scope, funcs, errors, captured, captured_names, types).clone();

            match scope.get_func_ret(FunctionName::Infix(*op, lt, rt))
            {
                Some(v) => v.clone(),
                None => Type::Unknown
            }
        }

        SExpr::As(m, _) => m._type.clone(),

        // Boolean and/or
        SExpr::And(_, l, r) | SExpr::Or(_, l, r) => {
            get_function_type(l, scope, funcs, errors, captured, captured_names, types);
            get_function_type(r, scope, funcs, errors, captured, captured_names, types);
            Type::Bool
        }

        // If expressions
        SExpr::If(_, _, body, elsy) => {
            let bt = get_function_type(body, scope, funcs, errors, captured, captured_names, types);
            let et = get_function_type(elsy, scope, funcs, errors, captured, captured_names, types);

            if bt == Type::Unknown && et == Type::Unknown
            {
                Type::Unknown
            } else if bt == et || et == Type::Unknown
            {
                bt
            } else if bt == Type::Unknown
            {
                et
            } else
            {
                let mut set = Vec::with_capacity(0);
                if let Type::Sum(v) = &body.get_metadata()._type
                {
                    set = v.0.iter().collect();
                } else
                {
                    set.push(&body.get_metadata()._type);
                }

                if let Type::Sum(v) = &elsy.get_metadata()._type
                {
                    for v in v.0.iter()
                    {
                        set.push(v);
                    }
                } else
                {
                    set.push(&elsy.get_metadata()._type);
                }

                Type::Sum(HashSetWrapper(HashSet::from_iter(set.into_iter().cloned())))
            }
        }

        // Applications
        SExpr::Application(_, f, a) => {
            // Quick hack for debug function
            if let SExpr::Symbol(_, s) = &**f
            {
                if s == "debug"
                {
                    return get_function_type(a, scope, funcs, errors, captured, captured_names, types);
                }
            }

            // Get function type
            let mut ft = get_function_type(f, scope, funcs, errors, captured, captured_names, types);
            if let Type::Unknown = ft
            {
                return Type::Unknown;
            }

            while let Type::Symbol(s) = ft
            {
                ft = types.get(&s).unwrap().clone();
            }

            match ft
            {
                // Strings concatenate to other strings
                // Type::String => Type::String,

                // Functions apply their arguments
                Type::Func(_, r) => {
                    *r.clone()
                }

                // Everything else is invalid
                _ => Type::Unknown
            }
        }

        // Assignments
        SExpr::Assign(m, name, value) => {
            let t = if m._type != Type::Error
            {
                m._type.clone()
            } else
            {
                get_function_type(value, scope, funcs, errors, captured, captured_names, types)
            };
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
                get_function_type(a, scope, funcs, errors, captured, captured_names, types);
            }

            // Get the function type
            let bt = get_function_type(body, scope, funcs, errors, captured, captured_names, types);

            // Pop the scope and return type
            scope.pop_scope();
            bt
        }

        SExpr::Match(_, _, arms) => {
            // Check match arms
            scope.push_scope(false);
            let mut set = Vec::with_capacity(0);
            for arm in arms.iter()
            {
                // Check body of match arm
                let name = if let Type::Tag(s, t) = &arm.0
                {
                    scope.put_var(s, t, 0, None, arm.1.get_metadata().span2.clone(), true);
                    Some(s)
                } else
                {
                    None
                };

                let mut _type = get_function_type(&arm.1, scope, funcs, errors, captured, captured_names, types);

                if let Some(s) = name
                {
                    scope.variables.remove(s);
                }

                if _type == Type::Unknown
                {
                    continue;
                }

                while let Type::Symbol(s) = _type
                {
                    _type = types.get(&s).unwrap().clone();
                }

                if let Type::Sum(v) = _type
                {
                    set.extend(v.0.into_iter());
                } else
                {
                    set.push(_type);
                }
            }

            scope.pop_scope();

            let set = HashSet::from_iter(set.into_iter());
            if set.len() == 0
            {
                Type::Unknown
            } else if set.len() == 1
            {
                set.into_iter().next().unwrap()
            } else
            {
                Type::Sum(HashSetWrapper(set))
            }
        }

        SExpr::MemberAccess(_, a) => {
            if let Some(t) = types.get(&a[0])
            {
                if let Type::Sum(f) = t
                {
                    if f.0.contains(&Type::Enum(a[1].clone()))
                    {
                        if a.len() != 2
                        {
                            Type::Unknown
                        } else
                        {
                            t.clone()
                        }
                    } else
                    {
                        Type::Unknown
                    }
                } else
                {
                    Type::Unknown
                }
            } else
            {
                Type::Unknown
            }
        }
    }
}

// check_function_body(&str, &str, &IRFunction, &mut Scope, &HashMap<String, IRFunction>, &mut Vec<CorrectnessError>) -> Vec<(String, Type)>
// Checks a function body and determines the return type of the function.
fn check_function_body(name: &str, refr: &str, func: &mut IRFunction, scope: &mut Scope, funcs: &mut HashMap<String, IRFunction>, errors: &mut Vec<CorrectnessError>, types: &HashMap<String, Type>)
{
    if let None = scope.get_var(name)
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
        let _type = get_function_type(&func.body, scope, funcs, errors, &mut captured, &mut captured_names, types);
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

            scope.variables.remove(refr);
            scope.variables.remove(name);

            // Get global scope
            let mut scope = scope;
            loop
            {
                if scope.parent.is_some()
                {
                    scope = &mut *scope.parent.as_mut().unwrap();
                } else
                {
                    break;
                }
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
        check_function_body(&name.0, &name.1, &mut func, &mut ir.scope, &mut ir.funcs, errors, &ir.types);
        ir.funcs.insert(name.0, func);
    }

    // Check all function bodies
    for name in names
    {
        // Remove function
        if let Some(mut func) = ir.funcs.remove(&name.0)
        {
            // Push scope and add arguments
            ir.scope.push_scope(false);
            for arg in &func.args
            {
                ir.scope.put_var(&arg.0, &arg.1, 0, None, Span { start: 0, end: 0 }, true);
            }

            // Check body
            check_sexpr(&mut func.body, ir, errors);
            func.checked = true;

            // Pop scope
            ir.scope.pop_scope();

            // Reinsert function
            ir.funcs.insert(name.0, func);
        }
    }
}

// check_functions(&mut IR, &mut Vec<CorrectnessError>) -> ()
// Checks function return types.
fn check_functions(ir: &mut IR, errors: &mut Vec<CorrectnessError>)
{
    // Get the set of all global functions
    let mut globals = HashSet::from_iter(ir.funcs.iter().filter_map(|v| {
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
        let mut removed = HashSet::with_capacity(0);
        for a in func.1.args.iter()
        {
            if globals.remove(&a.0)
            {
                removed.insert(a.0.clone());
            }
        }

        convert_function_symbols(&mut func.1.body, &mut globals);

        globals = HashSet::from_iter(globals.union(&removed).cloned());

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

// save_single_type(&mut usize, &Type, &mut HashMap) -> ()
// Saves one type.
fn save_single_type(id: &mut usize, _type: &Type, types: &mut HashMap<String, Type>)
{
    for t in types.iter()
    {
        if _type == t.1
        {
            return;
        }
    }

    types.insert(format!("{}", id), _type.clone());
    *id += 1;
}

// save_types(&mut IR) -> ()
// Saves anonymous types.
fn save_types(sexpr: &SExpr, types: &mut HashMap<String, Type>, id: &mut usize)
{
    let m = sexpr.get_metadata();
    if let Type::Sum(_) = &m._type
    {
        save_single_type(id, &m._type, types);
    }

    match sexpr
    {
        SExpr::Prefix(_, _, v) => {
            save_types(v, types, id);
        }

        SExpr::Infix(_, _, l, r)
            | SExpr::And(_, l, r)
            | SExpr::Or(_, l, r)
            | SExpr::Application(_, l, r)
            => {
            save_types(l, types, id);
            save_types(r, types, id);
        }

        SExpr::If(_, c, t, e) => {
            save_types(c, types, id);
            save_types(t, types, id);
            save_types(e, types, id);
        }

        SExpr::As(_, v) => {
            save_types(v, types, id);
        }

        SExpr::Assign(_, _, v) => {
            save_types(v, types, id);
        }

        SExpr::With(_, a, v) => {
            for a in a.iter()
            {
                save_types(a, types, id);
            }
            save_types(v, types, id);
        }

        SExpr::Match(_, v, a) => {
            save_types(v, types, id);
            for a in a
            {
                let _type = if let Type::Tag(_, t) = &a.0
                {
                    t
                } else
                {
                    &a.0
                };

                if let Type::Sum(_) = _type
                {
                    save_single_type(id, _type, types);
                }
                save_types(&a.1, types, id);
            }
        }

        _ => ()
    }
}

// check_type_validity(&IR) -> ()
// Checks whether the given types are valid or not.
fn check_type_validity(ir: &IR, errors: &mut Vec<CorrectnessError>)
{
    for s in ir.sexprs.iter()
    {
        if let SExpr::TypeAlias(m, n) = s
        {
            match &m._type
            {
                Type::Symbol(s) if s == n => {
                    errors.push(CorrectnessError::InfiniteSizedType(
                        m.span2.clone(),
                        m._type.clone()
                    ));
                }

                Type::Sum(s) if s.0.contains(&Type::Symbol(n.clone())) => {
                    errors.push(CorrectnessError::InfiniteSizedType(
                        m.span2.clone(),
                        m._type.clone()
                    ));
                }

                _ => ()
            }
        }
    }
}

// is_called(&mut SExpr, &str) -> bool
// Checks whether a given function is called or not.
fn is_called(sexpr: &mut SExpr, name: &str) -> bool
{
    match sexpr
    {
        //List(SExprMetadata, Vec<SExpr>),

        SExpr::Function(_, f) => {
            f == name
        }

        SExpr::Prefix(_, _, v)
            | SExpr::As(_, v)
            | SExpr::Assign(_, _, v)
            => {
            is_called(v, name)
        }

        SExpr::Infix(_, _, l, r)
            | SExpr::Application(_, l, r)
            | SExpr::And(_, l, r)
            | SExpr::Or(_, l, r)
            => {
            is_called(l, name) || is_called(r, name)
        }

        SExpr::If(_, c, t, e) => {
            is_called(c, name) || is_called(t, name) || is_called(e, name)
        }

        SExpr::With(_, a, v) => {
            for a in a
            {
                if is_called(a, name)
                {
                    return true;
                }
            }
            is_called(v, name)
        }

        SExpr::Match(_, v, a) => {
            if is_called(v, name)
            {
                return true;
            }

            for a in a
            {
                if is_called(&mut a.1, name)
                {
                    return true;
                }
            }

            false
        }

        _ => false
    }
}

// check_tailrec(&mut SExpr, &str) -> bool
// Checks whether a given function is tail recursive. Returns false if the function is called
// outside of a tail call.
fn check_tailrec(sexpr: &mut SExpr, name: &str, top: bool) -> bool
{
    match sexpr
    {
        SExpr::Function(m, f) => {
            if f == name
            {
                m.tailrec = true;
            }

            true
        }

        SExpr::If(m, c, t, e) => {
            if is_called(c, name)
            {
                return false;
            }

            if !check_tailrec(t, name, top)
            {
                return false;
            }

            if !check_tailrec(e, name, top)
            {
                return false;
            }

            if t.get_metadata().tailrec || e.get_metadata().tailrec
            {
                m.tailrec = true;
            }

            true
        }

        SExpr::Application(m, f, a) => {
            if top && m.arity != 0
            {
                return !is_called(f, name) && !is_called(a, name);
            } else if !top && m.arity == 0
            {
                return !is_called(f, name) && !is_called(a, name);
            }

            if is_called(a, name)
            {
                return false;
            }

            if !check_tailrec(f, name, false)
            {
                return false;
            } else if f.get_metadata().tailrec
            {
                m.tailrec = true;
            }

            true
        }

        SExpr::Match(m, v, a) => {
            if is_called(v, name)
            {
                return false;
            }

            for a in a.iter_mut()
            {
                if !check_tailrec(&mut a.1, name, top)
                {
                    return false;
                }
            }

            for a in a
            {
                if a.1.get_metadata().tailrec
                {
                    m.tailrec = true;
                    return true;
                }
            }

            true
        }

        _ => !is_called(sexpr, name)
    }
}

// check_correctness(&mut IR) -> ()
// Checks the correctness of ir.
pub fn check_correctness(ir: &mut IR) -> Result<(), Vec<CorrectnessError>>
{
    let mut errors = Vec::with_capacity(0);

    // Check types
    check_type_validity(ir, &mut errors);

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
        // Save types
        let mut id = 0;
        while let Some(_) = ir.types.get(&format!("{}", id))
        {
            id += 1;
        }

        for sexpr in ir.sexprs.iter()
        {
            save_types(sexpr, &mut ir.types, &mut id);
        }

        for f in ir.funcs.iter()
        {
            save_types(&f.1.body, &mut ir.types, &mut id);
        }

        // Check for tail recursion
        for f in ir.funcs.iter_mut()
        {
            check_tailrec(&mut f.1.body, &f.0, true);
        }

        Ok(())
    } else
    {
        Err(errors)
    }
}

