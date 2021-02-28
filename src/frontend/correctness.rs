use logos::Span;
use std::collections::{HashMap, HashSet};
use std::iter::FromIterator;
use std::mem::swap;

use super::ir::{BinOp, IR, IRExtern, IRFunction, IRImport, IRModule, Location, PrefixOp, SExpr, SExprMetadata};
use super::scopes::{FunctionName, Scope};
use super::types::{HashSetWrapper, Type};

#[derive(Debug)]
pub enum CorrectnessError
{
    UndefinedPrefixOp(Location, PrefixOp, Type),
    UndefinedInfixOp(Location, BinOp, Type, Type),
    NonboolInBoolExpr(Location, Type),
    NonboolInIfCond(Location, Type),
    NonmatchingAssignTypes(Location, Type, Location, Type),
    SymbolNotFound(Location, String),
    Reassignment(Location, Location, String),
    InvalidType(Location),
    DuplicateTypeInUnion(Location, Location, Type),
    UnknownFunctionReturnType(Location, String),
    MismatchedFunctionArgType(Location, Type, Type),
    InvalidApplication(Location, Type),
    InvalidCast(Location, Type, Location, Type),
    NonSubtypeOnMatch(Location, Type, Location, Type),
    InfiniteSizedType(Location, Type),
    NonmemberAccess(Location, String, String),
    MismatchedDeclarationAssignmentTypes(Location, Type, Location, Type),
    VariableImportedTwice(Location, Location),
    ImportedValueNotExported(Location, String, String),
    NoMainFunction,
    CurriedExternalFunc(Location),
    ImpureInPure(Location, Location),
    UnnecessaryImpure(Location)
}

// check_sexpr(&mut SExpr, &mut IRModule, &mut Vec<CorrectnessError>) -> ()
// Checks an s expression for type correctness and correct symbol usage.
fn check_sexpr(sexpr: &mut SExpr, module: &mut IRModule, errors: &mut Vec<CorrectnessError>)
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
        SExpr::Word(_, _) => (),
        SExpr::Char(_, _) => (),
        SExpr::String(_, _) => (),
        SExpr::True(_) => (),
        SExpr::False(_) => (),
        SExpr::Enum(_, _) => (),
        SExpr::ExternalFunc(_, _, _) => (),

        // Lists
        SExpr::List(_, _) => panic!("uwu"),

        // Functions
        SExpr::Function(m, f) => {
            match module.scope.get_var(f)
            {
                Some(t) => {
                    m._type = t.0.clone();
                    m.arity = t.1;
                    m.saved_argc = t.2;
                    m.origin = t.5.clone();

                    if module.funcs.get(f).is_some() && !module.funcs.get(f).unwrap().checked
                    {
                        let mut func = module.funcs.remove(f).unwrap();
                        module.scope.push_scope(true);
                        for a in func.args.iter()
                        {
                            module.scope.put_var(&a.0, &a.1, 0, None, &Location::empty(), true, "");
                        }
                        check_sexpr(&mut func.body, module, errors);
                        module.scope.pop_scope();
                        module.funcs.insert(f.clone(), func);
                    }

                    if m._type == Type::Unknown
                    {
                        module.funcs.remove(f);
                    }
                }

                None => {
                    let mut func = module.funcs.remove(f).unwrap();
                    check_function_body(f, f, &module.name, &mut func, &mut module.scope, &mut module.funcs, errors, &module.types, &module.externals, &module.imports);

                    module.scope.push_scope(true);
                    for a in func.args.iter()
                    {
                        if a.0 != "_"
                        {
                            module.scope.put_var(&a.0, &a.1, 0, None, &Location::empty(), true, "");
                        }
                    }
                    check_sexpr(&mut func.body, module, errors);
                    module.scope.pop_scope();

                    if let Some(v) = module.scope.get_var(f)
                    {
                        m._type = v.0.clone();
                    } else
                    {
                        return;
                    }

                    m.origin = module.name.clone();
                    m.arity = func.args.len();
                    m.saved_argc = Some(func.captured.len());
                    module.funcs.insert(f.clone(), func);
                }
            }
        }

        // Symbols
        SExpr::Symbol(m, s) => {
            match module.scope.get_var(s)
            {
                Some(t) => {
                    m._type = t.0.clone();
                    m.arity = t.1;
                    m.saved_argc = t.2;
                    m.origin = t.5.clone();
                }

                None => {
                    // External functions
                    if let Some(v) = module.externals.get(s)
                    {
                        m.arity = v.arg_types.len();
                        m.saved_argc = Some(0);
                        m._type = v.ret_type.clone();

                        for t in v.arg_types.iter().rev()
                        {
                            let mut _type = Type::Unknown;
                            swap(&mut _type, &mut m._type);
                            m._type = Type::Func(Box::new(t.clone()), Box::new(_type));
                        }

                        let mut temp = SExprMetadata::empty();
                        swap(&mut temp, m);
                        *sexpr = SExpr::ExternalFunc(temp, v.extern_name.clone(), Vec::with_capacity(0));
                    } else
                    {
                        errors.push(CorrectnessError::SymbolNotFound(
                            m.loc.clone(),
                            s.clone()
                        ));
                    }
                }
            }
        }

        // Prefix operators
        SExpr::Prefix(m, op, v) => {
            match op
            {
                // Negative has operators defined in scope
                PrefixOp::Neg => {
                    // Check child node
                    check_sexpr(v, module, errors);

                    // Check if an error occurred
                    if v.get_metadata()._type == Type::Error
                    {
                        return;
                    }

                    // Get type
                    if let Some(t) = module.scope.get_func_ret(FunctionName::Prefix(v.get_metadata()._type.clone()))
                    {
                        m._type = t.clone();
                    } else
                    {
                        errors.push(CorrectnessError::UndefinedPrefixOp(
                            m.loc.clone(),
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
            check_sexpr(left, module, errors);
            check_sexpr(right, module, errors);

            // Check if an error occurred
            if left.get_metadata()._type == Type::Error || right.get_metadata()._type == Type::Error
            {
                return;
            }

            // Get type
            if let Some(t) = module.scope.get_func_ret(FunctionName::Infix(
                *op,
                left.get_metadata()._type.clone(),
                right.get_metadata()._type.clone()
            ))
            {
                m._type = t.clone();
            } else
            {
                errors.push(CorrectnessError::UndefinedInfixOp(
                    m.loc.clone(),
                    *op,
                    left.get_metadata()._type.clone(),
                    right.get_metadata()._type.clone()
                ));
            }
        }

        SExpr::Chain(m, left, right) => {
            // Add scope if left is a walrus operator
            if let SExpr::Walrus(_, _, _) = **left
            {
                module.scope.push_scope(false);
            }

            // Check child nodes
            check_sexpr(left, module, errors);
            check_sexpr(right, module, errors);

            if let SExpr::Walrus(_, _, _) = **left
            {
                module.scope.pop_scope();
            }

            // Check if an error occurred
            if left.get_metadata()._type == Type::Error || right.get_metadata()._type == Type::Error
            {
                return;
            }

            m._type = right.get_metadata()._type.clone();
        }

        SExpr::As(m, v) => {
            // Check child node
            check_sexpr(v, module, errors);

            // Check if an error occurred
            if v.get_metadata()._type == Type::Error
            {
                return;
            }

            // Assert the types are valid
            if !v.get_metadata()._type.is_subtype(&m._type, &module.types)
            {
                errors.push(CorrectnessError::InvalidCast(
                    v.get_metadata().loc.clone(),
                    v.get_metadata()._type.clone(),
                    m.loc2.clone(),
                    m._type.clone()
                ));
                m._type = Type::Error;
            }
        }

        // And and or
        SExpr::And(m, left, right) | SExpr::Or(m, left, right) => {
            // Check child nodes
            check_sexpr(left, module, errors);
            check_sexpr(right, module, errors);

            // Check if an error occurred
            if left.get_metadata()._type == Type::Error || right.get_metadata()._type == Type::Error
            {
                return;
            }

            // Assert the types are booleans.
            if left.get_metadata()._type != Type::Bool
            {
                errors.push(CorrectnessError::NonboolInBoolExpr(
                    left.get_metadata().loc.clone(),
                    left.get_metadata()._type.clone(),
                ));
            }

            if right.get_metadata()._type != Type::Bool
            {
                errors.push(CorrectnessError::NonboolInBoolExpr(
                    right.get_metadata().loc.clone(),
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
            check_sexpr(cond, module, errors);
            check_sexpr(then, module, errors);
            check_sexpr(elsy, module, errors);

            // Check if an error occurred
            if cond.get_metadata()._type == Type::Error || then.get_metadata()._type == Type::Error || elsy.get_metadata()._type == Type::Error
            {
                return;
            }

            // Check that condition is a boolean
            if cond.get_metadata()._type != Type::Bool
            {
                errors.push(CorrectnessError::NonboolInIfCond(
                    cond.get_metadata().loc.clone(),
                    cond.get_metadata()._type.clone()
                ));
            }

            // Check that the types of the then and else blocks match
            if then.get_metadata()._type.is_subtype(&elsy.get_metadata()._type, &module.types)
            {
                m._type = elsy.get_metadata()._type.clone()
            } else if elsy.get_metadata()._type.is_subtype(&then.get_metadata()._type, &module.types)
            {
                m._type = then.get_metadata()._type.clone()
            } else if then.get_metadata()._type != elsy.get_metadata()._type
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
            check_sexpr(func, module, errors);
            check_sexpr(arg, module, errors);

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

            // External functions
            } else if let SExpr::ExternalFunc(m1, name, args) = &mut **func
            {
                if let Type::Func(l, r) = &mut m1._type
                {
                    if **l == arg.get_metadata()._type
                    {
                        let mut temp = SExpr::True(SExprMetadata::empty());
                        swap(&mut temp, arg);
                        args.push(temp);
                        let mut temp = Type::Unknown;
                        swap(&mut **r, &mut temp);
                        m1.loc.span.end = m.loc.span.end;
                        m1._type = temp;
                        m1.arity -= 1;
                        m1.saved_argc = Some(m1.saved_argc.unwrap() + 1);
                        let mut tm = SExprMetadata::empty();
                        let mut tn = String::with_capacity(0);
                        let mut ta = Vec::with_capacity(0);
                        swap(&mut tm, m1);
                        swap(&mut tn, name);
                        swap(&mut ta, args);
                        *sexpr = SExpr::ExternalFunc(tm, tn, ta);
                        return;
                    } else
                    {
                        errors.push(CorrectnessError::MismatchedFunctionArgType(
                            arg.get_metadata().loc.clone(),
                            *l.clone(),
                            arg.get_metadata()._type.clone()
                        ));
                        return;
                    }
                }
            }

            let mut _type = &func.get_metadata()._type;
            while let Type::Symbol(s) = _type
            {
                _type = module.types.get(s).unwrap();
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
                    if arg.get_metadata()._type.is_subtype(l, &module.types)
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
                            if let Type::Enum(_) = **l
                            {
                                m.saved_argc = Some(v);
                            } else
                            {
                                m.saved_argc = Some(v + 1);
                            }
                        }
                    } else
                    {
                        errors.push(CorrectnessError::MismatchedFunctionArgType(
                            arg.get_metadata().loc.clone(),
                            *l.clone(),
                            arg.get_metadata()._type.clone()
                        ));
                    }
                }

                // Everything else is invalid
                _ => {
                    errors.push(CorrectnessError::InvalidApplication(
                        func.get_metadata().loc.clone(),
                        func.get_metadata()._type.clone()
                    ));
                }
            }
        }

        // Assignments
        SExpr::Assign(m, name, value) => {
            // Check child node
            check_sexpr(value, module, errors);
            if value.get_metadata()._type == Type::Error
            {
                return;
            }

            // Check if variable already exists
            if let Some(v) = module.scope.variables.get(name)
            {
                if v.4
                {
                    errors.push(CorrectnessError::Reassignment(
                        m.loc.clone(),
                        v.3.clone(),
                        name.clone()
                    ));
                    if let SExpr::Function(_, f) = &**value
                    {
                        if f != name
                        {
                            module.scope.variables.remove(f);
                        }
                        module.funcs.remove(f);
                    }
                    m._type = Type::Error;
                    return;
                } else if m._type != Type::Error && !m._type.is_subtype(&v.0, &module.types)
                {
                    errors.push(CorrectnessError::MismatchedDeclarationAssignmentTypes(
                        v.3.clone(),
                        v.0.clone(),
                        m.loc.clone(),
                        m._type.clone()
                    ));
                    m._type = Type::Error;
                    return;
                } else if m._type == Type::Error && !value.get_metadata()._type.is_subtype(&v.0, &module.types)
                {
                    errors.push(CorrectnessError::MismatchedDeclarationAssignmentTypes(
                        v.3.clone(),
                        v.0.clone(),
                        value.get_metadata().loc.clone(),
                        value.get_metadata()._type.clone()
                    ));
                    m._type = Type::Error;
                    return;
                }

                if m._type != v.0
                {
                    m._type = v.0.clone();
                }
            }

            // Check if variable value is unknown type
            if value.get_metadata()._type == Type::Unknown
            {
                module.scope.variables.remove(name);
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
                    if !value.get_metadata()._type.is_subtype(&m._type, &module.types)
                    {
                        errors.push(CorrectnessError::NonmatchingAssignTypes(
                            m.loc2.clone(),
                            m._type.clone(),
                            value.get_metadata().loc.clone(),
                            value.get_metadata()._type.clone()
                        ));
                        m._type = Type::Error;
                    }
                }
            }

            // Add variable to scope if no error occurred
            if m._type != Type::Error && name != "_"
            {
                module.scope.put_var(name, &m._type, value.get_metadata().arity, value.get_metadata().saved_argc, &Location::new(Span { start: m.loc.span.start, end: value.get_metadata().loc.span.start }, &m.loc.filename), true, &module.name);
            }
        }

        // With expressions
        SExpr::With(m, assigns, body) => {
            // Push a new scope
            module.scope.push_scope(false);

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
            check_function_group(iter, module, errors);

            // Check assignments
            for a in assigns
            {
                check_sexpr(a, module, errors);
            }

            // Check body
            check_sexpr(body, module, errors);
            m._type = body.get_metadata()._type.clone();
            m.arity = body.get_metadata().arity;

            // Pop scope
            module.scope.pop_scope();
        }

        SExpr::Walrus(m, name, value) => {
            check_sexpr(value, module, errors);
            m._type = value.get_metadata()._type.clone();
            m.arity = value.get_metadata().arity;
            m.saved_argc = value.get_metadata().saved_argc;

            if name != "_"
            {
                module.scope.put_var(&name, &m._type, m.arity, m.saved_argc, &m.loc, true, &module.filename);
            }
        }

        SExpr::Match(m, value, arms) => {
            // Check value
            check_sexpr(value, module, errors);
            let _type = &value.get_metadata()._type;
            if *_type == Type::Error
            {
                return;
            } else if let Type::UndeclaredTypeError(_) = *_type
            {
                return;
            }

            // Check match arms
            module.scope.push_scope(false);
            let mut returned = Type::Sum(HashSetWrapper(HashSet::with_capacity(0)));
            let mut last = None;
            for arm in arms.iter_mut()
            {
                // Get name of symbol
                let (name, atype) = if let Type::Tag(s, t) = &arm.0
                {
                    let vtype = if let Type::Tag(_, t) = &**t
                    {
                        &**t
                    } else
                    {
                        &**t
                    };
                    module.scope.put_var(s, vtype, 0, None, &arm.1.get_metadata().loc2, true, "");
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
                    module.scope.pop_scope();
                    continue;
                }

                // Nonsubtypes are errors
                if !atype.is_subtype(&_type, &module.types)
                {
                    errors.push(CorrectnessError::NonSubtypeOnMatch(
                        value.get_metadata().loc.clone(),
                        _type.clone(),
                        arm.2.clone(),
                        arm.0.clone()
                    ));
                    module.scope.pop_scope();
                    return;
                }

                // Check body of match arm
                check_sexpr(&mut arm.1, module, errors);

                // Remove variable
                if let Some(s) = name
                {
                    module.scope.variables.remove(s);
                }

                // Check for error
                if arm.1.get_metadata()._type == Type::Error
                {
                    last = Some(Type::Error);
                    continue;
                }

                // Unwrap symbol
                let mut _type = arm.1.get_metadata()._type.clone();

                // Add types
                if returned.is_subtype(&_type, &module.types) && (last.is_none() || last.as_ref().unwrap().is_subtype(&_type, &module.types))
                {
                    last = Some(_type);
                } else if let Some(Type::Error) = last
                {
                    last = Some(Type::Error);
                } else if let Type::Sum(set) = &mut returned
                {
                    if let Some(mut t) = last
                    {
                        while let Type::Symbol(s) = t
                        {
                            t = module.types.get(&s).unwrap().clone();
                        }
                        if let Type::Sum(v) = t
                        {
                            for v in v.0
                            {
                                set.0.insert(v);
                            }
                        } else
                        {
                            set.0.insert(t);
                        }
                        last = None;
                    }

                    while let Type::Symbol(s) = _type
                    {
                        _type = module.types.get(&s).unwrap().clone();
                    }
                    if let Type::Sum(v) = _type
                    {
                        for v in v.0
                        {
                            set.0.insert(v);
                        }
                    } else
                    {
                        set.0.insert(_type);
                    }
                }
            }

            module.scope.pop_scope();
            m._type = if let Some(v) = last
            {
                v
            } else if let Type::Sum(v) = returned
            {
                if v.0.len() == 1
                {
                    v.0.into_iter().next().unwrap()
                } else
                {
                    Type::Sum(v)
                }
            } else
            {
                unreachable!("if youre here you dont deserve a single uwu");
            };
        }

        SExpr::MemberAccess(m, a) => {
            // Get module
            let mut module_name = String::new();
            let mut pos = 0;
            let types = &module.types;
            for a in a.iter().enumerate()
            {
                if module_name.len() != 0
                {
                    module_name.push_str("::");
                }
                module_name.push_str(a.1);

                if module.imports.contains_key(&module_name)
                {
                    pos = a.0 + 1;
                }
            }

            if pos > 0
            {
                module_name.clear();
                for a in a.iter().enumerate()
                {
                    if a.0 == pos
                    {
                        break;
                    }

                    if module_name.len() != 0
                    {
                        module_name.push_str("::");
                    }
                    module_name.push_str(a.1);
                }

                if let Some(v) = module.imports.get(&module_name).unwrap().imports.get(&a[pos])
                {
                    if a.len() > pos + 1
                    {
                        errors.push(CorrectnessError::NonmemberAccess(
                            m.loc.clone(),
                            format!("{}::{}", module_name, a[pos]),
                            a[pos + 1].clone()
                        ));
                    } else
                    {
                        m._type = v.0.clone();
                        m.origin = module.imports.get(&module_name).unwrap().name.clone();
                        let mut meta = SExprMetadata::empty();
                        swap(&mut meta, m);
                        *sexpr = SExpr::Function(meta, a[pos].clone());
                    }
                    return;
                }
            }

            // Get type
            if let Some(t) = types.get(&a[pos])
            {
                if let Type::Sum(f) = t
                {
                    if f.0.contains(&Type::Enum(a[1].clone()))
                    {
                        if a.len() != 2
                        {
                            errors.push(CorrectnessError::NonmemberAccess(
                                m.loc.clone(),
                                format!("{}::{}", a[0], a[1]),
                                a[2].clone()
                            ));
                        } else
                        {
                            m._type = Type::Symbol(a[pos].clone());
                        }
                    } else if let Some(v) = f.0.iter().filter(|v| if let Type::Tag(t, _) = v { t == &a[1] } else { false }).next()
                    {
                        if a.len() != 2
                        {
                            errors.push(CorrectnessError::NonmemberAccess(
                                m.loc.clone(),
                                format!("{}::{}", a[0], a[1]),
                                a[2].clone()
                            ));
                        } else if let Type::Tag(_, t) = v
                        {
                            let mut temp = SExprMetadata::empty();

                            swap(&mut temp, m);
                            temp.arity = 1;
                            temp.saved_argc = Some(0);
                            temp._type = Type::Func(t.clone(), Box::new(Type::Symbol(a[0].clone())));

                            *sexpr = SExpr::Function(temp, a.join("::").to_string());
                        }
                    } else
                    {
                        errors.push(CorrectnessError::NonmemberAccess(
                            m.loc.clone(),
                            a[0].clone(),
                            a[1].clone()
                        ));
                    }
                } else
                {
                    errors.push(CorrectnessError::NonmemberAccess(
                        m.loc.clone(),
                        a[0].clone(),
                        a[1].clone()
                    ));
                }
            } else if pos == 0
            {
                errors.push(CorrectnessError::SymbolNotFound(
                    m.loc.clone(),
                    a[0].clone(),
                ));
            } else
            {
                errors.push(CorrectnessError::NonmemberAccess(
                    m.loc.clone(),
                    module_name,
                    a[pos].clone()
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
                let mut meta = SExprMetadata::empty();

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

        SExpr::Chain(_, l, r) => {
            let removed = if let SExpr::Walrus(_, n, _) = &**l
            {
                funcs.remove(n)
            } else
            {
                false
            };

            convert_function_symbols(l, funcs);
            convert_function_symbols(r, funcs);

            if let SExpr::Walrus(_, n, _) = &**l
            {
                if removed
                {
                    funcs.insert(n.clone());
                }
            }
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

// get_function_type(so many arguments aaaa) -> Type
// Gets the function type, returning Type::Unknown if a type cannot be found.
fn get_function_type(sexpr: &SExpr, module_name: &str, scope: &mut Scope, funcs: &mut HashMap<String, IRFunction>, errors: &mut Vec<CorrectnessError>, captured: &mut HashMap<String, Type>, captured_names: &mut Vec<String>, types: &HashMap<String, Type>, externals: &HashMap<String, IRExtern>, imports: &HashMap<String, IRImport>) -> Type
{
    match sexpr
    {
        // Values
        SExpr::TypeAlias(m, _)
            | SExpr::Int(m, _)
            | SExpr::Float(m, _)
            | SExpr::Word(m, _)
            | SExpr::Char(m, _)
            | SExpr::String(m, _)
            | SExpr::True(m)
            | SExpr::False(m)
            | SExpr::Enum(m, _)
            | SExpr::ExternalFunc(m, _, _)
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
                    check_function_body(f, f, module_name, &mut func, scope, funcs, errors, types, externals, imports);
                    funcs.insert(f.clone(), func);
                    scope.get_var(f).unwrap().0.clone()
                }
            }
        }

        // Symbols
        SExpr::Symbol(_, s) => {
            // Check scope
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

                // Check externals
                None => {
                    if let Some(v) = externals.get(s)
                    {
                        let mut _type = v.ret_type.clone();
                        for t in v.arg_types.iter().rev()
                        {
                            let mut temp = Type::Unknown;
                            swap(&mut temp, &mut _type);
                            _type = Type::Func(Box::new(t.clone()), Box::new(temp));
                        }

                        _type
                    } else
                    {
                        Type::Unknown
                    }
                }
            }
        }

        // Prefix operators
        SExpr::Prefix(_, op, v) => {
            match op
            {
                PrefixOp::Neg => {
                    let vt = get_function_type(v, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports);

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
            let lt = get_function_type(l, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports);
            let rt = get_function_type(r, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports);

            match scope.get_func_ret(FunctionName::Infix(*op, lt, rt))
            {
                Some(v) => v.clone(),
                None => Type::Unknown
            }
        }

        SExpr::Chain(_, l, r) => {
            if let SExpr::Walrus(_, _, _) = **l
            {
                scope.push_scope(false);
            }
            get_function_type(l, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports);
            let v = get_function_type(r, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports);
            if let SExpr::Walrus(_, _, _) = **l
            {
                scope.pop_scope();
            }
            v
        }

        SExpr::As(m, _) => m._type.clone(),

        // Boolean and/or
        SExpr::And(_, l, r) | SExpr::Or(_, l, r) => {
            get_function_type(l, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports);
            get_function_type(r, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports);
            Type::Bool
        }

        // If expressions
        SExpr::If(_, _, body, elsy) => {
            let bt = get_function_type(body, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports);
            let et = get_function_type(elsy, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports);

            if bt == Type::Unknown && et == Type::Unknown
            {
                Type::Unknown
            } else if bt == et || et == Type::Unknown
            {
                bt
            } else if bt == Type::Unknown
            {
                et
            } else if bt.is_subtype(&et, types)
            {
                et
            } else if et.is_subtype(&bt, types)
            {
                bt
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
                    return get_function_type(a, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports);
                }
            }

            // Get function type
            let mut ft = get_function_type(f, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports);
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
                    get_function_type(a, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports);
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
                get_function_type(value, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports)
            };

            if name != "_"
            {
                scope.put_var(&name, &t, 0, None, &Location::new(Span { start: 0, end: 0 }, &m.loc.filename), true, "");
            }
            t
        }

        // With expressions
        SExpr::With(_, assigns, body) => {
            // Push scope
            scope.push_scope(false);

            // Populate scope with variable types
            for a in assigns
            {
                get_function_type(a, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports);
            }

            // Get the function type
            let bt = get_function_type(body, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports);

            // Pop the scope and return type
            scope.pop_scope();
            bt
        }

        SExpr::Walrus(m, name, value) => {
            let t = get_function_type(value, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports);
            if name != "_"
            {
                scope.put_var(&name, &t, 0, None, &Location::new(Span { start: 0, end: 0 }, &m.loc.filename), true, "");
            }
            t
        }

        SExpr::Match(_, _, arms) => {
            // Check match arms
            scope.push_scope(false);
            let mut returned = Type::Sum(HashSetWrapper(HashSet::with_capacity(0)));
            let mut last: Option<Type> = None;
            for arm in arms.iter()
            {
                // Check body of match arm
                let name = if let Type::Tag(s, t) = &arm.0
                {
                    scope.put_var(&s, &t, 0, None, &arm.1.get_metadata().loc2, true, "");
                    Some(s)
                } else
                {
                    None
                };

                let mut _type = get_function_type(&arm.1, module_name, scope, funcs, errors, captured, captured_names, types, externals, imports);

                if let Some(s) = name
                {
                    scope.variables.remove(s);
                }

                if _type == Type::Unknown
                {
                    continue;
                }

                if returned.is_subtype(&_type, types) && (last.is_none() || last.as_ref().unwrap().is_subtype(&_type, types))
                {
                    last = Some(_type);
                } else if let Some(Type::Error) = last
                {
                    last = Some(Type::Error);
                } else if let Type::Sum(set) = &mut returned
                {
                    if let Some(mut t) = last
                    {
                        while let Type::Symbol(s) = t
                        {
                            t = types.get(&s).unwrap().clone();
                        }
                        if let Type::Sum(v) = t
                        {
                            for v in v.0
                            {
                                set.0.insert(v);
                            }
                        } else
                        {
                            set.0.insert(t);
                        }
                        last = None;
                    }

                    while let Type::Symbol(s) = _type
                    {
                        _type = types.get(&s).unwrap().clone();
                    }
                    if let Type::Sum(v) = _type
                    {
                        for v in v.0
                        {
                            set.0.insert(v);
                        }
                    } else
                    {
                        set.0.insert(_type);
                    }
                }
            }

            scope.pop_scope();

            if let Some(v) = last
            {
                v
            } else if let Type::Sum(v) = returned
            {
                if v.0.len() == 0
                {
                    Type::Unknown
                } else if v.0.len() == 1
                {
                    v.0.into_iter().next().unwrap()
                } else
                {
                    Type::Sum(v)
                }
            } else
            {
                unreachable!("if youre here you dont deserve a single uwu");
            }
        }

        SExpr::MemberAccess(_, a) => {
            // Get module
            let mut module_name = String::new();
            let mut pos = 0;
            let types = types;
            for a in a.iter().enumerate()
            {
                if module_name.len() != 0
                {
                    module_name.push_str("::");
                }
                module_name.push_str(a.1);

                if imports.contains_key(&module_name)
                {
                    pos = a.0 + 1;
                }
            }

            if pos > 0
            {
                module_name.clear();
                for a in a.iter().enumerate()
                {
                    if a.0 == pos
                    {
                        break;
                    }

                    if module_name.len() != 0
                    {
                        module_name.push_str("::");
                    }
                    module_name.push_str(a.1);
                }

                if let Some(v) = imports.get(&module_name).unwrap().imports.get(&a[pos])
                {
                    return if a.len() > pos + 1
                    {
                        Type::Unknown
                    } else
                    {
                        v.0.clone()
                    };
                }
            }

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
                    } else if let Some(v) = f.0.iter().filter(|v| if let Type::Tag(t, _) = v { t == &a[1] } else { false }).next()
                    {
                        if a.len() != 2
                        {
                            Type::Unknown
                        } else if let Type::Tag(_, t2) = v
                        {
                            Type::Func(t2.clone(), Box::new(t.clone()))
                        } else
                        {
                            unreachable!("always a tag type");
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

// check_function_body(&str, &str, &str, &IRFunction, &mut Scope, &HashMap<String, IRFunction>, &mut Vec<CorrectnessError>) -> Vec<(String, Type)>
// Checks a function body and determines the return type of the function.
fn check_function_body(name: &str, refr: &str, module_name: &str, func: &mut IRFunction, scope: &mut Scope, funcs: &mut HashMap<String, IRFunction>, errors: &mut Vec<CorrectnessError>, types: &HashMap<String, Type>, externals: &HashMap<String, IRExtern>, imports: &HashMap<String, IRImport>)
{
    if scope.get_var(name).is_none() && scope.get_var(refr).is_none()
    {
        // Put function in scope
        scope.put_var_raw(String::from(name), Type::Unknown, func.args.len(), None, Location::new(Span { start: 0, end: 0 }, &func.loc.filename), false, String::from(module_name));
        if name != refr
        {
            scope.put_var_raw(String::from(refr), Type::Unknown, func.args.len(), None, Location::new(Span { start: 0, end: 0 }, &func.loc.filename), false, String::from(module_name));
        }
    }

    // Put arguments into scope
    scope.push_scope(true);
    for arg in func.args.iter()
    {
        if arg.0 != "_"
        {
            scope.put_var(&arg.0, &arg.1, 0, None, &Location::new(Span { start: 0, end: 0 }, &func.loc.filename), true, "");
        }
    }

    // Get the type
    let mut captured = HashMap::with_capacity(0);
    let mut captured_names = Vec::with_capacity(0);
    let _type = get_function_type(&func.body, module_name, scope, funcs, errors, &mut captured, &mut captured_names, types, externals, imports);
    func.captured = captured;
    func.captured_names = captured_names;
    scope.pop_scope();

    // Push an error if type is unknown
    if _type == Type::Unknown
    {
        errors.push(CorrectnessError::UnknownFunctionReturnType(
            func.body.get_metadata().loc.clone(),
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

        if let Some(v) = scope.variables.remove(refr)
        {
            if v.0 != Type::Unknown && v.0 != acc
            {
                errors.push(CorrectnessError::MismatchedDeclarationAssignmentTypes(
                    v.3,
                    v.0,
                    func.loc.clone(),
                    acc
                ));
                return
            }
        }
        if let Some(v) = scope.variables.remove(name)
        {
            if v.0 != Type::Unknown && v.0 != acc
            {
                errors.push(CorrectnessError::MismatchedDeclarationAssignmentTypes(
                    v.3,
                    v.0,
                    func.loc.clone(),
                    acc
                ));
                return
            }
        }

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
            scope.put_var_raw(String::from(refr), acc.clone(), func.args.len(), Some(func.captured.len()), func.loc.clone(), false, String::from(module_name));
        }
        scope.put_var_raw(String::from(name), acc, func.args.len(), Some(func.captured.len()), func.loc.clone(), false, String::from(module_name));
    }
}

// check_function_group(T, &HashMap<String, IRFunction>, &mut IRModule, &mut Vec<CorrectnessError>) -> ()
// Checks the group of functions for return types.
fn check_function_group<T>(names: T, module: &mut IRModule, errors: &mut Vec<CorrectnessError>)
    where T: Iterator<Item = (String, String)> + Clone
{
    // Generate function types
    for name in names.clone()
    {
        // Handle functions
        if let Some(mut func) = module.funcs.remove(&name.0)
        {
            check_function_body(&name.0, &name.1, &module.name, &mut func, &mut module.scope, &mut module.funcs, errors, &module.types, &module.externals, &module.imports);
            module.funcs.insert(name.0, func);
        } else
        {
            unreachable!("uwu");
        }
    }

    // Check all function bodies
    for name in names
    {
        // Remove function
        if let Some(mut func) = module.funcs.remove(&name.0)
        {
            // Push scope and add arguments
            module.scope.push_scope(true);
            for arg in &func.args
            {
                if arg.0 != "_"
                {
                    module.scope.put_var(&arg.0, &arg.1, 0, None, &Location::new(Span { start: 0, end: 0 }, &func.loc.filename), true, &module.name);
                }
            }

            // Check body
            check_sexpr(&mut func.body, module, errors);
            check_externals(&mut func.body, errors);
            func.checked = true;

            // Pop scope
            module.scope.pop_scope();

            // Reinsert function
            module.funcs.insert(name.0, func);
        }
    }
}

// check_globals(&mut IRModule, Vec<String>, &mut Vec<CorrectnessError>) -> ()
// Checks global function return types.
fn check_globals(module: &mut IRModule, errors: &mut Vec<CorrectnessError>)
{
    // Get the set of all global functions and globals
    let mut globals = HashSet::from_iter(module.scope.variables.iter().filter_map(|v| {
        if v.0 != "debug"
        {
            Some(v.0.clone())
        } else
        {
            None
        }
    }).chain(module.funcs.iter().filter_map(|v| {
        if v.0 != "_" && v.1.global
        {
            Some(v.0.clone())
        } else
        {
            None
        }
    })));

    // Iterate over every function
    for func in module.funcs.iter_mut()
    {
        // Remove arguments
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

    for sexpr in module.sexprs.iter_mut()
    {
        convert_function_symbols(sexpr, &mut globals);
    }

    // Check all globals
    let v: Vec<(String, String)> = module.funcs.iter().filter_map(
        |v| match v.1.global
        {
            true => Some((v.0.clone(), v.0.clone())),
            false => None
        }
    ).collect();
    check_function_group(v.into_iter(), module, errors);
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
    } else if let Type::Func(l, r) = &m._type
    {
        if let Type::Sum(_) = &**l
        {
            save_single_type(id, l, types);
        }

        let mut t = &**r;
        while let Type::Func(l, r) = t
        {
            if let Type::Sum(_) = &**l
            {
                save_single_type(id, &**l, types);
            }

            t = &**r;
        }

        if let Type::Sum(_) = t
        {
            save_single_type(id, t, types);
        }
    }

    match sexpr
    {
        SExpr::Prefix(_, _, v) => {
            save_types(v, types, id);
        }

        SExpr::Infix(_, _, l, r)
            | SExpr::Chain(_, l, r)
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

        SExpr::Walrus(_, _, v) => {
            save_types(v, types, id);
        }

        SExpr::Match(_, v, a) => {
            save_types(v, types, id);
            for a in a.iter()
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
                    save_single_type(id, &_type, types);
                }
                save_types(&a.1, types, id);
            }
        }

        _ => ()
    }
}

// check_type_validity(&IRModule) -> ()
// Checks whether the given types are valid or not.
fn check_type_validity(module: &IRModule, errors: &mut Vec<CorrectnessError>)
{
    for s in module.sexprs.iter()
    {
        if let SExpr::TypeAlias(m, n) = s
        {
            match &m._type
            {
                Type::Symbol(s) if s == n => {
                    errors.push(CorrectnessError::InfiniteSizedType(
                        m.loc2.clone(),
                        m._type.clone()
                    ));
                }

                Type::Sum(s) if s.0.contains(&Type::Symbol(n.clone())) => {
                    errors.push(CorrectnessError::InfiniteSizedType(
                        m.loc2.clone(),
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
            | SExpr::Chain(_, l, r)
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

        SExpr::Walrus(_, _, v) => {
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

        SExpr::Chain(m, l, r) => {
            if is_called(l, name)
            {
                return false;
            }

            if !check_tailrec(r, name, top)
            {
                return false;
            }

            m.tailrec = r.get_metadata().tailrec;
            true
        }

        SExpr::Walrus(_, n, v) => {
            if name != n
            {
                check_tailrec(v, name, top)
            } else
            {
                false
            }
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

// fix_arity(&mut SExpr, &mut Scope) -> ()
// Fixes the arity of imported functions.
fn fix_arity(sexpr: &mut SExpr, scope: &mut Scope)
{
    match sexpr
    {
        SExpr::Symbol(m, s)
            | SExpr::Function(m, s) => {
            if let Some(v) = scope.get_var(s)
            {
                m.arity = v.1;
                m.saved_argc = v.2;
            }
        }

        SExpr::Prefix(_, _, v)
            | SExpr::As(_, v)
            | SExpr::Assign(_, _, v)
            | SExpr::Walrus(_, _, v)
            => {
            fix_arity(v, scope);
        }

        SExpr::Infix(_, _, l, r)
            | SExpr::Chain(_, l, r)
            | SExpr::And(_, l, r)
            | SExpr::Or(_, l, r)
            => {
            fix_arity(l, scope);
            fix_arity(r, scope);
        }

        SExpr::Application(m, l, r) => {
            fix_arity(l, scope);

            if l.get_metadata().arity > 0
            {
                m.arity = l.get_metadata().arity - 1;
                if let Some(v) = l.get_metadata().saved_argc
                {
                    if let Type::Enum(_) = r.get_metadata()._type
                    {
                        m.saved_argc = Some(v);
                    } else
                    {
                        m.saved_argc = Some(v + 1);
                    }
                } else
                {
                    m.saved_argc = None;
                }
            }

            fix_arity(r, scope);
        }

        SExpr::If(_, c, t, e) => {
            fix_arity(c, scope);
            fix_arity(t, scope);
            fix_arity(e, scope);
        }

        SExpr::With(_, a, v) => {
            scope.push_scope(false);
            for a in a
            {
                fix_arity(a, scope);
            }
            fix_arity(v, scope);
            scope.pop_scope();
        }

        SExpr::Match(_, v, a) => {
            fix_arity(v, scope);
            scope.push_scope(false);
            for a in a.iter_mut()
            {
                fix_arity(&mut a.1, scope)
            }
            scope.pop_scope();
        }

        _ => ()
    }
}

// check_externals(&SExpr, &mut Vec<CorrectnessError>)
// Checks if external functions are applied fully.
fn check_externals(sexpr: &SExpr, errors: &mut Vec<CorrectnessError>)
{
    match sexpr
    {
        SExpr::ExternalFunc(m, _, _) => {
            if m.arity != 0
            {
                errors.push(CorrectnessError::CurriedExternalFunc(m.loc.clone()));
            }
        }

        SExpr::Prefix(_, _, v)
            | SExpr::As(_, v)
            | SExpr::Assign(_, _, v)
            | SExpr::Walrus(_, _, v)
            => {
            check_externals(v, errors);
        }

        SExpr::Infix(_, _, l, r)
            | SExpr::Chain(_, l, r)
            | SExpr::And(_, l, r)
            | SExpr::Or(_, l, r)
            | SExpr::Application(_, l, r)
            => {
            check_externals(l, errors);
            check_externals(r, errors);
        }
        SExpr::If(_, c, t, e) => {
            check_externals(c, errors);
            check_externals(t, errors);
            check_externals(e, errors);
        }

        SExpr::With(_, a, v) => {
            for a in a
            {
                check_externals(a, errors);
            }
            check_externals(v, errors);
        }

        SExpr::Match(_, v, a) => {
            check_externals(v, errors);

            for a in a
            {
                check_externals(&a.1, errors);
            }
        }

        _ => ()
    }
}

// check_purity(&SExpr, &IRModule) -> Result<(), Location>
// Checks the purity of a sexpression, erroring with the location if an impurity is found.
fn check_purity(sexpr: &SExpr, module: &IRModule) -> Result<(), Location>
{
    match sexpr
    {
        SExpr::ExternalFunc(m, f, a) => {
            for e in module.externals.iter()
            {
                if e.1.extern_name == *f
                {
                    if e.1.impure
                    {
                        return Err(m.loc.clone())
                    } else
                    {
                        break;
                    }
                }
            }

            for a in a
            {
                check_purity(a, module)?;
            }
            Ok(())
        }

        SExpr::Function(m, f) => {
            if module.funcs.get(f).unwrap().impure
            {
                Err(m.loc.clone())
            } else
            {
                Ok(())
            }
        }

        SExpr::Prefix(_, _, v)
            | SExpr::As(_, v)
            | SExpr::Assign(_, _, v)
            | SExpr::Walrus(_, _, v)
            => {
            check_purity(v, module)
        }

        SExpr::Infix(_, _, l, r)
            | SExpr::Chain(_, l, r)
            | SExpr::And(_, l, r)
            | SExpr::Or(_, l, r)
            | SExpr::Application(_, l, r)
            => {
            check_purity(l, module)?;
            check_purity(r, module)
        }

        SExpr::If(_, c, t, e) => {
            check_purity(c, module)?;
            check_purity(t, module)?;
            check_purity(e, module)
        }

        SExpr::With(_, a, v) => {
            for a in a
            {
                check_purity(a, module)?;
            }
            check_purity(v, module)
        }

        SExpr::Match(_, v, a) => {
            check_purity(v, module)?;

            for a in a
            {
                check_purity(&a.1, module)?;
            }
            Ok(())
        }

        _ => Ok(())
    }
}

// check_imports(&IRModule, &IR, &mut Vec<CorrectnessError>) -> ()
// Checks the validity of imported and exported items in a module.
fn check_module(module: &mut IRModule, ir: &IR, errors: &mut Vec<CorrectnessError>)
{
    // Put export types
    for export in module.exports.iter()
    {
        module.scope.put_var(export.0, &export.1.1, 0, None, &export.1.0, false, &module.name);
    }

    // Put unqualified imports in scope
    for import in module.imports.iter_mut()
    {
        if !import.1.qualified
        {
            // Import specific values
            if import.1.imports.len() != 0
            {
                let exporter = &ir.modules.get(&import.1.name).unwrap().exports;
                for i in import.1.imports.iter_mut()
                {
                    if module.scope.variables.contains_key(i.0)
                    {
                        errors.push(CorrectnessError::VariableImportedTwice(
                            module.scope.variables.get(i.0).unwrap().3.clone(),
                            import.1.loc.clone()
                        ));
                    } else if exporter.contains_key(i.0)
                    {
                        let mut impure = false;
                        for s in ir.modules.get(&import.1.name).unwrap().sexprs.iter()
                        {
                            if let SExpr::Assign(_, n, v) = s
                            {
                                if n == i.0
                                {
                                    if let SExpr::Function(_, f) = &**v
                                    {
                                        if ir.modules.get(&import.1.name).unwrap().funcs.get(f).unwrap().impure
                                        {
                                            impure = true;
                                        }
                                    }
                                    break;
                                }
                            }
                        }
                        *i.1 = (exporter.get(i.0).unwrap().1.clone(), 0, impure);
                        module.scope.put_var(i.0, &i.1.0, 0, None, &import.1.loc, true, &import.1.name);
                    } else
                    {
                        errors.push(CorrectnessError::ImportedValueNotExported(
                            import.1.loc.clone(),
                            i.0.clone(),
                            import.1.name.clone()
                        ));
                    }
                }

            // Import everything
            } else
            {
                for i in ir.modules.get(&import.1.name).unwrap().exports.iter()
                {
                    if module.scope.variables.contains_key(i.0)
                    {
                        errors.push(CorrectnessError::VariableImportedTwice(
                            module.scope.variables.get(i.0).unwrap().3.clone(),
                            i.1.0.clone()
                        ));
                    } else
                    {
                        let mut impure = false;
                        for s in ir.modules.get(&import.1.name).unwrap().sexprs.iter()
                        {
                            if let SExpr::Assign(_, n, v) = s
                            {
                                if n == i.0
                                {
                                    if let SExpr::Function(_, f) = &**v
                                    {
                                        if ir.modules.get(&import.1.name).unwrap().funcs.get(f).unwrap().impure
                                        {
                                            impure = true;
                                        }
                                    }
                                    break;
                                }
                            }
                        }
                        module.scope.put_var(i.0, &i.1.1, 0, None, &i.1.0, true, &import.1.name);
                        import.1.imports.insert(i.0.clone(), (i.1.1.clone(), 0, impure));
                    }
                }
            }

        // Import qualified
        } else
        {
            for i in ir.modules.get(&import.1.name).unwrap().exports.iter()
            {
                let mut impure = false;
                for s in ir.modules.get(&import.1.name).unwrap().sexprs.iter()
                {
                    if let SExpr::Assign(_, n, v) = s
                    {
                        if n == i.0
                        {
                            if let SExpr::Function(_, f) = &**v
                            {
                                if ir.modules.get(&import.1.name).unwrap().funcs.get(f).unwrap().impure
                                {
                                    impure = true;
                                }
                            }
                            break;
                        }
                    }
                }
                import.1.imports.insert(i.0.clone(), (i.1.1.clone(), 0, impure));
            }
        }
    }
}

// check_correctness(&mut IR, bool) -> ()
// Checks the correctness of module.
pub fn check_correctness(ir: &mut IR, require_main: bool) -> Result<(), Vec<CorrectnessError>>
{
    // Set up
    let mut errors = Vec::with_capacity(0);
    let keys: Vec<String> = ir.modules.iter().map(|v| v.0.clone()).collect();

    // Check types
    for module in ir.modules.iter_mut()
    {
        check_type_validity(module.1, &mut errors);
    }

    for name in keys
    {
        // Get module
        let mut module = ir.modules.remove(&name).unwrap();

        // Check the module
        check_module(&mut module, ir, &mut errors);

        // Collect globals
        for sexpr in module.sexprs.iter()
        {
            if let SExpr::Assign(m, a, _) = sexpr
            {
                if m._type != Type::Error
                {
                    module.scope.put_var(a, &m._type, 0, None, &m.loc, false, &module.name);
                }
            }
        }

        // Check globals
        check_globals(&mut module, &mut errors);

        // Check sexpressions
        let mut sexprs = Vec::with_capacity(0);
        swap(&mut module.sexprs, &mut sexprs);
        for sexpr in sexprs.iter_mut()
        {
            convert_function_symbols(sexpr, &mut HashSet::from_iter(module.funcs.iter().map(|v| v.0.clone())));
            check_sexpr(sexpr, &mut module, &mut errors);
            check_externals(sexpr, &mut errors);
        }
        swap(&mut module.sexprs, &mut sexprs);
        ir.modules.insert(name, module);
    }

    if require_main
    {
        let mut has_main = false;
        for module in ir.modules.iter()
        {
            if module.1.name == "Main" && module.1.scope.variables.contains_key("main")
            {
                has_main = true;
            }
        }

        if !has_main
        {
            errors.push(CorrectnessError::NoMainFunction);
        }
    }

    // Return error if they exist, otherwise return success
    if errors.len() == 0
    {
        let keys: Vec<String> = ir.modules.keys().cloned().collect();
        for name in keys
        {
            let mut module = ir.modules.remove(&name).unwrap();

            // Save types
            let mut id = 0;
            while let Some(_) = module.types.get(&format!("{}", id))
            {
                id += 1;
            }

            for sexpr in module.sexprs.iter()
            {
                save_types(sexpr, &mut module.types, &mut id);
            }

            for f in module.funcs.iter()
            {
                save_types(&f.1.body, &mut module.types, &mut id);
            }

            // Check for tail recursion
            for f in module.funcs.iter_mut()
            {
                check_tailrec(&mut f.1.body, &f.0, true);
            }

            // Import functions
            for import in module.imports.iter_mut()
            {
                // Fix arity
                let imported_mod = ir.modules.get(&import.1.name).unwrap();
                for i in import.1.imports.iter_mut()
                {
                    i.1.1 = imported_mod.scope.get_var(i.0).unwrap().1;
                }

                // Add functions
                for i in import.1.imports.iter()
                {
                    let mut func = IRFunction {
                        args: std::iter::once((String::with_capacity(0), Type::Unknown)).cycle().take(i.1.1).collect(),
                        loc: Location::empty(),
                        captured: HashMap::with_capacity(0),
                        captured_names: Vec::with_capacity(0),
                        body: SExpr::True(SExprMetadata::empty()),
                        global: true,
                        checked: true,
                        written: true,
                        impure: i.1.2
                    };
                    func.body.get_mutable_metadata()._type = i.1.0.clone();
                    module.funcs.insert(i.0.clone(), func);
                    if let Some(var) = module.scope.variables.get_mut(i.0)
                    {
                        var.1 = i.1.1;
                        var.2 = Some(0);
                    } else
                    {
                        module.scope.variables.insert(i.0.clone(), (i.1.0.clone(), i.1.1, Some(0), Location::empty(), true, String::with_capacity(0)));
                    }
                }
            }

            // Fix arity
            for sexpr in module.sexprs.iter_mut()
            {
                fix_arity(sexpr, &mut module.scope);
            }

            for func in module.funcs.iter_mut()
            {
                fix_arity(&mut func.1.body, &mut module.scope);
            }

            // Eta reduced functions are optimised away
            for sexpr in module.sexprs.iter_mut()
            {
                if let SExpr::Assign(_, n, v) = sexpr
                {
                    if let SExpr::Application(m, _, _) = &**v
                    {
                        if let Some(func) = module.funcs.get_mut(n)
                        {
                            if m.arity != 0 && func.args.len() == 0
                            {
                                let mut arity = m.arity;
                                while arity > 0
                                {
                                    if let Type::Func(a, r) = &func.body.get_metadata()._type
                                    {
                                        func.args.push((format!("$${}", arity), *a.clone()));
                                        let mut temp = SExpr::True(SExprMetadata::empty());
                                        let _type = *r.clone();
                                        swap(&mut func.body, &mut temp);
                                        func.body = SExpr::Application(SExprMetadata::empty(), Box::new(temp), Box::new(SExpr::Symbol(SExprMetadata::empty(), format!("$${}", arity))));
                                        func.body.get_mutable_metadata()._type = _type;
                                        arity -= 1;
                                    } else
                                    {
                                        unreachable!("nya");
                                    }
                                }

                                // Update variable
                                let var = module.scope.variables.get_mut(n).unwrap();
                                var.1 = func.args.len();
                                var.2 = Some(0);

                                // Transfer metadata
                                let mut meta = SExprMetadata::empty();
                                let n = n.clone();
                                swap(&mut meta, sexpr.get_mutable_metadata());
                                *sexpr = SExpr::Function(meta, n);
                            }
                        }
                    }
                }
            }

            // Fix arity (again)
            for sexpr in module.sexprs.iter_mut()
            {
                fix_arity(sexpr, &mut module.scope);
            }

            for func in module.funcs.iter_mut()
            {
                fix_arity(&mut func.1.body, &mut module.scope);
            }

            // Check for impurity in pure functions
            for f in module.funcs.iter()
            {
                if f.1.written
                {
                    continue;
                }

                if let Err(s) = check_purity(&f.1.body, &module)
                {
                    if !f.1.impure
                    {
                        errors.push(CorrectnessError::ImpureInPure(f.1.loc.clone(), s));
                    }
                } else if f.1.impure
                {
                    errors.push(CorrectnessError::UnnecessaryImpure(f.1.loc.clone()));
                }
            }

            // Reinsert module
            ir.modules.insert(name, module);
        }

        if errors.len() != 0
        {
            Err(errors)
        } else
        {
            Ok(())
        }
    } else
    {
        Err(errors)
    }
}

