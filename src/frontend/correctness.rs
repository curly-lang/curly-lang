use logos::Span;
use std::collections::{HashMap, HashSet};
use std::mem::swap;
use std::rc::Rc;

use super::ir::{
    BinOp, IRExtern, IRFunction, IRImport, IRModule, Location, PrefixOp, SExpr, SExprMetadata, IR,
};
use super::scopes::{FunctionName, Scope};
use super::types::{HashSetWrapper, Type, TypeRc};

#[derive(Debug)]
pub enum CorrectnessError {
    UndefinedPrefixOp(Location, PrefixOp, TypeRc),
    UndefinedInfixOp(Location, BinOp, TypeRc, TypeRc),
    NonboolInBoolExpr(Location, TypeRc),
    NonboolInIfCond(Location, TypeRc),
    NonmatchingAssignTypes(Location, TypeRc, Location, TypeRc),
    SymbolNotFound(Location, String),
    Reassignment(Location, Location, String),
    InvalidType(Location),
    DuplicateTypeInUnion(Location, Location, TypeRc),
    UnknownFunctionReturnType(Location, String),
    MismatchedFunctionArgType(Location, TypeRc, TypeRc),
    InvalidApplication(Location, TypeRc),
    InvalidCast(Location, TypeRc, Location, TypeRc),
    NonSubtypeOnMatch(Location, TypeRc, Location, TypeRc),
    InfiniteSizedType(Location, TypeRc),
    NonmemberAccess(Location, String, String),
    MismatchedDeclarationAssignmentTypes(Location, TypeRc, Location, TypeRc),
    VariableImportedTwice(Location, Location),
    ImportedValueNotExported(Location, String, String),
    NoMainFunction,
    CurriedExternalFunc(Location),
    ImpureInPure(Location, Location),
    UnnecessaryImpure(Location),
    AppliedImpureToPure(Location),
    ModuleNotFound(Location, String),
    UnimplementedExport(Location, String),
}

// check_sexpr(&mut SExpr, &mut IRModule, &mut Vec<CorrectnessError>) -> ()
// Checks an s expression for type correctness and correct symbol usage.
fn check_sexpr(sexpr: &mut SExpr, module: &mut IRModule, errors: &mut Vec<CorrectnessError>) {
    if let Type::UndeclaredTypeError(s) = &*sexpr.get_metadata()._type {
        let s = s.clone();
        sexpr.get_mutable_metadata()._type = Rc::new(Type::Error);
        errors.push(CorrectnessError::InvalidType(s));
        return;
    } else if let Type::DuplicateTypeError(s1, s2, t) = &*sexpr.get_metadata()._type {
        let s1 = s1.clone();
        let s2 = s2.clone();
        let t = t.clone();
        sexpr.get_mutable_metadata()._type = Rc::new(Type::Error);
        errors.push(CorrectnessError::DuplicateTypeInUnion(
            s1,
            s2,
            t
        ));
        return;
    }

    match sexpr {
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
        SExpr::Function(m, f) => match module.scope.get_var(f) {
            Some(t) => {
                m._type = t.0.clone();
                m.arity = t.1;
                m.saved_argc = t.2;
                m.origin = t.5.clone();

                if module.funcs.get(f).is_some() && !module.funcs.get(f).unwrap().checked {
                    let mut func = module.funcs.remove(f).unwrap();
                    module.scope.push_scope(true);
                    for a in func.args.iter() {
                        module
                            .scope
                            .put_var(&a.0, &a.1, 0, None, &Location::empty(), true, "");
                    }
                    check_sexpr(&mut func.body, module, errors);
                    module.scope.pop_scope();
                    module.funcs.insert(f.clone(), func);
                }

                if *m._type == Type::Unknown {
                    module.funcs.remove(f);
                }
            }

            None => {
                if let Some(mut func) = module.funcs.remove(f) {
                    if !func.checked {
                        check_function_body(
                            f,
                            f,
                            &module.name,
                            &mut func,
                            &mut module.scope,
                            &mut module.funcs,
                            errors,
                            &module.types,
                            &module.externals,
                            &module.imports,
                        );

                        module.scope.push_scope(true);
                        for a in func.args.iter() {
                            if a.0 != "_" {
                                module.scope.put_var(
                                    &a.0,
                                    &a.1,
                                    0,
                                    None,
                                    &Location::empty(),
                                    true,
                                    "",
                                );
                            }
                        }
                        check_sexpr(&mut func.body, module, errors);
                        module.scope.pop_scope();
                    }

                    if let Some(v) = module.scope.get_var(f) {
                        m._type = v.0.clone();
                    }

                    m.origin = module.name.clone();
                    m.arity = func.args.len();
                    m.saved_argc = Some(func.captured.len());
                    module.funcs.insert(f.clone(), func);
                }
            }
        },

        // Symbols
        SExpr::Symbol(m, s) => {
            match module.scope.get_var(s) {
                Some(t) => {
                    m._type = t.0.clone();
                    m.arity = t.1;
                    m.saved_argc = t.2;
                    m.origin = t.5.clone();
                }

                None => {
                    // External functions
                    if let Some(v) = module.externals.get(s) {
                        m.arity = v.arg_types.len();
                        m.saved_argc = Some(0);
                        m._type = v.ret_type.clone();

                        for t in v.arg_types.iter().rev() {
                            let mut _type = Rc::new(Type::Unknown);
                            swap(&mut _type, &mut m._type);
                            m._type = Rc::new(Type::Func(t.clone(), _type));
                        }

                        let mut temp = SExprMetadata::empty();
                        swap(&mut temp, m);
                        *sexpr =
                            SExpr::ExternalFunc(temp, v.extern_name.clone(), Vec::with_capacity(0));
                    } else {
                        errors.push(CorrectnessError::SymbolNotFound(m.loc.clone(), s.clone()));
                    }
                }
            }
        }

        // Prefix operators
        SExpr::Prefix(m, op, v) => {
            match op {
                // Negative has operators defined in scope
                PrefixOp::Neg => {
                    // Check child node
                    check_sexpr(v, module, errors);

                    // Check if an error occurred
                    if *v.get_metadata()._type == Type::Error {
                        return;
                    }

                    // Get type
                    if let Some(t) = module
                        .scope
                        .get_func_ret(FunctionName::Prefix(v.get_metadata()._type.clone()))
                    {
                        m._type = t.clone();
                    } else {
                        errors.push(CorrectnessError::UndefinedPrefixOp(
                            m.loc.clone(),
                            PrefixOp::Neg,
                            v.get_metadata()._type.clone(),
                        ));
                    }
                }

                PrefixOp::Span => panic!("unsupported operator!"),
            }
        }

        // Infix operators
        SExpr::Infix(m, op, left, right) => {
            // Check child nodes
            check_sexpr(left, module, errors);
            check_sexpr(right, module, errors);

            // Check if an error occurred
            if *left.get_metadata()._type == Type::Error || *right.get_metadata()._type == Type::Error
            {
                return;
            }

            // Get type
            if let Some(t) = module.scope.get_func_ret(FunctionName::Infix(
                *op,
                left.get_metadata()._type.clone(),
                right.get_metadata()._type.clone(),
            )) {
                m._type = t.clone();
            } else {
                errors.push(CorrectnessError::UndefinedInfixOp(
                    m.loc.clone(),
                    *op,
                    left.get_metadata()._type.clone(),
                    right.get_metadata()._type.clone(),
                ));
            }
        }

        SExpr::Chain(m, left, right) => {
            // Add scope if left is a walrus operator
            if let SExpr::Walrus(_, _, _) = **left {
                module.scope.push_scope(false);
            }

            // Check child nodes
            check_sexpr(left, module, errors);
            check_sexpr(right, module, errors);

            if let SExpr::Walrus(_, _, _) = **left {
                module.scope.pop_scope();
            }

            // Check if an error occurred
            if *left.get_metadata()._type == Type::Error || *right.get_metadata()._type == Type::Error
            {
                return;
            }

            m._type = right.get_metadata()._type.clone();
        }

        SExpr::As(m, v) => {
            // Check child node
            check_sexpr(v, module, errors);

            // Check if an error occurred
            if *v.get_metadata()._type == Type::Error {
                return;
            }

            // Assert the types are valid
            if !v.get_metadata()._type.is_subtype(&m._type, &module.types) {
                errors.push(CorrectnessError::InvalidCast(
                    v.get_metadata().loc.clone(),
                    v.get_metadata()._type.clone(),
                    m.loc2.clone(),
                    m._type.clone(),
                ));
                m._type = Rc::new(Type::Error);
            }
        }

        // And and or
        SExpr::And(m, left, right) | SExpr::Or(m, left, right) => {
            // Check child nodes
            check_sexpr(left, module, errors);
            check_sexpr(right, module, errors);

            // Check if an error occurred
            if *left.get_metadata()._type == Type::Error || *right.get_metadata()._type == Type::Error
            {
                return;
            }

            // Assert the types are booleans.
            if *left.get_metadata()._type != Type::Bool {
                errors.push(CorrectnessError::NonboolInBoolExpr(
                    left.get_metadata().loc.clone(),
                    left.get_metadata()._type.clone(),
                ));
            }

            if *right.get_metadata()._type != Type::Bool {
                errors.push(CorrectnessError::NonboolInBoolExpr(
                    right.get_metadata().loc.clone(),
                    right.get_metadata()._type.clone(),
                ));
            }

            if *left.get_metadata()._type == Type::Bool && *right.get_metadata()._type == Type::Bool {
                m._type = Rc::new(Type::Bool);
            }
        }

        // If expressions
        SExpr::If(m, cond, then, elsy) => {
            // Check child nodes
            check_sexpr(cond, module, errors);
            check_sexpr(then, module, errors);
            check_sexpr(elsy, module, errors);

            // Check if an error occurred
            if *cond.get_metadata()._type == Type::Error
                || *then.get_metadata()._type == Type::Error
                || *elsy.get_metadata()._type == Type::Error
            {
                return;
            }

            // Check that condition is a boolean
            if *cond.get_metadata()._type != Type::Bool {
                errors.push(CorrectnessError::NonboolInIfCond(
                    cond.get_metadata().loc.clone(),
                    cond.get_metadata()._type.clone(),
                ));
            }

            // Check that the types of the then and else blocks match
            if then
                .get_metadata()
                ._type
                .is_subtype(&elsy.get_metadata()._type, &module.types)
            {
                m._type = elsy.get_metadata()._type.clone()
            } else if elsy
                .get_metadata()
                ._type
                .is_subtype(&then.get_metadata()._type, &module.types)
            {
                m._type = then.get_metadata()._type.clone()
            } else if then.get_metadata()._type != elsy.get_metadata()._type {
                let mut set = Vec::with_capacity(0);
                if let Type::Sum(v) = &*then.get_metadata()._type {
                    set = v.0.iter().collect();
                } else {
                    set.push(&then.get_metadata()._type);
                }

                if let Type::Sum(v) = &*elsy.get_metadata()._type {
                    for v in v.0.iter() {
                        set.push(v);
                    }
                } else {
                    set.push(&elsy.get_metadata()._type);
                }

                m._type = Rc::new(Type::Sum(HashSetWrapper(set.into_iter().cloned().collect())));
            } else {
                m._type = then.get_metadata()._type.clone();
            }

            if then.get_metadata().saved_argc == elsy.get_metadata().saved_argc {
                m.saved_argc = then.get_metadata().saved_argc;
            }

            if then.get_metadata().arity == elsy.get_metadata().arity {
                m.arity = then.get_metadata().arity;
            }
        }

        SExpr::Application(m, func, arg) => {
            // Check the function and arg types
            check_sexpr(func, module, errors);
            check_sexpr(arg, module, errors);

            // Check if either the function or argument resulted in an error
            if *func.get_metadata()._type == Type::Error || *arg.get_metadata()._type == Type::Error {
                return;
            }

            // Quick hack for debug functions
            // TODO: Generic types so this isn't implemented as a hack
            if let SExpr::Symbol(_, v) = &**func {
                if v == "debug" {
                    m._type = arg.get_metadata()._type.clone();
                    return;
                }

            // External functions
            } else if let SExpr::ExternalFunc(m1, name, args) = &mut **func {
                if let Type::Func(l, r) = &*m1._type {
                    if l.equals(&arg.get_metadata()._type, &module.types) {
                        let mut temp = SExpr::True(SExprMetadata::empty());
                        swap(&mut temp, arg);
                        args.push(temp);
                        m1.loc.span.end = m.loc.span.end;
                        m1._type = r.clone();
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
                    } else {
                        errors.push(CorrectnessError::MismatchedFunctionArgType(
                            arg.get_metadata().loc.clone(),
                            l.clone(),
                            arg.get_metadata()._type.clone(),
                        ));
                        return;
                    }
                }
            }

            let mut _type = &*func.get_metadata()._type;
            while let Type::Symbol(s) = _type {
                _type = module.types.get(s).unwrap();
            }

            // Match the function
            match _type {
                // Strings concatenate to other strings
                // Type::String => {
                //    m._type = Type::String;
                // }

                // Functions apply their arguments
                Type::Func(l, r) => {
                    if arg.get_metadata()._type.is_subtype(l, &module.types) {
                        m._type = r.clone();
                        m.arity = if func.get_metadata().arity > 0 {
                            func.get_metadata().arity - 1
                        } else {
                            0
                        };

                        if let Some(v) = func.get_metadata().saved_argc {
                            if let Type::Enum(_) = **l {
                                m.saved_argc = Some(v);
                            } else {
                                m.saved_argc = Some(v + 1);
                            }
                        }
                    } else {
                        errors.push(CorrectnessError::MismatchedFunctionArgType(
                            arg.get_metadata().loc.clone(),
                            l.clone(),
                            arg.get_metadata()._type.clone(),
                        ));
                    }
                }

                // Everything else is invalid
                _ => {
                    errors.push(CorrectnessError::InvalidApplication(
                        func.get_metadata().loc.clone(),
                        func.get_metadata()._type.clone(),
                    ));
                }
            }
        }

        // Assignments
        SExpr::Assign(m, name, value) => {
            // Check child node
            check_sexpr(value, module, errors);
            if *value.get_metadata()._type == Type::Error {
                return;
            }

            // Check if variable already exists
            if let Some(v) = module.scope.variables.get(name) {
                if v.4 {
                    errors.push(CorrectnessError::Reassignment(
                        m.loc.clone(),
                        v.3.clone(),
                        name.clone(),
                    ));
                    if let SExpr::Function(_, f) = &**value {
                        if f != name {
                            module.scope.variables.remove(f);
                        }
                        module.funcs.remove(f);
                    }
                    m._type = Rc::new(Type::Error);
                    return;
                } else if *m._type != Type::Error && !m._type.is_subtype(&v.0, &module.types) {
                    errors.push(CorrectnessError::MismatchedDeclarationAssignmentTypes(
                        v.3.clone(),
                        v.0.clone(),
                        m.loc.clone(),
                        m._type.clone(),
                    ));
                    m._type = Rc::new(Type::Error);
                    return;
                } else if *m._type == Type::Error
                    && !value.get_metadata()._type.is_subtype(&v.0, &module.types)
                {
                    errors.push(CorrectnessError::MismatchedDeclarationAssignmentTypes(
                        v.3.clone(),
                        v.0.clone(),
                        value.get_metadata().loc.clone(),
                        value.get_metadata()._type.clone(),
                    ));
                    m._type = Rc::new(Type::Error);
                    return;
                }

                if m._type != v.0 {
                    m._type = v.0.clone();
                }
            }

            // Check if variable value is unknown type
            if *value.get_metadata()._type == Type::Unknown {
                module.scope.variables.remove(name);
                return;
            }

            // Check that the types match
            match *m._type {
                // No preassigned type
                Type::Error => m._type = value.get_metadata()._type.clone(),

                // Preassigned type
                _ => {
                    // Types do not match
                    if !value
                        .get_metadata()
                        ._type
                        .is_subtype(&m._type, &module.types)
                    {
                        errors.push(CorrectnessError::NonmatchingAssignTypes(
                            m.loc2.clone(),
                            m._type.clone(),
                            value.get_metadata().loc.clone(),
                            value.get_metadata()._type.clone(),
                        ));
                        m._type = Rc::new(Type::Error);
                    }
                }
            }

            // Add variable to scope if no error occurred
            if *m._type != Type::Error && name != "_" {
                module.scope.put_var(
                    name,
                    &m._type,
                    value.get_metadata().arity,
                    value.get_metadata().saved_argc,
                    &Location::new(
                        Span {
                            start: m.loc.span.start,
                            end: value.get_metadata().loc.span.start,
                        },
                        &m.loc.filename,
                    ),
                    true,
                    &module.name,
                );
            }
        }

        // With expressions
        SExpr::With(m, assigns, body) => {
            // Push a new scope
            module.scope.push_scope(false);

            // Function iterator
            let iter = assigns.iter().filter_map(|a| {
                if let SExpr::Assign(_, s, a) = &a {
                    if let SExpr::Function(_, v) = &**a {
                        Some((v.clone(), s.clone()))
                    } else {
                        None
                    }
                } else {
                    None
                }
            });

            // Deal with functions
            check_function_group(iter, module, errors);

            // Check assignments
            for a in assigns {
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

            if name != "_" {
                module.scope.put_var(
                    &name,
                    &m._type,
                    m.arity,
                    m.saved_argc,
                    &m.loc,
                    true,
                    &module.filename,
                );
            }
        }

        SExpr::Match(m, value, arms) => {
            // Check value
            check_sexpr(value, module, errors);
            let _type = &value.get_metadata()._type;
            if **_type == Type::Error {
                return;
            } else if let Type::UndeclaredTypeError(_) = **_type {
                return;
            }

            // Check match arms
            module.scope.push_scope(false);
            let mut returned = Type::Sum(HashSetWrapper(HashSet::with_capacity(0)));
            let mut last = None;
            for arm in arms.iter_mut() {
                // Get name of symbol
                let (name, atype) = if let Type::Tag(s, t) = &*arm.0 {
                    let vtype = if let Type::Tag(_, t) = &**t {
                        t
                    } else {
                        t
                    };
                    module
                        .scope
                        .put_var(s, vtype, 0, None, &arm.1.get_metadata().loc2, true, "");
                    (Some(s), t)
                } else {
                    (None, &arm.0)
                };

                if let Type::UndeclaredTypeError(s) = &*arm.0 {
                    errors.push(CorrectnessError::InvalidType(s.clone()));
                    module.scope.pop_scope();
                    continue;
                }

                // Nonsubtypes are errors
                if !atype.is_subtype(&_type, &module.types) || atype.equals(_type, &module.types) {
                    errors.push(CorrectnessError::NonSubtypeOnMatch(
                        value.get_metadata().loc.clone(),
                        _type.clone(),
                        arm.2.clone(),
                        arm.0.clone(),
                    ));
                    module.scope.pop_scope();
                    return;
                }

                // Check body of match arm
                check_sexpr(&mut arm.1, module, errors);

                // Remove variable
                if let Some(s) = name {
                    module.scope.variables.remove(s);
                }

                // Check for error
                if *arm.1.get_metadata()._type == Type::Error {
                    last = Some(Rc::new(Type::Error));
                    continue;
                }

                // Unwrap symbol
                let mut _type = arm.1.get_metadata()._type.clone();

                // Add types
                if returned.is_subtype(&_type, &module.types)
                    && (last.is_none() || last.as_ref().unwrap().is_subtype(&_type, &module.types))
                {
                    last = Some(_type);
                } else if last.is_some() && **last.as_ref().unwrap() == Type::Error {
                } else if let Type::Sum(set) = &mut returned {
                    if let Some(mut t) = last {
                        while let Type::Symbol(s) = &*t {
                            t = module.types.get(s).unwrap().clone();
                        }
                        if let Type::Sum(v) = &*t {
                            for v in v.0.iter() {
                                set.0.insert(v.clone());
                            }
                        } else {
                            set.0.insert(t);
                        }
                        last = None;
                    }

                    while let Type::Symbol(s) = &*_type {
                        _type = module.types.get(s).unwrap().clone();
                    }
                    if let Type::Sum(v) = &*_type {
                        for v in v.0.iter() {
                            set.0.insert(v.clone());
                        }
                    } else {
                        set.0.insert(_type);
                    }
                }
            }

            module.scope.pop_scope();
            m._type = if let Some(v) = last {
                v
            } else if let Type::Sum(v) = returned {
                if v.0.len() == 1 {
                    v.0.into_iter().next().unwrap()
                } else {
                    Rc::new(Type::Sum(v))
                }
            } else {
                unreachable!("if youre here you dont deserve a single uwu");
            };
        }

        SExpr::MemberAccess(m, a) => {
            // Get module
            let mut module_name = String::new();
            let mut pos = 0;
            let types = &module.types;
            for a in a.iter().enumerate() {
                if !module_name.is_empty() {
                    module_name.push_str("::");
                }
                module_name.push_str(a.1);

                if module.imports.contains_key(&module_name) {
                    pos = a.0 + 1;
                }
            }

            if pos > 0 {
                module_name.clear();
                for a in a.iter().enumerate() {
                    if a.0 == pos {
                        break;
                    }

                    if !module_name.is_empty() {
                        module_name.push_str("::");
                    }
                    module_name.push_str(a.1);
                }
                m.origin = module.imports.get(&module_name).unwrap().name.clone();

                if pos < a.len() {
                    if let Some(v) = module
                        .imports
                        .get(&module_name)
                        .unwrap()
                        .imports
                        .get(&a[pos])
                    {
                        if a.len() > pos + 1 {
                            errors.push(CorrectnessError::NonmemberAccess(
                                m.loc.clone(),
                                format!("{}::{}", module_name, a[pos]),
                                a[pos + 1].clone(),
                            ));
                        } else {
                            m._type = v.0.clone();
                            let mut meta = SExprMetadata::empty();
                            swap(&mut meta, m);
                            *sexpr = SExpr::Function(meta, a[pos].clone());
                        }
                        return;
                    }
                } else {
                    errors.push(CorrectnessError::SymbolNotFound(
                        m.loc.clone(),
                        a[pos - 1].clone(),
                    ));
                    return;
                }
            } else {
                m.origin = module.name.clone();
            }

            // Get type
            if let Some(t) = types.get(&a[pos]) {
                if let Type::Sum(f) = &**t {
                    if f.0.contains(&Type::Enum(a[1].clone())) {
                        if a.len() != 2 {
                            errors.push(CorrectnessError::NonmemberAccess(
                                m.loc.clone(),
                                format!("{}::{}", a[0], a[1]),
                                a[2].clone(),
                            ));
                        } else {
                            m._type = Rc::new(Type::Symbol(a[pos].clone()));
                        }
                    } else if let Some(v) = f.0.iter().find(|v| {
                        if let Type::Tag(t, _) = &***v {
                            t == &a[1]
                        } else {
                            false
                        }
                    }) {
                        if a.len() != 2 {
                            errors.push(CorrectnessError::NonmemberAccess(
                                m.loc.clone(),
                                format!("{}::{}", a[0], a[1]),
                                a[2].clone(),
                            ));
                        } else if let Type::Tag(_, t) = &**v {
                            let mut temp = SExprMetadata::empty();

                            swap(&mut temp, m);
                            temp.arity = 1;
                            temp.saved_argc = Some(0);
                            temp._type = Rc::new(
                                Type::Func(t.clone(), Rc::new(Type::Symbol(a[0].clone()))));

                            *sexpr = SExpr::Function(temp, a.join("::"));
                        }
                    } else {
                        errors.push(CorrectnessError::NonmemberAccess(
                            m.loc.clone(),
                            a[0].clone(),
                            a[1].clone(),
                        ));
                    }
                } else {
                    errors.push(CorrectnessError::NonmemberAccess(
                        m.loc.clone(),
                        a[0].clone(),
                        a[1].clone(),
                    ));
                }
            } else if pos == 0 {
                errors.push(CorrectnessError::SymbolNotFound(
                    m.loc.clone(),
                    a[0].clone(),
                ));
            } else {
                errors.push(CorrectnessError::NonmemberAccess(
                    m.loc.clone(),
                    module_name,
                    a[pos].clone(),
                ));
            }
        }
    }
}

// convert_function_symbols(&mut SExpr, &mut HashSet<String>) -> ()
// Converts function symbols in a sexpression into function references.
fn convert_function_symbols(sexpr: &mut SExpr, funcs: &mut HashMap<String, String>) {
    match sexpr {
        // Check symbol for functions
        SExpr::Symbol(m, s) => {
            if funcs.contains_key(s) {
                let mut meta = SExprMetadata::empty();

                swap(&mut meta, m);
                *sexpr = SExpr::Function(meta, funcs.get(s).unwrap().clone());
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
        | SExpr::Application(_, l, r) => {
            convert_function_symbols(l, funcs);
            convert_function_symbols(r, funcs);
        }

        SExpr::Chain(_, l, r) => {
            convert_function_symbols(l, funcs);

            let removed = if let SExpr::Walrus(_, n, _) = &**l {
                funcs.remove(n)
            } else {
                None
            };

            convert_function_symbols(r, funcs);

            if let SExpr::Walrus(_, n, _) = &**l {
                if let Some(v) = removed {
                    funcs.insert(n.clone(), v);
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
            let mut removed = HashMap::with_capacity(0);

            // Check assigns
            for a in assigns.iter_mut() {
                convert_function_symbols(a, funcs);
            }

            // Remove assignments from function set
            for a in assigns {
                if let SExpr::Assign(_, a, _) = a {
                    if let Some(v) = funcs.remove(a) {
                        removed.insert(a.clone(), v);
                    }
                }
            }

            // Check body
            convert_function_symbols(body, funcs);
            for (a, v) in removed {
                funcs.insert(a, v);
            }
        }

        // Check assignments
        SExpr::Assign(_, _, value) => {
            convert_function_symbols(value, funcs);
        }

        SExpr::Walrus(_, _, value) => {
            convert_function_symbols(value, funcs);
        }

        SExpr::Match(_, v, a) => {
            convert_function_symbols(v, funcs);
            for a in a.iter_mut() {
                convert_function_symbols(&mut a.1, funcs);
            }
        }

        // Ignore everything else
        _ => (),
    }
}

// get_function_type(so many arguments aaaa) -> Type
// Gets the function type, returning Type::Unknown if a type cannot be found.
fn get_function_type(
    sexpr: &SExpr,
    module_name: &str,
    scope: &mut Scope,
    funcs: &mut HashMap<String, IRFunction>,
    errors: &mut Vec<CorrectnessError>,
    captured: &mut HashMap<String, TypeRc>,
    captured_names: &mut Vec<String>,
    types: &HashMap<String, TypeRc>,
    externals: &HashMap<String, IRExtern>,
    imports: &HashMap<String, IRImport>,
) -> TypeRc {
    match sexpr {
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
        | SExpr::ExternalFunc(m, _, _) => m._type.clone(),

        // Lists
        SExpr::List(_, _) => panic!("uwu"),

        // Functions
        SExpr::Function(_, f) => {
            // Check scope
            match scope.get_var(f) {
                Some(v) => v.0.clone(),

                // Check child function
                None => {
                    let mut func = funcs.remove(f).unwrap();
                    check_function_body(
                        f,
                        f,
                        module_name,
                        &mut func,
                        scope,
                        funcs,
                        errors,
                        types,
                        externals,
                        imports,
                    );
                    funcs.insert(f.clone(), func);
                    scope.get_var(f).unwrap().0.clone()
                }
            }
        }

        // Symbols
        SExpr::Symbol(_, s) => {
            // Check scope
            match scope.get_var(s) {
                Some(v) => {
                    // Check if captured
                    if scope.is_captured(s) && !captured.contains_key(s) {
                        captured.insert(s.clone(), v.0.clone());
                        captured_names.push(s.clone());
                    }
                    v.0.clone()
                }

                // Check externals
                None => {
                    if let Some(v) = externals.get(s) {
                        let mut _type = v.ret_type.clone();
                        for t in v.arg_types.iter().rev() {
                            let mut temp = Rc::new(Type::Unknown);
                            swap(&mut temp, &mut _type);
                            _type = Rc::new(Type::Func(t.clone(), temp));
                        }

                        _type
                    } else {
                        Rc::new(Type::Unknown)
                    }
                }
            }
        }

        // Prefix operators
        SExpr::Prefix(_, op, v) => match op {
            PrefixOp::Neg => {
                let vt = get_function_type(
                    v,
                    module_name,
                    scope,
                    funcs,
                    errors,
                    captured,
                    captured_names,
                    types,
                    externals,
                    imports,
                );

                match scope.get_func_ret(FunctionName::Prefix(vt)) {
                    Some(v) => v.clone(),
                    None => Rc::new(Type::Unknown),
                }
            }

            PrefixOp::Span => panic!("unsupported operator"),
        },

        // Infix operators
        SExpr::Infix(_, op, l, r) => {
            let lt = get_function_type(
                l,
                module_name,
                scope,
                funcs,
                errors,
                captured,
                captured_names,
                types,
                externals,
                imports,
            );
            let rt = get_function_type(
                r,
                module_name,
                scope,
                funcs,
                errors,
                captured,
                captured_names,
                types,
                externals,
                imports,
            );

            match scope.get_func_ret(FunctionName::Infix(*op, lt, rt)) {
                Some(v) => v.clone(),
                None => Rc::new(Type::Unknown),
            }
        }

        SExpr::Chain(_, l, r) => {
            if let SExpr::Walrus(_, _, _) = **l {
                scope.push_scope(false);
            }
            get_function_type(
                l,
                module_name,
                scope,
                funcs,
                errors,
                captured,
                captured_names,
                types,
                externals,
                imports,
            );
            let v = get_function_type(
                r,
                module_name,
                scope,
                funcs,
                errors,
                captured,
                captured_names,
                types,
                externals,
                imports,
            );
            if let SExpr::Walrus(_, _, _) = **l {
                scope.pop_scope();
            }
            v
        }

        SExpr::As(m, _) => m._type.clone(),

        // Boolean and/or
        SExpr::And(_, l, r) | SExpr::Or(_, l, r) => {
            get_function_type(
                l,
                module_name,
                scope,
                funcs,
                errors,
                captured,
                captured_names,
                types,
                externals,
                imports,
            );
            get_function_type(
                r,
                module_name,
                scope,
                funcs,
                errors,
                captured,
                captured_names,
                types,
                externals,
                imports,
            );
            Rc::new(Type::Bool)
        }

        // If expressions
        SExpr::If(_, _, body, elsy) => {
            let bt = get_function_type(
                body,
                module_name,
                scope,
                funcs,
                errors,
                captured,
                captured_names,
                types,
                externals,
                imports,
            );
            let et = get_function_type(
                elsy,
                module_name,
                scope,
                funcs,
                errors,
                captured,
                captured_names,
                types,
                externals,
                imports,
            );

            if *bt == Type::Unknown && *et == Type::Unknown {
                Rc::new(Type::Unknown)
            } else if bt.equals(&et, types) || *et == Type::Unknown {
                if let Type::Symbol(_) = *bt {
                    bt
                } else if *et != Type::Unknown {
                    et
                } else {
                    bt
                }
            } else if *bt == Type::Unknown || bt.is_subtype(&et, types) {
                et
            } else if et.is_subtype(&*bt, types) {
                bt
            } else {
                let mut set = Vec::with_capacity(0);
                if let Type::Sum(v) = &*bt {
                    set = v.0.iter().cloned().collect();
                } else {
                    set.push(bt);
                }

                if let Type::Sum(v) = &*et {
                    for v in v.0.iter() {
                        set.push(v.clone());
                    }
                } else {
                    set.push(et);
                }

                Rc::new(Type::Sum(HashSetWrapper(set.into_iter().collect())))
            }
        }

        // Applications
        SExpr::Application(_, f, a) => {
            // Quick hack for debug function
            if let SExpr::Symbol(_, s) = &**f {
                if s == "debug" {
                    return get_function_type(
                        a,
                        module_name,
                        scope,
                        funcs,
                        errors,
                        captured,
                        captured_names,
                        types,
                        externals,
                        imports,
                    );
                }
            }

            // Get function type
            let mut ft = get_function_type(
                f,
                module_name,
                scope,
                funcs,
                errors,
                captured,
                captured_names,
                types,
                externals,
                imports,
            );
            if let Type::Unknown = *ft {
                return Rc::new(Type::Unknown);
            }

            while let Type::Symbol(s) = &*ft {
                ft = types.get(s).unwrap().clone();
            }

            match &*ft {
                // Strings concatenate to other strings
                // Type::String => Type::String,

                // Functions apply their arguments
                Type::Func(_, r) => {
                    get_function_type(
                        a,
                        module_name,
                        scope,
                        funcs,
                        errors,
                        captured,
                        captured_names,
                        types,
                        externals,
                        imports,
                    );
                    r.clone()
                }

                // Everything else is invalid
                _ => Rc::new(Type::Unknown),
            }
        }

        // Assignments
        SExpr::Assign(m, name, value) => {
            let t = if *m._type != Type::Error {
                m._type.clone()
            } else {
                get_function_type(
                    value,
                    module_name,
                    scope,
                    funcs,
                    errors,
                    captured,
                    captured_names,
                    types,
                    externals,
                    imports,
                )
            };

            if name != "_" {
                scope.put_var(
                    &name,
                    &t,
                    0,
                    None,
                    &Location::new(Span { start: 0, end: 0 }, &m.loc.filename),
                    true,
                    "",
                );
            }
            t
        }

        // With expressions
        SExpr::With(_, assigns, body) => {
            // Push scope
            scope.push_scope(false);

            // Populate scope with variable types
            for a in assigns {
                get_function_type(
                    a,
                    module_name,
                    scope,
                    funcs,
                    errors,
                    captured,
                    captured_names,
                    types,
                    externals,
                    imports,
                );
            }

            // Get the function type
            let bt = get_function_type(
                body,
                module_name,
                scope,
                funcs,
                errors,
                captured,
                captured_names,
                types,
                externals,
                imports,
            );

            // Pop the scope and return type
            scope.pop_scope();
            bt
        }

        SExpr::Walrus(m, name, value) => {
            let t = get_function_type(
                value,
                module_name,
                scope,
                funcs,
                errors,
                captured,
                captured_names,
                types,
                externals,
                imports,
            );
            if name != "_" {
                scope.put_var(
                    &name,
                    &t,
                    0,
                    None,
                    &Location::new(Span { start: 0, end: 0 }, &m.loc.filename),
                    true,
                    "",
                );
            }
            t
        }

        SExpr::Match(_, _, arms) => {
            // Check match arms
            scope.push_scope(false);
            let mut returned = Type::Sum(HashSetWrapper(HashSet::with_capacity(0)));
            let mut last: Option<TypeRc> = None;
            for arm in arms.iter() {
                // Check body of match arm
                let name = if let Type::Tag(s, t) = &*arm.0 {
                    scope.put_var(&s, &t, 0, None, &arm.1.get_metadata().loc2, true, "");
                    Some(s)
                } else {
                    None
                };

                let mut _type = get_function_type(
                    &arm.1,
                    module_name,
                    scope,
                    funcs,
                    errors,
                    captured,
                    captured_names,
                    types,
                    externals,
                    imports,
                );

                if let Some(s) = name {
                    scope.variables.remove(s);
                }

                if *_type == Type::Unknown {
                    continue;
                }

                if returned.is_subtype(&_type, types)
                    && (last.is_none() || last.as_ref().unwrap().is_subtype(&_type, types))
                {
                    last = Some(_type);
                } else if last.is_some() && **last.as_ref().unwrap() == Type::Error {
                } else if let Type::Sum(set) = &mut returned {
                    if let Some(mut t) = last {
                        while let Type::Symbol(s) = &*t {
                            t = types.get(s).unwrap().clone();
                        }
                        if let Type::Sum(v) = &*t {
                            for v in v.0.iter() {
                                set.0.insert(v.clone());
                            }
                        } else {
                            set.0.insert(t);
                        }
                        last = None;
                    }

                    while let Type::Symbol(s) = &*_type {
                        _type = types.get(s).unwrap().clone();
                    }
                    if let Type::Sum(v) = &*_type {
                        for v in v.0.iter() {
                            set.0.insert(v.clone());
                        }
                    } else {
                        set.0.insert(_type);
                    }
                }
            }

            scope.pop_scope();

            if let Some(v) = last {
                v
            } else if let Type::Sum(v) = returned {
                if v.0.is_empty() {
                    Rc::new(Type::Unknown)
                } else if v.0.len() == 1 {
                    v.0.into_iter().next().unwrap()
                } else {
                    Rc::new(Type::Sum(v))
                }
             } else {
                unreachable!("if youre here you dont deserve a single uwu");
             }
        }

        SExpr::MemberAccess(_, a) => {
            // Get module
            let mut module_name = String::new();
            let mut pos = 0;
            let types = types;
            for a in a.iter().enumerate() {
                if !module_name.is_empty() {
                    module_name.push_str("::");
                }
                module_name.push_str(a.1);

                if imports.contains_key(&module_name) {
                    pos = a.0 + 1;
                }
            }

            if pos > 0 {
                module_name.clear();
                for a in a.iter().enumerate() {
                    if a.0 == pos {
                        break;
                    }

                    if !module_name.is_empty() {
                        module_name.push_str("::");
                    }
                    module_name.push_str(a.1);
                }

                if pos < a.len() {
                    if let Some(v) = imports.get(&module_name).unwrap().imports.get(&a[pos]) {
                        return if a.len() > pos + 1 {
                            Rc::new(Type::Unknown)
                        } else {
                            v.0.clone()
                        };
                    }
                } else {
                    return Rc::new(Type::Unknown);
                }
            }

            if let Some(t) = types.get(&a[0]) {
                if let Type::Sum(f) = &**t {
                    if f.0.contains(&Type::Enum(a[1].clone())) {
                        if a.len() != 2 {
                            Rc::new(Type::Unknown)
                        } else {
                            t.clone()
                        }
                    } else if let Some(v) = f.0.iter().find(|v| {
                        if let Type::Tag(t, _) = &***v {
                            t == &a[1]
                        } else {
                            false
                        }
                    }) {
                        if a.len() != 2 {
                            Rc::new(Type::Unknown)
                        } else if let Type::Tag(_, t2) = &**v {
                            Rc::new(Type::Func(t2.clone(), t.clone()))
                        } else {
                            unreachable!("always a tag type");
                        }
                    } else {
                        Rc::new(Type::Unknown)
                    }
                } else {
                    Rc::new(Type::Unknown)
                }
            } else {
                Rc::new(Type::Unknown)
            }
        }
    }
}

// check_function_body(&str, &str, &str, &IRFunction, &mut Scope, &HashMap<String, IRFunction>, &mut Vec<CorrectnessError>) -> Vec<(String, Type)>
// Checks a function body and determines the return type of the function.
fn check_function_body(
    name: &str,
    refr: &str,
    module_name: &str,
    func: &mut IRFunction,
    scope: &mut Scope,
    funcs: &mut HashMap<String, IRFunction>,
    errors: &mut Vec<CorrectnessError>,
    types: &HashMap<String, TypeRc>,
    externals: &HashMap<String, IRExtern>,
    imports: &HashMap<String, IRImport>,
) {
    if func.checked {
        return;
    }

    if scope.get_var(name).is_none() && scope.get_var(refr).is_none() {
        // Put function in scope
        scope.put_var_raw(
            String::from(name),
            Rc::new(Type::Unknown),
            func.args.len(),
            None,
            Location::new(Span { start: 0, end: 0 }, &func.loc.filename),
            false,
            String::from(module_name),
        );
        if name != refr {
            scope.put_var_raw(
                String::from(refr),
                Rc::new(Type::Unknown),
                func.args.len(),
                None,
                Location::new(Span { start: 0, end: 0 }, &func.loc.filename),
                false,
                String::from(module_name),
            );
        }
    }

    // Put arguments into scope
    scope.push_scope(true);
    for arg in func.args.iter() {
        if arg.0 != "_" {
            scope.put_var(
                &arg.0,
                &arg.1,
                0,
                None,
                &Location::new(Span { start: 0, end: 0 }, &func.loc.filename),
                true,
                "",
            );
        }
    }

    // Get the type
    let mut captured = HashMap::with_capacity(0);
    let mut captured_names = Vec::with_capacity(0);
    let _type = get_function_type(
        &func.body,
        module_name,
        scope,
        funcs,
        errors,
        &mut captured,
        &mut captured_names,
        types,
        externals,
        imports,
    );
    func.captured = captured;
    func.captured_names = captured_names;
    scope.pop_scope();

    // Push an error if type is unknown
    if *_type == Type::Unknown {
        errors.push(CorrectnessError::UnknownFunctionReturnType(
            func.body.get_metadata().loc.clone(),
            String::from(name),
        ));
    } else {
        // Construct the type
        let mut acc = _type;
        for t in func.args.iter().rev() {
            acc = Rc::new(Type::Func(t.1.clone(), acc));
        }

        if let Some(v) = scope.variables.remove(refr) {
            if *v.0 != Type::Unknown && !v.0.equals(&acc, types) {
                errors.push(CorrectnessError::MismatchedDeclarationAssignmentTypes(
                    v.3,
                    v.0,
                    func.loc.clone(),
                    acc,
                ));
                return;
            }
        }
        if let Some(v) = scope.variables.remove(name) {
            if *v.0 != Type::Unknown && !v.0.equals(&acc, types) {
                errors.push(CorrectnessError::MismatchedDeclarationAssignmentTypes(
                    v.3,
                    v.0,
                    func.loc.clone(),
                    acc,
                ));
                return;
            }
        }

        // Get global scope
        let mut scope = scope;
        loop {
            if scope.parent.is_some() {
                scope = &mut *scope.parent.as_mut().unwrap();
            } else {
                break;
            }
        }

        // Put function type in global scope
        if name != refr {
            scope.put_var_raw(
                String::from(refr),
                acc.clone(),
                func.args.len(),
                Some(func.captured.len()),
                func.loc.clone(),
                false,
                String::from(module_name),
            );
        }
        scope.put_var_raw(
            String::from(name),
            acc,
            func.args.len(),
            Some(func.captured.len()),
            func.loc.clone(),
            false,
            String::from(module_name),
        );
    }
}

// check_function_group(T, &HashMap<String, IRFunction>, &mut IRModule, &mut Vec<CorrectnessError>) -> ()
// Checks the group of functions for return types.
fn check_function_group<T>(names: T, module: &mut IRModule, errors: &mut Vec<CorrectnessError>)
where
    T: Iterator<Item = (String, String)> + Clone,
{
    // Generate function types
    for name in names.clone() {
        // Handle functions
        if let Some(mut func) = module.funcs.remove(&name.0) {
            check_function_body(
                &name.0,
                &name.1,
                &module.name,
                &mut func,
                &mut module.scope,
                &mut module.funcs,
                errors,
                &module.types,
                &module.externals,
                &module.imports,
            );
            module.funcs.insert(name.0, func);
        }
    }

    // Check all function bodies
    for name in names {
        // Remove function
        if let Some(mut func) = module.funcs.remove(&name.0) {
            // Push scope and add arguments
            module.scope.push_scope(true);
            for arg in &func.args {
                if arg.0 != "_" {
                    module.scope.put_var(
                        &arg.0,
                        &arg.1,
                        0,
                        None,
                        &Location::new(Span { start: 0, end: 0 }, &func.loc.filename),
                        true,
                        &module.name,
                    );
                }
            }

            // Check body
            check_sexpr(&mut func.body, module, errors);
            check_externals(&func.body, errors);

            // Pop scope
            module.scope.pop_scope();

            // Reinsert function
            module.funcs.insert(name.0, func);
        }
    }
}

// check_globals(&mut IRModule, Vec<String>, &mut Vec<CorrectnessError>) -> ()
// Checks global function return types.
fn check_globals(module: &mut IRModule, errors: &mut Vec<CorrectnessError>) {
    // Get the set of all global functions and globals
    let mut globals: HashMap<String, String> = module
        .funcs
        .iter()
        .map(|v| (v.1.name.clone(), v.0.clone()))
        .collect();

    // Add imported functions
    for v in module.scope.variables.iter() {
        if !globals.contains_key(v.0) && v.0 != "debug" {
            globals.insert(v.0.clone(), v.0.clone());
        }
    }

    // Iterate over every function
    for func in module.funcs.iter_mut() {
        // Remove arguments
        let mut removed = HashMap::with_capacity(0);
        for a in func.1.args.iter() {
            if let Some(v) = globals.remove(&a.0) {
                removed.insert(a.0.clone(), v);
            }
        }

        convert_function_symbols(&mut func.1.body, &mut globals);

        for (a, v) in removed {
            globals.insert(a, v);
        }
    }

    for sexpr in module.sexprs.iter_mut() {
        convert_function_symbols(sexpr, &mut globals);
    }

    // Check all globals
    let v: Vec<(String, String)> = module
        .funcs
        .iter()
        .filter_map(|v| match v.1.global {
            true => Some((v.0.clone(), v.0.clone())),
            false => None,
        })
        .collect();
    check_function_group(v.into_iter(), module, errors);
}

// save_single_type(&mut usize, &Type, &mut HashMap) -> ()
// Saves one type.
fn save_single_type(id: &mut usize, _type: &TypeRc, types: &mut HashMap<String, TypeRc>) {
    for t in types.iter() {
        if _type.equals(t.1, types) {
            return;
        }
    }

    types.insert(format!("{}", id), _type.clone());
    *id += 1;
}

// save_types(&mut IR) -> ()
// Saves anonymous types.
fn save_types(sexpr: &SExpr, types: &mut HashMap<String, TypeRc>, id: &mut usize) {
    let m = sexpr.get_metadata();
    if let Type::Sum(_) = &*m._type {
        save_single_type(id, &m._type, types);
    } else if let Type::Func(l, r) = &*m._type {
        if let Type::Sum(_) = &**l {
            save_single_type(id, l, types);
        }

        let mut t = r;
        while let Type::Func(l, r) = &**t {
            if let Type::Sum(_) = &**l {
                save_single_type(id, l, types);
            }

            t = r;
        }

        if let Type::Sum(_) = **t {
            save_single_type(id, t, types);
        }
    }

    match sexpr {
        SExpr::Prefix(_, _, v) => {
            save_types(v, types, id);
        }

        SExpr::Infix(_, _, l, r)
        | SExpr::Chain(_, l, r)
        | SExpr::And(_, l, r)
        | SExpr::Or(_, l, r)
        | SExpr::Application(_, l, r) => {
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
            for a in a.iter() {
                save_types(a, types, id);
            }
            save_types(v, types, id);
        }

        SExpr::Walrus(_, _, v) => {
            save_types(v, types, id);
        }

        SExpr::Match(_, v, a) => {
            save_types(v, types, id);
            for a in a.iter() {
                let _type = if let Type::Tag(_, t) = &*a.0 { t } else { &a.0 };

                if let Type::Sum(_) = &**_type {
                    save_single_type(id, &_type, types);
                }
                save_types(&a.1, types, id);
            }
        }

        _ => (),
    }
}

// check_type_validity(&IRModule) -> ()
// Checks whether the given types are valid or not.
fn check_type_validity(module: &IRModule, errors: &mut Vec<CorrectnessError>) {
    for s in module.sexprs.iter() {
        if let SExpr::TypeAlias(m, n) = s {
            match &*m._type {
                Type::Symbol(s) if s == n => {
                    errors.push(CorrectnessError::InfiniteSizedType(
                        m.loc2.clone(),
                        m._type.clone(),
                    ));
                }

                Type::Sum(s) if s.0.contains(&Rc::new(Type::Symbol(n.clone()))) => {
                    errors.push(CorrectnessError::InfiniteSizedType(
                        m.loc2.clone(),
                        m._type.clone(),
                    ));
                }

                _ => (),
            }
        }
    }
}

// is_called(&SExpr, &str) -> bool
// Checks whether a given function is called or not.
fn is_called(sexpr: &SExpr, name: &str) -> bool {
    match sexpr {
        //List(SExprMetadata, Vec<SExpr>),
        SExpr::Function(_, f) => f == name,

        SExpr::Prefix(_, _, v) | SExpr::As(_, v) | SExpr::Assign(_, _, v) => is_called(v, name),

        SExpr::Infix(_, _, l, r)
        | SExpr::Chain(_, l, r)
        | SExpr::Application(_, l, r)
        | SExpr::And(_, l, r)
        | SExpr::Or(_, l, r) => is_called(l, name) || is_called(r, name),

        SExpr::If(_, c, t, e) => is_called(c, name) || is_called(t, name) || is_called(e, name),

        SExpr::With(_, a, v) => {
            for a in a {
                if is_called(a, name) {
                    return true;
                }
            }
            is_called(v, name)
        }

        SExpr::Walrus(_, _, v) => is_called(v, name),

        SExpr::Match(_, v, a) => {
            if is_called(v, name) {
                return true;
            }

            for a in a {
                if is_called(&a.1, name) {
                    return true;
                }
            }

            false
        }

        _ => false,
    }
}

// undo_tailrec(&mut SExpr) -> bool
// Undoes setting tailrec to true.
fn undo_tailrec(sexpr: &mut SExpr) {
    sexpr.get_mutable_metadata().tailrec = false;

    match sexpr {
        //List(SExprMetadata, Vec<SExpr>),
        SExpr::Prefix(_, _, v) | SExpr::As(_, v) | SExpr::Assign(_, _, v) => undo_tailrec(v),

        SExpr::Infix(_, _, l, r)
        | SExpr::Chain(_, l, r)
        | SExpr::Application(_, l, r)
        | SExpr::And(_, l, r)
        | SExpr::Or(_, l, r) => {
            undo_tailrec(l);
            undo_tailrec(r);
        }

        SExpr::If(_, c, t, e) => {
            undo_tailrec(c);
            undo_tailrec(t);
            undo_tailrec(e);
        }
        SExpr::With(_, a, v) => {
            for a in a {
                undo_tailrec(a);
            }
            undo_tailrec(v)
        }

        SExpr::Walrus(_, _, v) => undo_tailrec(v),

        SExpr::Match(_, v, a) => {
            undo_tailrec(v);
            for a in a {
                undo_tailrec(&mut a.1)
            }
        }

        _ => (),
    }
}

// check_tailrec(&mut SExpr, &str) -> bool
// Checks whether a given function is tail recursive. Returns false if the function is called
// outside of a tail call.
fn check_tailrec(sexpr: &mut SExpr, name: &str, top: bool) -> bool {
    match sexpr {
        SExpr::As(m, v) => {
            if !check_tailrec(v, name, top) {
                return false;
            }
            m.tailrec = v.get_metadata().tailrec;
            true
        }

        SExpr::Function(m, f) => {
            if f == name {
                m.tailrec = true;
            }

            true
        }

        SExpr::Chain(m, l, r) => {
            if is_called(l, name) {
                return false;
            }

            if !check_tailrec(r, name, top) {
                return false;
            }

            m.tailrec = r.get_metadata().tailrec;
            true
        }

        SExpr::Walrus(_, _, v) => check_tailrec(v, name, top),

        SExpr::If(m, c, t, e) => {
            if is_called(c, name) {
                return false;
            }

            if !check_tailrec(t, name, top) {
                return false;
            }

            if !check_tailrec(e, name, top) {
                return false;
            }

            if t.get_metadata().tailrec || e.get_metadata().tailrec {
                m.tailrec = true;
            }

            true
        }

        SExpr::Application(m, f, a) => {
            if top ^ (m.arity == 0) {
                return !is_called(f, name) && !is_called(a, name);
            }

            if is_called(a, name) {
                return false;
            }

            if !check_tailrec(f, name, false) {
                return false;
            } else if f.get_metadata().tailrec {
                m.tailrec = true;
            }

            true
        }

        SExpr::Match(m, v, a) => {
            if is_called(v, name) {
                return false;
            }

            for a in a.iter_mut() {
                if !check_tailrec(&mut a.1, name, top) {
                    return false;
                }
            }

            for a in a {
                if a.1.get_metadata().tailrec {
                    m.tailrec = true;
                    return true;
                }
            }

            true
        }

        SExpr::With(m, a, v) => {
            for a in a {
                if is_called(a, name) {
                    return false;
                }
            }

            let b = check_tailrec(v, name, top);
            if v.get_metadata().tailrec {
                m.tailrec = true;
            }
            b
        }

        _ => !is_called(sexpr, name),
    }
}

// fix_arity(&mut SExpr, &mut Scope) -> ()
// Fixes the arity of imported functions.
fn fix_arity(sexpr: &mut SExpr, scope: &mut Scope, funcs: &HashMap<String, IRFunction>) {
    match sexpr {
        SExpr::Symbol(m, s) => {
            if let Some(v) = scope.get_var(s) {
                m.arity = v.1;
                m.saved_argc = v.2;
            }
        }

        SExpr::Function(m, f) => {
            if let Some(v) = funcs.get(f) {
                m.arity = v.args.len();
                m.saved_argc = Some(0);
            }
        }

        SExpr::Prefix(_, _, v)
        | SExpr::As(_, v)
        | SExpr::Assign(_, _, v)
        | SExpr::Walrus(_, _, v) => {
            fix_arity(v, scope, funcs);
        }

        SExpr::Infix(_, _, l, r) | SExpr::And(_, l, r) | SExpr::Or(_, l, r) => {
            fix_arity(l, scope, funcs);
            fix_arity(r, scope, funcs);
        }

        SExpr::Chain(_, l, r) => {
            fix_arity(l, scope, funcs);
            fix_arity(r, scope, funcs);
        }

        SExpr::Application(m, l, r) => {
            fix_arity(l, scope, funcs);

            if l.get_metadata().arity > 0 {
                m.arity = l.get_metadata().arity - 1;
                if let Some(v) = l.get_metadata().saved_argc {
                    if let Type::Enum(_) = *r.get_metadata()._type {
                        m.saved_argc = Some(v);
                    } else {
                        m.saved_argc = Some(v + 1);
                    }
                } else {
                    m.saved_argc = None;
                }
            } else {
                m.arity = 0;
                m.saved_argc = None;
            }

            fix_arity(r, scope, funcs);
        }

        SExpr::ExternalFunc(_, _, v) => {
            for v in v {
                fix_arity(v, scope, funcs);
            }
        }

        SExpr::If(_, c, t, e) => {
            fix_arity(c, scope, funcs);
            fix_arity(t, scope, funcs);
            fix_arity(e, scope, funcs);
        }

        SExpr::With(_, a, v) => {
            scope.push_scope(false);
            for a in a {
                fix_arity(a, scope, funcs);
            }
            fix_arity(v, scope, funcs);
            scope.pop_scope();
        }

        SExpr::Match(_, v, a) => {
            fix_arity(v, scope, funcs);
            scope.push_scope(false);
            for a in a.iter_mut() {
                fix_arity(&mut a.1, scope, funcs)
            }
            scope.pop_scope();
        }

        _ => (),
    }
}

// check_externals(&SExpr, &mut Vec<CorrectnessError>)
// Checks if external functions are applied fully.
fn check_externals(sexpr: &SExpr, errors: &mut Vec<CorrectnessError>) {
    match sexpr {
        SExpr::ExternalFunc(m, _, _) => {
            if m.arity != 0 {
                errors.push(CorrectnessError::CurriedExternalFunc(m.loc.clone()));
            }
        }

        SExpr::Prefix(_, _, v)
        | SExpr::As(_, v)
        | SExpr::Assign(_, _, v)
        | SExpr::Walrus(_, _, v) => {
            check_externals(v, errors);
        }

        SExpr::Infix(_, _, l, r)
        | SExpr::Chain(_, l, r)
        | SExpr::And(_, l, r)
        | SExpr::Or(_, l, r)
        | SExpr::Application(_, l, r) => {
            check_externals(l, errors);
            check_externals(r, errors);
        }
        SExpr::If(_, c, t, e) => {
            check_externals(c, errors);
            check_externals(t, errors);
            check_externals(e, errors);
        }

        SExpr::With(_, a, v) => {
            for a in a {
                check_externals(a, errors);
            }
            check_externals(v, errors);
        }

        SExpr::Match(_, v, a) => {
            check_externals(v, errors);

            for a in a {
                check_externals(&a.1, errors);
            }
        }

        _ => (),
    }
}

// check_purity(&mut SExpr, &IRModule, &mut Vec<CorrectnessError>) -> Result<(), Location>
// Checks the purity of a sexpression, erroring with the location if an impurity is found.
fn check_purity(
    sexpr: &mut SExpr,
    module: &IRModule,
    errors: &mut Vec<CorrectnessError>,
) -> Result<(), Location> {
    match sexpr {
        SExpr::ExternalFunc(m, f, a) => {
            for e in module.externals.iter() {
                if e.1.extern_name == *f {
                    if e.1.impure {
                        m.impure = m.arity > 0;
                        return Err(m.loc.clone());
                    } else {
                        break;
                    }
                }
            }

            for a in a {
                check_purity(a, module, errors)?;
            }
            Ok(())
        }

        SExpr::Function(m, f) => {
            if let Some(f) = module.funcs.get(f) {
                if f.impure {
                    m.impure = !f.args.is_empty();
                    Err(m.loc.clone())
                } else {
                    Ok(())
                }
            } else {
                Ok(())
            }
        }

        SExpr::Prefix(_, _, v)
        | SExpr::As(_, v)
        | SExpr::Assign(_, _, v)
        | SExpr::Walrus(_, _, v) => check_purity(v, module, errors),

        SExpr::Infix(_, _, l, r)
        | SExpr::Chain(_, l, r)
        | SExpr::And(_, l, r)
        | SExpr::Or(_, l, r) => {
            check_purity(l, module, errors)?;
            check_purity(r, module, errors)
        }

        SExpr::Application(m, l, r) => {
            let vl = check_purity(l, module, errors);
            let vr = check_purity(r, module, errors);

            if r.get_metadata().arity > 0 && vr.is_err() && vl.is_ok() {
                errors.push(CorrectnessError::AppliedImpureToPure(m.loc.clone()));
            }
            m.impure = l.get_metadata().impure;

            if let Err(e) = vl {
                Err(e)
            } else {
                vr
            }
        }

        SExpr::If(_, c, t, e) => {
            check_purity(c, module, errors)?;
            check_purity(t, module, errors)?;
            check_purity(e, module, errors)
        }

        SExpr::With(_, a, v) => {
            for a in a {
                check_purity(a, module, errors)?;
            }
            check_purity(v, module, errors)
        }

        SExpr::Match(_, v, a) => {
            check_purity(v, module, errors)?;

            for a in a {
                check_purity(&mut a.1, module, errors)?;
            }
            Ok(())
        }

        _ => Ok(()),
    }
}

// check_imports(&IRModule, &IR, &mut Vec<CorrectnessError>) -> ()
// Checks the validity of imported and exported items in a module.
fn check_module(module: &mut IRModule, ir: &IR, errors: &mut Vec<CorrectnessError>) {
    // Put export types
    for export in module.exports.iter() {
        module.scope.put_var(
            export.0,
            &export.1 .1,
            0,
            None,
            &export.1 .0,
            false,
            &module.name,
        );
    }

    // Put unqualified imports in scope
    for import in module.imports.iter_mut() {
        if ir.modules.get(&import.1.name).is_none() {
            errors.push(CorrectnessError::ModuleNotFound(
                import.1.loc.clone(),
                import.1.name.clone(),
            ));
            continue;
        }

        if !import.1.qualified {
            // Import specific values
            if !import.1.imports.is_empty() {
                let exporter = &ir.modules.get(&import.1.name).unwrap().exports;
                for i in import.1.imports.iter_mut() {
                    if module.scope.variables.contains_key(i.0) {
                        errors.push(CorrectnessError::VariableImportedTwice(
                            module.scope.variables.get(i.0).unwrap().3.clone(),
                            import.1.loc.clone(),
                        ));
                    } else if exporter.contains_key(i.0) {
                        let mut impure = false;
                        for s in ir.modules.get(&import.1.name).unwrap().sexprs.iter() {
                            if let SExpr::Assign(_, n, v) = s {
                                if n == i.0 {
                                    if let SExpr::Function(_, f) = &**v {
                                        if ir
                                            .modules
                                            .get(&import.1.name)
                                            .unwrap()
                                            .funcs
                                            .get(f)
                                            .unwrap()
                                            .impure
                                        {
                                            impure = true;
                                        }
                                    }
                                    break;
                                }
                            }
                        }
                        *i.1 = (exporter.get(i.0).unwrap().1.clone(), 0, impure);
                        module.scope.put_var(
                            i.0,
                            &i.1 .0,
                            0,
                            None,
                            &import.1.loc,
                            true,
                            &import.1.name,
                        );
                    } else {
                        errors.push(CorrectnessError::ImportedValueNotExported(
                            import.1.loc.clone(),
                            i.0.clone(),
                            import.1.name.clone(),
                        ));
                    }
                }

            // Import everything
            } else {
                for i in ir.modules.get(&import.1.name).unwrap().exports.iter() {
                    if module.scope.variables.contains_key(i.0) {
                        errors.push(CorrectnessError::VariableImportedTwice(
                            module.scope.variables.get(i.0).unwrap().3.clone(),
                            i.1 .0.clone(),
                        ));
                    } else {
                        let mut impure = false;
                        for s in ir.modules.get(&import.1.name).unwrap().sexprs.iter() {
                            if let SExpr::Assign(_, n, v) = s {
                                if n == i.0 {
                                    if let SExpr::Function(_, f) = &**v {
                                        if ir
                                            .modules
                                            .get(&import.1.name)
                                            .unwrap()
                                            .funcs
                                            .get(f)
                                            .unwrap()
                                            .impure
                                        {
                                            impure = true;
                                        }
                                    }
                                    break;
                                }
                            }
                        }
                        module
                            .scope
                            .put_var(i.0, &i.1 .1, 0, None, &i.1 .0, true, &import.1.name);
                        import
                            .1
                            .imports
                            .insert(i.0.clone(), (i.1 .1.clone(), 0, impure));
                    }
                }
            }

        // Import qualified
        } else {
            for i in ir.modules.get(&import.1.name).unwrap().exports.iter() {
                let mut impure = false;
                for s in ir.modules.get(&import.1.name).unwrap().sexprs.iter() {
                    if let SExpr::Assign(_, n, v) = s {
                        if n == i.0 {
                            if let SExpr::Function(_, f) = &**v {
                                if ir
                                    .modules
                                    .get(&import.1.name)
                                    .unwrap()
                                    .funcs
                                    .get(f)
                                    .unwrap()
                                    .impure
                                {
                                    impure = true;
                                }
                            }
                            break;
                        }
                    }
                }
                import
                    .1
                    .imports
                    .insert(i.0.clone(), (i.1 .1.clone(), 0, impure));
            }
        }
    }
}

// check_correctness(&mut IR, bool) -> ()
// Checks the correctness of module.
pub fn check_correctness(ir: &mut IR, require_main: bool) -> Result<(), Vec<CorrectnessError>> {
    // Set up
    let mut errors = Vec::with_capacity(0);
    let keys: Vec<String> = ir.modules.iter().map(|v| v.0.clone()).collect();

    // Check types
    for module in ir.modules.iter_mut() {
        check_type_validity(module.1, &mut errors);
    }

    for name in keys {
        if ir.modules.get(&name).unwrap().lib {
            continue;
        }

        // Get module
        let mut module = ir.modules.remove(&name).unwrap();

        // Check the module
        check_module(&mut module, ir, &mut errors);

        // Collect globals
        for sexpr in module.sexprs.iter() {
            if let SExpr::Assign(m, a, _) = sexpr {
                if *m._type != Type::Error {
                    module
                        .scope
                        .put_var(a, &m._type, 0, None, &m.loc, false, &module.name);
                }
            }
        }

        // Check globals
        check_globals(&mut module, &mut errors);

        // Check sexpressions
        let mut sexprs = Vec::with_capacity(0);
        swap(&mut module.sexprs, &mut sexprs);
        for sexpr in sexprs.iter_mut() {
            check_sexpr(sexpr, &mut module, &mut errors);
            check_externals(sexpr, &mut errors);
        }
        swap(&mut module.sexprs, &mut sexprs);

        // Check if all exports are implemented
        for v in module.exports.iter() {
            if let Some(a) = module.scope.variables.get(v.0) {
                if !a.4 {
                    errors.push(CorrectnessError::UnimplementedExport(
                        v.1 .0.clone(),
                        v.0.clone(),
                    ));
                }
            }
        }

        ir.modules.insert(name, module);
    }

    if require_main {
        let mut has_main = false;
        for module in ir.modules.iter() {
            if module.1.name == "Main" && module.1.scope.variables.contains_key("main") {
                has_main = module.1.scope.variables.get("main").unwrap().1 == 0;
                break;
            }
        }

        if !has_main {
            errors.push(CorrectnessError::NoMainFunction);
        }
    }

    // Return error if they exist, otherwise return success
    if errors.is_empty() {
        for (_, module) in ir.modules.iter_mut() {
            // Save types
            let mut id = 0;
            while module.types.get(&format!("{}", id)).is_some() {
                id += 1;
            }

            for sexpr in module.sexprs.iter() {
                save_types(sexpr, &mut module.types, &mut id);
            }

            for f in module.funcs.iter() {
                save_types(&f.1.body, &mut module.types, &mut id);
            }

            // Check for tail recursion
            for f in module.funcs.iter_mut() {
                check_tailrec(&mut f.1.body, &f.0, true);

                if !f.1.body.get_metadata().tailrec {
                    undo_tailrec(&mut f.1.body);
                }
            }
        }

        for _ in 0..2 {
            let keys: Vec<String> = ir.modules.keys().cloned().collect();
            for name in keys {
                let mut module = ir.modules.remove(&name).unwrap();

                // Import functions
                for import in module.imports.iter_mut() {
                    // Fix arity
                    let imported_mod = ir.modules.get(&import.1.name).unwrap();
                    for i in import.1.imports.iter_mut() {
                        i.1 .1 = imported_mod.scope.get_var(i.0).unwrap().1;
                    }

                    // Add functions
                    for i in import.1.imports.iter() {
                        let mut func = IRFunction {
                            args: std::iter::once((String::with_capacity(0), Rc::new(Type::Unknown)))
                                .cycle()
                                .take(i.1 .1)
                                .collect(),
                            name: i.0.clone(),
                            loc: Location::empty(),
                            captured: HashMap::with_capacity(0),
                            captured_names: Vec::with_capacity(0),
                            body: SExpr::True(SExprMetadata::empty()),
                            global: true,
                            checked: true,
                            written: true,
                            impure: i.1 .2,
                        };
                        func.body.get_mutable_metadata()._type = i.1 .0.clone();
                        module.funcs.insert(i.0.clone(), func);
                        if let Some(var) = module.scope.variables.get_mut(i.0) {
                            var.1 = i.1 .1;
                            var.2 = Some(0);
                        } else {
                            module.scope.variables.insert(
                                i.0.clone(),
                                (
                                    i.1 .0.clone(),
                                    i.1 .1,
                                    Some(0),
                                    Location::empty(),
                                    true,
                                    String::with_capacity(0),
                                ),
                            );
                        }
                    }
                }

                // Fix arity
                for sexpr in module.sexprs.iter_mut() {
                    fix_arity(sexpr, &mut module.scope, &module.funcs);
                }

                let keys: Vec<String> = module.funcs.keys().cloned().collect();
                for key in keys {
                    let mut func = module.funcs.remove(&key).unwrap();
                    fix_arity(&mut func.body, &mut module.scope, &module.funcs);
                    module.funcs.insert(key, func);
                }

                // Eta reduced functions are optimised away
                for f in module.funcs.iter_mut() {
                    if f.1.impure {
                        continue;
                    }

                    let func = f.1;
                    if let SExpr::Application(m, _, _) = &func.body {
                        if m.arity > 0 && m.saved_argc.is_some() {
                            let mut arity = m.arity;
                            while arity > 0 {
                                if let Type::Func(a, r) = &*func.body.get_metadata()._type {
                                    func.args.push((format!("$${}", arity), a.clone()));
                                    let mut temp = SExpr::True(SExprMetadata::empty());
                                    let _type = r.clone();
                                    let _t2 = a.clone();
                                    swap(&mut func.body, &mut temp);
                                    func.body = SExpr::Application(
                                        SExprMetadata {
                                            loc: Location::empty(),
                                            loc2: Location::empty(),
                                            origin: String::with_capacity(0),
                                            _type,
                                            arity,
                                            saved_argc: Some(
                                                temp.get_metadata().saved_argc.unwrap() + 1,
                                            ),
                                            tailrec: false,
                                            impure: false,
                                        },
                                        Box::new(temp),
                                        Box::new(SExpr::Symbol(
                                            SExprMetadata {
                                                loc: Location::empty(),
                                                loc2: Location::empty(),
                                                origin: String::with_capacity(0),
                                                _type: _t2,
                                                arity: 0,
                                                saved_argc: None,
                                                tailrec: false,
                                                impure: false,
                                            },
                                            format!("$${}", arity),
                                        )),
                                    );
                                    arity -= 1;
                                } else {
                                    unreachable!("nya");
                                }
                            }

                            // Update variable if available
                            if let Some(var) = module.scope.variables.get_mut(&func.name) {
                                var.1 = func.args.len();
                                var.2 = Some(0);
                            }
                        }
                    }
                }

                // Check for impurity in pure functions
                let keys: Vec<String> = module.funcs.keys().cloned().collect();
                for k in keys {
                    let mut f = module.funcs.remove(&k).unwrap();
                    if f.written {
                        module.funcs.insert(k, f);
                        continue;
                    }

                    if let Err(s) = check_purity(&mut f.body, &module, &mut errors) {
                        if !f.impure {
                            errors.push(CorrectnessError::ImpureInPure(f.loc.clone(), s));
                        }
                    } else if f.impure {
                        errors.push(CorrectnessError::UnnecessaryImpure(f.loc.clone()));
                    }

                    module.funcs.insert(k, f);
                }

                // Reinsert module
                ir.modules.insert(name, module);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    } else {
        Err(errors)
    }
}
