use logos::Span;

use super::ir::{BinOp, IR, IRMetadata, PrefixOp, SExpr};
use super::scopes::FunctionName;
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
    SymbolRedeclared(Span, String)
}

// check_sexpr(&mut SExpr, &mut SExprMetadata) -> ()
// Checks an s expression for type correctness and correct symbol usage.
fn check_sexpr(sexpr: &mut SExpr, root: &mut IRMetadata, errors: &mut Vec<CorrectnessError>)
{
    match sexpr
    {
        // Values
        SExpr::Int(_, _) => (),
        SExpr::Float(_, _) => (),
        SExpr::String(_, _) => (),
        SExpr::True(_) => (),
        SExpr::False(_) => (),

        // Symbols
        SExpr::Symbol(m, s) => {
            match root.scope.variables.get(s)
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
                    if v.get_metadata()._type == Type::Unknown
                    {
                        return;
                    }

                    // Get type
                    if let Some(t) = root.scope.func_ret_types.get(&FunctionName::Prefix(v.get_metadata()._type.clone()))
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
            if left.get_metadata()._type == Type::Unknown || right.get_metadata()._type == Type::Unknown
            {
                return;
            }

            // Get type
            if let Some(t) = root.scope.func_ret_types.get(&FunctionName::Infix(
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
            if left.get_metadata()._type == Type::Unknown || right.get_metadata()._type == Type::Unknown
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
        SExpr::If(_, cond, then, elsy) => {
            // Check child nodes
            check_sexpr(cond, root, errors);
            check_sexpr(then, root, errors);
            check_sexpr(elsy, root, errors);

            // Check if an error occured
            if cond.get_metadata()._type == Type::Unknown || then.get_metadata()._type == Type::Unknown || elsy.get_metadata()._type == Type::Unknown
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
            }
        }

        SExpr::Assign(m, name, value) => {
            // Check child node
            check_sexpr(value, root, errors);

            // Check if an error occured
            if value.get_metadata()._type == Type::Unknown
            {
                return;
            }

            // Check that the types match
            match m._type
            {
                // No preassigned type
                Type::Unknown => m._type = value.get_metadata()._type.clone(),

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
                        m._type = Type::Unknown;
                    }
                }
            }

            // Add variable to scope if no error occured
            if m._type != Type::Unknown
            {
                root.scope.variables.insert(name.clone(), m._type.clone());
            }
        }

        _ => panic!("unsupported s expression!")
    }
}

// check_correctness(&mut IR) -> ()
// Checks the correctness of ir.
pub fn check_correctness(ir: &mut IR) -> Result<(), Vec<CorrectnessError>>
{
    let mut errors = Vec::with_capacity(0);
    for sexpr in &mut ir.sexprs
    {
        check_sexpr(sexpr, &mut ir.metadata, &mut errors);
    }

    if errors.len() == 0
    {
        Ok(())
    } else
    {
        Err(errors)
    }
}
