use super::ir::{BinOp, IR, IRMetadata, PrefixOp, SExpr};
use super::scopes::FunctionName;
use super::types::Type;

#[derive(Debug)]
pub enum CorrectnessError
{
    UndefinedPrefixOp(PrefixOp, Type),
    UndefinedInfixOp(BinOp, Type, Type),
    NonboolInBoolExpr(Type, Type)
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
                        errors.push(CorrectnessError::UndefinedPrefixOp(PrefixOp::Neg, v.get_metadata()._type.clone()));
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
            if let Some(t) = root.scope.func_ret_types.get(&FunctionName::Infix(*op, left.get_metadata()._type.clone(), right.get_metadata()._type.clone()))
            {
                m._type = t.clone();
            } else
            {
                errors.push(CorrectnessError::UndefinedInfixOp(*op, left.get_metadata()._type.clone(), right.get_metadata()._type.clone()));
            }
        }

        // Boolean and/or
        SExpr::And(_, left, right) | SExpr::Or(_, left, right) => {
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
                errors.push(CorrectnessError::NonboolInBoolExpr(left.get_metadata()._type.clone(), right.get_metadata()._type.clone()));
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
