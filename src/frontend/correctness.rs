use super::ir::{BinOp, IR, IRMetadata, PrefixOp, SExpr};
use super::scopes::FunctionName;
use super::types::Type;

#[derive(Debug)]
pub enum CorrectnessError<'a>
{
    UndefinedPrefixOp(PrefixOp, &'a Type),
    UndefinedInfixOp(BinOp, &'a Type, &'a Type)
}

// check_sexpr(&mut SExpr, &mut SExprMetadata) -> ()
// Checks an s expression for type correctness and correct symbol usage.
fn check_sexpr<'a>(sexpr: &'a mut SExpr, root: &mut IRMetadata) -> Result<(), CorrectnessError<'a>>
{
    match sexpr
    {
        SExpr::Int(_, _) => Ok(()),
        SExpr::Float(_, _) => Ok(()),
        SExpr::String(_, _) => Ok(()),

        SExpr::Prefix(m, op, v) => {
            match op
            {
                PrefixOp::Neg => {
                    if let Some(t) = root.scope.func_ret_types.get(&FunctionName::Prefix(v.get_metadata()._type.clone()))
                    {
                        m._type = t.clone();
                        Ok(())
                    } else
                    {
                        Err(CorrectnessError::UndefinedPrefixOp(PrefixOp::Neg, &v.get_metadata()._type))
                    }
                }

                PrefixOp::Span => panic!("unsupported operator!")
            }
        }

        SExpr::Infix(m, op, left, right) => {
            if let Some(t) = root.scope.func_ret_types.get(&FunctionName::Infix(*op, left.get_metadata()._type.clone(), right.get_metadata()._type.clone()))
            {
                m._type = t.clone();
                Ok(())
            } else
            {
                Err(CorrectnessError::UndefinedInfixOp(*op, &left.get_metadata()._type, &right.get_metadata()._type))
            }
        }

        _ => panic!("unsupported s expression!")
    }
}

// check_correctness(&mut IR) -> ()
// Checks the correctness of ir.
pub fn check_correctness(ir: &mut IR) -> Result<(), CorrectnessError>
{
    for sexpr in &mut ir.sexprs
    {
        check_sexpr(sexpr, &mut ir.metadata)?;
    }

    Ok(())
}
