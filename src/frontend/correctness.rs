use super::ir::{IR, IRMetadata, PrefixOp, SExpr};
use super::scopes::FunctionName;

// check_sexpr(&mut SExpr, &mut SExprMetadata) -> ()
// Checks an s expression for type correctness and correct symbol usage.
fn check_sexpr(sexpr: &mut SExpr, root: &mut IRMetadata)
{
    match sexpr
    {
        SExpr::Int(_, _) => (),
        SExpr::Float(_, _) => (),
        SExpr::String(_, _) => (),

        SExpr::Prefix(m, op, v) => {
            match op
            {
                PrefixOp::Neg => {
                    if let Some(t) = root.scope.func_ret_types.get(&FunctionName::Prefix(v.get_metadata()._type.clone()))
                    {
                        m._type = t.clone();
                    }
                }

                PrefixOp::Span => panic!("unsupported operator!")
            }
        }

        SExpr::Infix(m, op, left, right) => {
            if let Some(t) = root.scope.func_ret_types.get(&FunctionName::Infix(*op, left.get_metadata()._type.clone(), right.get_metadata()._type.clone()))
            {
                m._type = t.clone();
            } else
            {
                panic!("unsupported infix operation {:?} {:?} {:?}", left.get_metadata()._type, op, right.get_metadata()._type);
            }
        }

        _ => panic!("unsupported s expression!")
    }
}

// check_correctness(&mut IR) -> ()
// Checks the correctness of ir.
pub fn check_correctness(ir: &mut IR)
{
    for sexpr in &mut ir.sexprs
    {
        check_sexpr(sexpr, &mut ir.metadata);
    }
}
