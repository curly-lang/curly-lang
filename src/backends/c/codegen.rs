use crate::frontend::ir::{BinOp, IR, PrefixOp, SExpr};
use crate::frontend::types::Type;

// Represents a function in C.
struct CFunction
{
    // signature: Vec<CType>,
    code: String,
    last_reference: usize
}

fn get_c_type(_type: &Type) -> &str
{
    match _type
    {
        Type::Int => "int",
        Type::Float => "double",
        Type::Bool => "char",
        _ => panic!("unsupported type!")
    }
}

// convert_sexpr(&SExpr, &mut CFunction) -> String
// Converts a s expression into C code.
fn convert_sexpr(sexpr: &SExpr, func: &mut CFunction) -> String
{
    match sexpr
    {
        // Ints
        SExpr::Int(_, n) => {
            // Get name
            let name = format!("_{}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str("int ");
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&format!("{}", n));
            func.code.push_str("; ");

            name
        }

        // Floats
        SExpr::Float(_, n) => {
            // Get name
            let name = format!("_{}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str("double ");
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&format!("{}", n));
            func.code.push_str("; ");

            name
        }

        // Booleans
        SExpr::True(_) => {
            // Get name
            let name = format!("_{}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str("char ");
            func.code.push_str(&name);
            func.code.push_str(" = 1; ");

            name
        }

        SExpr::False(_) => {
            // Get name
            let name = format!("_{}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str("char ");
            func.code.push_str(&name);
            func.code.push_str(" = 0; ");

            name
        }

        SExpr::Symbol(_, s) => {
            s.clone()
        }

        // Prefix
        SExpr::Prefix(m, op, v) => {
            // Get name and value
            let val = convert_sexpr(v, func);
            let name = format!("_{}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str(get_c_type(&m._type));
            func.code.push(' ');
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(match op
            {
                PrefixOp::Neg => "-",
                PrefixOp::Span => panic!("unsupported operator!")
            });
            func.code.push_str(&val);
            func.code.push_str("; ");

            name
        }

        // Infix
        SExpr::Infix(m, op, l, r) => {
            // Get name and operands
            let left = convert_sexpr(l, func);
            let right = convert_sexpr(r, func);
            let name = format!("_{}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str(get_c_type(&m._type));
            func.code.push(' ');
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&left);
            func.code.push(' ');
            func.code.push_str(match op
            {
                BinOp::Mul => "*",
                BinOp::Div => "/",
                BinOp::Mod => "%",
                BinOp::Add => "+",
                BinOp::Sub => "-",
                BinOp::BSL => "<<",
                BinOp::BSR => ">>",
                BinOp::LT => "<",
                BinOp::GT => ">",
                BinOp::LEQ => "<=",
                BinOp::GEQ => ">=",
                BinOp::EQ => "==",
                BinOp::NEQ => "!=",
                BinOp::In => panic!("unsupported operator!"),
                BinOp::And => "&",
                BinOp::Or => "|",
                BinOp::Xor => "^",
                BinOp::BoolAnd => "&&",
                BinOp::BoolOr => "||",
                BinOp::BoolXor => "^"
            });
            func.code.push(' ');
            func.code.push_str(&right);
            func.code.push_str("; ");

            name
        }

        SExpr::If(m, c, b, e) => {
            // Get name
            let name = format!("_{}", func.last_reference);
            func.last_reference += 1;

            // Declare variable
            func.code.push_str(&get_c_type(&m._type));
            func.code.push(' ');
            func.code.push_str(&name);
            func.code.push_str("; ");

            // Get condition
            let cond = convert_sexpr(c, func);
            func.code.push_str("if (");
            func.code.push_str(&cond);
            func.code.push_str(") { ");

            // Get body
            let body = convert_sexpr(b, func);
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&body);
            func.code.push_str("; } else { ");

            // Get else clause
            let elsy = convert_sexpr(e, func);
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&elsy);
            func.code.push_str("; } ");

            name
        }

        SExpr::Assign(m, a, v) => {
            // Get value and generate code
            let val = convert_sexpr(v, func);
            func.code.push_str(get_c_type(&m._type));
            func.code.push(' ');
            func.code.push_str(a);
            func.code.push_str(" = ");
            func.code.push_str(&val);
            func.code.push_str("; ");
            a.clone()
        }

        _ => panic!("unimplemented s expression!")
    }
}

// convert_ir_to_c(&IR, bool) -> String
// Converts Curly IR to C code.
pub fn convert_ir_to_c(ir: &IR, repl_mode: bool) -> String
{
    // Create the main function
    let mut main_func = CFunction {
        // signature: Vec::with_capacity(0),
        code: String::new(),
        last_reference: 0
    };

    // Populate the main function
    let mut last_reference = String::with_capacity(0);
    for s in ir.sexprs.iter()
    {
        last_reference = convert_sexpr(s, &mut main_func);
    }

    // Build the C code for main
    let mut code_string = String::new();
    code_string.push_str("void printf(char*, ...); int main() { ");
    code_string.push_str(&main_func.code);

    // Determine the type to print if in repl mode
    if repl_mode
    {
        if let Some(last) = ir.sexprs.last()
        {
            code_string.push_str("printf(\"");
            code_string.push_str(
                match last.get_metadata()._type
                {
                    Type::Int => "%i",
                    Type::Float => "%0.5f",
                    Type::Bool => "%s",
                    _ => panic!("unsupported type!")
                }
            );
            code_string.push_str("\\n\", ");
            code_string.push_str(&last_reference);

            // Deal with booleans
            if let Type::Bool = last.get_metadata()._type
            {
                code_string.push_str(" ? \"true\" : \"false\"");
            }

            code_string.push_str("); ");
        }
    }

    // End main function
    code_string.push_str("return 0; }");

    code_string
}
