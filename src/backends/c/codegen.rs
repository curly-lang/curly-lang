use crate::frontend::ir::{IR, SExpr};

// Represents a function in C.
struct CFunction
{
    // signature: Vec<CType>,
    code: String,
    last_reference: usize
}

// convert_sexpr(&SExpr, &mut CFunction) -> String
// Converts a s expression into C code.
fn convert_sexpr(sexpr: &SExpr, func: &mut CFunction) -> String
{
    match sexpr
    {
        SExpr::Int(_, n) => {
            let name = format!("_{}", func.last_reference);
            func.last_reference += 1;

            func.code.push_str("int ");
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&format!("{}", n));
            func.code.push_str("; ");
            func.last_reference += 1;

            name
        }

        SExpr::Float(_, n) => {
            let name = format!("_{}", func.last_reference);
            func.last_reference += 1;

            func.code.push_str("double ");
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&format!("{}", n));
            func.code.push_str("; ");
            func.last_reference += 1;

            name
        }

        _ => panic!("unimplemented s expression!")
    }
}

// convert_ir_to_c(&IR) -> String
// Converts Curly IR to C code.
pub fn convert_ir_to_c(ir: &IR) -> String
{
    // Create the main function
    let mut main_func = CFunction {
        // signature: Vec::with_capacity(0),
        code: String::new(),
        last_reference: 0
    };

    // Populate the main function
    for s in ir.sexprs.iter()
    {
        convert_sexpr(s, &mut main_func);
    }

    // Build the C code
    let mut code_string = String::new();
    code_string.push_str("int main() { ");
    code_string.push_str(&main_func.code);
    code_string.push_str(" return 0; }");

    code_string
}
