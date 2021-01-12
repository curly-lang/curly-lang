use crate::frontend::ir::{IR, SExpr};
use crate::frontend::types::Type;

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
    code_string.push_str("int main() { ");
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
                    _ => panic!("unsupported type!")
                }
            );
            code_string.push_str("\", ");
            code_string.push_str(&last_reference);
            code_string.push_str("); ");
        }
    }

    // End main function
    code_string.push_str("return 0; }");

    code_string
}
