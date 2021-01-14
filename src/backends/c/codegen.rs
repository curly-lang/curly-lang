use std::collections::HashMap;

use crate::frontend::ir::{BinOp, IR, PrefixOp, SExpr};
use crate::frontend::types::Type;

// Represents a function in C.
struct CFunction<'a>
{
    args: Vec<(&'a String, &'a Type)>,
    ret_type: &'a Type,
    code: String,
    last_reference: usize
}

// get_c_type(&Type) -> &str
// Converts an IR type into a C type.
fn get_c_type(_type: &Type) -> &str
{
    match _type
    {
        Type::Int => "long long",
        Type::Float => "double",
        Type::Bool => "char",
        Type::Func(_, _) => "func_t",
        _ => panic!("unsupported type!")
    }
}

// convert_sexpr(&SExpr, &IR, &mut CFunction) -> String
// Converts a s expression into C code.
fn convert_sexpr(sexpr: &SExpr, root: &IR, func: &mut CFunction) -> String
{
    match sexpr
    {
        // Ints
        SExpr::Int(_, n) => {
            // Get name
            let name = format!("_{}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str("long long ");
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&format!("{}", n));
            func.code.push_str(";\n");

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
            func.code.push_str(";\n");

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
            func.code.push_str(" = 1;\n");

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

        // Symbols
        SExpr::Symbol(_, s) => {
            s.clone()
        }

        // Functions
        SExpr::Function(_, s) => {
            // Get name
            let name = format!("_{}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str("func_t ");
            func.code.push_str(&name);
            func.code.push_str(" = { 1, (void*) ");
            func.code.push_str(s);
            func.code.push_str(", (void*) 0, ");
            func.code.push_str(&format!("{}", root.funcs.get(s).unwrap().args.len()));
            func.code.push_str(", 0, (void*) 0 };\n");

            name
        }

        // Prefix
        SExpr::Prefix(m, op, v) => {
            // Get name and value
            let val = convert_sexpr(v, root, func);
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
            let left = convert_sexpr(l, root, func);
            let right = convert_sexpr(r, root, func);
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
                BinOp::BoolXor => "^"
            });
            func.code.push(' ');
            func.code.push_str(&right);
            func.code.push_str(";\n");

            name
        }

        // Boolean and
        SExpr::And(m, l, r) => {
            // Get name and left operand
            let left = convert_sexpr(l, root, func);
            let name = format!("_{}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str(get_c_type(&m._type));
            func.code.push(' ');
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&left);
            func.code.push_str(";\nif (");
            func.code.push_str(&name);
            func.code.push_str(") {\n");
            let right = convert_sexpr(r, root, func);
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&right);
            func.code.push_str(";\n}\n");

            name
        }

        // Boolean or
        SExpr::Or(m, l, r) => {
            // Get name and left operand
            let left = convert_sexpr(l, root, func);
            let name = format!("_{}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str(get_c_type(&m._type));
            func.code.push(' ');
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&left);
            func.code.push_str(";\nif (!");
            func.code.push_str(&name);
            func.code.push_str(") {\n");
            let right = convert_sexpr(r, root, func);
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&right);
            func.code.push_str(";\n}\n");

            name
        }

        // If expressions
        SExpr::If(m, c, b, e) => {
            // Get name
            let name = format!("_{}", func.last_reference);
            func.last_reference += 1;

            // Declare variable
            func.code.push_str(&get_c_type(&m._type));
            func.code.push(' ');
            func.code.push_str(&name);
            func.code.push_str(";\n");

            // Get condition
            let cond = convert_sexpr(c, root, func);
            func.code.push_str("if (");
            func.code.push_str(&cond);
            func.code.push_str(") {\n");

            // Get body
            let body = convert_sexpr(b, root, func);
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&body);
            func.code.push_str(";\n} else {\n");

            // Get else clause
            let elsy = convert_sexpr(e, root, func);
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&elsy);
            func.code.push_str(";\n}\n");

            name
        }

        // Applications
        SExpr::Application(m, l, r) => {
            // Get the list of arguments and the function
            let mut args = vec![r];
            let mut f = &**l;
            while let SExpr::Application(_, l, r) = f
            {
                args.insert(0, r);
                f = l;
            }

            match f.get_metadata()._type
            {
                // Functions
                Type::Func(_, _) => {
                    // Get function and args
                    let fstr = convert_sexpr(f, root, func);
                    let mut astrs = vec![];
                    for a in args.iter()
                    {
                        astrs.push(convert_sexpr(a, root, func));
                    }

                    // Get name
                    let name = format!("_{}", func.last_reference);
                    func.last_reference += 1;

                    // Functions with unknown arity
                    if f.get_metadata().arity == 0 || f.get_metadata().saved_argc.is_none()
                    {
                        panic!("calling functions with unknown arity is not yet supported!");

                    // Functions with known arity and fully applied
                    } else if f.get_metadata().saved_argc.is_some() && f.get_metadata().arity <= args.len() + f.get_metadata().saved_argc.unwrap()
                    {
                        // Assignment
                        let saved_argc = f.get_metadata().saved_argc.unwrap();
                        func.code.push_str(get_c_type(&m._type));
                        func.code.push(' ');
                        func.code.push_str(&name);
                        func.code.push_str(" = ((");

                        // Create function pointer
                        func.code.push_str(get_c_type(&m._type));
                        func.code.push_str(" (*)(");
                        for i in 0..saved_argc + f.get_metadata().arity
                        {
                            if i != 0
                            {
                                func.code.push_str(", ");
                            }
                            func.code.push_str("void*");
                        }

                        // Call the function
                        func.code.push_str(")) ");
                        func.code.push_str(&fstr);
                        func.code.push_str(".func)(");

                        // Print saved arguments
                        for i in 0..saved_argc
                        {
                            if i != 0
                            {
                                func.code.push_str(", ");
                            }

                            func.code.push_str(&fstr);
                            func.code.push_str(&format!(".args[{}]", i));
                        }

                        // Pass new arguments
                        for i in 0..f.get_metadata().arity
                        {
                            if i != 0 || saved_argc > 0
                            {
                                func.code.push_str(", ");
                            }

                            func.code.push_str("(void*) ");
                            func.code.push_str(&astrs[i]);
                        }

                        // Close parentheses
                        func.code.push_str(");\n");

                    // Currying
                    } else
                    {
                        // Create new function structure
                        func.code.push_str("func_t ");
                        func.code.push_str(&name);
                        func.code.push_str(" = { 1, ");
                        func.code.push_str(&fstr);
                        func.code.push_str(".func, ");
                        func.code.push_str(&fstr);
                        func.code.push_str(".wrapper, ");
                        func.code.push_str(&fstr);
                        func.code.push_str(".arity, ");
                        func.code.push_str(&fstr);
                        func.code.push_str(".argc, calloc(sizeof(void*), ");
                        func.code.push_str(&fstr);
                        func.code.push_str(".argc + ");
                        func.code.push_str(&format!("{}", args.len()));
                        func.code.push_str(") };\n");

                        // Copy previous args
                        func.code.push_str("for (unsigned int i = 0; i < ");
                        func.code.push_str(&fstr);
                        func.code.push_str(".argc; i++)\n");
                        func.code.push_str(&name);
                        func.code.push_str(".args[i] = ");
                        func.code.push_str(&fstr);
                        func.code.push_str(".args[i];\n");

                        // Put in new args
                        for arg in astrs.into_iter().enumerate()
                        {
                            func.code.push_str(&name);
                            func.code.push_str(".args[");
                            func.code.push_str(&fstr);
                            func.code.push_str(".argc + ");
                            func.code.push_str(&format!("{}", arg.0));
                            func.code.push_str("] = (void*) ");
                            func.code.push_str(&arg.1);
                            func.code.push_str(";\n");
                        }
                    }

                    name
                }

                // String and list concatenation is unsupported at the moment
                _ => panic!("unsupported application!")
            }
        }

        // Assignments
        SExpr::Assign(m, a, v) => {
            // Get value and generate code
            let val = convert_sexpr(v, root, func);
            func.code.push_str(get_c_type(&m._type));
            func.code.push(' ');
            func.code.push_str(a);
            func.code.push_str(" = ");
            func.code.push_str(&val);
            func.code.push_str(";\n");
            a.clone()
        }

        // With expressions
        SExpr::With(m, a, b) => {
            // Get name
            let name = format!("_{}", func.last_reference);
            func.last_reference += 1;

            // Declare variable
            func.code.push_str(&get_c_type(&m._type));
            func.code.push(' ');
            func.code.push_str(&name);
            func.code.push_str(";\n{\n");

            // Assignments
            for a in a
            {
                convert_sexpr(a, root, func);
            }

            // Body
            let body = convert_sexpr(b, root, func);
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&body);
            func.code.push_str(";\n}\n");

            name
        }

        _ => panic!("unimplemented s expression!")
    }
}

// put_fn_declaration(&mut String, &str, &CFunction) -> ()
// Puts a function declaration in the built string.
fn put_fn_declaration(s: &mut String, name: &str, func: &CFunction)
{
    s.push_str(get_c_type(func.ret_type));
    s.push(' ');
    s.push_str(name);
    s.push('(');

    let mut comma = false;
    for a in func.args.iter()
    {
        if comma
        {
            s.push_str(", ");
        } else
        {
            comma = true;
        }

        s.push_str(get_c_type(a.1));
        s.push(' ');
        s.push_str(a.0);
    }

    s.push(')');
}

// convert_ir_to_c(&IR, bool) -> String
// Converts Curly IR to C code.
#[allow(unused_variables)]
pub fn convert_ir_to_c(ir: &IR, repl_mode: bool) -> String
{
    // Create and populate functions
    let mut funcs = HashMap::new();
    for f in ir.funcs.iter()
    {
        let mut cf = CFunction {
            args: f.1.args.iter().map(|v| (&v.0, &v.1)).collect(),
            ret_type: &f.1.body.get_metadata()._type,
            code: String::new(),
            last_reference: 0
        };

        let last = convert_sexpr(&f.1.body, ir, &mut cf);

        // Return statement
        cf.code.push_str("return ");
        cf.code.push_str(&last);
        cf.code.push_str(";\n");

        funcs.insert(f.0, cf);
    }

    // Create the main function
    let mut main_func = CFunction {
        args: Vec::with_capacity(0),
        ret_type: &Type::Int,
        code: String::new(),
        last_reference: 0
    };

    // Populate the main function
    let mut printables = vec![];
    for s in ir.sexprs.iter()
    {
        printables.push(convert_sexpr(s, ir, &mut main_func));
    }

    // Declare all functions
    let mut code_string = String::from("typedef struct {\nunsigned int refc;\nvoid* func;\nvoid* wrapper;\nunsigned int arity;\nunsigned int argc;\nvoid** args;\n} func_t;\nint printf(char*, ...);\nvoid* calloc(size_t, size_t);\n");
    for f in funcs.iter()
    {
        put_fn_declaration(&mut code_string, f.0, &f.1);
        code_string.push_str(";\n");
    }

    // Put all function definitions
    for f in funcs
    {
        put_fn_declaration(&mut code_string, f.0, &f.1);
        code_string.push_str(" {\n");
        code_string.push_str(&f.1.code);
        code_string.push_str("}\n");
    }

    code_string.push_str("int main() {\n");
    code_string.push_str(&main_func.code);

    // Print out the value of each statement
    let mut iter = ir.sexprs.iter();
    if true // repl_mode
    {
        for p in printables
        {
            code_string.push_str("printf(\"");

            // Determine the type
            let _type = &iter.next().unwrap().get_metadata()._type;
            code_string.push_str(
                match _type
                {
                    Type::Int => "%lli",
                    Type::Float => "%0.5f",
                    Type::Bool => "%s",
                    Type::Func(_, _) => "<func %p>",
                    _ => panic!("unsupported type!")
                }
            );

            code_string.push_str("\\n\", ");
            code_string.push_str(&p);

            // Deal with booleans and functions
            code_string.push_str(
                match _type
                {
                    Type::Bool => " ? \"true\" : \"false\"",
                    Type::Func(_, _) => ".func",
                    _ => ""
                }
            );

            code_string.push_str(");\n");
        }
    }

    // End main function
    code_string.push_str("return 0;\n}\n");

    code_string
}
