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
            func.code.push_str(" = { (void*) ");
            func.code.push_str(s);
            func.code.push_str(", ");
            func.code.push_str(&format!("{}", root.funcs.get(s).unwrap().args.len()));
            func.code.push_str(", 0, (void*) 0 }; ");

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
            func.code.push_str("; ");

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
            func.code.push_str("; if (");
            func.code.push_str(&name);
            func.code.push_str(") { ");
            let right = convert_sexpr(r, root, func);
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&right);
            func.code.push_str("; } ");

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
            func.code.push_str("; if (!");
            func.code.push_str(&name);
            func.code.push_str(") { ");
            let right = convert_sexpr(r, root, func);
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&right);
            func.code.push_str("; } ");

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
            func.code.push_str("; ");

            // Get condition
            let cond = convert_sexpr(c, root, func);
            func.code.push_str("if (");
            func.code.push_str(&cond);
            func.code.push_str(") { ");

            // Get body
            let body = convert_sexpr(b, root, func);
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&body);
            func.code.push_str("; } else { ");

            // Get else clause
            let elsy = convert_sexpr(e, root, func);
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&elsy);
            func.code.push_str("; } ");

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
                    // Functions with known arity and fully applied
                    if f.get_metadata().arity <= args.len() + f.get_metadata().saved_argc
                    {
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

                        // Assignment
                        func.code.push_str(get_c_type(&m._type));
                        func.code.push(' ');
                        func.code.push_str(&name);
                        func.code.push_str(" = ((");

                        // Create function pointer
                        func.code.push_str(get_c_type(&m._type));
                        func.code.push_str(" (*)(");
                        let mut comma = false;
                        for a in args
                        {
                            if comma
                            {
                                func.code.push_str(", ");
                            } else
                            {
                                comma = true;
                            }

                            func.code.push_str(get_c_type(&a.get_metadata()._type));
                        }

                        // Call the function
                        func.code.push_str("))");
                        func.code.push_str(&fstr);
                        func.code.push_str(".func)(");

                        // Print saved arguments
                        for i in 0..f.get_metadata().saved_argc
                        {
                            if i != 0
                            {
                                func.code.push_str(", ");
                            }

                            func.code.push_str(&fstr);
                            func.code.push_str(&format!("->args[{}]", i));
                        }

                        // Pass new arguments
                        for i in f.get_metadata().saved_argc..f.get_metadata().arity
                        {
                            if i != 0
                            {
                                func.code.push_str(", ");
                            }

                            func.code.push_str(&astrs[i - f.get_metadata().saved_argc]);
                        }

                        // Close parentheses
                        func.code.push_str("); ");
                        name

                    // Currying and unknown arity
                    } else
                    {
                        panic!("currying and calling functions with unknown arity is not yet supported!");
                    }
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
            func.code.push_str("; ");
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
            func.code.push_str("; { ");

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
            func.code.push_str("; } ");

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
        cf.code.push_str("; ");

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
    let mut code_string = String::from("typedef struct { void* func; unsigned int arity; unsigned int argc; void** args; } func_t; void printf(char*, ...); ");
    for f in funcs.iter()
    {
        put_fn_declaration(&mut code_string, f.0, &f.1);
        code_string.push_str("; ");
    }

    // Put all function definitions
    for f in funcs
    {
        put_fn_declaration(&mut code_string, f.0, &f.1);
        code_string.push_str(" { ");
        code_string.push_str(&f.1.code);
        code_string.push_str("} ");
    }

    code_string.push_str("int main() { ");
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

            // Deal with booleans
            if let Type::Bool = _type
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
