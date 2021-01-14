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

// get_c_type(&Type, bool) -> &str
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
            func.code.push_str(" = { 0, (void*) ");
            func.code.push_str(s);
            func.code.push_str(", (void*) __func_wrapper_");
            func.code.push_str(s);
            func.code.push_str(&format!(", {}", root.funcs.get(s).unwrap().args.len()));
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
            let mut funcs = vec![l];
            let mut f = &**l;
            while let SExpr::Application(_, l, r) = f
            {
                args.insert(0, r);
                funcs.insert(0, l);
                f = l;
            }

            match f.get_metadata()._type
            {
                // Functions
                Type::Func(_, _) => {
                    // Get function and args
                    let fstr = convert_sexpr(f, root, func);
                    let mut astrs = vec![];
                    let mut name = String::with_capacity(0);
                    for a in args.iter().enumerate()
                    {
                        // Get argument
                        let (n, a) = a;
                        let mut v = convert_sexpr(a, root, func);
                        if a.get_metadata()._type == Type::Float
                        {
                            let name = format!("_{}", func.last_reference);
                            func.last_reference += 1;
                            func.code.push_str("double_wrapper_t ");
                            func.code.push_str(&name);
                            func.code.push_str(";\n");
                            func.code.push_str(&name);
                            func.code.push_str(".d = ");
                            func.code.push_str(&v);
                            func.code.push_str(";\nvoid* ");
                            v = format!("_{}", func.last_reference);
                            func.last_reference += 1;
                            func.code.push_str(&v);
                            func.code.push_str(" = ");
                            func.code.push_str(&name);
                            func.code.push_str(".v;\n");
                        }


                        // Functions with unknown arity
                        if f.get_metadata().arity == 0 || f.get_metadata().saved_argc.is_none()
                        {
                            panic!("calling functions with unknown arity is not yet supported!");

                        // Functions with known arity and fully applied
                        } else if f.get_metadata().arity <= astrs.len() + f.get_metadata().saved_argc.unwrap()
                        {
                            // Get name
                            astrs.push(v);
                            name = format!("_{}", func.last_reference);
                            func.last_reference += 1;
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
                            f = funcs[n];
                            astrs.clear();
                        } else
                        {
                            astrs.push(v);
                        }
                    }

                    // Currying
                    if astrs.len() > 0
                    {
                        // Get name
                        name = format!("_{}", func.last_reference);
                        func.last_reference += 1;

                        // Create new function structure
                        func.code.push_str("func_t ");
                        func.code.push_str(&name);
                        func.code.push_str(" = { 0, ");
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
                        for arg in astrs.iter().enumerate()
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

            // Increment reference counter
            match m._type
            {
                Type::Func(_, _) => {
                    func.code.push_str(&a);
                    func.code.push_str(".refc++;\n");
                }

                _ => ()
            }

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
            let mut astrs = vec![];
            for a in a
            {
                astrs.push(convert_sexpr(a, root, func));
            }

            // Body
            let body = convert_sexpr(b, root, func);
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&body);
            func.code.push_str(";\n");

            // Increment body reference count
            match m._type
            {
                Type::Func(_, _) => {
                    func.code.push_str(&name);
                    func.code.push_str(".refc++;\n");
                }

                _ => ()
            }

            // Decrement assignment reference counts and free if necessary
            for a in a.iter().enumerate()
            {
                match a.1.get_metadata()._type
                {
                    Type::Func(_, _) => {
                        func.code.push_str(&astrs[a.0]);
                        func.code.push_str(".refc--;\nif (");
                        func.code.push_str(&astrs[a.0]);
                        func.code.push_str(".refc == 0) {\nfree(");
                        func.code.push_str(&astrs[a.0]);
                        func.code.push_str(".args);\n}\n");
                    }

                    _ => ()
                }
            }

            // Decrement body reference count
            match m._type
            {
                Type::Func(_, _) => {
                    func.code.push_str(&name);
                    func.code.push_str(".refc--;\n");
                }

                _ => ()
            }

            // Exit block and return success
            func.code.push_str(";\n}\n");
            name
        }

        _ => panic!("unimplemented s expression!")
    }
}

// put_fn_wrapper(&mut String, &str, &CFunction) -> ()
// Puts the wrapper for a given function in the built string.
fn put_fn_wrapper(s: &mut String, name: &str, func: &CFunction)
{
    s.push_str(get_c_type(func.ret_type));
    s.push_str(" __func_wrapper_");
    s.push_str(&name);
    s.push_str("(func_t* f) {\nreturn ((");
    s.push_str(get_c_type(func.ret_type));
    s.push_str(" (*) (");

    for i in 0..func.args.len()
    {
        if i != 0
        {
            s.push_str(", ");
        }

        s.push_str("void*");
    }

    s.push_str(")) f->func)(");

    for i in 0..func.args.len()
    {
        if i != 0
        {
            s.push_str(", ");
        }

        s.push_str("f->args[");
        s.push_str(&format!("{}", i));
        s.push_str("]");
    }

    s.push_str(");\n}\n");
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
        if *a.1 == Type::Float
        {
            s.push_str("*_");
        }
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

        // Fix doubles
        for a in cf.args.iter()
        {
            if *a.1 == Type::Float
            {
                // Get name
                let name = format!("_{}", cf.last_reference);
                cf.last_reference += 1;

                // Convert pointer to double
                cf.code.push_str("double_wrapper_t ");
                cf.code.push_str(&name);
                cf.code.push_str(";\n");
                cf.code.push_str(&name);
                cf.code.push_str(".v = _");
                cf.code.push_str(&a.0);
                cf.code.push_str(";\ndouble ");
                cf.code.push_str(&a.0);
                cf.code.push_str(" = ");
                cf.code.push_str(&name);
                cf.code.push_str(".d;\n");
            }
        }

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
    let mut cleanup = vec![];
    for s in ir.sexprs.iter()
    {
        let v = convert_sexpr(s, ir, &mut main_func);

        // Debug print
        if true// repl_mode
        {
            main_func.code.push_str("printf(\"");

            // Determine the type
            main_func.code.push_str(
                match s.get_metadata()._type
                {
                    Type::Int => "%lli",
                    Type::Float => "%0.5f",
                    Type::Bool => "%s",
                    Type::Func(_, _) => "<func %p>",
                    _ => panic!("unsupported type!")
                }
            );

            main_func.code.push_str("\\n\", ");
            main_func.code.push_str(&v);

            // Deal with booleans and functions
            main_func.code.push_str(
                match s.get_metadata()._type
                {
                    Type::Bool => " ? \"true\" : \"false\"",
                    Type::Func(_, _) => ".func",
                    _ => ""
                }
            );

            main_func.code.push_str(");\n");
        }

        // Deallocation
        match s.get_metadata()._type
        {
            Type::Func(_, _) => {
                main_func.code.push_str("if (");
                main_func.code.push_str(&v);
                main_func.code.push_str(".refc == 0)\nfree(");
                main_func.code.push_str(&v);
                main_func.code.push_str(".args);\n");
            }

            _ => ()
        }

        cleanup.push(v);
    }

    // Declare all functions
    let mut code_string = String::from("typedef struct {\nunsigned int refc;\nvoid* func;\nvoid* wrapper;\nunsigned int arity;\nunsigned int argc;\nvoid** args;\n} func_t;\ntypedef union {\ndouble d;\nvoid* v;\n} double_wrapper_t;\nint printf(const char*, ...);\nvoid* calloc(long unsigned int, long unsigned int);\nvoid free(void*);\n");
    for f in funcs.iter()
    {
        put_fn_declaration(&mut code_string, f.0, &f.1);
        code_string.push_str(";\n");
        put_fn_wrapper(&mut code_string, f.0, &f.1);
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

    // Deallocate everything
    for v in ir.sexprs.iter().enumerate()
    {
        match v.1.get_metadata()._type
        {
            Type::Func(_, _) => {
                main_func.code.push_str("if (");
                main_func.code.push_str(&cleanup[v.0]);
                main_func.code.push_str(".refc != 0)\nfree(");
                main_func.code.push_str(&cleanup[v.0]);
                main_func.code.push_str(".args);\n");
            }

            _ => ()
        }
    }

    // End main function
    code_string.push_str("return 0;\n}\n");

    code_string
}
