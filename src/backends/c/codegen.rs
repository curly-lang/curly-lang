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
            let f = root.funcs.get(s).unwrap();
            func.code.push_str("func_t ");
            func.code.push_str(&name);
            func.code.push_str(" = { 0, (void*) ");
            func.code.push_str(s);
            func.code.push_str(", (void*) __func_wrapper_");
            func.code.push_str(s);
            func.code.push_str(&format!(", {}", f.args.len() + f.captured.len()));
            let count = if f.captured.len() > 0
            {
                f.args.len() + f.captured.len()
            } else
            {
                0
            };
            func.code.push_str(", 0, calloc(");
            func.code.push_str(&format!("{}", count));
            func.code.push_str(", sizeof(void*)), calloc(");
            func.code.push_str(&format!("{}", count));
            func.code.push_str(", sizeof(void*)) };\n");

            // Save captured variables
            for c in f.captured_names.iter()
            {
                // Fix doubles
                let mut v = c.clone();
                if *f.captured.get(c).unwrap() == Type::Float
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

                func.code.push_str(&name);
                func.code.push_str(".args[");
                func.code.push_str(&name);
                func.code.push_str(".argc++] = (void*) ");
                func.code.push_str(&v);
                func.code.push_str(";\n");
            }

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
        SExpr::Application(_, l, r) => {
            // Get the list of arguments and the function
            let mut args = vec![r];
            let mut funcs = vec![sexpr];
            let mut f = &**l;
            while let SExpr::Application(_, l, r) = f
            {
                args.insert(0, r);
                funcs.insert(0, f);
                f = l;
            }

            // Quick hack for debug function
            // TODO: Make this not a hack
            if let SExpr::Symbol(_, v) = f
            {
                if v == "debug"
                {
                    let arg = convert_sexpr(&args[0], root, func);
                    put_debug_fn(&mut func.code, &arg, &args[0].get_metadata()._type);
                    if args.len() == 1
                    {
                        return arg;
                    } else
                    {
                        panic!("Using debug on multiple arguments is not supported");
                    }
                }
            }

            let mut unknown_arity = false;
            match f.get_metadata()._type
            {
                // Functions
                Type::Func(_, _) => {
                    // Get function and args
                    let mut fstr = convert_sexpr(f, root, func);
                    let mut astrs = vec![];
                    let mut name = String::with_capacity(0);
                    for a in args.iter().enumerate()
                    {
                        // Get argument
                        let (n, a) = a;
                        let mut v = convert_sexpr(a, root, func);
                        match a.get_metadata()._type
                        {
                            Type::Float => {
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

                            Type::Func(_, _) => {
                                v = format!("&{}", &v);
                            }

                            _ => ()
                        }

                        // Functions with unknown arity
                        if f.get_metadata().arity == 0 || f.get_metadata().saved_argc.is_none()
                        {
                            // Check for function list if just got into this state of unknown arity
                            if !unknown_arity
                            {
                                unknown_arity = true;

                                // Init list
                                func.code.push_str("if (");
                                func.code.push_str(&fstr);
                                func.code.push_str(".cleaners == (void*) 0)\n");
                                func.code.push_str(&fstr);
                                func.code.push_str(".cleaners = calloc(");
                                func.code.push_str(&fstr);
                                func.code.push_str(".arity, sizeof(void*));\n");
func.code.push_str("if (");
                                func.code.push_str(&fstr);
                                func.code.push_str(".args == (void*) 0)\n");
                                func.code.push_str(&fstr);
                                func.code.push_str(".args = calloc(");
                                func.code.push_str(&fstr);
                                func.code.push_str(".arity, sizeof(void*));\n");
                            }

                            // Save argument
                            func.code.push_str(&fstr);
                            func.code.push_str(".args[");
                            func.code.push_str(&fstr);
                            func.code.push_str(".argc++] = (void*) ");
                            func.code.push_str(&v);
                            func.code.push_str(";\n");

                            // Call the function
                            let _type = &funcs[n].get_metadata()._type;
                            name = format!("_{}", func.last_reference);
                            func.last_reference += 1;
                            func.code.push_str(get_c_type(_type));
                            func.code.push(' ');
                            func.code.push_str(&name);
                            func.code.push_str(";\nif (");
                            func.code.push_str(&fstr);
                            func.code.push_str(".arity == ");
                            func.code.push_str(&fstr);
                            func.code.push_str(".argc) {\n");
                            func.code.push_str(&name);
                            func.code.push_str(" = ((");
                            func.code.push_str(get_c_type(_type));
                            func.code.push_str(" (*)(func_t*))");
                            func.code.push_str(&fstr);
                            func.code.push_str(".wrapper)(&");
                            func.code.push_str(&fstr);
                            func.code.push_str(");\nfree_func(&");
                            func.code.push_str(&fstr);
                            func.code.push_str(");\n");

                            // Reset the list of arguments
                            if n != args.len() - 1
                            {
                                fstr = name.clone();
                                func.code.push_str(";\n");
                                func.code.push_str("if (");
                                func.code.push_str(&fstr);
                                func.code.push_str(".args == (void*) 0)\n");
                                func.code.push_str(&fstr);
                                func.code.push_str(".args = calloc(");
                                func.code.push_str(&fstr);
                                func.code.push_str(".arity, sizeof(void*));\n");
func.code.push_str("if (");
                                func.code.push_str(&fstr);
                                func.code.push_str(".cleaners == (void*) 0)\n");
                                func.code.push_str(&fstr);
                                func.code.push_str(".cleaners = calloc(");
                                func.code.push_str(&fstr);
                                func.code.push_str(".arity, sizeof(void*));\n");
                            }

                            func.code.push_str("}\n");

                            f = funcs[n];

                        // Functions with known arity and fully applied
                        } else if f.get_metadata().arity <= astrs.len() + 1
                        {
                            // Get name
                            astrs.push(v);
                            name = format!("_{}", func.last_reference);
                            func.last_reference += 1;
                            let saved_argc = f.get_metadata().saved_argc.unwrap();
                            let _type = &funcs[n].get_metadata()._type;
                            func.code.push_str(get_c_type(_type));
                            func.code.push(' ');
                            func.code.push_str(&name);
                            func.code.push_str(" = ((");

                            // Create function pointer
                            func.code.push_str(get_c_type(_type));
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

                            if n < funcs.len()
                            {
                                f = funcs[n];
                            }

                            astrs.clear();

                            if n < args.len() - 1
                            {
                                fstr = name.clone();
                            }
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
                        func.code.push_str(";\ncopy_func(&");
                        func.code.push_str(&name);
                        func.code.push_str(", &");
                        func.code.push_str(&fstr);
                        func.code.push_str(");");

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
            let ptr = format!("_{}", func.last_reference);
            func.last_reference += 1;
            func.code.push_str(&get_c_type(&m._type));
            func.code.push_str("* ");
            func.code.push_str(&ptr);
            func.code.push_str(" = &");
            func.code.push_str(&body);
            func.code.push_str(";\n");

            // Increment body reference count
            match m._type
            {
                Type::Func(_, _) => {
                    func.code.push_str(&ptr);
                    func.code.push_str("->refc++;\n");
                }

                _ => ()
            }

            // Decrement assignment reference counts and free if necessary
            for a in a.iter().enumerate()
            {
                match a.1.get_metadata()._type
                {
                    Type::Func(_, _) => {
                        func.code.push_str("refc_func(&");
                        func.code.push_str(&astrs[a.0]);
                        func.code.push_str(");\n");
                    }

                    _ => ()
                }
            }

            // Decrement body reference count
            match m._type
            {
                Type::Func(_, _) => {
                    func.code.push_str(&ptr);
                    func.code.push_str("->refc--;\n");
                }

                _ => ()
            }

            // Copy data from ptr
            func.code.push_str(&name);
            func.code.push_str(" = *");
            func.code.push_str(&ptr);

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
        match a.1
        {
            Type::Float
                | Type::Func(_, _)
                => s.push_str("*_"),
            _ => ()
        }
        s.push_str(a.0);
    }

    s.push(')');
}

// put_debug_fn(&mut String, &str, &Type) -> ()
// Puts a debug function in the built string.
fn put_debug_fn(code: &mut String, v: &str, _type: &Type)
{
    code.push_str("printf(\"");

    // Determine the type
    code.push_str(
        match _type
        {
            Type::Int => "%lli",
            Type::Float => "%0.5f",
            Type::Bool => "%s",
            Type::Func(_, _) => "<func %p>",
            _ => panic!("unsupported type!")
        }
    );

    code.push_str("\\n\", ");
    code.push_str(v);

    // Deal with booleans and functions
    code.push_str(
        match _type
        {
            Type::Bool => " ? \"true\" : \"false\"",
            Type::Func(_, _) => ".func",
            _ => ""
        }
    );

    code.push_str(");\n");
}

// convert_ir_to_c(&IR, Option<&mut Vec<String>>) -> String
// Converts Curly IR to C code.
pub fn convert_ir_to_c(ir: &IR, repl_vars: Option<&Vec<String>>) -> String
{
    // Create and populate functions
    let mut funcs = HashMap::new();
    for f in ir.funcs.iter()
    {
        let mut cf = CFunction {
            args: f.1.captured_names.iter().map(|v| (v, f.1.captured.get(v).unwrap())).chain(f.1.args.iter().map(|v| (&v.0, &v.1))).collect(),
            ret_type: &f.1.body.get_metadata()._type,
            code: String::new(),
            last_reference: 0
        };

        // Fix doubles and functions
        for a in cf.args.iter()
        {
            match a.1
            {
                Type::Float => {
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

                Type::Func(_, _) => {
                    // Copy
                    cf.code.push_str("func_t ");
                    cf.code.push_str(&a.0);
                    cf.code.push_str(";\ncopy_func(&");
                    cf.code.push_str(&a.0);
                    cf.code.push_str(", _");
                    cf.code.push_str(&a.0);
                    cf.code.push_str(");\n");
                }

                _ => ()
            }
            if *a.1 == Type::Float
            {
                
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
        ret_type: if let Some(v) = ir.sexprs.last()
        {
            &v.get_metadata()._type
        } else
        {
            &Type::Int
        },
        code: String::new(),
        last_reference: 0
    };

    // Populate the main function
    let mut cleanup = vec![];
    for s in ir.sexprs.iter()
    {
        let v = convert_sexpr(s, ir, &mut main_func);

        // Debug print
        if let Some(_) = repl_vars
        {
            put_debug_fn(&mut main_func.code, &v, &s.get_metadata()._type);
        }

        // Deallocation
        match s.get_metadata()._type
        {
            Type::Func(_, _) => {
                main_func.code.push_str("free_func(&");
                main_func.code.push_str(&v);
                main_func.code.push_str(");\n");
            }

            _ => ()
        }

        cleanup.push(v);
    }

    // Define structures and helper functions
    let mut code_string = String::from("
typedef struct {
    unsigned int refc;
    void* func;
    void* wrapper;
    unsigned int arity;
    unsigned int argc;
    char (**cleaners)(void*);
    void** args;
} func_t;

typedef union {
    double d;
    void* v;
} double_wrapper_t;

int printf(const char*, ...);

void* calloc(long unsigned int, long unsigned int);

void free(void*);

char force_free_func(func_t* func) {
    for (int i = 0; i < func->argc; i++) {
        if (func->cleaners[i] != (void*) 0 && func->cleaners[i](func->args[i]))
            free(func->args[i]);
    }

    free(func->args);
    free(func->cleaners);
    return (char) 1;
}

char free_func(func_t* func) {
    if (func->refc == 0) {
        return force_free_func(func);
    }

    return (char) 0;
}

char refc_func(func_t* func) {
    if (func->refc > 0)
        func->refc--;
    return free_func(func);
}

void copy_func(func_t* dest, func_t* source) {
    dest->refc = 0;
    dest->func = source->func;
    dest->wrapper = source->wrapper;dest->arity = source->arity;
    dest->argc = source->argc;

    if (dest->argc != 0)
    {
        dest->cleaners = calloc(dest->arity, sizeof(void*));
        dest->args = calloc(dest->arity, sizeof(void*));
    } else
    {
        dest->cleaners = (void*) 0;
        dest->args = (void*) 0;
    }

    for (int i = 0; i < dest->argc; i++) {
        dest->cleaners[i] = source->cleaners[i];
        dest->args[i] = source->args[i];
    }
}
");

    // Define repl value struct
    if let Some(_) = repl_vars
    {
        code_string.push_str("
typedef struct {
    unsigned int tag;
    union {
        long long i;
        double d;
        char b;
        func_t f;
    } vals;
} repl_value_t;
");
    }

    // Declare all functions
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

    // Retrieve previous arguments
    if let Some(vec) = &repl_vars
    {
        code_string.push_str(get_c_type(main_func.ret_type));

        code_string.push_str(" __repl_line(repl_value_t** vars) {\n");
        for v in vec.iter().enumerate()
        {
            code_string.push_str(get_c_type(&ir.scope.get_var(v.1).unwrap().0));
            code_string.push(' ');
            code_string.push_str(&v.1);
            code_string.push_str(" = vars[");
            code_string.push_str(&format!("{}", v.0));
            code_string.push_str("]->vals.");

            code_string.push_str(
                match &ir.scope.get_var(v.1).unwrap().0
                {
                    Type::Int => "i",
                    Type::Float => "d",
                    Type::Bool => "b",
                    Type::Func(_, _) => "f",
                    _ => panic!("unsupported type!")
                }
            );

            code_string.push_str(";\n");
        }
    } else
    {
        code_string.push_str("int main() {\n");
    }

    // Main function code
    code_string.push_str(&main_func.code);

    // Deallocate everything
    for v in ir.sexprs.iter().enumerate()
    {
        match v.1.get_metadata()._type
        {
            Type::Func(_, _) => {
                code_string.push_str("if (");
                code_string.push_str(&cleanup[v.0]);
                code_string.push_str(".refc != 0) {");
                code_string.push_str(&cleanup[v.0]);
                code_string.push_str(".refc = 0;\nfree_func(&");
                code_string.push_str(&cleanup[v.0]);
                code_string.push_str(");\n}\n");
            }

            _ => ()
        }
    }

    // End main function
    if let Some(_) = repl_vars
    {
        code_string.push_str("return ");
        match cleanup.last()
        {
            Some(v) => code_string.push_str(v),
            None => code_string.push_str("0")
        }
        code_string.push_str(";\n}\n");
    } else
    {
        code_string.push_str("return 0;\n}\n");
    }

    code_string
}
