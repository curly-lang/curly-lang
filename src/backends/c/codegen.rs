use std::collections::HashMap;
use std::iter::FromIterator;

use crate::frontend::ir::{BinOp, IRModule, Location, PrefixOp, SExpr, IR};
use crate::frontend::types::Type;

// Represents a function in C.
#[derive(Debug)]
struct CFunction<'a> {
    name: String,
    args: Vec<(&'a String, &'a Type)>,
    ret_type: &'a Type,
    code: String,
    last_reference: usize,
}

// Represents a structure in C
#[derive(Clone, Debug)]
enum CType {
    Primitive(String, Type),
    Sum(String, Type, HashMap<Type, usize>),
}

impl CType {
    // get_c_name(&self) -> &String
    // Gets the c name of the type.
    fn get_c_name(&self) -> &String {
        match self {
            CType::Primitive(s, _) | CType::Sum(s, _, _) => s,
        }
    }

    // get_curly_type(&self) -> &Type
    // Returns the Curly IRModule type.
    fn get_curly_type(&self) -> &Type {
        match self {
            CType::Primitive(_, t) | CType::Sum(_, t, _) => t,
        }
    }

    // get_hashmap(&self) -> Option<&HashMap<Type, usize>>
    // Gets the hashmap associated with a sum type.
    fn get_hashmap(&self) -> Option<&HashMap<Type, usize>> {
        match self {
            CType::Sum(_, _, v) => Some(v),
            _ => None,
        }
    }
}

// get_c_type(&Type, &HashMap<Type, String>) -> &str
// Converts an IRModule type into a C type.
fn get_c_type<'a>(_type: &Type, types: &'a HashMap<Type, CType>) -> &'a str {
    match _type {
        Type::Int => "int_t",
        Type::Float => "float_t",
        Type::Bool => "bool",
        Type::Word => "word_t",
        Type::Char => "char",
        Type::Func(_, _) => "func_t",
        Type::Symbol(_) => types.get(_type).unwrap().get_c_name(),
        Type::Sum(_) => types.get(_type).unwrap().get_c_name(),
        Type::Pointer(_) => "void*",
        _ => panic!("unsupported type!"),
    }
}

// sanitise_symbol(&str) -> String
// Sanitises a symbol.
fn sanitise_symbol(value: &str) -> String {
    let mut s = value
        .replace("'", "$$PRIME$$")
        .replace("::", "$$DOUBLECOLON$$");
    s.push_str("$");
    s
}

// convert_sexpr(&SExpr, &IRModule, &mut CFunction, &HashMap<Type, String>) -> String
// Converts a s expression into C code.
fn convert_sexpr(
    sexpr: &SExpr,
    root: &IRModule,
    func: &mut CFunction,
    types: &HashMap<Type, CType>,
) -> String {
    match sexpr {
        // Ints
        SExpr::Int(_, n) => {
            // Get name
            let name = format!("$${}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str("int_t ");
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&format!("{}", n));
            func.code.push_str(";\n");

            name
        }

        // Floats
        SExpr::Float(_, n) => {
            // Get name
            let name = format!("$${}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str("float_t ");
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&format!("{}", n));
            func.code.push_str(";\n");

            name
        }

        // Word
        SExpr::Word(_, n) => {
            // Get name
            let name = format!("$${}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str("word_t ");
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&format!("{}", n));
            func.code.push_str("u;\n");

            name
        }

        // Char
        SExpr::Char(_, c) => {
            // Get name
            let name = format!("$${}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str("char ");
            func.code.push_str(&name);
            func.code.push_str(" = (char) ");
            func.code.push_str(&format!("{}", c));
            func.code.push_str(";\n");

            name
        }

        // Booleans
        SExpr::True(_) => {
            // Get name
            let name = format!("$${}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str("bool ");
            func.code.push_str(&name);
            func.code.push_str(" = 1;\n");

            name
        }

        SExpr::False(_) => {
            // Get name
            let name = format!("$${}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str("bool ");
            func.code.push_str(&name);
            func.code.push_str(" = 0;\n");

            name
        }

        SExpr::Enum(_, _) => String::with_capacity(0),

        // Symbols
        SExpr::Symbol(m, s) => {
            if let Type::Enum(_) = m._type {
                String::with_capacity(0)
            } else {
                sanitise_symbol(s)
            }
        }

        // Functions
        SExpr::Function(m, f) => {
            // Get name
            let name = format!("$${}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            let s = sanitise_symbol(f);
            let mod_name = sanitise_symbol(&m.origin);
            if let Some(f) = root.funcs.get(f) {
                if f.args.len() != 0 {
                    func.code.push_str("func_t ");
                    func.code.push_str(&name);
                    func.code.push_str(" = { 0, (void*) ");
                    func.code.push_str(&mod_name);
                    func.code.push_str(&s);
                    func.code.push_str("$FUNC$$, (void*) ");
                    func.code.push_str(&mod_name);
                    func.code.push_str(&s);
                    func.code
                        .push_str(&format!("$WRAPPER$$, {}", f.args.len() + f.captured.len()));
                    func.code.push_str(", 0, ");
                    if f.captured.len() > 0 {
                        let count = f.args.len() + f.captured.len();
                        func.code.push_str("calloc(");
                        func.code.push_str(&format!("{}", count));
                        func.code.push_str(", sizeof(void*)), calloc(");
                        func.code.push_str(&format!("{}", count));
                        func.code.push_str(", sizeof(void*)) };\n");
                    } else {
                        func.code.push_str("(void*) 0, (void*) 0 };\n");
                    }

                    // Save captured variables
                    for c in f.captured_names.iter() {
                        // Get type
                        let mut v = sanitise_symbol(c);
                        let mut _type = f.captured.get(c).unwrap();
                        while let Type::Symbol(s) = _type {
                            _type = root.types.get(s).unwrap();
                        }

                        // Fix functions, floats, and sum types
                        match _type {
                            Type::Float => {
                                let name = format!("$${}", func.last_reference);
                                func.last_reference += 1;
                                func.code.push_str("double_wrapper_t ");
                                func.code.push_str(&name);
                                func.code.push_str(";\n");
                                func.code.push_str(&name);
                                func.code.push_str(".d = ");
                                func.code.push_str(&v);
                                func.code.push_str(";\nvoid* ");
                                v = format!("$${}", func.last_reference);
                                func.last_reference += 1;
                                func.code.push_str(&v);
                                func.code.push_str(" = ");
                                func.code.push_str(&name);
                                func.code.push_str(".v;\n");
                            }

                            Type::Func(_, _) => {
                                v = {
                                    let name = format!("$${}", func.last_reference);
                                    func.last_reference += 1;
                                    func.code.push_str("func_t* ");
                                    func.code.push_str(&name);
                                    func.code.push_str(" = copy_func_arg(&");
                                    func.code.push_str(&v);
                                    func.code.push_str(");\n");
                                    name
                                };
                                func.code.push_str(&name);
                                func.code.push_str(".args[");
                                func.code.push_str(&name);
                                func.code.push_str(".argc] = (void*) force_free_func;\n");
                            }

                            Type::Sum(_) => {
                                let name = format!("$${}", func.last_reference);
                                func.last_reference += 1;
                                func.code.push_str(get_c_type(&_type, types));
                                func.code.push_str("* ");
                                func.code.push_str(&name);
                                func.code.push_str(" = malloc(sizeof(");
                                func.code.push_str(get_c_type(&_type, types));
                                func.code.push_str("));\n*");
                                func.code.push_str(&name);
                                func.code.push_str(" = ");
                                func.code.push_str(&v);
                                func.code.push_str(";\n");
                                v = name;
                            }

                            _ => (),
                        }

                        func.code.push_str(&name);
                        func.code.push_str(".args[");
                        func.code.push_str(&name);
                        func.code.push_str(".argc++] = (void*) ");
                        func.code.push_str(&v);
                        func.code.push_str(";\n");
                    }
                } else if m.tailrec {
                    func.code.push_str("$$LOOP$$ = 1;\n");
                } else {
                    if let Type::Enum(_) = f.body.get_metadata()._type {
                    } else {
                        func.code
                            .push_str(get_c_type(&f.body.get_metadata()._type, types));
                        func.code.push(' ');
                        func.code.push_str(&name);
                        func.code.push_str(" = ");
                    }
                    func.code.push_str(&mod_name);
                    func.code.push_str(&s);
                    func.code.push_str("$GET$$();\n");
                }

                name
            } else {
                unreachable!(
                    "function is always defined as a function, {} is not in {:?}",
                    f,
                    root.funcs.keys()
                );
            }
        }

        // External functions
        SExpr::ExternalFunc(m, name, args) => {
            // Get function references
            let mut arg_refs = Vec::with_capacity(0);
            for arg in args {
                let v = convert_sexpr(arg, root, func, types);
                arg_refs.push(v);
            }

            // Push function declaration
            let ret_type = if let Type::Enum(_) = m._type {
                "void"
            } else {
                get_c_type(&m._type, types)
            };
            func.code.push_str(ret_type);
            func.code.push(' ');
            func.code.push_str(name);
            func.code.push('(');
            let mut comma = false;
            for arg in args {
                if let Type::Enum(_) = arg.get_metadata()._type {
                    continue;
                }

                if comma {
                    func.code.push_str(", ");
                } else {
                    comma = true;
                }

                func.code
                    .push_str(get_c_type(&arg.get_metadata()._type, types));
            }
            func.code.push_str(");\n");

            // Call function
            let mut _ref = String::with_capacity(0);
            if ret_type != "void" {
                _ref = format!("$${}", func.last_reference);
                func.last_reference += 1;
                func.code.push_str(ret_type);
                func.code.push(' ');
                func.code.push_str(&_ref);
                func.code.push_str(" = ");
            }

            func.code.push_str(name);
            func.code.push('(');
            let mut comma = false;
            for arg in arg_refs {
                if arg == "" {
                    continue;
                }

                if comma {
                    func.code.push_str(", ");
                } else {
                    comma = true;
                }

                func.code.push_str(&arg);
            }
            func.code.push_str(");\n");

            _ref
        }

        // Prefix
        SExpr::Prefix(m, op, v) => {
            // Get name and value
            let val = convert_sexpr(v, root, func, types);
            let name = format!("$${}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str(get_c_type(&m._type, types));
            func.code.push(' ');
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(match op {
                PrefixOp::Neg => "-",
                PrefixOp::Span => panic!("unsupported operator!"),
            });
            func.code.push_str(&val);
            func.code.push_str(";\n");

            name
        }

        // Infix
        SExpr::Infix(m, op, l, r) => {
            // Get name and operands
            let left = convert_sexpr(l, root, func, types);
            let right = convert_sexpr(r, root, func, types);
            let name = format!("$${}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str(get_c_type(&m._type, types));
            func.code.push(' ');
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&left);
            func.code.push(' ');
            func.code.push_str(match op {
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
                BinOp::And => "&",
                BinOp::Or => "|",
                BinOp::Xor => "^",
                BinOp::BoolXor => "^",
            });
            func.code.push(' ');
            func.code.push_str(&right);
            func.code.push_str(";\n");

            name
        }

        SExpr::Chain(m, l, r) => {
            let a = format!("$${}", func.last_reference);
            func.last_reference += 1;
            if let Type::Enum(_) = m._type {
            } else {
                func.code.push_str(get_c_type(&m._type, types));
                func.code.push(' ');
                func.code.push_str(&a);
                func.code.push_str(";\n");
            }

            if let SExpr::Walrus(_, _, _) = **l {
                func.code.push_str("{\n");
            }

            convert_sexpr(l, root, func, types);
            let v = convert_sexpr(r, root, func, types);

            if let SExpr::Walrus(_, _, _) = **l {
                if let Type::Enum(_) = m._type {
                    func.code.push_str("}\n");
                    v
                } else {
                    func.code.push_str(&a);
                    func.code.push_str(" = ");
                    func.code.push_str(&v);
                    func.code.push_str(";\n}\n");
                    a
                }
            } else {
                v
            }
        }

        // Boolean and
        SExpr::And(m, l, r) => {
            // Get name and left operand
            let left = convert_sexpr(l, root, func, types);
            let name = format!("$${}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str(get_c_type(&m._type, types));
            func.code.push(' ');
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&left);
            func.code.push_str(";\nif (");
            func.code.push_str(&name);
            func.code.push_str(") {\n");
            let right = convert_sexpr(r, root, func, types);
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&right);
            func.code.push_str(";\n}\n");

            name
        }

        // Boolean or
        SExpr::Or(m, l, r) => {
            // Get name and left operand
            let left = convert_sexpr(l, root, func, types);
            let name = format!("$${}", func.last_reference);
            func.last_reference += 1;

            // Generate code
            func.code.push_str(get_c_type(&m._type, types));
            func.code.push(' ');
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&left);
            func.code.push_str(";\nif (!");
            func.code.push_str(&name);
            func.code.push_str(") {\n");
            let right = convert_sexpr(r, root, func, types);
            func.code.push_str(&name);
            func.code.push_str(" = ");
            func.code.push_str(&right);
            func.code.push_str(";\n}\n");

            name
        }

        // As operator
        SExpr::As(m, v) => {
            // Get value and types
            let value = convert_sexpr(v, root, func, types);

            let mut _type = &m._type;
            while let Type::Symbol(s) = _type {
                _type = root.types.get(s).unwrap();
            }

            let mut vtype = &v.get_metadata()._type;
            while let Type::Symbol(s) = vtype {
                vtype = root.types.get(s).unwrap();
            }

            // Check if types are equal
            if _type == vtype {
                return value;
            }

            // Get name and value
            let name = format!("$${}", func.last_reference);
            func.last_reference += 1;

            func.code.push_str(get_c_type(&m._type, types));
            func.code.push(' ');
            func.code.push_str(&name);

            // Check result type
            match &_type {
                // Sum types
                Type::Sum(_) => {
                    func.code.push_str(";\n");
                    func.code.push_str(&name);
                    func.code.push_str(".tag = ");
                    let ctype = types.get(&_type).unwrap();
                    let id = ctype
                        .get_hashmap()
                        .unwrap()
                        .get(&v.get_metadata()._type)
                        .unwrap();
                    func.code.push_str(&format!("{}", id));
                    func.code.push_str(";\n");

                    if value != "" {
                        func.code.push_str(&name);
                        func.code.push_str(".values.$$");
                        func.code.push_str(&format!("{}", id));
                        func.code.push_str(" = ");
                        func.code.push_str(&value);
                        func.code.push_str(";\n");
                    }
                }

                // Everything else is unsupported
                _ => panic!("unsupported type!"),
            }

            name
        }

        // If expressions
        SExpr::If(m, c, b, e) => {
            // Get name
            let name = if let Type::Enum(_) = m._type {
                String::with_capacity(0)
            } else {
                let name = format!("$${}", func.last_reference);
                func.last_reference += 1;

                // Declare variable
                func.code.push_str(&get_c_type(&m._type, types));
                func.code.push(' ');
                func.code.push_str(&name);
                func.code.push_str(";\n");
                name
            };

            // Get condition
            let cond = convert_sexpr(c, root, func, types);
            func.code.push_str("if (");
            func.code.push_str(&cond);
            func.code.push_str(") {\n");

            // Get types
            let mut mtype = &m._type;
            while let Type::Symbol(s) = mtype {
                mtype = root.types.get(s).unwrap();
            }
            let mut btype = &b.get_metadata()._type;
            while let Type::Symbol(s) = btype {
                btype = root.types.get(s).unwrap();
            }
            let mut etype = &e.get_metadata()._type;
            while let Type::Symbol(s) = etype {
                etype = root.types.get(s).unwrap();
            }

            // Get body
            let body = convert_sexpr(b, root, func, types);

            // Save
            if mtype == btype {
                if let Type::Enum(_) = mtype {
                } else {
                    func.code.push_str(&name);
                    func.code.push_str(" = ");
                    func.code.push_str(&body);
                    func.code.push_str(";\n");
                }
            } else if let Type::Sum(_) = btype {
                let map = types.get(mtype).unwrap().get_hashmap().unwrap();
                let subtype = types.get(btype).unwrap();
                let submap = subtype.get_hashmap().unwrap();
                func.code.push_str("switch (");
                func.code.push_str(&body);
                func.code.push_str(".tag) {\n");

                for s in submap {
                    func.code.push_str("case ");
                    func.code.push_str(&format!("{}:\n", s.1));
                    func.code.push_str(&name);
                    func.code.push_str(".tag = ");
                    func.code.push_str(&format!("{};\n", map.get(s.0).unwrap()));

                    if let Type::Enum(_) = s.0 {
                    } else {
                        func.code.push_str(&name);
                        func.code.push_str(".values.$$");
                        func.code.push_str(&format!("{}", map.get(s.0).unwrap()));
                        func.code.push_str(" = ");
                        func.code.push_str(&body);
                        func.code.push_str(".values.$$");
                        func.code.push_str(&format!("{};\n", s.1));
                    }
                    func.code.push_str("break;\n");
                }
                func.code.push_str("}\n");
            } else if let Type::Enum(_) = btype {
                let id = types
                    .get(mtype)
                    .unwrap()
                    .get_hashmap()
                    .unwrap()
                    .get(&b.get_metadata()._type)
                    .unwrap();
                func.code.push_str(&name);
                func.code.push_str(".tag = ");
                func.code.push_str(&format!("{}", id));
                func.code.push_str(";\n");
            } else {
                let id = types
                    .get(mtype)
                    .unwrap()
                    .get_hashmap()
                    .unwrap()
                    .get(&b.get_metadata()._type)
                    .unwrap();
                func.code.push_str(&name);
                func.code.push_str(".tag = ");
                func.code.push_str(&format!("{}", id));
                func.code.push_str(";\n");
                func.code.push_str(&name);
                func.code.push_str(".values.$$");
                func.code.push_str(&format!("{}", id));
                func.code.push_str(" = ");
                func.code.push_str(&body);
                func.code.push_str(";\n");
            }
            func.code.push_str("} else {\n");

            // Get else clause
            let elsy = convert_sexpr(e, root, func, types);

            // Save
            if mtype == etype {
                if let Type::Enum(_) = mtype {
                } else {
                    func.code.push_str(&name);
                    func.code.push_str(" = ");
                    func.code.push_str(&elsy);
                    func.code.push_str(";\n");
                }
            } else if let Type::Sum(_) = etype {
                let map = types.get(mtype).unwrap().get_hashmap().unwrap();
                let subtype = types.get(etype).unwrap();
                let submap = subtype.get_hashmap().unwrap();
                func.code.push_str("switch (");
                func.code.push_str(&elsy);
                func.code.push_str(".tag) {\n");

                for s in submap {
                    func.code.push_str("case ");
                    func.code.push_str(&format!("{}:\n", s.1));
                    func.code.push_str(&name);
                    func.code.push_str(".tag = ");
                    func.code.push_str(&format!("{};\n", map.get(s.0).unwrap()));

                    if let Type::Enum(_) = s.0 {
                    } else {
                        func.code.push_str(&name);
                        func.code.push_str(".values.$$");
                        func.code.push_str(&format!("{}", map.get(s.0).unwrap()));
                        func.code.push_str(" = ");
                        func.code.push_str(&elsy);
                        func.code.push_str(".values.$$");
                        func.code.push_str(&format!("{};\n", s.1));
                    }
                    func.code.push_str("break;\n");
                }
                func.code.push_str("}\n");
            } else if let Type::Enum(_) = btype {
                let id = types
                    .get(mtype)
                    .unwrap()
                    .get_hashmap()
                    .unwrap()
                    .get(&b.get_metadata()._type)
                    .unwrap();
                func.code.push_str(&name);
                func.code.push_str(".tag = ");
                func.code.push_str(&format!("{}", id));
                func.code.push_str(";\n");
            } else {
                let id = types
                    .get(&m._type)
                    .unwrap()
                    .get_hashmap()
                    .unwrap()
                    .get(&e.get_metadata()._type)
                    .unwrap();
                func.code.push_str(&name);
                func.code.push_str(".tag = ");
                func.code.push_str(&format!("{}", id));
                func.code.push_str(";\n");
                func.code.push_str(&name);
                func.code.push_str(".values.$$");
                func.code.push_str(&format!("{}", id));
                func.code.push_str(" = ");
                func.code.push_str(&elsy);
                func.code.push_str(";\n");
            }
            func.code.push_str("}\n");

            name
        }

        // Applications
        SExpr::Application(_, l, r) => {
            // Get the list of arguments and the function
            let mut args = vec![r];
            let mut funcs = vec![sexpr];
            let mut f = &**l;
            while let SExpr::Application(_, l, r) = f {
                args.insert(0, r);
                funcs.insert(0, f);
                f = l;
            }

            // Quick hack for debug function
            // TODO: Make this not a hack
            if let SExpr::Symbol(m, v) = f {
                if v == "debug" {
                    let arg = convert_sexpr(&args[0], root, func, types);
                    put_debug_fn(
                        &m.loc,
                        &mut func.code,
                        &arg,
                        &args[0].get_metadata()._type,
                        root,
                        types,
                        true,
                    );
                    if args.len() == 1 {
                        return arg;
                    } else {
                        panic!("Using debug on multiple arguments is not supported");
                    }
                }
            }

            let mut _type = &f.get_metadata()._type;
            while let Type::Symbol(s) = _type {
                _type = root.types.get(s).unwrap();
            }

            let mut ftype = &f.get_metadata()._type;

            let mut unknown_arity = false;
            match _type {
                // Functions
                Type::Func(_, _) => {
                    // Get function and args
                    let mut fstr = if let SExpr::Function(m, _) = f {
                        if m.arity == 0 || m.saved_argc.is_none() || m.saved_argc.unwrap() != 0 {
                            let v = convert_sexpr(f, root, func, types);
                            let name = format!("$${}", func.last_reference);
                            func.last_reference += 1;
                            func.code.push_str("func_t ");
                            func.code.push_str(&name);
                            func.code.push_str(";\ncopy_func(&");
                            func.code.push_str(&name);
                            func.code.push_str(", &");
                            func.code.push_str(&v);
                            func.code.push_str(");\n");
                            name
                        } else {
                            String::with_capacity(0)
                        }
                    } else {
                        let v = convert_sexpr(f, root, func, types);
                        let name = format!("$${}", func.last_reference);
                        func.last_reference += 1;
                        func.code.push_str("func_t ");
                        func.code.push_str(&name);
                        func.code.push_str(";\ncopy_func(&");
                        func.code.push_str(&name);
                        func.code.push_str(", &");
                        func.code.push_str(&v);
                        func.code.push_str(");\n");
                        name
                    };

                    let mut astrs = vec![];
                    let mut name = String::with_capacity(0);
                    for a in args.iter().enumerate() {
                        // Get argument
                        let (n, a) = a;
                        let mut v = convert_sexpr(a, root, func, types);
                        let mut _type = &a.get_metadata()._type;
                        while let Type::Symbol(s) = _type {
                            _type = root.types.get(s).unwrap();
                        }

                        while let Type::Symbol(s) = ftype {
                            ftype = root.types.get(s).unwrap();
                        }

                        let mut arg_type = &**if let Type::Func(a, b) = &ftype {
                            ftype = b;
                            a
                        } else {
                            unreachable!("this is always a function")
                        };
                        while let Type::Symbol(s) = arg_type {
                            arg_type = root.types.get(s).unwrap();
                        }

                        if !f.get_metadata().tailrec {
                            match arg_type {
                                Type::Float => {
                                    let name = format!("$${}", func.last_reference);
                                    func.last_reference += 1;
                                    func.code.push_str("double_wrapper_t ");
                                    func.code.push_str(&name);
                                    func.code.push_str(";\n");
                                    func.code.push_str(&name);
                                    func.code.push_str(".d = ");
                                    func.code.push_str(&v);
                                    func.code.push_str(";\nvoid* ");
                                    v = format!("$${}", func.last_reference);
                                    func.last_reference += 1;
                                    func.code.push_str(&v);
                                    func.code.push_str(" = ");
                                    func.code.push_str(&name);
                                    func.code.push_str(".v;\n");
                                }

                                Type::Func(_, _) => {
                                    v = format!("&{}", &v);
                                }

                                Type::Sum(_) => {
                                    // Autocast argument if not already done so
                                    if _type == arg_type {
                                        v = format!("&{}", &v);
                                    } else if let Type::Sum(_) = _type {
                                        let name = format!("$${}", func.last_reference);
                                        func.last_reference += 1;
                                        let map =
                                            types.get(arg_type).unwrap().get_hashmap().unwrap();
                                        let subtype = types.get(_type).unwrap();
                                        let submap = subtype.get_hashmap().unwrap();
                                        func.code
                                            .push_str(types.get(arg_type).unwrap().get_c_name());
                                        func.code.push(' ');
                                        func.code.push_str(&name);
                                        func.code.push_str(";\n");
                                        func.code.push_str("switch (");
                                        func.code.push_str(&v);
                                        func.code.push_str(".tag) {\n");

                                        for s in submap {
                                            func.code.push_str("case ");
                                            func.code.push_str(&format!("{}:\n", s.1));
                                            func.code.push_str(&name);
                                            func.code.push_str(".tag = ");
                                            func.code
                                                .push_str(&format!("{};\n", map.get(s.0).unwrap()));
                                            func.code.push_str(&name);
                                            func.code.push_str(".values.$$");
                                            func.code
                                                .push_str(&format!("{}", map.get(s.0).unwrap()));
                                            func.code.push_str(" = ");
                                            func.code.push_str(&v);
                                            func.code.push_str(".values.$$");
                                            func.code.push_str(&format!("{}", s.1));
                                            func.code.push_str(";\nbreak;\n");
                                        }
                                        func.code.push_str("}\n");
                                        v = format!("&{}", name);
                                    } else {
                                        let name = format!("$${}", func.last_reference);
                                        func.last_reference += 1;
                                        let arg_ctype = types.get(arg_type).unwrap();
                                        func.code.push_str(arg_ctype.get_c_name());
                                        func.code.push(' ');
                                        func.code.push_str(&name);
                                        func.code.push_str(";\n");
                                        func.code.push_str(&name);
                                        func.code.push_str(".tag = ");
                                        func.code.push_str(&format!(
                                            "{};\n",
                                            arg_ctype.get_hashmap().unwrap().get(_type).unwrap()
                                        ));
                                        func.code.push_str(&name);
                                        func.code.push_str(".values.");
                                        func.code.push_str(&format!(
                                            "$${}",
                                            arg_ctype.get_hashmap().unwrap().get(_type).unwrap()
                                        ));
                                        func.code.push_str(" = ");
                                        func.code.push_str(&v);
                                        func.code.push_str(";\n");
                                        v = format!("&{}", name);
                                    }
                                }

                                _ => (),
                            }
                        } else if let Type::Sum(_) = arg_type {
                            // Autocast argument if not already done so
                            if _type == arg_type {
                            } else if let Type::Sum(_) = _type {
                                let name = format!("$${}", func.last_reference);
                                func.last_reference += 1;
                                let map = types.get(arg_type).unwrap().get_hashmap().unwrap();
                                let subtype = types.get(_type).unwrap();
                                let submap = subtype.get_hashmap().unwrap();
                                func.code
                                    .push_str(types.get(arg_type).unwrap().get_c_name());
                                func.code.push(' ');
                                func.code.push_str(&name);
                                func.code.push_str(";\n");
                                func.code.push_str("switch (");
                                func.code.push_str(&v);
                                func.code.push_str(".tag) {\n");

                                for s in submap {
                                    func.code.push_str("case ");
                                    func.code.push_str(&format!("{}:\n", s.1));
                                    func.code.push_str(&name);
                                    func.code.push_str(".tag = ");
                                    func.code.push_str(&format!("{};\n", map.get(s.0).unwrap()));
                                    func.code.push_str(&name);
                                    func.code.push_str(".values.$$");
                                    func.code.push_str(&format!("{}", map.get(s.0).unwrap()));
                                    func.code.push_str(" = ");
                                    func.code.push_str(&v);
                                    func.code.push_str(".values.$$");
                                    func.code.push_str(&format!("{}", s.1));
                                    func.code.push_str(";\nbreak;\n");
                                }
                                func.code.push_str("}\n");
                                v = name;
                            } else {
                                let name = format!("$${}", func.last_reference);
                                func.last_reference += 1;
                                let arg_ctype = types.get(arg_type).unwrap();
                                func.code.push_str(arg_ctype.get_c_name());
                                func.code.push(' ');
                                func.code.push_str(&name);
                                func.code.push_str(";\n");
                                func.code.push_str(&name);
                                func.code.push_str(".tag = ");
                                func.code.push_str(&format!(
                                    "{};\n",
                                    arg_ctype.get_hashmap().unwrap().get(_type).unwrap()
                                ));
                                func.code.push_str(&name);
                                func.code.push_str(".values.");
                                func.code.push_str(&format!(
                                    "$${}",
                                    arg_ctype.get_hashmap().unwrap().get(_type).unwrap()
                                ));
                                func.code.push_str(" = ");
                                func.code.push_str(&v);
                                func.code.push_str(";\n");
                                v = name;
                            }
                        }

                        // Functions with unknown arity
                        if f.get_metadata().arity == 0 || f.get_metadata().saved_argc.is_none() {
                            // Check for function list if just got into this state of unknown arity
                            if !unknown_arity {
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

                            if let Type::Func(_, _) = arg_type {
                                let name = format!("$${}", func.last_reference);
                                func.last_reference += 1;
                                func.code.push_str("func_t* ");
                                func.code.push_str(&name);
                                func.code.push_str(" = copy_func_arg(");
                                func.code.push_str(&v);
                                func.code.push_str(");\n");
                                v = name;
                                func.code.push_str(&fstr);
                                func.code.push_str(".cleaners[");
                                func.code.push_str(&fstr);
                                func.code.push_str(".argc] = force_free_func;\n");
                            } else if let Type::Sum(_) = arg_type {
                                let name = format!("$${}", func.last_reference);
                                func.last_reference += 1;
                                let type_name = types.get(_type).unwrap().get_c_name();
                                func.code.push_str(type_name);
                                func.code.push_str("* ");
                                func.code.push_str(&name);
                                func.code.push_str(" = malloc(sizeof(");
                                func.code.push_str(type_name);
                                func.code.push_str("));\n*");
                                func.code.push_str(&name);
                                func.code.push_str(" = ");
                                func.code.push_str(&v[1..]);
                                func.code.push_str(";\n");
                            }

                            // Save argument
                            let _type = &funcs[n].get_metadata()._type;
                            let is_enum = if let Type::Enum(_) = _type {
                                true
                            } else {
                                false
                            };
                            if is_enum {
                                func.code.push_str(&fstr);
                                func.code.push_str(".argc++;\n");
                            } else {
                                func.code.push_str(&fstr);
                                func.code.push_str(".args[");
                                func.code.push_str(&fstr);
                                func.code.push_str(".argc++] = (void*) ");
                                func.code.push_str(&v);
                                func.code.push_str(";\n");

                                // Call the function
                                name = format!("$${}", func.last_reference);
                                func.last_reference += 1;
                                func.code.push_str(get_c_type(_type, types));
                                func.code.push(' ');
                                func.code.push_str(&name);
                                if let Type::Func(_, _) = _type {
                                    func.code.push_str(" = ");
                                    func.code.push_str(&fstr);
                                }

                                func.code.push_str(";\n");
                            }

                            func.code.push_str("if (");
                            func.code.push_str(&fstr);
                            func.code.push_str(".arity == ");
                            func.code.push_str(&fstr);
                            func.code.push_str(".argc) {\n");
                            if !is_enum {
                                func.code.push_str(&name);
                                func.code.push_str(" = ");
                            }
                            func.code.push_str("((");
                            func.code.push_str(if is_enum {
                                "void"
                            } else {
                                get_c_type(_type, types)
                            });
                            func.code.push_str(" (*)(func_t*))");
                            func.code.push_str(&fstr);
                            func.code.push_str(".wrapper)(&");
                            func.code.push_str(&fstr);
                            func.code.push_str(");\n");

                            // Reset the list of arguments
                            if n != args.len() - 1 {
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
                        } else if f.get_metadata().arity <= astrs.len() + 1 {
                            // Get name
                            astrs.push((v, arg_type));
                            name = format!("$${}", func.last_reference);
                            func.last_reference += 1;

                            if f.get_metadata().tailrec {
                                for a in func.args.iter().enumerate() {
                                    if let Type::Enum(_) = a.1 .1 {
                                        continue;
                                    }

                                    func.code.push_str(&sanitise_symbol(a.1 .0));
                                    func.code.push_str("$PARAM$$ = ");
                                    func.code.push_str(&astrs[a.0].0);
                                    func.code.push_str(";\n");
                                }

                                func.code.push_str("$$LOOP$$ = 1;\n");
                                if let Type::Enum(_) = ftype {
                                } else {
                                    func.code.push_str(get_c_type(ftype, types));
                                    func.code.push(' ');
                                    func.code.push_str(&name);
                                    func.code.push_str(";\n");
                                }
                            } else {
                                let saved_argc = f.get_metadata().saved_argc.unwrap();
                                let is_enum = if let Type::Enum(_) = ftype {
                                    true
                                } else {
                                    false
                                };

                                if !is_enum {
                                    func.code.push_str(get_c_type(ftype, types));
                                    func.code.push(' ');
                                    func.code.push_str(&name);
                                    func.code.push_str(" = ");
                                }

                                if fstr == "" {
                                    // Get function name
                                    fstr = format!(
                                        "{}{}$FUNC$$",
                                        sanitise_symbol(&f.get_metadata().origin),
                                        if let SExpr::Function(_, f) = f {
                                            sanitise_symbol(f)
                                        } else {
                                            unreachable!("always a function");
                                        }
                                    );
                                    func.code.push_str(&fstr);
                                    func.code.push('(');

                                    // Pass arguments
                                    let mut comma = saved_argc > 0;
                                    for i in 0..f.get_metadata().arity {
                                        if let Type::Enum(_) = astrs[i].1 {
                                            continue;
                                        }

                                        if comma {
                                            func.code.push_str(", ");
                                        } else {
                                            comma = true;
                                        }

                                        func.code.push_str(&astrs[i].0);
                                    }

                                    // Close parentheses
                                    func.code.push_str(");\n");
                                } else {
                                    func.code.push_str("((");

                                    // Create function pointer
                                    func.code.push_str(if let Type::Enum(_) = ftype {
                                        "void"
                                    } else {
                                        get_c_type(ftype, types)
                                    });
                                    func.code.push_str(" (*)(");
                                    let mut comma = false;
                                    for i in 0..saved_argc + f.get_metadata().arity {
                                        if i >= saved_argc {
                                            if let Type::Enum(_) = astrs[i - saved_argc].1 {
                                                continue;
                                            }
                                        }

                                        if comma {
                                            func.code.push_str(", ");
                                        } else {
                                            comma = true;
                                        }
                                        func.code.push_str("void*");
                                    }

                                    // Call the function
                                    func.code.push_str(")) ");
                                    func.code.push_str(&fstr);
                                    func.code.push_str(".func)(");

                                    // Print saved arguments
                                    for i in 0..saved_argc {
                                        if i != 0 {
                                            func.code.push_str(", ");
                                        }

                                        func.code.push_str(&fstr);
                                        func.code.push_str(&format!(".args[{}]", i));
                                    }

                                    // Pass new arguments
                                    let mut comma = saved_argc > 0;
                                    for i in astrs.iter() {
                                        if let Type::Enum(_) = i.1 {
                                            continue;
                                        }

                                        if comma {
                                            func.code.push_str(", ");
                                        } else {
                                            comma = true;
                                        }

                                        func.code.push_str("(void*) ");
                                        func.code.push_str(&i.0);
                                    }

                                    // Close parentheses
                                    func.code.push_str(");\n");
                                }
                            }

                            if n < funcs.len() {
                                f = funcs[n];
                            }

                            for a in astrs.iter() {
                                match a.1 {
                                    Type::Func(_, _) => {}

                                    _ => (),
                                }
                            }

                            astrs.clear();

                            if n < args.len() - 1 {
                                fstr = name.clone();
                            }
                        } else {
                            astrs.push((v, arg_type));
                        }
                    }

                    // Currying
                    if astrs.len() > 0 {
                        if fstr == "" {
                            fstr = convert_sexpr(f, root, func, types)
                        }

                        // Get name
                        name = format!("$${}", func.last_reference);
                        func.last_reference += 1;

                        // Create new function structure
                        func.code.push_str("func_t ");
                        func.code.push_str(&name);
                        func.code.push_str(";\ncopy_func(&");
                        func.code.push_str(&name);
                        func.code.push_str(", &");
                        func.code.push_str(&fstr);
                        func.code.push_str(");\n");

                        // Init list
                        func.code.push_str("if (");
                        func.code.push_str(&name);
                        func.code.push_str(".cleaners == (void*) 0)\n");
                        func.code.push_str(&name);
                        func.code.push_str(".cleaners = calloc(");
                        func.code.push_str(&name);
                        func.code.push_str(".arity, sizeof(void*));\n");
                        func.code.push_str("if (");
                        func.code.push_str(&name);
                        func.code.push_str(".args == (void*) 0)\n");
                        func.code.push_str(&name);
                        func.code.push_str(".args = calloc(");
                        func.code.push_str(&name);
                        func.code.push_str(".arity, sizeof(void*));\n");

                        // Put in new args
                        for arg in astrs.iter_mut() {
                            // If it's a function or sum type allocate space in the heap for it
                            if let Type::Func(_, _) = arg.1 {
                                {
                                    let name = format!("$${}", func.last_reference);
                                    func.last_reference += 1;
                                    func.code.push_str("func_t* ");
                                    func.code.push_str(&name);
                                    func.code.push_str(" = copy_func_arg(");
                                    func.code.push_str(&arg.0);
                                    func.code.push_str(");\n");
                                    func.code.push_str(&name);
                                    func.code.push_str("->refc++;\n");
                                    arg.0 = name;
                                }

                                func.code.push_str(&name);
                                func.code.push_str(".cleaners[");
                                func.code.push_str(&name);
                                func.code.push_str(".argc] = force_free_func;\n");
                            } else if let Type::Sum(_) = arg.1 {
                                let name = format!("$${}", func.last_reference);
                                func.last_reference += 1;
                                let type_name = types.get(arg.1).unwrap().get_c_name();
                                func.code.push_str(type_name);
                                func.code.push_str("* ");
                                func.code.push_str(&name);
                                func.code.push_str(" = malloc(sizeof(");
                                func.code.push_str(type_name);
                                func.code.push_str("));\n*");
                                func.code.push_str(&name);
                                func.code.push_str(" = ");
                                func.code.push_str(&arg.0[1..]);
                                func.code.push_str(";\n");
                                arg.0 = name;
                            } else if let Type::Enum(_) = arg.1 {
                                continue;
                            }

                            func.code.push_str(&name);
                            func.code.push_str(".args[");
                            func.code.push_str(&name);
                            func.code.push_str(".argc++] = (void*) ");
                            func.code.push_str(&arg.0);
                            func.code.push_str(";\n");
                        }
                    }

                    name
                }

                // Everything else is unsupported
                _ => panic!("unsupported application of `{}` on {:?}!", _type, sexpr),
            }
        }

        // Assignments
        SExpr::Assign(m, a, v) => {
            let val = convert_sexpr(v, root, func, types);
            if a == "_" {
                return val;
            }
            if let Type::Enum(_) = m._type {
                return String::with_capacity(0);
            }

            let _type = if let Type::Symbol(_) = m._type {
                types.get(&m._type).unwrap().get_curly_type()
            } else {
                &m._type
            };
            let vtype = if let Type::Symbol(_) = v.get_metadata()._type {
                types.get(&v.get_metadata()._type).unwrap().get_curly_type()
            } else {
                &v.get_metadata()._type
            };
            let a = &sanitise_symbol(a);

            match _type {
                Type::Sum(_) if _type != vtype => {
                    // Get value and generate code
                    let map = types.get(&m._type).unwrap().get_hashmap().unwrap();
                    func.code.push_str(get_c_type(&m._type, types));
                    func.code.push(' ');
                    func.code.push_str(a);
                    func.code.push_str(";\n");
                    func.code.push_str(a);
                    func.code.push_str(".tag = ");
                    let id = format!("{}", map.get(&v.get_metadata()._type).unwrap());
                    func.code.push_str(&id);
                    func.code.push_str(";\n");
                    func.code.push_str(a);
                    func.code.push_str(".values.$$");
                    func.code.push_str(&id);
                    func.code.push_str(" = ");
                    func.code.push_str(&val);
                    func.code.push_str(";\n");

                    a.clone()
                }

                _ => {
                    // Get value and generate code
                    func.code.push_str(get_c_type(&m._type, types));
                    func.code.push(' ');
                    func.code.push_str(a);
                    func.code.push_str(" = ");
                    func.code.push_str(&val);
                    func.code.push_str(";\n");

                    // Increment reference counter
                    match m._type {
                        Type::Func(_, _) => {
                            func.code.push_str(&a);
                            func.code.push_str(".refc++;\n");
                        }

                        _ => (),
                    }

                    a.clone()
                }
            }
        }

        // With expressions
        SExpr::With(m, a, b) => {
            // Get name
            let mut name = String::with_capacity(0);

            // Declare variable
            let is_enum = if let Type::Enum(_) = m._type {
                true
            } else {
                false
            };
            if !is_enum {
                name = format!("$${}", func.last_reference);
                func.last_reference += 1;
                func.code.push_str(&get_c_type(&m._type, types));
                func.code.push(' ');
                func.code.push_str(&name);
                func.code.push_str(";\n");
            }

            func.code.push_str("{\n");

            // Assignments
            let mut astrs = vec![];
            for a in a {
                astrs.push(convert_sexpr(a, root, func, types));
            }

            // Body
            let body = convert_sexpr(b, root, func, types);
            let mut ptr = String::with_capacity(0);

            if !is_enum {
                ptr = format!("$${}", func.last_reference);
                func.last_reference += 1;
                func.code.push_str(&get_c_type(&m._type, types));
                func.code.push_str("* ");
                func.code.push_str(&ptr);
                func.code.push_str(" = &");
                func.code.push_str(&body);
                func.code.push_str(";\n");

                // Increment body reference count
                match m._type {
                    Type::Func(_, _) => {
                        func.code.push_str(&ptr);
                        func.code.push_str("->refc++;\n");
                    }

                    _ => (),
                }
            }

            // Decrement assignment reference counts and free if necessary
            for a in a.iter().enumerate() {
                match a.1.get_metadata()._type {
                    Type::Func(_, _) => {
                        func.code.push_str("refc_func(&");
                        func.code.push_str(&astrs[a.0]);
                        func.code.push_str(");\n");
                    }

                    _ => (),
                }
            }

            // Decrement body reference count
            match m._type {
                Type::Func(_, _) => {
                    func.code.push_str(&ptr);
                    func.code.push_str("->refc--;\n");
                }

                _ => (),
            }

            // Copy data from ptr
            if !is_enum {
                func.code.push_str(&name);
                func.code.push_str(" = *");
                func.code.push_str(&ptr);
                func.code.push_str(";\n");
            }

            // Exit block and return success
            func.code.push_str("}\n");
            name
        }

        SExpr::Walrus(m, n, v) => {
            let v = convert_sexpr(v, root, func, types);
            if let Type::Enum(_) = m._type {
                v
            } else {
                let a = sanitise_symbol(n);
                func.code.push_str(get_c_type(&m._type, types));
                func.code.push(' ');
                func.code.push_str(&a);
                func.code.push_str(" = ");
                func.code.push_str(&v);
                func.code.push_str(";\n");
                a
            }
        }

        SExpr::Match(m, v, a) => {
            // Get value and name
            let value = convert_sexpr(v, root, func, types);
            let _type = types.get(&v.get_metadata()._type).unwrap();
            let map = _type.get_hashmap().unwrap();

            // Create switch statement
            let name = if let Type::Enum(_) = m._type {
                String::with_capacity(0)
            } else {
                let name = format!("$${}", func.last_reference);
                func.last_reference += 1;
                func.code.push_str(get_c_type(&m._type, types));
                func.code.push(' ');
                func.code.push_str(&name);
                func.code.push_str(";\n");
                name
            };

            func.code.push_str("switch (");
            func.code.push_str(&value);
            func.code.push_str(".tag) {\n");

            let mut mtype = &m._type;
            while let Type::Symbol(s) = mtype {
                mtype = root.types.get(s).unwrap();
            }

            // Create match arms
            for a in a.iter() {
                let mut _type = &a.0;
                let varname = if let Type::Tag(s, t) = _type {
                    _type = &**t;
                    Some(sanitise_symbol(s))
                } else {
                    None
                };

                while let Type::Symbol(s) = _type {
                    _type = root.types.get(s).unwrap();
                }

                let mut vtype = if let Type::Tag(_, t) = _type {
                    &**t
                } else {
                    _type
                };

                while let Type::Symbol(s) = vtype {
                    vtype = root.types.get(s).unwrap();
                }

                if let Type::Sum(_) = vtype {
                    let subtype = types.get(vtype).unwrap();
                    let submap = subtype.get_hashmap().unwrap();
                    for s in submap {
                        func.code.push_str("case ");
                        func.code
                            .push_str(&format!("{}:\n", map.get(&s.0).unwrap()));
                    }

                    func.code.push_str("{\n");
                    func.code.push_str(subtype.get_c_name());
                    func.code.push_str(" $$MATCHTEMP$$;\nswitch (");
                    func.code.push_str(&value);
                    func.code.push_str(".tag) {\n");

                    for s in submap {
                        func.code.push_str("case ");
                        func.code.push_str(&format!("{}:\n", map.get(s.0).unwrap()));
                        func.code.push_str("$$MATCHTEMP$$.tag = ");
                        func.code.push_str(&format!("{};\n", s.1));
                        func.code.push_str("$$MATCHTEMP$$.values.$$");
                        func.code.push_str(&format!("{}", s.1));
                        func.code.push_str(" = ");
                        func.code.push_str(&value);
                        func.code.push_str(".values.$$");
                        func.code.push_str(&format!("{}", map.get(s.0).unwrap()));
                        func.code.push_str(";\nbreak;\n");
                    }
                    func.code.push_str("}\n");
                    if let Some(s) = &varname {
                        func.code.push_str(subtype.get_c_name());
                        func.code.push(' ');
                        func.code.push_str(s);
                        func.code.push_str(" = $$MATCHTEMP$$;\n");
                    }
                } else if let Type::Enum(_) = _type {
                    let id = map.get(_type).unwrap();
                    func.code.push_str("case ");
                    func.code.push_str(&format!("{}: {{\n", id));
                } else {
                    let id = map.get(_type).unwrap();
                    func.code.push_str("case ");
                    func.code.push_str(&format!("{}: {{\n", id));

                    func.code.push_str(get_c_type(vtype, types));
                    func.code.push_str(" $$MATCHTEMP$$ = ");
                    func.code.push_str(&value);
                    func.code.push_str(".values.$$");
                    func.code.push_str(&format!("{}", id));
                    func.code.push_str(";\n");

                    if let Some(s) = &varname {
                        func.code.push_str(get_c_type(vtype, types));
                        func.code.push(' ');
                        func.code.push_str(s);
                        func.code.push_str(" = $$MATCHTEMP$$;\n");
                    }
                }

                let arm = convert_sexpr(&a.1, root, func, types);

                let mut atype = &a.1.get_metadata()._type;
                while let Type::Symbol(s) = atype {
                    atype = root.types.get(s).unwrap();
                }

                if atype == mtype {
                    if let Type::Enum(_) = atype {
                    } else {
                        func.code.push_str(&name);
                        func.code.push_str(" = ");
                        func.code.push_str(&arm);
                        func.code.push_str(";\n");
                    }
                } else if let Type::Sum(_) = atype {
                    let _type = types.get(&m._type).unwrap();
                    let map = _type.get_hashmap().unwrap();
                    let subtype = types.get(atype).unwrap();
                    let submap = subtype.get_hashmap().unwrap();

                    func.code.push_str("switch (");
                    func.code.push_str(&arm);
                    func.code.push_str(".tag) {\n");

                    for s in submap {
                        let id = map.get(s.0).unwrap();
                        func.code.push_str("case ");
                        func.code.push_str(&format!("{}:\n", s.1));
                        func.code.push_str(&name);
                        func.code.push_str(".tag = ");
                        func.code.push_str(&format!("{};\n", id));

                        if let Type::Enum(_) = s.0 {
                        } else {
                            func.code.push_str(&name);
                            func.code.push_str(".values.$$");
                            func.code.push_str(&format!("{}", id));
                            func.code.push_str(" = ");
                            func.code.push_str(&arm);
                            func.code.push_str(".values.$$");
                            func.code.push_str(&format!("{};\n", s.1));
                        }
                        func.code.push_str("break;\n");
                    }
                    func.code.push_str("}\n");
                } else if let Type::Enum(_) = atype {
                    let _type = types.get(&m._type).unwrap();
                    let map = _type.get_hashmap().unwrap();
                    let id = map.get(&atype).unwrap();
                    func.code.push_str(&name);
                    func.code.push_str(".tag = ");
                    func.code.push_str(&format!("{};\n", id));
                } else {
                    let _type = types.get(&m._type).unwrap();
                    let map = _type.get_hashmap().unwrap();
                    let id = map.get(atype).unwrap();
                    func.code.push_str(&name);
                    func.code.push_str(".tag = ");
                    func.code.push_str(&format!("{}", id));
                    func.code.push_str(";\n");
                    func.code.push_str(&name);
                    func.code.push_str(".values.$$");
                    func.code.push_str(&format!("{}", id));
                    func.code.push_str(" = ");
                    func.code.push_str(&arm);
                    func.code.push_str(";\n");
                }

                func.code.push_str("break;\n}\n");
            }
            func.code.push_str("}\n");

            name
        }

        SExpr::MemberAccess(m, a) => {
            let mut _type = &m._type;
            while let Type::Symbol(s) = _type {
                _type = root.types.get(s).unwrap();
            }

            if let Type::Sum(_) = &_type {
                let name = format!("$${}", func.last_reference);
                func.last_reference += 1;
                let t = types.get(&m._type).unwrap();
                func.code.push_str(t.get_c_name());
                func.code.push(' ');
                func.code.push_str(&name);
                func.code.push_str(";\n");
                func.code.push_str(&name);
                func.code.push_str(".tag = ");
                func.code.push_str(&format!(
                    "{}",
                    t.get_hashmap()
                        .unwrap()
                        .get(&Type::Enum(a[1].clone()))
                        .unwrap()
                ));
                func.code.push_str(";\n");

                name
            } else {
                panic!("unimplemented member access");
            }
        }

        _ => panic!("unimplemented s expression!"),
    }
}

// put_fn_wrapper(&mut String, &CFunction) -> ()
// Puts the wrapper for a given function in the built string.
fn put_fn_wrapper(s: &mut String, mod_name: &str, func: &CFunction, types: &HashMap<Type, CType>) {
    s.push_str(if let Type::Enum(_) = func.ret_type {
        "void"
    } else {
        get_c_type(func.ret_type, types)
    });
    s.push(' ');
    s.push_str(&sanitise_symbol(mod_name));
    s.push_str(&func.name);
    s.push_str("$WRAPPER$$");
    s.push_str("(func_t* f) {\nreturn ((");
    s.push_str(if let Type::Enum(_) = func.ret_type {
        "void"
    } else {
        get_c_type(func.ret_type, types)
    });
    s.push_str(" (*) (");

    let mut comma = false;
    for a in func.args.iter() {
        if let Type::Enum(_) = a.1 {
            continue;
        }

        if comma {
            s.push_str(", ");
        } else {
            comma = true;
        }

        s.push_str("void*");
    }

    s.push_str(")) f->func)(");

    for i in 0..func.args.len() {
        if i != 0 {
            s.push_str(", ");
        }

        s.push_str("f->args[");
        s.push_str(&format!("{}", i));
        s.push_str("]");
    }

    s.push_str(");\n}\n");
}

// put_fn_declaration(&mut String, &CFunction, &HashMap<Type, CType>) -> ()
// Puts a function declaration in the built string.
fn put_fn_declaration(
    s: &mut String,
    mod_name: &str,
    func: &CFunction,
    types: &HashMap<Type, CType>,
) {
    s.push_str(if let Type::Enum(_) = func.ret_type {
        "void"
    } else {
        get_c_type(func.ret_type, types)
    });
    s.push(' ');
    s.push_str(&sanitise_symbol(mod_name));
    s.push_str(&func.name);
    s.push_str("$FUNC$$");
    s.push('(');

    let mut comma = false;
    for a in func.args.iter() {
        let mut _type = a.1;
        if let Type::Symbol(_) = &_type {
            _type = types.get(&_type).unwrap().get_curly_type();
        }

        if let Type::Enum(_) = _type {
            continue;
        }

        if comma {
            s.push_str(", ");
        } else {
            comma = true;
        }

        s.push_str(get_c_type(_type, types));
        s.push(' ');
        match _type {
            Type::Float | Type::Func(_, _) | Type::Sum(_) => s.push_str("*$$"),
            _ => (),
        }

        s.push_str(&sanitise_symbol(a.0));
    }

    s.push(')');
}

// get_lino(&Location, &str) -> usize
// Gets the line number of the span.
fn get_lino(span: &Location, contents: &str) -> usize {
    let mut lino = 1;
    for c in contents.bytes().enumerate() {
        if c.1 == '\n' as u8 {
            lino += 1;
        }
        if span.span.start <= c.0 && c.0 <= span.span.end {
            break;
        }
    }

    lino
}

// put_debug_fn(&mut String, &str, &Type, &IRModule, &HashMap<Type, CType>, bool) -> ()
// Puts a debug function in the built string.
fn put_debug_fn(
    span: &Location,
    code: &mut String,
    v: &str,
    _type: &Type,
    ir: &IRModule,
    types: &HashMap<Type, CType>,
    newline: bool,
) {
    if newline {
        code.push_str(&format!(
            "printf(\"[{}:{}] = \");\n",
            &ir.filename,
            get_lino(span, &ir.contents)
        ));
    }

    let original_type = _type;
    let mut _type = _type;
    while let Type::Symbol(v) = _type {
        _type = ir.types.get(v).unwrap()
    }

    match _type {
        // Print out primatives
        Type::Int => {
            let ptr_size = std::mem::size_of::<&char>();
            code.push_str("printf(\"");
            match ptr_size {
                4 => code.push_str("%i\", "),
                8 => code.push_str("%lli\", "),
                _ => panic!("unsupported pointer size {}", ptr_size),
            }
            code.push_str(v);
            code.push_str(");\n");
        }

        Type::Float => {
            code.push_str("printf(\"%.5f\", ");
            code.push_str(v);
            code.push_str(");\n");
        }

        Type::Word => {
            let ptr_size = std::mem::size_of::<&char>();
            code.push_str("printf(\"");
            match ptr_size {
                4 => code.push_str("%uu\", "),
                8 => code.push_str("%lluu\", "),
                _ => panic!("unsupported pointer size {}", ptr_size),
            }
            code.push_str(v);
            code.push_str(");\n");
        }

        Type::Char => {
            code.push_str("printf(\"'%c'\", ");
            code.push_str(v);
            code.push_str(");\n");
        }

        Type::Bool => {
            code.push_str("printf(\"%s\", ");
            code.push_str(v);
            code.push_str(" ? \"true\" : \"false\");\n");
        }

        // Print out aggregate types
        Type::Func(_, _) => {
            code.push_str(&format!("printf(\"<func %p> : {}\", ", original_type));
            code.push_str(v);
            code.push_str(".func);\n");
        }

        Type::Sum(_) => {
            code.push_str("switch (");
            code.push_str(v);
            code.push_str(".tag) {\n");
            let _type = types.get(_type).unwrap();

            if let CType::Sum(_, _, fields) = _type {
                for field in fields.iter() {
                    code.push_str(&format!("case {}: {{\n", field.1));
                    put_debug_fn(
                        span,
                        code,
                        &format!("{}.values.$${}", v, field.1),
                        &field.0,
                        ir,
                        types,
                        false,
                    );
                    code.push_str("break;\n}\n");
                }
            }
            code.push_str("}\n");
            code.push_str(&format!("printf(\" : {}\");\n", original_type));
        }

        Type::Enum(v) => {
            code.push_str("printf(\"enum ");
            code.push_str(v);
            code.push_str("\");\n");
        }

        Type::Pointer(p) => {
            code.push_str(&format!("printf(\"{} <%p>\", ", p));
            code.push_str(v);
            code.push_str(");\n");
        }

        Type::Tag(s, t) => {
            code.push_str("printf(\"{ ");
            code.push_str(s);
            code.push_str(" = \");\n");
            put_debug_fn(span, code, v, t, ir, types, false);
            code.push_str("printf(\" }\");\n");
        }

        _ => panic!("uwu"),
    }

    if newline {
        code.push_str("printf(\"\\n\");\n");
    }
}

// collect_types(&IRModule, &mut HashMap<Type, String>, &mut String) -> ()
// Collects user defined types into a string containing all type definitions.
fn collect_types(
    ir: &IRModule,
    types: &mut HashMap<Type, CType>,
    types_string: &mut String,
    last_reference: &mut usize,
) {
    // Iterate over every type
    for _type in ir.types.iter().filter(|v| {
        if let Type::Symbol(_) = v.1 {
            false
        } else {
            true
        }
    }) {
        // Check if the type was actually already added
        // This is necessary if you define types like so:
        // type A = Int
        // type B = A -> Int
        // type C = Int -> A
        // type D = Int -> Int
        // type E = A -> A
        // B, C, D, and E are all the same type
        let mut add = None;
        for t in types.iter() {
            if _type.1.equals(t.0, &ir.types) {
                add = Some(t.1.clone());
                break;
            }
        }

        if let Some(add) = add {
            types.insert(_type.1.clone(), add);
            continue;
        }

        match _type.1 {
            // Primitives get mapped to old type
            Type::Int => {
                types.insert(
                    Type::Symbol(_type.0.clone()),
                    CType::Primitive(String::from("int_t"), _type.1.clone()),
                );
            }

            Type::Float => {
                types.insert(
                    Type::Symbol(_type.0.clone()),
                    CType::Primitive(String::from("float_t"), _type.1.clone()),
                );
            }

            Type::Word => {
                types.insert(
                    Type::Symbol(_type.0.clone()),
                    CType::Primitive(String::from("word_t"), _type.1.clone()),
                );
            }

            Type::Char => {
                types.insert(
                    Type::Symbol(_type.0.clone()),
                    CType::Primitive(String::from("char"), _type.1.clone()),
                );
            }

            Type::Bool => {
                types.insert(
                    Type::Symbol(_type.0.clone()),
                    CType::Primitive(String::from("bool"), _type.1.clone()),
                );
            }

            Type::Func(_, _) => {
                types.insert(
                    Type::Symbol(_type.0.clone()),
                    CType::Primitive(String::from("func_t"), _type.1.clone()),
                );
            }

            Type::Pointer(_) => {
                types.insert(
                    Type::Symbol(_type.0.clone()),
                    CType::Primitive(String::from("void*"), _type.1.clone()),
                );
            }

            // Sum types are tagged unions
            Type::Sum(v) => {
                types_string.push_str(&format!(
                    "struct $${} {{\n    unsigned int tag;\n    union {{\n",
                    last_reference
                ));

                let mut field_ref = 0;
                let mut iter: Vec<&Type> = v.0.iter().collect();
                while let Some(t) = iter.get(field_ref) {
                    let mut t = *t;
                    while let Type::Symbol(s) = t {
                        t = ir.types.get(s).unwrap();
                    }

                    if &t != iter.get(field_ref).unwrap() {
                        iter[field_ref] = t;
                    }

                    match t {
                        Type::Int => types_string.push_str("        int_t"),
                        Type::Float => types_string.push_str("        float_t"),
                        Type::Bool => types_string.push_str("        bool"),
                        Type::Char => types_string.push_str("        char"),
                        Type::Word => types_string.push_str("        word_t"),
                        Type::Func(_, _) => types_string.push_str("        func_t"),

                        Type::Enum(_) => {
                            field_ref += 1;
                            continue;
                        }

                        Type::Sum(v) => {
                            for v in v.0.iter() {
                                if !iter.contains(&v) {
                                    iter.push(v);
                                }
                            }
                            iter.remove(field_ref);
                            continue;
                        }

                        Type::Tag(_, t) => match &**t {
                            Type::Int => types_string.push_str("        int_t"),
                            Type::Float => types_string.push_str("        float_t"),
                            Type::Bool => types_string.push_str("        bool"),
                            Type::Func(_, _) => types_string.push_str("        func_t"),
                            _ => panic!("unsupported type!"),
                        },

                        _ => panic!("unsupported type!"),
                    }

                    types_string.push_str(&format!(" $${};\n", field_ref));
                    field_ref += 1;
                }

                // Save type definitions
                let map =
                    HashMap::from_iter(iter.into_iter().cloned().enumerate().filter_map(|v| {
                        if let Type::Sum(_) = v.1 {
                            None
                        } else {
                            Some((v.1, v.0))
                        }
                    }));
                let ct = CType::Sum(format!("struct $${}", last_reference), _type.1.clone(), map);
                types_string.push_str("    } values;\n};\n\n");
                types.insert(_type.1.clone(), ct.clone());
                types.insert(Type::Symbol(_type.0.clone()), ct);
                *last_reference += 1;
            }

            _ => (),
        }
    }

    // Do symbols
    for _type in ir.types.iter() {
        // Symbols get mapped to last type in chain
        if let Type::Symbol(_s) = _type.1 {
            let mut s = _s;
            let __type = loop {
                let _type = ir.types.get(s).unwrap();
                match _type {
                    Type::Symbol(v) => s = v,
                    _ => break _type,
                }
            };

            types.insert(
                Type::Symbol(_type.0.clone()),
                types.get(__type).unwrap().clone(),
            );
        }
    }
}

// collect_type_functions(&IRModule, &HashMap<Type, CType>, &mut String, &mut String) -> ()
// Collect type functions (ie, in type Option 'a = Some: 'a | enum None, Option::Some is a type
// function).
fn collect_type_functions(
    ir: &IRModule,
    types: &HashMap<Type, CType>,
    header: &mut String,
    bodies: &mut String,
) {
    'a: for t in types.iter() {
        // Find symbols
        if let Type::Symbol(tname) = t.0 {
            // Skip if the first character is a digit
            if "0123456789".contains(tname.chars().next().unwrap()) {
                continue;
            }

            // Get type
            let mut t = t;
            while let Type::Symbol(s) = t.0 {
                if let Some(v) = ir.types.get(s) {
                    t.0 = v;
                } else {
                    continue 'a;
                }
            }

            // Find sum types
            if let Type::Sum(f) = t.0 {
                for v in f.0.iter() {
                    // Find tags
                    if let Type::Tag(fname, t2) = v {
                        let sanitised = sanitise_symbol(&format!("{}::{}", tname, fname));
                        let aname = String::from("arg");
                        let cf = CFunction {
                            name: sanitised,
                            args: vec![(&aname, &**t2)],
                            ret_type: t.0,
                            code: String::with_capacity(0),
                            last_reference: 0,
                        };

                        put_fn_declaration(header, &ir.name, &cf, types);
                        header.push_str(";\n\n");
                        header.push_str(if let Type::Enum(_) = cf.ret_type {
                            "void"
                        } else {
                            get_c_type(cf.ret_type, types)
                        });
                        header.push(' ');
                        header.push_str(&sanitise_symbol(&ir.name));
                        header.push_str(&cf.name);
                        header.push_str("$WRAPPER$$");
                        header.push_str("(func_t* f);\n\n");

                        put_fn_declaration(bodies, &ir.name, &cf, types);
                        let mut last_reference = 0;

                        // Build function
                        bodies.push_str(" {\n");
                        fix_argument(&(&aname, &**t2), ir, bodies, types, &mut last_reference);
                        bodies.push_str(get_c_type(t.0, types));
                        bodies.push_str(" result;\nresult.tag = ");
                        let id = *types
                            .get(t.0)
                            .unwrap()
                            .get_hashmap()
                            .unwrap()
                            .get(v)
                            .unwrap();
                        bodies.push_str(&format!("{}", id));
                        bodies.push_str(";\nresult.values.$$");
                        bodies.push_str(&format!("{}", id));
                        bodies.push_str(" = arg$;\nreturn result;\n}\n");

                        // Build wrapper
                        put_fn_wrapper(bodies, &ir.name, &cf, types)
                    }
                }
            }
        }
    }
}

// fix_argument(&(String, &Type), &IRModule, &mut String, &HashMap<Type, CType>, &mut usize) -> ()
// Fixes an argument where necessary.
fn fix_argument(
    a: &(&String, &Type),
    ir: &IRModule,
    code: &mut String,
    types: &HashMap<Type, CType>,
    last_reference: &mut usize,
) {
    let mut _type = a.1;
    while let Type::Symbol(s) = _type {
        _type = ir.types.get(s).unwrap();
    }

    match _type {
        Type::Float => {
            // Get name
            let name = format!("$${}", last_reference);
            *last_reference += 1;
            let arg_name = sanitise_symbol(&a.0);

            // Convert pointer to double
            code.push_str("double_wrapper_t ");
            code.push_str(&name);
            code.push_str(";\n");
            code.push_str(&name);
            code.push_str(".v = $$");
            code.push_str(&arg_name);
            code.push_str(";\nfloat_t ");
            code.push_str(&arg_name);
            code.push_str(" = ");
            code.push_str(&name);
            code.push_str(".d;\n");
        }

        Type::Func(_, _) => {
            // Copy
            let arg_name = sanitise_symbol(&a.0);
            code.push_str("func_t ");
            code.push_str(&arg_name);
            code.push_str(";\ncopy_func(&");
            code.push_str(&arg_name);
            code.push_str(", $$");
            code.push_str(&arg_name);
            code.push_str(");\n");
            code.push_str(&arg_name);
            code.push_str(".refc++;\n");
        }

        Type::Sum(_) => {
            // Copy
            let arg_name = sanitise_symbol(&a.0);
            let type_name = types.get(&a.1).unwrap().get_c_name();
            code.push_str(type_name);
            code.push(' ');
            code.push_str(&arg_name);
            code.push_str(" = *$$");
            code.push_str(&arg_name);
            code.push_str(";\n");
        }

        _ => (),
    }
}

// convert_module_to_c(&IRModule, Option<&mut Vec<String>>) -> String
// Converts Curly IRModule to C code.
fn convert_module_to_c(
    module: &IRModule,
    funcs: &mut HashMap<String, CFunction>,
    types: &mut HashMap<Type, CType>,
) -> String {
    // Create and populate functions
    for f in module.funcs.iter() {
        if f.1.written || f.1.args.len() == 0 {
            continue;
        }

        let cf = funcs.get_mut(f.0).unwrap();

        // Fix doubles and functions
        for a in cf.args.iter() {
            fix_argument(a, module, &mut cf.code, &types, &mut cf.last_reference);
        }

        if f.1.body.get_metadata().tailrec {
            for a in cf.args.iter() {
                if let Type::Enum(_) = a.1 {
                    continue;
                }

                cf.code.push_str(get_c_type(a.1, &types));
                cf.code.push(' ');
                cf.code.push_str(&sanitise_symbol(a.0));
                cf.code.push_str("$PARAM$$;\n");
            }

            cf.code.push_str("bool $$LOOP$$ = 1;\n");
            if let Type::Enum(_) = f.1.body.get_metadata()._type {
            } else {
                cf.code
                    .push_str(get_c_type(&f.1.body.get_metadata()._type, &types));
                cf.code.push_str(" $$RET$$;\n");
            }
            cf.code.push_str("while ($$LOOP$$) {\n$$LOOP$$ = 0;\n");
        }

        let last = convert_sexpr(&f.1.body, module, cf, &types);

        if f.1.body.get_metadata().tailrec {
            for a in cf.args.iter() {
                if let Type::Enum(_) = a.1 {
                    continue;
                }

                cf.code.push_str(&sanitise_symbol(a.0));
                cf.code.push_str(" = ");
                cf.code.push_str(&sanitise_symbol(a.0));
                cf.code.push_str("$PARAM$$");
                cf.code.push_str(";\n");
            }

            if let Type::Enum(_) = f.1.body.get_metadata()._type {
            } else {
                cf.code.push_str("$$RET$$ = ");
                cf.code.push_str(&last);
                cf.code.push_str(";\n");
            }
            cf.code.push_str("}\n");
        }

        // Deallocate functions
        for a in cf.args.iter() {
            match a.1 {
                Type::Func(_, _) => {
                    // Delete
                    cf.code.push_str("refc_func(&");
                    cf.code.push_str(&sanitise_symbol(&a.0));
                    cf.code.push_str(");\n");
                }

                _ => (),
            }
        }

        // Return statement
        if let Type::Enum(_) = f.1.body.get_metadata()._type {
        } else {
            cf.code.push_str("return ");
            if f.1.body.get_metadata().tailrec {
                cf.code.push_str("$$RET$$");
            } else {
                cf.code.push_str(&last);
            }
            cf.code.push_str(";\n");
        }
    }

    // Import modules
    let mut code_string = format!("#include \"{}.h\"\n\n", sanitise_symbol(&module.name));
    for import in module.imports.iter() {
        code_string.push_str("#include \"");
        code_string.push_str(&sanitise_symbol(&import.1.name));
        code_string.push_str(".h\"\n");
    }
    code_string.push('\n');

    // Declare all functions
    for f in funcs.iter() {
        if f.1.args.len() != 0 {
            put_fn_declaration(&mut code_string, &module.name, &f.1, &types);
            code_string.push_str(";\n\n");
            put_fn_wrapper(&mut code_string, &module.name, &f.1, &types);
            code_string.push('\n');
        }
    }

    // Declare getter functions
    for s in module.sexprs.iter() {
        if let SExpr::Assign(m, n, v) = s {
            code_string.push_str(if let Type::Enum(_) = m._type {
                "void"
            } else {
                get_c_type(&v.get_metadata()._type, types)
            });
            code_string.push(' ');
            code_string.push_str(&sanitise_symbol(&module.name));
            code_string.push_str(&sanitise_symbol(n));
            code_string.push_str("$GET$$();\n\n");
        }
    }

    // Put all function definitions
    for f in funcs {
        if f.1.args.len() == 0 {
            continue;
        }

        put_fn_declaration(&mut code_string, &module.name, &f.1, &types);
        code_string.push_str(" {\n");
        code_string.push_str(&f.1.code);
        code_string.push_str("}\n");
    }

    // Create getters
    for s in module.sexprs.iter() {
        if let SExpr::Assign(m, n, v) = s {
            if let SExpr::Function(_, f) = &**v {
                // Get function
                let v = module.funcs.get(f).unwrap();
                if v.args.len() != 0 {
                    continue;
                }
                let impure = v.impure;
                let v = &v.body;

                // Create getter
                let is_enum = if let Type::Enum(_) = m._type {
                    true
                } else {
                    false
                };
                let sanitised = sanitise_symbol(n);
                let mod_name = sanitise_symbol(&module.name);

                if !is_enum && !impure {
                    code_string.push_str(get_c_type(&v.get_metadata()._type, types));
                    code_string.push(' ');
                    code_string.push_str(&mod_name);
                    code_string.push_str(&sanitised);
                    code_string.push_str("$VALUE$$;\nbool ");
                    code_string.push_str(&mod_name);
                    code_string.push_str(&sanitised);
                    code_string.push_str("$SAVED$$ = 0;\n");
                }

                code_string.push_str(if is_enum {
                    "void"
                } else {
                    get_c_type(&v.get_metadata()._type, types)
                });
                code_string.push(' ');
                code_string.push_str(&mod_name);
                code_string.push_str(&sanitised);
                code_string.push_str("$GET$$() {\n");

                // Tail call optimisation
                if v.get_metadata().tailrec {
                    code_string.push_str("bool $$LOOP$$ = 1;\nwhile ($$LOOP$$) {\n$$LOOP$$ = 0;\n");
                }

                if !is_enum && !impure {
                    code_string.push_str("if (");
                    code_string.push_str(&mod_name);
                    code_string.push_str(&sanitised);
                    code_string.push_str("$SAVED$$)\nreturn ");
                    code_string.push_str(&mod_name);
                    code_string.push_str(&sanitised);
                    code_string.push_str("$VALUE$$;\n");
                }

                let mut func = CFunction {
                    name: String::with_capacity(0),
                    args: Vec::with_capacity(0),
                    ret_type: &Type::Unknown,
                    code: code_string,
                    last_reference: 0,
                };
                let ret = convert_sexpr(v, module, &mut func, types);
                code_string = func.code;

                if !impure && !is_enum {
                    code_string.push_str(&mod_name);
                    code_string.push_str(&sanitised);
                    code_string.push_str("$SAVED$$ = 1;\n");
                    code_string.push_str(&mod_name);
                    code_string.push_str(&sanitised);
                    code_string.push_str("$VALUE$$ = ");
                    code_string.push_str(&ret);
                    code_string.push_str(";\n");
                }

                if v.get_metadata().tailrec {
                    code_string.push_str("}\n");
                }

                if !is_enum {
                    code_string.push_str("return ");
                    code_string.push_str(&ret);
                    code_string.push_str(";\n");
                }

                code_string.push_str("}\n");
            }
        }
    }

    if module.name == "Main" && module.funcs.contains_key("main") {
        code_string.push_str("int_t main(void) {\n    ");
        let _type = &module.funcs.get("main").unwrap().body.get_metadata()._type;
        if *_type == Type::Int {
            code_string.push_str("return Main$main$$GET$$();\n");
        } else if let Type::Enum(v) = _type {
            code_string.push_str("Main$main$$GET$$();\n    ");
            if v == "Ok" {
                code_string.push_str("return 0;\n");
            } else {
                code_string.push_str("return 1;\n");
            }
        } else if let Type::Sum(v) = _type {
            if v.0.contains(&Type::Enum(String::from("Ok"))) {
                code_string.push_str(get_c_type(_type, types));
                code_string.push_str(" v = Main$main$$GET$$();\n    if (v.tag == ");
                code_string.push_str(&format!(
                    "{}",
                    types
                        .get(_type)
                        .unwrap()
                        .get_hashmap()
                        .unwrap()
                        .get(&Type::Enum(String::from("Ok")))
                        .unwrap()
                ));
                code_string.push_str(") \n        return 0;\n    else\n        return 1;\n");
            } else {
                code_string.push_str("Main$main$$GET$$();\n    return 1;\n");
            }
        } else {
            code_string.push_str("Main$main$$GET$$();\n    return 1;\n");
        }
        code_string.push_str("}\n");
    }

    code_string
}

// generate_header_files(&IRModule, &HashMap<String, CFunction>, &HashMap<Type, CType>) -> (String, String)
// Generates the header file for a given module.
fn generate_header_files(
    module: &IRModule,
    funcs: &HashMap<String, CFunction>,
    types: &HashMap<Type, CType>,
) -> (String, String) {
    // Include Curly stuff
    let mut filename = sanitise_symbol(&module.name);
    let mut header = format!(
        "#ifndef {}_H\n#define {}_H\n#include \"curly.h\"\n#include \"types.h\"\n",
        filename, filename
    );

    for export in module.exports.iter() {
        // Put function declaration
        let mut add_getter = false;
        if let Some(f) = funcs.get(export.0) {
            if f.args.len() != 0 {
                put_fn_declaration(&mut header, &module.name, f, types);
                header.push_str(";\n\n");
                header.push_str(if let Type::Enum(_) = f.ret_type {
                    "void"
                } else {
                    get_c_type(f.ret_type, types)
                });
                header.push(' ');
                header.push_str(&sanitise_symbol(&module.name));
                header.push_str(&f.name);
                header.push_str("$WRAPPER$$");
                header.push_str("(func_t* f);\n\n");
            } else {
                add_getter = true;
            }
        } else {
            add_getter = true;
        }

        // Put getter
        if add_getter {
            header.push_str(get_c_type(&export.1 .1, types));
            header.push(' ');
            header.push_str(&sanitise_symbol(&module.name));
            header.push_str(&sanitise_symbol(export.0));
            header.push_str("$GET$$();\n\n");
        }
    }

    header.push_str("#endif\n");
    filename.push_str(".h");

    (filename, header)
}

// convert_ir_to_c(&IR) -> Vec<(String, String)>
// Converts an IR representation of Curly into a list of C files.
pub fn convert_ir_to_c(ir: &IR) -> Vec<(String, String)> {
    let mut files = vec![];

    // Create and populate types
    let mut last_reference = 0;
    let mut types_header =
        String::from("#ifndef TYPES_H\n#define TYPES_H\n#include \"curly.h\"\n\n");
    let mut types_body = String::from("#include \"types.h\"\n\n");
    let mut types = HashMap::new();

    for module in ir.modules.iter() {
        collect_types(module.1, &mut types, &mut types_header, &mut last_reference);
        collect_type_functions(module.1, &types, &mut types_header, &mut types_body);
    }

    for module in ir.modules.iter() {
        let mut cfs = HashMap::with_capacity(0);
        for f in module.1.funcs.iter() {
            if f.1.written {
                continue;
            }

            let cf = CFunction {
                name: sanitise_symbol(&f.0),
                args: f
                    .1
                    .captured_names
                    .iter()
                    .map(|v| (v, f.1.captured.get(v).unwrap()))
                    .chain(f.1.args.iter().map(|v| (&v.0, &v.1)))
                    .collect(),
                ret_type: &f.1.body.get_metadata()._type,
                code: String::new(),
                last_reference: 0,
            };
            cfs.insert(f.0.clone(), cf);
        }

        let f = convert_module_to_c(module.1, &mut cfs, &mut types);
        files.push((format!("{}.c", sanitise_symbol(&module.1.name)), f));
        files.push(generate_header_files(module.1, &cfs, &types));
    }

    types_header.push_str("#endif\n");
    files.push((String::from("types.h"), types_header));
    files.push((String::from("types.c"), types_body));
    let ptr_size = std::mem::size_of::<&char>();
    files.push((
        String::from("curly.h"),
        format!(
            "
#ifndef CURLY_H
#define CURLY_H
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef {} float_t;
typedef int{}_t int_t;
typedef uint{}_t word_t;

typedef struct {{
    unsigned int refc;
    void* func;
    void* wrapper;
    unsigned int arity;
    unsigned int argc;
    bool (**cleaners)(void*);
    void** args;
}} func_t;

typedef union {{
    float_t d;
    void* v;
}} double_wrapper_t;

bool force_free_func(void* _func);

bool free_func(func_t* func);

bool refc_func(func_t* func);

void copy_func(func_t* dest, func_t* source);

func_t* copy_func_arg(func_t* source);

#endif
",
            match ptr_size {
                4 => "float",
                8 => "double",
                _ => panic!("unsupported architecture with pointer size {}", ptr_size),
            },
            match ptr_size {
                4 => 32,
                8 => 64,
                _ => panic!("unsupported architecture with pointer size {}", ptr_size),
            },
            match ptr_size {
                4 => 32,
                8 => 64,
                _ => panic!("unsupported architecture with pointer size {}", ptr_size),
            }
        ),
    ));
    files.push((
        String::from("curly.c"),
        String::from(
            "
#include \"curly.h\"

bool force_free_func(void* _func) {
    // func_t* func = (func_t*) _func;
    // for (int i = 0; i < func->argc; i++) {
        // if (func->cleaners[i] != (void*) 0 && func->cleaners[i](func->args[i]))
            // free(func->args[i]);
    // }

    // free(func->args);
    // free(func->cleaners);
    return 1;
}

bool free_func(func_t* func) {
    // if (func->refc == 0) {
    //    return force_free_func(func);
    //}

    return 0;
}

bool refc_func(func_t* func) {
    // if (func->refc > 0)
        // func->refc--;
    return free_func(func);
}

void copy_func(func_t* dest, func_t* source) {
    dest->refc = 0;
    dest->func = source->func;
    dest->wrapper = source->wrapper;
    dest->arity = source->arity;
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

func_t* copy_func_arg(func_t* source) {
    func_t* dest = malloc(sizeof(func_t));
    copy_func(dest, source);
    return dest;
}
",
        ),
    ));
    files
}

// generate_module_file(&IR) -> String
// Generates a module file.
pub fn generate_module_file(ir: &IR) -> String {
    let mut m = String::with_capacity(0);

    for (_, module) in ir.modules.iter() {
        m.push_str("module ");
        m.push_str(&module.name);

        if !module.exports.is_empty() {
            m.push_str("\n    ( ");
            let mut comma = false;
            for export in module.exports.iter() {
                if comma {
                    m.push_str("    , ");
                } else {
                    comma = true;
                }
                m.push_str(export.0);
                m.push_str(" : ");
                m.push_str(&format!(
                    "{} : {}\n",
                    module.scope.variables.get(export.0).unwrap().1,
                    export.1 .1
                ));
            }
            m.push_str("    )");
        }

        m.push_str("\n\n");
    }

    m
}
