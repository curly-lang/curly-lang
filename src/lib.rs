pub mod backends;
pub mod frontend;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::collections::HashMap;

use crate::frontend::correctness;
use crate::frontend::correctness::CorrectnessError;
use crate::frontend::ir;
use crate::frontend::ir::{IRError, IR};
use crate::frontend::parser;

pub static DEBUG: bool = false;

pub type Res<'a> = Result<(), (Vec<Diagnostic<usize>>, SimpleFiles<&'a String, String>)>;

// check(&Vec<(String, bool)>, &Vec<String>, &mut IR, bool) -> Result<(), ()>
// Checks whether given code is valid.
pub fn check<'a>(
    filenames: &'a [(String, bool)],
    codes: &[String],
    ir: &mut IR,
    require_main: bool,
    emit: bool
) -> Res<'a> {
    // Set up codespan
    let mut files = SimpleFiles::new();
    let mut file_hash = HashMap::new();
    for file in filenames.iter().enumerate() {
        file_hash.insert(&file.1 .0, files.add(&file.1 .0, codes[file.0].clone()));
    }
    let file_hash = file_hash;

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();
    let mut diagnostics = Vec::new();

    for file in filenames.iter().enumerate() {
        let code = &codes[file.0];
        let file_id = *file_hash.get(&file.1 .0).unwrap();

        // Generate the ast
        if file.1 .1 {
            let ast = match parser::parse_library(code) {
                Ok(v) => v,
                Err(e) => {
                    let diagnostic = Diagnostic::error()
                        .with_message(&e.msg)
                        .with_labels(vec![Label::primary(file_id, e.span)]);
                    if emit {
                        term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
                    }
                    diagnostics.push(diagnostic);
                    return Err((diagnostics, files));
                }
            };

            // Print out the ast
            if DEBUG {
                println!("{:#?}", &ast);
            }

            // Add library module
            match ir::convert_library_header(&file.1 .0, ast, ir) {
                Ok(_) if DEBUG => {
                    dbg!(&ir);
                }
                Ok(_) => (),
                Err(e) => {
                    for e in e {
                        let mut diagnostic = Diagnostic::error();
                        match e {
                            IRError::InvalidType(s) => {
                                diagnostic = diagnostic
                                    .with_message("Invalid type used")
                                    .with_labels(vec![Label::primary(
                                        *file_hash.get(&s.filename).unwrap(),
                                        s.span,
                                    )
                                    .with_message("Undeclared type")])
                            }

                            IRError::DuplicateTypeInUnion(s1, s2, t) => {
                                diagnostic = diagnostic
                                    .with_message("Duplicate type in union type declaration")
                                    .with_labels(vec![
                                        Label::secondary(
                                            *file_hash.get(&s1.filename).unwrap(),
                                            s1.span,
                                        )
                                        .with_message("Type used here first"),
                                        Label::primary(
                                            *file_hash.get(&s2.filename).unwrap(),
                                            s2.span,
                                        )
                                        .with_message(
                                            format!("Type `{}` used a second time here", t),
                                        ),
                                    ])
                            }

                            IRError::DoubleExport(s1, s2, e) => {
                                diagnostic = diagnostic
                                    .with_message("Value exported twice")
                                    .with_labels(vec![
                                        Label::secondary(
                                            *file_hash.get(&s1.filename).unwrap(),
                                            s1.span,
                                        )
                                        .with_message("Value exported here first"),
                                        Label::primary(
                                            *file_hash.get(&s2.filename).unwrap(),
                                            s2.span,
                                        )
                                        .with_message(
                                            format!("Value {} exported a second time here", e),
                                        ),
                                    ])
                            }

                            IRError::RedefineImportAlias(s1, s2, a) => {
                                diagnostic = diagnostic
                                    .with_message("Alias defined twice")
                                    .with_labels(vec![
                                        Label::secondary(
                                            *file_hash.get(&s1.filename).unwrap(),
                                            s1.span,
                                        )
                                        .with_message("Alias defined here first"),
                                        Label::primary(
                                            *file_hash.get(&s2.filename).unwrap(),
                                            s2.span,
                                        )
                                        .with_message(
                                            format!("Alias {} defined a second time here", a),
                                        ),
                                    ])
                            }

                            IRError::UnsupportedAnnotation(s, a) => {
                                diagnostic = diagnostic
                                    .with_message("Unsupported annotation used")
                                    .with_labels(vec![Label::primary(
                                        *file_hash.get(&s.filename).unwrap(),
                                        s.span,
                                    )
                                    .with_message(format!("Annotation {} is unsupported", a))])
                            }

                            IRError::InvalidFFIType(s, t) => {
                                diagnostic = diagnostic
                                    .with_message("Unsupported type used for FFI")
                                    .with_labels(vec![Label::primary(
                                        *file_hash.get(&s.filename).unwrap(),
                                        s.span,
                                    )
                                    .with_message(format!("Type {} is unsupported by FFI", t))])
                            }

                            IRError::DuplicateModule(v, _t) => {
                                diagnostic =
                                    diagnostic.with_message(format!("Duplicate module `{}`", v))
                            }
                        }
                        if emit {
                            term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
                        }
                        diagnostics.push(diagnostic);
                    }
                }
            }
        } else {
            let ast = match parser::parse(code) {
                Ok(v) => v,
                Err(e) => {
                    let diagnostic = Diagnostic::error()
                        .with_message(&e.msg)
                        .with_labels(vec![Label::primary(file_id, e.span)]);
                    if emit {
                        term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
                    }
                    diagnostics.push(diagnostic);
                    return Err((diagnostics, files));
                }
            };

            // Print out the ast
            if DEBUG {
                println!("{:#?}", &ast);
            }
            match ir::convert_ast_to_ir(&file.1 .0, code, ast, ir) {
                Ok(_) if DEBUG => {
                    dbg!(&ir);
                }
                Ok(_) => (),
                Err(e) => {
                    for e in e {
                        let mut diagnostic = Diagnostic::error();
                        match e {
                            IRError::InvalidType(s) => {
                                diagnostic = diagnostic
                                    .with_message("Invalid type used")
                                    .with_labels(vec![Label::primary(
                                        *file_hash.get(&s.filename).unwrap(),
                                        s.span,
                                    )
                                    .with_message("Undeclared type")])
                            }

                            IRError::DuplicateTypeInUnion(s1, s2, t) => {
                                diagnostic = diagnostic
                                    .with_message("Duplicate type in union type declaration")
                                    .with_labels(vec![
                                        Label::secondary(
                                            *file_hash.get(&s1.filename).unwrap(),
                                            s1.span,
                                        )
                                        .with_message("Type used here first"),
                                        Label::primary(
                                            *file_hash.get(&s2.filename).unwrap(),
                                            s2.span,
                                        )
                                        .with_message(
                                            format!("Type `{}` used a second time here", t),
                                        ),
                                    ])
                            }

                            IRError::DoubleExport(s1, s2, e) => {
                                diagnostic = diagnostic
                                    .with_message("Value exported twice")
                                    .with_labels(vec![
                                        Label::secondary(
                                            *file_hash.get(&s1.filename).unwrap(),
                                            s1.span,
                                        )
                                        .with_message("Value exported here first"),
                                        Label::primary(
                                            *file_hash.get(&s2.filename).unwrap(),
                                            s2.span,
                                        )
                                        .with_message(
                                            format!("Value {} exported a second time here", e),
                                        ),
                                    ])
                            }

                            IRError::RedefineImportAlias(s1, s2, a) => {
                                diagnostic = diagnostic
                                    .with_message("Alias defined twice")
                                    .with_labels(vec![
                                        Label::secondary(
                                            *file_hash.get(&s1.filename).unwrap(),
                                            s1.span,
                                        )
                                        .with_message("Alias defined here first"),
                                        Label::primary(
                                            *file_hash.get(&s2.filename).unwrap(),
                                            s2.span,
                                        )
                                        .with_message(
                                            format!("Alias {} defined a second time here", a),
                                        ),
                                    ])
                            }

                            IRError::UnsupportedAnnotation(s, a) => {
                                diagnostic = diagnostic
                                    .with_message("Unsupported annotation used")
                                    .with_labels(vec![Label::primary(
                                        *file_hash.get(&s.filename).unwrap(),
                                        s.span,
                                    )
                                    .with_message(format!("Annotation {} is unsupported", a))])
                            }

                            IRError::InvalidFFIType(s, t) => {
                                diagnostic = diagnostic
                                    .with_message("Unsupported type used for FFI")
                                    .with_labels(vec![Label::primary(
                                        *file_hash.get(&s.filename).unwrap(),
                                        s.span,
                                    )
                                    .with_message(format!("Type {} is unsupported by FFI", t))])
                            }

                            IRError::DuplicateModule(v, _t) => {
                                diagnostic =
                                    diagnostic.with_message(format!("Duplicate module `{}`", v))
                            }
                        }
                        if emit {
                            term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
                        }
                        diagnostics.push(diagnostic);
                    }
                }
            }
        }
    }

    if !diagnostics.is_empty() {
        return Err((diagnostics, files));
    }

    // Check correctness
    let err = correctness::check_correctness(ir, require_main);

    // Print out the ir or the error
    match err {
        Ok(_) if DEBUG => {
            dbg!(ir);
            Ok(())
        }

        Ok(_) => Ok(()),

        Err(e) => {
            for e in e {
                let mut diagnostic = Diagnostic::error();
                match e {
                    CorrectnessError::UndefinedPrefixOp(s, _, t) => {
                        diagnostic = diagnostic
                            .with_message("Undefined prefix operator")
                            .with_labels(vec![Label::primary(
                                *file_hash.get(&s.filename).unwrap(),
                                s.span,
                            )
                            .with_message(format!("`-` is undefined on `{}`", t))])
                    }

                    CorrectnessError::UndefinedInfixOp(s, op, l, r) => {
                        diagnostic = diagnostic
                            .with_message("Undefined infix operator")
                            .with_labels(vec![Label::primary(
                                *file_hash.get(&s.filename).unwrap(),
                                s.span,
                            )
                            .with_message(format!(
                                "`{:?}` is undefined on `{}` and `{}`",
                                op, l, r
                            ))])
                    }

                    CorrectnessError::NonboolInBoolExpr(s, t) => {
                        diagnostic = diagnostic
                            .with_message("Nonboolean in boolean expression")
                            .with_labels(vec![Label::primary(
                                *file_hash.get(&s.filename).unwrap(),
                                s.span,
                            )
                            .with_message(format!("Expected `Bool`, got `{}`", t))])
                    }

                    CorrectnessError::NonboolInIfCond(s, t) => {
                        diagnostic = diagnostic
                            .with_message("Nonboolean in if condition")
                            .with_labels(vec![Label::primary(
                                *file_hash.get(&s.filename).unwrap(),
                                s.span,
                            )
                            .with_message(format!("Expected `Bool`, got `{}`", t))])
                    }

                    CorrectnessError::NonmatchingAssignTypes(s1, t1, s2, t2) => {
                        diagnostic = diagnostic
                            .with_message("Nonmatching types in assignment")
                            .with_labels(vec![
                                Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                    .with_message(format!(
                                        "Assignment is declared with type `{}`",
                                        t1
                                    )),
                                Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                    .with_message(format!("Expected `{}`, got `{}`", t1, t2)),
                            ])
                    }

                    CorrectnessError::SymbolNotFound(s, v) => {
                        diagnostic = diagnostic
                            .with_message("Symbol not found")
                            .with_labels(vec![Label::primary(
                                *file_hash.get(&s.filename).unwrap(),
                                s.span,
                            )
                            .with_message(format!("Could not find symbol `{}`", v))])
                    }

                    CorrectnessError::Reassignment(s1, s2, v) => {
                        diagnostic = diagnostic
                            .with_message("Redefinition of previously declared variable")
                            .with_labels(vec![
                                Label::primary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                    .with_message(format!(
                                        "`{}` is already defined and not declared as mutable",
                                        v
                                    )),
                                Label::secondary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                    .with_message(format!("`{}` previously defined here", v)),
                            ])
                    }

                    CorrectnessError::InvalidType(s) => {
                        diagnostic = diagnostic
                            .with_message("Invalid type used")
                            .with_labels(vec![Label::primary(
                                *file_hash.get(&s.filename).unwrap(),
                                s.span,
                            )
                            .with_message("Undeclared type")])
                    }

                    CorrectnessError::DuplicateTypeInUnion(s1, s2, t) => {
                        diagnostic = diagnostic
                            .with_message("Duplicate type in union type declaration")
                            .with_labels(vec![
                                Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                    .with_message("Type used here first"),
                                Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                    .with_message(format!("Type `{}` used a second time here", t)),
                            ])
                    }

                    CorrectnessError::UnknownFunctionReturnType(s, v) => {
                        diagnostic = diagnostic
                            .with_message("Could not determine the return type of the function")
                            .with_labels(vec![Label::primary(
                                *file_hash.get(&s.filename).unwrap(),
                                s.span,
                            )
                            .with_message(format!("Could not determine return type for `{}`", v))])
                    }

                    CorrectnessError::MismatchedFunctionArgType(s, t1, t2) => {
                        diagnostic = diagnostic
                            .with_message("Wrong type passed as an argument")
                            .with_labels(vec![Label::primary(
                                *file_hash.get(&s.filename).unwrap(),
                                s.span,
                            )
                            .with_message(format!("Expected `{}`, got `{}`", t1, t2))])
                    }

                    CorrectnessError::InvalidApplication(s, t) => {
                        diagnostic =
                            diagnostic
                                .with_message("Invalid application")
                                .with_labels(vec![Label::primary(
                                    *file_hash.get(&s.filename).unwrap(),
                                    s.span,
                                )
                                .with_message(format!("Expected function, got `{}`", t))]);
                    }

                    CorrectnessError::InvalidCast(s1, t1, s2, t2) => {
                        diagnostic = diagnostic.with_message("Invalid cast").with_labels(vec![
                            Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                .with_message(format!("Value has type `{}`", t1)),
                            Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                .with_message(format!("Cannot convert `{}` to `{}`", t1, t2)),
                        ]);
                    }

                    CorrectnessError::NonSubtypeOnMatch(s1, t1, s2, t2) => {
                        diagnostic = diagnostic
                            .with_message("Nonsubtype checked for in match arm")
                            .with_labels(vec![
                                Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                    .with_message(format!("Value has type `{}`", t1)),
                                Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                    .with_message(format!("`{}` is not a subtype of `{}`", t2, t1)),
                            ]);
                    }

                    CorrectnessError::InfiniteSizedType(s, t) => {
                        diagnostic = diagnostic
                            .with_message("Type has infinite size")
                            .with_labels(vec![Label::primary(
                                *file_hash.get(&s.filename).unwrap(),
                                s.span,
                            )
                            .with_message(format!("Type `{}` has infinite size", t))]);
                    }

                    CorrectnessError::NonmemberAccess(s, a, b) => {
                        diagnostic = diagnostic
                            .with_message("Attempted to access a member that does not exist")
                            .with_labels(vec![Label::primary(
                                *file_hash.get(&s.filename).unwrap(),
                                s.span,
                            )
                            .with_message(format!("`{}` has no member `{}`", a, b))]);
                    }

                    CorrectnessError::MismatchedDeclarationAssignmentTypes(s1, t1, s2, t2) => {
                        diagnostic = diagnostic
                            .with_message("Assignment of variable declared with incompatible type")
                            .with_labels(vec![
                                Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                    .with_message(format!(
                                        "Variable declared here with type {}",
                                        t1
                                    )),
                                Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                    .with_message(format!(
                                        "Assignment of value with type {} to variable of type {}",
                                        t2, t1
                                    )),
                            ]);
                    }

                    CorrectnessError::VariableImportedTwice(s1, s2) => {
                        diagnostic = diagnostic
                            .with_message("Variable imported twice")
                            .with_labels(vec![
                                Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                    .with_message("Variable declared here at first"),
                                Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                    .with_message("Variable redeclared here"),
                            ]);
                    }

                    CorrectnessError::ImportedValueNotExported(s, v, i) => {
                        diagnostic = diagnostic
                            .with_message("Imported value not exported")
                            .with_labels(vec![Label::primary(
                                *file_hash.get(&s.filename).unwrap(),
                                s.span,
                            )
                            .with_message(format!("{} not exported by {}", v, i))]);
                    }

                    CorrectnessError::NoMainFunction => {
                        diagnostic = diagnostic.with_message("No main function found");
                    }

                    CorrectnessError::CurriedExternalFunc(s) => {
                        diagnostic = diagnostic
                            .with_message("Cannot curry external function")
                            .with_labels(vec![Label::primary(
                                *file_hash.get(&s.filename).unwrap(),
                                s.span,
                            )
                            .with_message("Curried function found here")])
                    }

                    CorrectnessError::ImpureInPure(s1, s2) => {
                        diagnostic = diagnostic
                            .with_message("Cannot use impure function in pure function")
                            .with_labels(vec![
                                Label::secondary(*file_hash.get(&s1.filename).unwrap(), s1.span)
                                    .with_message("Function defined as pure"),
                                Label::primary(*file_hash.get(&s2.filename).unwrap(), s2.span)
                                    .with_message("Impurity found here"),
                            ])
                    }

                    CorrectnessError::UnnecessaryImpure(s) => {
                        diagnostic =
                            diagnostic
                                .with_message("Unnecessary impurity")
                                .with_labels(vec![Label::secondary(
                                    *file_hash.get(&s.filename).unwrap(),
                                    s.span,
                                )
                                .with_message("Function defined as impure but has no impurities")])
                    }

                    CorrectnessError::AppliedImpureToPure(s) => {
                        diagnostic = diagnostic
                            .with_message("Applied impure function to pure function")
                            .with_labels(vec![Label::primary(
                                *file_hash.get(&s.filename).unwrap(),
                                s.span,
                            )
                            .with_message(
                                "Function defined as impure but passed to pure function",
                            )])
                    }

                    CorrectnessError::ModuleNotFound(s, m) => {
                        diagnostic = diagnostic
                            .with_message("Module not found")
                            .with_labels(vec![Label::primary(
                                *file_hash.get(&s.filename).unwrap(),
                                s.span,
                            )
                            .with_message(format!("Module `{}` not found", m))])
                    }

                    CorrectnessError::UnimplementedExport(s, v) => {
                        diagnostic =
                            diagnostic
                                .with_message("Unimplemented export")
                                .with_labels(vec![Label::primary(
                                    *file_hash.get(&s.filename).unwrap(),
                                    s.span,
                                )
                                .with_message(format!("{} not implemented", v))])
                    }
                }
                if emit {
                    term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
                }
                diagnostics.push(diagnostic);
            }
            Err((diagnostics, files))
        }
    }
}
