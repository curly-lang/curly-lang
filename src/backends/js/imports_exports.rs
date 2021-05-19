use std::collections::HashMap;

use crate::frontend::ir::{IRImport, IRModule};

use super::common_funcs::sanitize_symbol;

// get_import_string(&str, &IRImport) -> String
// Gets the string corresponding to a single curly import statement.
fn get_import_string(
    qualified_name: &str,
    import: &IRImport
) -> String {
    let mut import_string = "".to_string();

    if !import.qualified {
        import_string.push_str("import {");
        for (name, _) in import.imports.iter() {
            import_string.push_str(&sanitize_symbol(name));
            import_string.push_str(", ");
        }
        import_string.push_str(
            &format!("}} from \"./{}.js\";\n", sanitize_symbol(&import.name))
        );
    }
    import_string.push_str(
        &format!("import * as {} from \"./{}.js\";\n", sanitize_symbol(qualified_name), sanitize_symbol(&import.name))
    );

    import_string
}

// get_imports(&HashMap<String, IRImport>) -> String
// Gets the string corresponding to a module's curly import statements.
fn get_imports(imports: &HashMap<String, IRImport>) -> String {
    let mut imports_string = "// Imports:\n".to_string();

    for (import_name, import) in imports.iter() {
        imports_string.push_str(&get_import_string(import_name, import));
    
        imports_string.push('\n');
    }

    imports_string
}

fn get_exports<T>(exports: &HashMap<String, T>) -> String {
    let mut exports_string = "// Exports:\n".to_string();

    exports_string.push_str("export {");

    for export_name in exports.keys() {
        exports_string.push_str(&sanitize_symbol(export_name));

        exports_string.push_str(", ");
    }

    exports_string.push_str("};\n");

    exports_string.push('\n');

    exports_string
}

// get_imports_exports(&IRModule) -> String
// Gets the string corresponding to a curly module's imports and exports
pub fn get_imports_exports(module: &IRModule, ) -> String {
    let mut imports_exports = "// Imports and Exports:\n".to_string();

    imports_exports.push_str(&get_imports(&module.imports));
    imports_exports.push('\n');

    imports_exports.push_str(&get_exports(&module.exports));
    imports_exports.push('\n');

    imports_exports
}