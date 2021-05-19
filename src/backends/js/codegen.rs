use crate::frontend::ir::{IR, };

use super::common_funcs::sanitize_symbol;
use super::imports_exports::get_imports_exports;


// convert_ir_to_js(&IR) -> Vec<(String, String)>
// Converts an IR Struct to a collection of js files.
pub fn convert_ir_to_js(ir: &IR) -> Vec<(String, String)> {
    let mut files = Vec::new();
    for (mod_name, module) in ir.modules.iter() {
        let mut module_name = sanitize_symbol(mod_name);
        module_name.push_str(".js");


        let mut current_file = "".to_string();

        current_file.push_str(&get_imports_exports(module));
        
        files.push((module_name, current_file))
    }
    files
}

