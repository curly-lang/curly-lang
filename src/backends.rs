pub mod c;

use crate::frontend::ir::IR;
use std::fmt::Write;

pub enum Backend {
  C,
}

pub fn compile_ir(
  ir: &IR,
  backend: Backend,
) -> Vec<(String, String)> {
  match backend {
    Backend::C => c::codegen::convert_ir_to_c(ir),
    
  }
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
              let _ = m.write_fmt(format_args!(
                  "{} {} : {}\n",
                  module.scope.variables.get(export.0).unwrap().1,
                  module.funcs.get(export.0).unwrap().impure,
                  export.1 .1
              ));
          }
          m.push_str("    )");
      }

      m.push_str("\n\n");
  }

  m
}
