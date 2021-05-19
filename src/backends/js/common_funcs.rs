// sanitize_symbol(&str) -> String
// Sanitizes a symbol.
pub fn sanitize_symbol(value: &str) -> String {
    let mut s = value
        .replace("'", "$$PRIME$$")
        .replace("::", "$$DOUBLECOLON$$");
    s.push('$');
    s
}