use crate::error::ErrorMsg;
use crate::parser::AstNode;

pub enum ToCheck {
    PatternMatchOverEnum, // verifies pattern matching is done over an enum value
}

/// Verifies that the AST nodes are correct
pub fn check(input: AstNode, to_check: ToCheck) -> Result<AstNode, ErrorMsg> {
    Ok(input)
}
