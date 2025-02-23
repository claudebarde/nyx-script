use crate::error::ErrorMsg;
use crate::parser::AstNode;
use crate::transpiler::Context;

pub enum ToCheck {
    PatternMatchOverEnum, // verifies pattern matching is done over an enum value
}

/// Verifies that the AST nodes are correct
pub fn check(
    input: AstNode,
    to_check: ToCheck,
    context: &mut Context,
) -> Result<AstNode, ErrorMsg> {
    match to_check {
        ToCheck::PatternMatchOverEnum => {
            // check if the pattern matching is done over an enum value
            match &input {
                AstNode::Ident(ident, _) => {
                    // verifies that the identifier is an enum
                    match context.decl_vars.get(ident) {
                        Some(var) => {
                            if var.0.is_enum() {
                                Ok(input)
                            } else {
                                Err(ErrorMsg::PatternMatchNoEnum(ident.clone(), input.get_pos()))
                            }
                        }
                        None => Err(ErrorMsg::PatternMatchNoEnum(ident.clone(), input.get_pos())),
                    }
                }
                node => Err(ErrorMsg::PatternMatchNoIdent(
                    format!("{:#?}", node),
                    node.clone().get_pos(),
                )),
            }
        }
        _ => Ok(input),
    }
}
