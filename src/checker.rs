use crate::error::ErrorMsg;
use crate::parser::{AstNode, NyxType};
use crate::transpiler::Context;

fn all_strings_are_same(strings: &[String]) -> bool {
    if strings.is_empty() {
        return true;
    }
    let first = &strings[0];
    strings.iter().all(|s| s == first)
}

pub enum ToCheck {
    ExhaustiveMatch(Vec<(AstNode, AstNode)>), // verifies that the pattern matching is exhaustive
    IdentIsDeclared(String),                  // verifies that the identifier is declared
    PatternMatchOverEnum, // verifies pattern matching is done over an enum value
}

/// Verifies that the AST nodes are correct
pub fn check(
    input: AstNode,
    to_check: ToCheck,
    context: &mut Context,
) -> Result<AstNode, ErrorMsg> {
    match to_check {
        ToCheck::IdentIsDeclared(ident) => {
            // check if the identifier is declared
            match context.decl_vars.get(&ident) {
                Some(_) => Ok(input),
                None => {
                    // identifier could also be an enum
                    match context.custom_types.get(&ident) {
                        Some(_) => Ok(input),
                        None => Err(ErrorMsg::UnknownVar(ident, input.get_pos())),
                    }
                }
            }
        }
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
        ToCheck::ExhaustiveMatch(patterns) => {
            check(input.clone(), ToCheck::PatternMatchOverEnum, context)?;
            // check the cases iterate over the same enum
            let enum_idents_props = patterns
                .clone()
                .into_iter()
                .map(|(p, _)| match p {
                    AstNode::EnumAccess(ident, prop, _) => Ok((ident, prop)),
                    node => Err(ErrorMsg::PatternMatchNoEnumAccess(
                        node.clone().print()?,
                        node.get_pos(),
                    )),
                })
                .map(|p| match p {
                    Err(err) => Err(err),
                    Ok((ident, prop)) => {
                        if ident.clone().is_ident() && prop.clone().is_ident() {
                            return Ok((ident.from_ident().unwrap(), prop.from_ident().unwrap()));
                        } else {
                            Err(ErrorMsg::PatternMatchNoIdent(
                                ident.clone().print()?,
                                ident.get_pos(),
                            ))
                        }
                    }
                })
                .collect::<Result<Vec<(String, String)>, ErrorMsg>>()?;
            let enum_idents = enum_idents_props
                .clone()
                .into_iter()
                .map(|(ident, _)| ident)
                .collect::<Vec<String>>();
            if !all_strings_are_same(&enum_idents) {
                Err(ErrorMsg::PatternMatchDifferentEnums(
                    enum_idents,
                    input.get_pos(),
                ))
            } else {
                // verifies that the enum exists
                let enum_name = enum_idents[0].clone();
                match context.custom_types.get(&enum_name) {
                    None => Err(ErrorMsg::UnknownVar(enum_name, input.get_pos())),
                    Some(var) => {
                        // sort the pattern matching branches by alphabetical order
                        let mut enum_branches = enum_idents_props
                            .into_iter()
                            .map(|(_, prop)| prop)
                            .collect::<Vec<String>>();
                        enum_branches.sort();
                        // fetches the enum branches
                        if var.is_enum() {
                            let mut enum_props = var.clone().get_enum_props();
                            enum_props.sort();

                            if enum_branches.len() < enum_props.len() {
                                return Err(ErrorMsg::PatternMatchMissingCase(
                                    enum_name,
                                    input.get_pos(),
                                ));
                            } else if enum_branches.len() > enum_props.len() {
                                return Err(ErrorMsg::PatternMatchTooManyCases(
                                    enum_name,
                                    input.get_pos(),
                                ));
                            } else {
                                if enum_branches != enum_props {
                                    return Err(ErrorMsg::PatternMatchWrongCases(
                                        enum_name,
                                        input.get_pos(),
                                    ));
                                } else {
                                    Ok(input)
                                }
                            }
                        } else {
                            Err(ErrorMsg::UnexpectedType(
                                NyxType::Enum(enum_name, enum_branches),
                                var.clone(),
                                input.get_pos(),
                            ))
                        }
                    }
                }
            }
        }
    }
}
