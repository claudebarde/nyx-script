use crate::checker::{check, ToCheck};
use crate::error::ErrorMsg;
use crate::parser::{AstNode, NyxType, Position};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Scope {
    Global,
    Circuit(String),
    Constructor,
}

pub struct Context {
    pub custom_types: HashMap<String, NyxType>, // enum + user custom types
    pub ledger: HashMap<String, (NyxType, Position)>,
    pub decl_vars: HashMap<String, (NyxType, Scope, Position)>,
    pub current_scope: Scope,
}

/// Transpile the AST into an AST ready to be printed into official Compact
pub fn transpile(input: AstNode, context: &mut Context) -> Result<AstNode, ErrorMsg> {
    match input.clone() {
        AstNode::Argument(name, typ, pos) => {
            let transpiled_type = transpile(*typ, context)?;
            return Ok(AstNode::Argument(name, Box::new(transpiled_type), pos));
        }
        AstNode::Block(statements, pos) => {
            let transpiled_statements = statements
                .into_iter()
                .map(|x| transpile(x, context))
                .collect::<Result<Vec<AstNode>, ErrorMsg>>()?;
            return Ok(AstNode::Block(transpiled_statements, pos));
        }
        AstNode::CircuitDef(name, args, return_type, block, pos) => {
            let transpiled_args = match args {
                None => None,
                Some(a) => {
                    let transpiled_args = a
                        .into_iter()
                        .map(|x| transpile(x, context))
                        .collect::<Result<Vec<AstNode>, ErrorMsg>>()?;
                    Some(transpiled_args)
                }
            };
            let transpiled_return_type = transpile(*return_type, context)?;
            let transpiled_block = transpile(*block, context)?;
            return Ok(AstNode::CircuitDef(
                name,
                transpiled_args,
                Box::new(transpiled_return_type),
                Box::new(transpiled_block),
                pos,
            ));
        }
        AstNode::Const(name, typ, value, pos) => {
            // if the const has a type, we need to transpile it
            match typ {
                None => return Ok(input),
                Some(t) => {
                    let transpiled_type = transpile(*t, context)?;
                    let transpiled_value = transpile(*value, context)?;
                    return Ok(AstNode::Const(
                        name,
                        Some(Box::new(transpiled_type)),
                        Box::new(transpiled_value),
                        pos,
                    ));
                }
            }
        }
        AstNode::Constructor(args, block, pos) => {
            let transpiled_args = args
                .into_iter()
                .map(|x| transpile(x, context))
                .collect::<Result<Vec<AstNode>, ErrorMsg>>()
                .unwrap();
            let transpiled_block = transpile(*block, context)?;
            return Ok(AstNode::Constructor(
                transpiled_args,
                Box::new(transpiled_block),
                pos,
            ));
        }
        AstNode::CustomType(name, pos) => {
            // looks for the custom type in the custom types map
            match context.custom_types.get(&name) {
                None => return Err(ErrorMsg::UnknownCustomType(name, pos)),
                Some(t) => {
                    // the whole enum definition must not be printed
                    match t {
                        NyxType::Enum(name, _) => Ok(AstNode::Ident(name.to_string(), pos)),
                        _ => Ok(AstNode::Type(t.clone(), pos)),
                    }
                }
            }
        }
        AstNode::CustomTypeDef(name, typ, pos) => {
            // records the custom type with its equivalent type
            let custom_type_vec = typ.to_type()?;
            // there should be only one element in the vector
            if custom_type_vec.len() != 1 {
                return Err(ErrorMsg::UnexpectedLength(
                    1,
                    custom_type_vec.len(),
                    "custom_type_def_transpile".to_string(),
                ));
            }
            let custom_type = match custom_type_vec[0].clone() {
                NyxType::Enum(name, branches) => {
                    // registers the enum in custom_types
                    context
                        .custom_types
                        .insert(name.clone(), NyxType::Enum(name.clone(), branches.clone()));
                    NyxType::Enum(name, branches)
                }
                val => val,
            };
            context.custom_types.insert(name, custom_type.clone());
            // custom types are not output in the final contract
            // except for enums
            match custom_type {
                NyxType::Enum(name, elements) => Ok(AstNode::EnumDef(name, elements, pos)),
                _ => Ok(AstNode::Empty),
            }
        }
        AstNode::EnumDef(name, elements, pos) => {
            // registers the enum in the custom types
            context
                .custom_types
                .insert(name.clone(), NyxType::Enum(name.clone(), elements.clone()));
            return Ok(AstNode::EnumDef(name, elements, pos));
        }
        AstNode::Export(child, pos) => {
            let transpiled_node = transpile(*child, context)?;
            return Ok(AstNode::Export(Box::new(transpiled_node), pos));
        }
        AstNode::FunCall(name, fun_type, params, pos) => {
            let transpiled_params = params
                .into_iter()
                .map(|x| transpile(x, context))
                .collect::<Result<Vec<AstNode>, ErrorMsg>>()?;
            match fun_type {
                Some(t) => {
                    let transpiled_type = transpile(*t, context)?;
                    return Ok(AstNode::FunCall(
                        name,
                        Some(Box::new(transpiled_type)),
                        transpiled_params,
                        pos,
                    ));
                }
                None => return Ok(AstNode::FunCall(name, None, transpiled_params, pos)),
            }
        }
        AstNode::LedgerDef(name, typ, pos) => {
            let transpiled_type = transpile(*typ.clone(), context)?;
            // println!("context custom types: {:#?}", context.custom_types);
            let val_type = match transpiled_type.clone().to_type() {
                Ok(t) => {
                    if t.len() != 1 {
                        return Err(ErrorMsg::UnexpectedLength(
                            1,
                            t.len(),
                            "ledger_def_transpile".to_string(),
                        ));
                    }
                    t.into_iter().nth(0).unwrap()
                }
                Err(err) => {
                    match err {
                        ErrorMsg::InvalidType(invalid_type) => {
                            // looks for the string in the custom types
                            match context.custom_types.get(&invalid_type) {
                                None => return Err(ErrorMsg::UnknownType(invalid_type, pos)),
                                Some(t) => match t {
                                    NyxType::Enum(name, branches) => {
                                        NyxType::Enum(name.to_string(), branches.clone())
                                    }
                                    _ => t.clone(),
                                },
                            }
                        }
                        _ => return Err(ErrorMsg::UnknownRule("ledger_def_transpile".to_string())),
                    }
                }
            };
            // records the variable in the ledger and decl_vars
            context
                .ledger
                .insert(name.clone(), (val_type.clone(), pos.clone()));
            context.decl_vars.insert(
                name.clone(),
                (val_type, context.current_scope.clone(), pos.clone()),
            );
            return Ok(AstNode::LedgerDef(name, Box::new(transpiled_type), pos));
        }
        AstNode::PatternMatch(value, patterns, pos) => {
            // pattern matching is basically transpiled into if conditions
            // 1 case = if condition { block }
            // 2 cases = if condition { block } else { block }
            // More than 2 cases = if condition { block } else if condition { block } else { block
            let transpiled_value = transpile(*value, context)?;
            let checked_value = check(transpiled_value, ToCheck::PatternMatchOverEnum, context)?;
            // TODO: check that the value is an enum
            if patterns.len() == 0 {
                return Err(ErrorMsg::UnexpectedLength(
                    1,
                    0,
                    "pattern_match".to_string(),
                ));
            } else if patterns.len() == 1 {
                let (left, right) = patterns[0].clone();
                let transpiled_left = transpile(left, context)?;
                let transpiled_right = transpile(right, context)?;
                return Ok(AstNode::If(
                    Box::new(AstNode::ExprEq(
                        Box::new(checked_value),
                        Box::new(transpiled_left),
                        Position {
                            line: pos.line,
                            column: pos.column,
                            depth: 0,
                        },
                    )),
                    Box::new(transpiled_right),
                    pos,
                ));
            } else if patterns.len() == 2 {
                let (left1, right1) = patterns[0].clone();
                let (_left2, right2) = patterns[1].clone();
                let transpiled_left1 = transpile(left1, context)?;
                let transpiled_right1 = {
                    let transpiled = transpile(right1, context)?;
                    match transpiled.clone() {
                        AstNode::Block(_, _) => transpiled,
                        node => {
                            let pos = node.get_pos();
                            AstNode::Block(vec![transpiled], pos)
                        }
                    }
                };
                // let transpiled_left2 = transpile(left2, context)?;
                let transpiled_right2 = {
                    let transpiled = transpile(right2, context)?;
                    match transpiled.clone() {
                        AstNode::Block(_, _) => transpiled,
                        node => {
                            let pos = node.get_pos();
                            AstNode::Block(
                                vec![transpiled],
                                Position {
                                    line: pos.line,
                                    column: pos.column,
                                    depth: pos.depth,
                                },
                            )
                        }
                    }
                };
                return Ok(AstNode::IfElse(
                    Box::new(AstNode::ExprEq(
                        Box::new(checked_value.clone()),
                        Box::new(transpiled_left1),
                        Position {
                            line: pos.line,
                            column: pos.column,
                            depth: 0,
                        },
                    )),
                    Box::new(transpiled_right1),
                    Box::new(transpiled_right2),
                    pos,
                ));
            } else {
                // println!("patterns: {:#?}", patterns);
                let mut transpiled_patterns = patterns
                    .into_iter()
                    .map(|(left, right)| {
                        let transpiled_left = transpile(left, context)?;
                        let transpiled_right = transpile(right, context)?;
                        Ok((transpiled_left, transpiled_right))
                    })
                    .collect::<Result<Vec<(AstNode, AstNode)>, ErrorMsg>>()?;
                let else_block = {
                    let block = transpiled_patterns.pop().unwrap();
                    match block.1.clone() {
                        AstNode::Block(_, _) => block,
                        node => {
                            let pos = node.get_pos();
                            (block.0, AstNode::Block(vec![block.1], pos))
                        }
                    }
                };
                let if_block = {
                    let block = transpiled_patterns.remove(0);
                    match block.1.clone() {
                        AstNode::Block(_, _) => block,
                        node => {
                            let pos = node.get_pos();
                            (block.0, AstNode::Block(vec![block.1], pos))
                        }
                    }
                };
                let else_if_blocks = transpiled_patterns
                    .into_iter()
                    .map(|(left, right)| {
                        let formatted_left = AstNode::ExprEq(
                            Box::new(checked_value.clone()),
                            Box::new(left),
                            Position {
                                line: pos.line,
                                column: pos.column,
                                depth: 0,
                            },
                        );
                        let formatted_right = match right.clone() {
                            AstNode::Block(_, _) => right,
                            node => {
                                let pos = node.get_pos();
                                AstNode::Block(
                                    vec![right],
                                    Position {
                                        line: pos.line,
                                        column: pos.column,
                                        depth: pos.depth,
                                    },
                                )
                            }
                        };
                        Ok((formatted_left, formatted_right))
                    })
                    .collect::<Result<Vec<(AstNode, AstNode)>, ErrorMsg>>()?;
                return Ok(AstNode::IfElseIf(
                    Box::new(AstNode::ExprEq(
                        Box::new(checked_value),
                        Box::new(if_block.0),
                        Position {
                            line: pos.line,
                            column: pos.column,
                            depth: 0,
                        },
                    )),
                    Box::new(if_block.1),
                    else_if_blocks,
                    Box::new(else_block.1),
                    pos,
                ));
            }
        }
        AstNode::StructType(name, type_args, pos) => {
            let transpiled_type_args = transpile(*type_args, context)?;
            return Ok(AstNode::StructType(
                name,
                Box::new(transpiled_type_args),
                pos,
            ));
        }
        AstNode::Type(nyx_type, pos) => {
            return Ok(AstNode::Type(nyx_type, pos));
        }
        AstNode::TypeList(list, pos) => {
            let transpiled_list = list
                .into_iter()
                .map(|x| transpile(x, context))
                .collect::<Result<Vec<AstNode>, ErrorMsg>>()?;
            return Ok(AstNode::TypeList(transpiled_list, pos));
        }
        AstNode::VarAssignment(ident, value, pos) => {
            let transpiled_ident = transpile(*ident, context)?;
            let transpiled_value = transpile(*value, context)?;
            return Ok(AstNode::VarAssignment(
                Box::new(transpiled_ident),
                Box::new(transpiled_value),
                pos,
            ));
        }
        _ => Ok(input),
        // _ => todo!("implement transpile for {:#?}", input),
    }
}
