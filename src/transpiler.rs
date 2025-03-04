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
    pub standard_library_import: bool,
    pub has_language_version: bool,
    pub language_version: String,
}

/// Transpile the AST into an AST ready to be printed into official Compact
pub fn transpile(input: AstNode, context: &mut Context) -> Result<AstNode, ErrorMsg> {
    match input.clone() {
        AstNode::Argument(name, typ, pos) => {
            let transpiled_type = match transpile(*typ, context) {
                Ok(t) => Ok(t),
                Err(err) => {
                    match err {
                        ErrorMsg::InvalidType(invalid_type, pos) => {
                            // looks for the string in the custom types
                            match context.custom_types.get(&invalid_type) {
                                None => Err(ErrorMsg::UnknownType(invalid_type, pos.clone())),
                                Some(t) => match t {
                                    NyxType::Enum(name, _) => {
                                        Ok(AstNode::Ident(name.to_string(), pos.clone()))
                                    }
                                    _ => todo!("Invalid type in argument"),
                                },
                            }
                        }
                        _ => Err(err),
                    }
                }
            }?;
            // the argument is registered in the declared variables
            let t = match transpiled_type.clone() {
                AstNode::Ident(ident, pos) => {
                    // the identifier may be an enum or a custom type
                    match context.custom_types.get(&ident) {
                        None => Err(ErrorMsg::InvalidType(ident, pos)),
                        Some(t) => Ok(vec![t.clone()]),
                    }
                }
                _ => transpiled_type.clone().to_type(),
            }?;
            if t.len() != 1 {
                return Err(ErrorMsg::UnexpectedLength(
                    1,
                    t.len(),
                    "argument_transpile".to_string(),
                ));
            }
            let arg_type = t.into_iter().nth(0).unwrap();
            context.decl_vars.insert(
                name.clone(),
                (arg_type, context.current_scope.clone(), pos.clone()),
            );
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
            match transpiled_node.clone() {
                AstNode::MultiExport(_, _) => Ok(transpiled_node),
                _ => Ok(AstNode::Export(Box::new(transpiled_node), pos)),
            }
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
        AstNode::Ident(ident, _) => {
            // checks if the ident is declared
            match check(
                input.clone(),
                ToCheck::IdentIsDeclared(ident.clone()),
                context,
            ) {
                Ok(node) => {
                    // checks if the ident is a keyword
                    check(node, ToCheck::IdentIsCorrect(ident), context)
                }
                Err(err) => Err(err),
            }
        }
        AstNode::Import(name, pos) => {
            // the standard library is imported by default
            if name == "CompactStandardLibrary" {
                context.standard_library_import = true;
            }
            return Ok(AstNode::Import(name, pos));
        }
        AstNode::LedgerDef(name, typ, pos) => {
            let transpiled_type = transpile(*typ.clone(), context)?;
            // checks if the ident is correct
            let _ = check(input, ToCheck::IdentIsCorrect(name.clone()), context)?;
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
                        ErrorMsg::InvalidType(invalid_type, pos) => {
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
            // println!("value: {:#?} / patterns: {:#?}", value, patterns);
            // pattern matching is basically transpiled into if conditions
            // 1 case = if condition { block }
            // 2 cases = if condition { block } else { block }
            // More than 2 cases = if condition { block } else if condition { block } else { block
            let transpiled_value = transpile(*value, context)?;
            let mut transpiled_patterns = patterns
                .into_iter()
                .map(|(left, right)| {
                    let transpiled_left = transpile(left, context)?;
                    let transpiled_right = transpile(right, context)?;
                    Ok((transpiled_left, transpiled_right))
                })
                .collect::<Result<Vec<(AstNode, AstNode)>, ErrorMsg>>()?;
            // all the transpiled patterns must be EnumAccess
            for (left, _) in transpiled_patterns.clone() {
                match left {
                    AstNode::EnumAccess(_, _, _) => continue,
                    _ => {
                        return Err(ErrorMsg::PatternMatchNoEnum(
                            left.clone().print()?,
                            left.get_pos(),
                        ))
                    }
                }
            }
            // checks that the value is an enum
            let checked_value = check(
                transpiled_value.clone(),
                ToCheck::PatternMatchOverEnum,
                context,
            )?;
            // checks that the pattern matching is exhaustive
            check(
                transpiled_value,
                ToCheck::ExhaustiveMatch(transpiled_patterns.clone()),
                context,
            )?;

            if transpiled_patterns.len() == 0 {
                return Err(ErrorMsg::UnexpectedLength(
                    1,
                    0,
                    "pattern_match".to_string(),
                ));
            } else if transpiled_patterns.len() == 1 {
                let (left, right) = transpiled_patterns[0].clone();
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
            } else if transpiled_patterns.len() == 2 {
                let (left1, right1) = transpiled_patterns[0].clone();
                let (_left2, right2) = transpiled_patterns[1].clone();
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
        AstNode::PragmaLanguage(comp, version, pos) => {
            context.has_language_version = true;
            Ok(AstNode::PragmaLanguage(comp, version, pos))
        }
        AstNode::PropAccess(name, prop, pos) => {
            // it can be a property access or an enum access
            let var_name = name.clone().from_ident()?;
            match (
                context.custom_types.get(&var_name),
                context.ledger.get(&var_name),
            ) {
                (Some(ntype), None) => {
                    match ntype {
                        NyxType::Enum(enum_name, _) => {
                            // enum access
                            return Ok(AstNode::EnumAccess(
                                Box::new(AstNode::Ident(enum_name.to_string(), pos.clone())),
                                prop,
                                pos,
                            ));
                        }
                        _ => {
                            // property access
                            return Ok(AstNode::PropAccess(name, prop, pos));
                        }
                    }
                }
                (None, Some((ntype, _))) => {
                    // ledger value
                    match ntype {
                        NyxType::Enum(_, _) => {
                            // enum access
                            return Ok(AstNode::EnumAccess(name, prop, pos));
                        }
                        _ => {
                            // property access
                            return Ok(AstNode::PropAccess(name, prop, pos));
                        }
                    }
                }
                _ => Ok(AstNode::PropAccess(name, prop, pos)),
            }
        }
        AstNode::StructDef(name, props, pos) => {
            if name != "Ledger" {
                Ok(AstNode::StructDef(name, props, pos))
            } else {
                // Ledger is a special struct
                // each branch is turned into a ledger property
                let transpiled_props = props
                    .into_iter()
                    .map(|arg| {
                        if arg.is_argument() {
                            let transpiled_arg = transpile(arg, context)?;
                            match transpiled_arg {
                                AstNode::Argument(name, typ, pos) => {
                                    Ok(AstNode::LedgerDef(name, typ, pos))
                                }
                                _ => Err(ErrorMsg::UnexpectedNode(
                                    String::from("Argument"),
                                    transpiled_arg,
                                    pos.clone(),
                                )),
                            }
                        } else {
                            Err(ErrorMsg::UnexpectedNode(
                                String::from("Argument"),
                                arg,
                                pos.clone(),
                            ))
                        }
                    })
                    .collect::<Result<Vec<AstNode>, ErrorMsg>>()?;

                Ok(AstNode::MultiExport(transpiled_props, pos))
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

pub fn auto_fills(ast: Vec<AstNode>, context: &mut Context) -> Vec<AstNode> {
    let mut optimized_ast = ast;
    // adds import for standard library if it is missing
    if context.standard_library_import == false {
        let std_import_node =
            AstNode::Import(String::from("CompactStandardLibrary"), Position::default());
        optimized_ast = vec![vec![std_import_node], optimized_ast].concat();
    }
    println!("Autofills AST: \n{:#?}", context.standard_library_import);
    // adds pragma languge version if unavailable
    if context.has_language_version == false {
        let pragma_lang = AstNode::PragmaLanguage(
            String::from(">="),
            context.language_version.clone(),
            Position::default(),
        );
        optimized_ast = vec![vec![pragma_lang], optimized_ast].concat();
    }

    return optimized_ast;
}
