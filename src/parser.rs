use crate::checker::{check, ToCheck};
use crate::error::ErrorMsg;
use crate::transpiler::{transpile, Context};
use pest::iterators::Pair;
use pest::Parser;
use pest_derive::Parser;
use std::collections::HashMap;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct NyxParser;

#[derive(Debug, Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub depth: usize,
}

#[derive(Debug, Clone)]
pub enum NyxType {
    Boolean,
    Bytes(usize),
    Counter,
    Enum(String, Vec<String>),
    Field,
    Maybe(Box<NyxType>),
    OpaqueString,
    Struct(String, Box<NyxType>),
    Uint(usize),
    Vector(usize, Box<NyxType>),
    Void,
}
impl NyxType {
    pub fn print(self: NyxType) -> String {
        match self {
            NyxType::Boolean => "Boolean".to_string(),
            NyxType::Bytes(size) => format!("Bytes<{}>", size),
            NyxType::Counter => "Counter".to_string(),
            NyxType::Enum(name, elements) => format!("enum {} {{ {} }}", name, elements.join(", ")),
            NyxType::Field => "Field".to_string(),
            NyxType::Maybe(inner) => format!("Maybe<{}>", inner.print()),
            NyxType::OpaqueString => "Opaque<\"string\">".to_string(),
            NyxType::Struct(name, type_args) => format!("{}<{}>", name, type_args.print()),
            NyxType::Uint(size) => format!("Uint<{}>", size),
            NyxType::Vector(size, inner) => format!("Vector<{}, {}>", size, inner.print()),
            NyxType::Void => "[]".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum AstNode {
    /*
        NYX PATTERNS
    */
    CustomTypeDef(String, Box<AstNode>, Position), // name, type args, position
    CustomType(String, Position),                  // type, position
    Empty, // necessary to skip Nyx nodes that are not represented in the Compact grammar
    PatternMatch(Box<AstNode>, Vec<(AstNode, AstNode)>, Position), // value, patterns, position
    /*
        COMPACT OFFICIAL GRAMMAR
    */
    AdtOp(String, String, Option<Vec<AstNode>>, Position), // variable, method, type, position
    Argument(String, Box<AstNode>, Position),              // name, type, position
    Assert(Box<AstNode>, String, Position),                // condition, message, position
    Block(Vec<AstNode>, Position),                         // list of statements, position
    BooleanLiteral(bool, Position),                        // value, position
    Constructor(Vec<AstNode>, Box<AstNode>, Position),     // args, block, position
    CircuitDef(
        String,
        Option<Vec<AstNode>>,
        Box<AstNode>,
        Box<AstNode>,
        Position,
    ), // name, args, return type, block, position
    Const(String, Option<Box<AstNode>>, Box<AstNode>, Position), // name, optional type, value, position
    EnumDef(String, Vec<String>, Position),                      // name, elements, position
    Export(Box<AstNode>, Position),
    ExprEq(Box<AstNode>, Box<AstNode>, Position), // left, right, position
    ExprAs(Box<AstNode>, Vec<AstNode>, Position), // value, vec of types, position
    FunCall(String, Option<Box<AstNode>>, Vec<AstNode>, Position), // name, optional return type, params, position
    Ident(String, Position),                                       // name, position
    If(Box<AstNode>, Box<AstNode>, Position),                      // condition, block, position
    IfElse(Box<AstNode>, Box<AstNode>, Box<AstNode>, Position), // condition, if block, else block, position
    IfElseIf(
        Box<AstNode>,
        Box<AstNode>,
        Vec<(AstNode, AstNode)>,
        Box<AstNode>,
        Position,
    ), // condition, if block, vector of (condition / else if), else block, position
    Import(String, Position),
    LedgerDef(String, Box<AstNode>, Position), // name, type/ident, position
    PragmaLanguage(String, String, Position),
    PragmaCompiler(String, String, Position),
    PropAccess(Box<AstNode>, Box<AstNode>, Position), // ident, property, position
    Return(Box<AstNode>, Position),                   // value, position
    StringLiteral(String, Position),                  // value, position
    StructType(String, Box<AstNode>, Position),       // name, type args, position
    Tuple(Vec<AstNode>, Position),                    // list of values, position
    Type(NyxType, Position),                          // type, position
    TypeList(Vec<AstNode>, Position),                 // list of types, position
    UintLiteral(usize, Position),                     // size
    VarAssignment(Box<AstNode>, Box<AstNode>, Position), // ident, value, position
    Wdecl(String, Box<AstNode>, Position),            // name, type, position
    EOI,
}
impl AstNode {
    pub fn print(self: AstNode) -> Result<String, ErrorMsg> {
        fn tab(num: usize) -> String {
            if num <= 0 {
                return "".to_string();
            }
            return "   ".repeat(num);
            // "*".repeat(num)
        }

        match self {
            /*
                NYX PATTERNS
            */
            AstNode::CustomTypeDef(_, _, pos) | AstNode::CustomType(_, pos) => {
                // println!("self: {:#?}", self);
                Err(ErrorMsg::NyxCustomPrint(String::from("CustomTypeDef"), pos))
            }
            AstNode::PatternMatch(_, _, pos) => {
                Err(ErrorMsg::NyxCustomPrint(String::from("PatternMatch"), pos))
            }
            /*
            COMPACT OFFICIAL GRAMMAR
            */
            AstNode::AdtOp(variable, method, type_args, pos) => {
                let type_args_list = match type_args {
                    Some(args) => {
                        let args_list = args
                            .iter()
                            .map(|x| x.clone().print())
                            .collect::<Result<Vec<String>, ErrorMsg>>()?
                            .join(", ");
                        format!("{}", args_list)
                    }
                    None => "".to_string(),
                };
                Ok(format!(
                    "{}{}.{}({});",
                    tab(pos.depth),
                    variable,
                    method,
                    type_args_list
                ))
            }
            AstNode::Argument(name, typ, pos) => {
                Ok(format!("{}{}: {}", tab(pos.depth), name, typ.print()?))
            }
            AstNode::Assert(condition, message, pos) => Ok(format!(
                "{}assert {} \"{}\";",
                tab(pos.depth),
                condition.print()?,
                message
            )),
            AstNode::Block(statements, pos) => {
                let block = statements
                    .clone()
                    .into_iter()
                    .map(|x| x.print())
                    .collect::<Result<Vec<String>, ErrorMsg>>()?
                    .join("\n");
                if statements.len() == 0 {
                    Ok(format!("{{}}"))
                } else {
                    Ok(format!(
                        "{{\n{}\n{}}}",
                        block,
                        tab(pos.depth.saturating_sub(1))
                    ))
                }
            }
            AstNode::BooleanLiteral(value, pos) => Ok(format!("{}{}", tab(pos.depth), value)),
            AstNode::CircuitDef(name, args, return_type, block, pos) => {
                let args_list = match args {
                    None => "".to_string(),
                    Some(args) => {
                        let args_list = args
                            .iter()
                            .map(|x| x.clone().print())
                            .collect::<Result<Vec<String>, ErrorMsg>>()?
                            .join(", ");
                        format!("{}", args_list)
                    }
                };
                Ok(format!(
                    "{}circuit {}({}): {} {}",
                    tab(pos.depth),
                    name,
                    args_list,
                    return_type.print()?,
                    block.print()?
                ))
            }
            AstNode::Const(name, typ, value, pos) => {
                let type_str = match typ {
                    Some(t) => format!(": {}", t.print()?),
                    None => "".to_string(),
                };
                Ok(format!(
                    "{}const {}{} = {};",
                    tab(pos.depth),
                    name,
                    type_str,
                    value.print()?
                ))
            }
            AstNode::Constructor(args, block, pos) => {
                let args_list = args
                    .iter()
                    .map(|x| x.clone().print())
                    .collect::<Result<Vec<String>, ErrorMsg>>()?
                    .join(", ");
                Ok(format!(
                    "\n{}constructor({}) {}\n",
                    tab(pos.depth),
                    args_list,
                    block.print()?
                ))
            }
            AstNode::Empty => Ok("".to_string()),
            AstNode::EnumDef(name, elements, pos) => Ok(format!(
                "{}enum {} {{ {} }}",
                tab(pos.depth),
                name,
                elements.join(", ")
            )),
            AstNode::Export(child, pos) => match *child {
                AstNode::CircuitDef(_, _, _, _, _) => {
                    // no semi-colon required
                    Ok(format!("\n{}export {}", tab(pos.depth), child.print()?))
                }
                _ => Ok(format!("{}export {};", tab(pos.depth), child.print()?)),
            },
            AstNode::ExprAs(value, types, pos) => {
                let types_list = types
                    .iter()
                    .map(|x| x.clone().print())
                    .collect::<Result<Vec<String>, ErrorMsg>>()?
                    .join(" as ");
                Ok(format!(
                    "{}{} as {}",
                    tab(pos.depth),
                    value.print()?,
                    types_list
                ))
            }
            AstNode::ExprEq(left, right, pos) => Ok(format!(
                "{}{} == {}",
                tab(pos.depth),
                left.print()?,
                right.print()?
            )),
            AstNode::FunCall(name, fun_type, params, pos) => {
                let params_list = params
                    .iter()
                    .map(|x| x.clone().print())
                    .collect::<Result<Vec<String>, ErrorMsg>>()?
                    .join(", ");
                match fun_type {
                    Some(t) => Ok(format!("{}<{}>({})", name, t.print()?, params_list)),
                    None => Ok(format!("{}{}({})", tab(pos.depth), name, params_list)),
                }
            }
            AstNode::Ident(name, pos) => Ok(format!("{}{}", tab(pos.depth), name)),
            AstNode::If(condition, if_block, pos) => Ok(format!(
                "{}if ({}) {}",
                tab(pos.depth),
                condition.print()?,
                if_block.print()?
            )),
            AstNode::IfElse(condition, if_block, else_block, pos) => Ok(format!(
                "{}if ({}) {} else {}",
                tab(pos.depth),
                condition.print()?,
                if_block.print()?,
                else_block.print()?
            )),
            AstNode::IfElseIf(condition, if_block, else_if, else_block, pos) => {
                let else_if_list = else_if
                    .iter()
                    .map(|(cond, block)| {
                        let cond_str = cond.clone().print()?;
                        let block_str = block.clone().print()?;
                        Ok(format!("else if ({}) {}", cond_str, block_str))
                    })
                    .collect::<Result<Vec<String>, ErrorMsg>>()?
                    .join(" ");
                Ok(format!(
                    "{}if ({}) {} {} else {}",
                    tab(pos.depth),
                    condition.print()?,
                    if_block.print()?,
                    else_if_list,
                    else_block.print()?
                ))
            }
            AstNode::Import(import, pos) => Ok(format!("{}import {};", tab(pos.depth), import)),
            AstNode::LedgerDef(name, typ, pos) => Ok(format!(
                "{}ledger {}: {}",
                tab(pos.depth),
                name,
                typ.print()?
            )),
            AstNode::PragmaLanguage(comp_op, version, _) => Ok(format!(
                "pragma language_version {} {};\n",
                comp_op, version
            )),
            AstNode::PragmaCompiler(comp_op, version, _) => Ok(format!(
                "pragma compiler_version {} {};\n",
                comp_op, version
            )),
            AstNode::PropAccess(ident, prop, pos) => Ok(format!(
                "{}{}.{}",
                tab(pos.depth),
                ident.print()?,
                prop.print()?
            )),
            AstNode::Return(value, pos) => {
                Ok(format!("{}return {};", tab(pos.depth), value.print()?))
            }
            AstNode::StringLiteral(value, pos) => Ok(format!("{}\"{}\"", tab(pos.depth), value)),
            AstNode::StructType(name, type_args, pos) => Ok(format!(
                "{}{}<{}>",
                tab(pos.depth),
                name,
                type_args.print()?
            )),
            AstNode::Tuple(tuple, pos) => {
                let tuple_list = tuple
                    .iter()
                    .map(|x| x.clone().print())
                    .collect::<Result<Vec<String>, ErrorMsg>>()?
                    .join(", ");
                Ok(format!("{}[{}]", tab(pos.depth), tuple_list))
            }
            AstNode::Type(nyx_type, pos) => Ok(format!("{}{}", tab(pos.depth), nyx_type.print())),
            AstNode::TypeList(list, pos) => {
                let args_list = list
                    .iter()
                    .map(|x| x.clone().print())
                    .collect::<Result<Vec<String>, ErrorMsg>>()?
                    .join(", ");
                Ok(format!("{}{}", tab(pos.depth), args_list))
            }
            AstNode::UintLiteral(size, _) => Ok(format!("{}", size)),
            AstNode::VarAssignment(ident, value, pos) => Ok(format!(
                "{}{} = {};",
                tab(pos.depth),
                ident.print()?,
                value.print()?
            )),
            AstNode::Wdecl(name, typ, pos) => Ok(format!(
                "{}witness {}(): {};",
                tab(pos.depth),
                name,
                typ.print()?
            )),
            AstNode::EOI => Ok("".to_string()),
        }
    }

    pub fn get_pos(self) -> Position {
        match self {
            AstNode::CustomTypeDef(_, _, pos)
            | AstNode::CustomType(_, pos)
            | AstNode::PatternMatch(_, _, pos)
            | AstNode::AdtOp(_, _, _, pos)
            | AstNode::Argument(_, _, pos)
            | AstNode::Assert(_, _, pos)
            | AstNode::Block(_, pos)
            | AstNode::BooleanLiteral(_, pos)
            | AstNode::Constructor(_, _, pos)
            | AstNode::CircuitDef(_, _, _, _, pos)
            | AstNode::Const(_, _, _, pos)
            | AstNode::EnumDef(_, _, pos)
            | AstNode::Export(_, pos)
            | AstNode::ExprEq(_, _, pos)
            | AstNode::ExprAs(_, _, pos)
            | AstNode::FunCall(_, _, _, pos)
            | AstNode::Ident(_, pos)
            | AstNode::If(_, _, pos)
            | AstNode::IfElse(_, _, _, pos)
            | AstNode::IfElseIf(_, _, _, _, pos)
            | AstNode::Import(_, pos)
            | AstNode::LedgerDef(_, _, pos)
            | AstNode::PragmaLanguage(_, _, pos)
            | AstNode::PragmaCompiler(_, _, pos)
            | AstNode::PropAccess(_, _, pos)
            | AstNode::Return(_, pos)
            | AstNode::StringLiteral(_, pos)
            | AstNode::StructType(_, _, pos)
            | AstNode::Tuple(_, pos)
            | AstNode::Type(_, pos)
            | AstNode::TypeList(_, pos)
            | AstNode::UintLiteral(_, pos)
            | AstNode::VarAssignment(_, _, pos)
            | AstNode::Wdecl(_, _, pos) => pos,
            AstNode::Empty | AstNode::EOI => Position {
                line: 0,
                column: 0,
                depth: 0,
            },
        }
    }

    pub fn to_type(self: AstNode) -> Result<Vec<NyxType>, ErrorMsg> {
        // println!("self: {:#?}", self);
        match self {
            AstNode::Type(t, _) => Ok(vec![t]),
            AstNode::StructType(name, type_args, _) => match name.as_str() {
                "Maybe" => {
                    let inner = type_args.to_type()?;
                    let el_type = inner[0].clone();
                    Ok(vec![NyxType::Maybe(Box::new(el_type))])
                }
                _ => todo!("cast struct type to NyxType"),
            },
            AstNode::TypeList(types, _) => {
                let types = types
                    .into_iter()
                    .map(|x| x.to_type())
                    .collect::<Result<Vec<Vec<NyxType>>, ErrorMsg>>()?
                    .into_iter()
                    .flatten()
                    .collect();
                Ok(types)
            }
            AstNode::EnumDef(name, elements, _) => Ok(vec![NyxType::Enum(name, elements)]),
            _ => Err(ErrorMsg::InvalidType(self.print()?)),
        }
    }
}

fn tokenize(pair: Pair<'_, Rule>, depth: usize) -> Result<AstNode, ErrorMsg> {
    let (line, column) = pair.line_col();
    match pair.as_rule() {
        /*
           NYX PATTERNS
        */
        Rule::nyx_type_def => {
            let children = pair.clone().into_inner();
            if children.len() != 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    children.len(),
                    "nyx_custom_type".to_string(),
                ));
            }
            let name = children.clone().nth(0).unwrap().as_str().to_string();
            let type_arg = tokenize(children.clone().nth(1).unwrap(), depth)?;
            Ok(AstNode::CustomTypeDef(
                name,
                Box::new(type_arg),
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::nyx_custom_type => {
            let custom_type = pair.as_span().as_str().to_string();
            Ok(AstNode::CustomType(
                custom_type,
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::pattern_match => {
            let children = pair.clone().into_inner();
            if children.len() < 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    children.len(),
                    "pattern_match".to_string(),
                ));
            }
            let value = tokenize(children.clone().nth(0).unwrap(), 0)?;
            let patterns = children
                .skip(1)
                .map(|x| {
                    let pattern = x.into_inner();
                    let left = tokenize(pattern.clone().nth(0).unwrap(), 0)?;
                    let right = tokenize(pattern.clone().nth(1).unwrap(), depth + 1)?;
                    Ok((left, right))
                })
                .collect::<Result<Vec<(AstNode, AstNode)>, ErrorMsg>>()?;

            Ok(AstNode::PatternMatch(
                Box::new(value),
                patterns,
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        /*
           COMPACT OFFICIAL GRAMMAR
        */
        Rule::adt_op => {
            let children = pair.clone().into_inner();
            if children.len() < 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    children.len(),
                    "adt_op".to_string(),
                ));
            }
            let variable = children.clone().nth(0).unwrap().as_str().to_string();
            let method = children.clone().nth(1).unwrap().as_str().to_string();
            let type_args = if children.len() > 2 {
                let type_args = children
                    .into_iter()
                    .skip(2)
                    .map(|x| tokenize(x, depth))
                    .collect::<Result<Vec<AstNode>, ErrorMsg>>()?;
                Some(type_args)
            } else {
                None
            };

            Ok(AstNode::AdtOp(
                variable,
                method,
                type_args,
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::argument => {
            let children = pair.clone().into_inner();
            if children.len() != 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    children.len(),
                    "argument".to_string(),
                ));
            }
            let name = children.clone().nth(0).unwrap().as_str().to_string();
            let typ = tokenize(children.clone().nth(1).unwrap(), 0)?;

            Ok(AstNode::Argument(
                name,
                Box::new(typ),
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::assert => {
            let children = pair.clone().into_inner();
            if children.len() != 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    children.len(),
                    "assert".to_string(),
                ));
            }
            let condition = tokenize(children.clone().nth(0).unwrap(), 0)?;
            let message = children
                .clone()
                .nth(1)
                .unwrap()
                .as_str()
                .trim_matches('"')
                .to_string();

            Ok(AstNode::Assert(
                Box::new(condition),
                message,
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::block => {
            let children = pair
                .clone()
                .into_inner()
                .map(|x| tokenize(x, depth + 1))
                .collect::<Result<Vec<AstNode>, ErrorMsg>>()?;

            Ok(AstNode::Block(
                children,
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::boolean => {
            let boolean = pair.as_str().parse::<bool>().unwrap();

            Ok(AstNode::BooleanLiteral(
                boolean,
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::circuit_def => {
            let children = pair.clone().into_inner();
            if children.len() < 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    children.len(),
                    "circuit_def".to_string(),
                ));
            }
            let name = children.clone().nth(0).unwrap().as_str().to_string();
            let mut args = children
                .skip(1)
                .map(|x| tokenize(x, depth))
                .collect::<Result<Vec<AstNode>, ErrorMsg>>()?;
            // the vector must have a length of 2 or more here
            // 2 means there is no argument
            // more means there are arguments (at least 1)

            if args.len() == 2 {
                let return_type = args[0].clone();
                let block = args[1].clone();
                Ok(AstNode::CircuitDef(
                    name,
                    None,
                    Box::new(return_type),
                    Box::new(block),
                    Position {
                        line,
                        column,
                        depth,
                    },
                ))
            } else {
                // last element is the block
                // second to last element is the return type
                // the rest are arguments
                let block = args.pop().unwrap();
                let return_type = args.pop().unwrap();
                Ok(AstNode::CircuitDef(
                    name,
                    Some(args),
                    Box::new(return_type),
                    Box::new(block),
                    Position {
                        line,
                        column,
                        depth,
                    },
                ))
            }
        }
        Rule::compiler_version => {
            let atoms = pair.clone().into_inner();
            if atoms.len() != 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    atoms.len(),
                    "compiler_version".to_string(),
                ));
            }
            match [atoms.clone().nth(0), atoms.clone().nth(1)] {
                [Some(comp_op), Some(version)] => Ok(AstNode::PragmaCompiler(
                    String::from(comp_op.as_str()),
                    String::from(version.as_str()),
                    Position {
                        line: pair.line_col().0,
                        column: pair.line_col().1,
                        depth: depth,
                    },
                )),
                _ => Err(ErrorMsg::UnknownRule(pair.as_str().to_string())),
            }
        }
        Rule::const_stmt => {
            let children = pair.clone().into_inner();
            if children.len() < 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    children.len(),
                    "const_stmt".to_string(),
                ));
            }
            let name = children.clone().nth(0).unwrap().as_str().to_string();
            let value = tokenize(children.clone().nth(1).unwrap(), 0)?;

            Ok(AstNode::Const(
                name,
                None,
                Box::new(value),
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::const_type_stmt => {
            let children = pair.clone().into_inner();
            if children.len() < 3 {
                return Err(ErrorMsg::UnexpectedLength(
                    3,
                    children.len(),
                    "const_type_stmt".to_string(),
                ));
            }
            let name = children.clone().nth(0).unwrap().as_str().to_string();
            let typ = tokenize(children.clone().nth(1).unwrap(), 0)?;
            let value = tokenize(children.clone().nth(2).unwrap(), 0)?;

            Ok(AstNode::Const(
                name,
                Some(Box::new(typ)),
                Box::new(value),
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::constructor => {
            let children = pair
                .clone()
                .into_inner()
                .map(|x| tokenize(x, depth))
                .collect::<Result<Vec<AstNode>, ErrorMsg>>()?;

            // figures out which children are arguments and which is the block
            let arguments = children
                .clone()
                .into_iter()
                .take_while(|x| match x {
                    AstNode::Block(_, _) => false,
                    _ => true,
                })
                .collect();
            let blocks = children
                .clone()
                .into_iter()
                .filter(|x| match x {
                    AstNode::Block(_, _) => true,
                    _ => false,
                })
                .collect::<Vec<AstNode>>();
            if blocks.len() != 1 {
                return Err(ErrorMsg::UnexpectedLength(
                    1,
                    blocks.len(),
                    "constructor block".to_string(),
                ));
            }

            Ok(AstNode::Constructor(
                arguments,
                Box::new(blocks.into_iter().nth(0).unwrap()),
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::enumdef => {
            let enum_children = pair.clone().into_inner();
            // there must be at least 2 children
            // 1. the name of the enum
            // 2. one element in the body of the enum
            if enum_children.len() < 2 {
                return Err(ErrorMsg::UnexpectedEnumLength(enum_children.len()));
            }
            let enum_name = enum_children.clone().nth(0).unwrap().as_str().to_string();
            let enum_elements = enum_children
                .skip(1)
                .map(|x| x.as_str().to_string())
                .collect();

            Ok(AstNode::EnumDef(
                enum_name,
                enum_elements,
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::export => {
            let mut export = pair.clone().into_inner();
            if export.len() != 1 {
                return Err(ErrorMsg::UnexpectedLength(
                    1,
                    export.len(),
                    "export".to_string(),
                ));
            }
            let child = tokenize(export.nth(0).unwrap(), 0)?;

            Ok(AstNode::Export(
                Box::new(child),
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::expr_as => {
            let children = pair.clone().into_inner();
            if children.len() < 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    children.len(),
                    "expr_as".to_string(),
                ));
            }
            let value = tokenize(children.clone().nth(0).unwrap(), 0)?;
            let types = children
                .skip(1)
                .map(|x| tokenize(x, depth))
                .collect::<Result<Vec<AstNode>, ErrorMsg>>()?;

            Ok(AstNode::ExprAs(
                Box::new(value),
                types,
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::expr_eq => {
            let children = pair.clone().into_inner();
            if children.len() != 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    children.len(),
                    "expr_eq".to_string(),
                ));
            }
            let left = tokenize(children.clone().nth(0).unwrap(), 0)?;
            let right = tokenize(children.clone().nth(1).unwrap(), 0)?;

            Ok(AstNode::ExprEq(
                Box::new(left),
                Box::new(right),
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::fn_call => {
            let children = pair.clone().into_inner();
            if children.len() == 0 {
                return Err(ErrorMsg::UnexpectedLength(
                    1,
                    children.len(),
                    "fun_call".to_string(),
                ));
            } else if children.len() == 1 {
                // just the name of the function, no type or params
                let fun_name = children.clone().nth(0).unwrap().as_str().to_string();

                Ok(AstNode::FunCall(
                    fun_name,
                    None,
                    vec![],
                    Position {
                        line,
                        column,
                        depth,
                    },
                ))
            } else {
                let fun_name = children.clone().nth(0).unwrap().as_str().to_string();
                let return_type = children.clone().nth(1).unwrap();
                match return_type.as_rule() {
                    Rule::type_list => {
                        // function has a return type
                        let params = children
                            .skip(2)
                            .map(|x| tokenize(x, depth))
                            .collect::<Result<Vec<AstNode>, ErrorMsg>>()?;
                        let fun_return_type = tokenize(return_type, 0)?;

                        Ok(AstNode::FunCall(
                            fun_name,
                            Some(Box::new(fun_return_type)),
                            params,
                            Position {
                                line,
                                column,
                                depth,
                            },
                        ))
                    }
                    _ => {
                        // function has no return type
                        let params = children
                            .skip(1)
                            .map(|x| tokenize(x, depth))
                            .collect::<Result<Vec<AstNode>, ErrorMsg>>()?;

                        Ok(AstNode::FunCall(
                            fun_name,
                            None,
                            params,
                            Position {
                                line,
                                column,
                                depth,
                            },
                        ))
                    }
                }
            }
        }
        Rule::fn_return => {
            let children = pair.clone().into_inner();
            if children.len() != 1 {
                return Err(ErrorMsg::UnexpectedLength(
                    1,
                    children.len(),
                    "fn_return".to_string(),
                ));
            }
            let value = tokenize(children.clone().nth(0).unwrap(), 0)?;

            Ok(AstNode::Return(
                Box::new(value),
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::ident => {
            let ident = pair.as_str().to_string();

            Ok(AstNode::Ident(
                ident,
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::if_cond => {
            let children = pair.clone().into_inner();
            if children.len() != 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    children.len(),
                    "if_cond".to_string(),
                ));
            }
            let condition = tokenize(children.clone().nth(0).unwrap(), depth)?;
            let block = tokenize(children.clone().nth(1).unwrap(), depth + 1)?;

            Ok(AstNode::If(
                Box::new(condition),
                Box::new(block),
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::if_else_cond => {
            let children = pair.clone().into_inner();
            if children.len() != 3 {
                return Err(ErrorMsg::UnexpectedLength(
                    3,
                    children.len(),
                    "if_else_cond".to_string(),
                ));
            }
            let condition = tokenize(children.clone().nth(0).unwrap(), depth)?;
            let if_block = tokenize(children.clone().nth(1).unwrap(), depth + 1)?;
            let else_block = tokenize(children.clone().nth(2).unwrap(), depth + 1)?;

            Ok(AstNode::Block(
                vec![
                    AstNode::Assert(
                        Box::new(condition),
                        "condition failed".to_string(),
                        Position {
                            line,
                            column,
                            depth,
                        },
                    ),
                    if_block,
                    else_block,
                ],
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::import => {
            // println!("{:#?}", pair);
            let atoms = pair.clone().into_inner();
            if atoms.len() != 1 {
                return Err(ErrorMsg::UnexpectedLength(
                    1,
                    atoms.len(),
                    "import".to_string(),
                ));
            }
            let import = atoms.clone().nth(0).unwrap().as_str();

            Ok(AstNode::Import(
                String::from(import),
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        // pragma language version
        Rule::language_version => {
            let atoms = pair.clone().into_inner();
            if atoms.len() != 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    atoms.len(),
                    "language_version".to_string(),
                ));
            }
            match [atoms.clone().nth(0), atoms.clone().nth(1)] {
                [Some(comp_op), Some(version)] => Ok(AstNode::PragmaLanguage(
                    String::from(comp_op.as_str()),
                    String::from(version.as_str()),
                    Position {
                        line: pair.line_col().0,
                        column: pair.line_col().1,
                        depth: depth,
                    },
                )),
                _ => Err(ErrorMsg::UnknownRule(pair.as_str().to_string())),
            }
        }
        Rule::ledger_def => {
            let ledger_children = pair.clone().into_inner();
            // there must be at least 2 children
            // 1. the name of the ledger value
            // 2. the type of the ledger value or its identifier
            if ledger_children.len() != 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    ledger_children.len(),
                    "ledger_def".to_string(),
                ));
            }
            let ledger_name = ledger_children.clone().nth(0).unwrap().as_str().to_string();
            let ledger_type = tokenize(ledger_children.clone().nth(1).unwrap(), depth)?;

            Ok(AstNode::LedgerDef(
                ledger_name,
                Box::new(ledger_type),
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::prop_access => {
            let children = pair.clone().into_inner();
            if children.len() != 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    children.len(),
                    "prop_access".to_string(),
                ));
            }
            let ident = tokenize(children.clone().nth(0).unwrap(), depth)?;
            let prop = tokenize(children.clone().nth(1).unwrap(), depth)?;

            Ok(AstNode::PropAccess(
                Box::new(ident),
                Box::new(prop),
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::string => {
            let string = pair.as_str().trim_matches('"').to_string();

            Ok(AstNode::StringLiteral(
                string,
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::struct_type => {
            let children = pair.clone().into_inner();
            if children.len() != 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    children.len(),
                    "struct_type".to_string(),
                ));
            }

            let type_name = children.clone().nth(0).unwrap().as_str().to_string();
            let type_args = tokenize(children.clone().nth(1).unwrap(), depth)?;

            Ok(AstNode::StructType(
                type_name,
                Box::new(type_args),
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::tuple => {
            let tuple_children = pair.clone().into_inner();
            let tuple = tuple_children
                .map(|x| tokenize(x, depth))
                .collect::<Result<Vec<AstNode>, ErrorMsg>>()?;

            Ok(AstNode::Tuple(
                tuple,
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::type_ => {
            let type_children = pair.clone().into_inner();
            if type_children.len() == 0 {
                // simple type
                let simple_type = pair.as_str();

                let pos = Position {
                    line,
                    column,
                    depth,
                };
                match simple_type {
                    // Opaque String
                    "Opaque<\"string\">" => Ok(AstNode::Type(NyxType::OpaqueString, pos)),
                    "[]" => Ok(AstNode::Type(NyxType::Void, pos)),
                    "Field" => Ok(AstNode::Type(NyxType::Field, pos)),
                    "Counter" => Ok(AstNode::Type(NyxType::Counter, pos)),
                    // Uint
                    _ if simple_type.starts_with("Uint<") && simple_type.ends_with('>') => {
                        let size_str = &simple_type[5..simple_type.len() - 1];
                        if let Ok(size) = size_str.parse::<usize>() {
                            Ok(AstNode::Type(NyxType::Uint(size), pos))
                        } else {
                            Err(ErrorMsg::UnknownType(simple_type.to_string(), pos))
                        }
                    }
                    // Bytes
                    _ if simple_type.starts_with("Bytes<") && simple_type.ends_with('>') => {
                        let size_str = &simple_type[6..simple_type.len() - 1];
                        if let Ok(size) = size_str.parse::<usize>() {
                            Ok(AstNode::Type(NyxType::Bytes(size), pos))
                        } else {
                            Err(ErrorMsg::UnknownType(simple_type.to_string(), pos))
                        }
                    }
                    _ => Err(ErrorMsg::UnknownType(simple_type.to_string(), pos)),
                }
            } else if type_children.len() == 1 {
                // complex type
                let complex_type = tokenize(type_children.clone().nth(0).unwrap(), depth)?;
                Ok(complex_type)
            } else {
                Err(ErrorMsg::UnexpectedLength(
                    1,
                    type_children.len(),
                    "type".to_string(),
                ))
            }
        }
        Rule::type_list => {
            let list = pair.clone().into_inner();
            let type_list = list
                .map(|x| tokenize(x, depth))
                .collect::<Result<Vec<AstNode>, ErrorMsg>>()?;

            Ok(AstNode::TypeList(
                type_list,
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::uint => {
            let size = pair.as_str().parse::<usize>().unwrap();
            Ok(AstNode::UintLiteral(
                size,
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::var_assignment => {
            let children = pair.clone().into_inner();
            if children.len() != 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    children.len(),
                    "var_assignment".to_string(),
                ));
            }
            let ident = tokenize(children.clone().nth(0).unwrap(), 0)?;
            let value = tokenize(children.clone().nth(1).unwrap(), 0)?;

            Ok(AstNode::VarAssignment(
                Box::new(ident),
                Box::new(value),
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::vector_type => {
            let children = pair.clone().into_inner();
            if children.len() != 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    children.len(),
                    "vector_type".to_string(),
                ));
            }
            let size = children
                .clone()
                .nth(0)
                .unwrap()
                .as_str()
                .parse::<usize>()
                .unwrap();
            let inner = tokenize(children.clone().nth(1).unwrap(), 0)?;
            let el_type_vec = inner.to_type()?;
            // there should be only one element in the vector
            if el_type_vec.len() != 1 {
                return Err(ErrorMsg::UnexpectedLength(
                    1,
                    el_type_vec.len(),
                    "vector_type".to_string(),
                ));
            }
            let el_type = el_type_vec[0].clone();

            Ok(AstNode::Type(
                NyxType::Vector(size, Box::new(el_type)),
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::wdecl => {
            let children = pair.clone().into_inner();
            if children.len() != 2 {
                return Err(ErrorMsg::UnexpectedLength(
                    2,
                    children.len(),
                    "wdecl".to_string(),
                ));
            }
            let name = children.clone().nth(0).unwrap().as_str().to_string();
            let typ = tokenize(children.clone().nth(1).unwrap(), 0)?;

            Ok(AstNode::Wdecl(
                name,
                Box::new(typ),
                Position {
                    line,
                    column,
                    depth,
                },
            ))
        }
        Rule::EOI => Ok(AstNode::EOI),
        _ => {
            println!("{:#?}", pair);
            Err(ErrorMsg::UnknownRule(pair.as_str().to_string()))
        }
    }
}

pub fn parse(input: &str) -> Result<Vec<AstNode>, ErrorMsg> {
    let mut ast: Vec<AstNode> = vec![];

    let pairs = NyxParser::parse(Rule::program, input).unwrap_or_else(|e| panic!("{}", e));
    for pair in pairs {
        let ast_node = tokenize(pair, 0)?;
        ast.push(ast_node);
    }

    let mut context = Context {
        custom_types: HashMap::new(),
    };
    let mut transpiled_ast: Vec<AstNode> = vec![];
    for node in ast {
        let transpiled_node = transpile(node, &mut context)?;
        transpiled_ast.push(transpiled_node);
    }

    // println!("Transpiled AST: \n{:#?}", transpiled_ast);

    return Ok(transpiled_ast);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_if1() {
        let input = r"if (test == true) { return true; } ";
        let expected_output = input;
        match parse(input) {
            Ok(ast) => {
                let mut context = Context {
                    custom_types: HashMap::new(),
                };
                let mut output = String::new();
                for node in ast {
                    let transpiled_node = transpile(node, &mut context).unwrap();
                    output.push_str(&transpiled_node.print().unwrap());
                }

                // Normalize the strings by trimming whitespace and replacing newlines
                let normalized_output = regex::Regex::new(r"(\n|\s)+")
                    .unwrap()
                    .replace_all(&output, " ")
                    .to_string();
                let normalized_expected_output = regex::Regex::new(r"(\n|\s)+")
                    .unwrap()
                    .replace_all(&expected_output, " ")
                    .to_string();

                assert_eq!(normalized_output, normalized_expected_output);
            }
            Err(e) => panic!("{:#?}", e),
        }
    }

    #[test]
    fn transpile_custom_type1() {
        let input = r"
            type CustomType = Uint<32>;
            const test: CustomType = 32;
        ";
        let expected_output = r"const test: Uint<32> = 32;";
        match parse(input) {
            Ok(ast) => {
                let mut context = Context {
                    custom_types: HashMap::new(),
                };
                let mut output = String::new();
                for node in ast {
                    let transpiled_node = transpile(node, &mut context).unwrap();
                    output.push_str(&transpiled_node.print().unwrap());
                }

                // Normalize the strings by trimming whitespace and replacing newlines
                let normalized_output = output.trim().replace("\r\n", "\n").replace("\r", "\n");
                let normalized_expected_output = expected_output
                    .trim()
                    .replace("\r\n", "\n")
                    .replace("\r", "\n");

                assert_eq!(normalized_output, normalized_expected_output);
            }
            Err(e) => panic!("{:#?}", e),
        }
    }

    #[test]
    fn transpile_custom_type2() {
        let input = r#"
            type CustomType = Maybe<Opaque<"string">>;
            const test: CustomType = none<CustomType>();
        "#;
        let expected_output =
            r#"const test: Maybe<Opaque<"string">> = none<Maybe<Opaque<"string">>>();"#;
        match parse(input) {
            Ok(ast) => {
                let mut output = String::new();
                for node in ast {
                    match node.print() {
                        Ok(s) => output.push_str(&s),
                        Err(e) => panic!("{:#?}", e),
                    }
                }
                // Normalize the strings by trimming whitespace and replacing newlines
                let normalized_output = output.trim().replace("\r\n", "\n").replace("\r", "\n");
                let normalized_expected_output = expected_output
                    .trim()
                    .replace("\r\n", "\n")
                    .replace("\r", "\n");

                assert_eq!(normalized_output, normalized_expected_output);
            }
            Err(e) => panic!("{:#?}", e),
        }
    }
}
