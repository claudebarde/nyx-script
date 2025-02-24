use crate::parser::{AstNode, NyxType, Position};

#[derive(Debug)]
pub enum ErrorMsg {
    InvalidType(String, Position),                     // type, position
    IdentIsKeyword(String, Position),                  // ident, position
    PatternMatchDifferentEnums(Vec<String>, Position), // position
    PatternMatchMissingCase(String, Position),         // value, position
    PatternMatchNoIdent(String, Position),             // value, position
    PatternMatchNoEnum(String, Position),              // value, position
    PatternMatchNoEnumAccess(String, Position),        // value, position
    PatternMatchTooManyCases(String, Position),        // value, position
    PatternMatchWrongCases(String, Position),          // value, position
    UnexpectedLength(usize, usize, String),            // expected, actual, rule
    UnexpectedEnumLength(usize),                       // actual
    UnexpectedNode(String, AstNode, Position),         // expected, actual, position
    UnexpectedType(NyxType, NyxType, Position),        // expected, actual, position
    UnknownRule(String),
    UnknownType(String, Position),
    NotAnIdent(String, Position),
    NyxCustomPrint(String, Position),
    UnknownCustomType(String, Position),
    UnknownVar(String, Position),
}
