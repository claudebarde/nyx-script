use crate::parser::Position;

#[derive(Debug)]
pub enum ErrorMsg {
    InvalidType(String),                    // type
    PatternMatchNoIdent(String, Position),  // value, position
    PatternMatchNoEnum(String, Position),   // value, position
    UnexpectedLength(usize, usize, String), // expected, actual, rule
    UnexpectedEnumLength(usize),            // actual
    UnknownRule(String),
    UnknownType(String, Position),
    NyxCustomPrint(String, Position),
    UnknownCustomType(String, Position),
}
