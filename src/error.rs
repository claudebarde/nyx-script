use crate::parser::Position;

#[derive(Debug)]
pub enum ErrorMsg {
    InvalidType(String),                    // type
    UnexpectedLength(usize, usize, String), // expected, actual, rule
    UnexpectedEnumLength(usize),            // actual
    UnknownRule(String),
    UnknownType(String, Position),
    NyxCustomPrint(Position),
    UnknownCustomType(String, Position),
}
