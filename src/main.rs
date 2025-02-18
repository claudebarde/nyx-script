mod error;
mod parser;
use regex;
use std::fs::{self, OpenOptions};
use std::io::Write;

fn main() {
    let file_name = "contract-test";
    let input = fs::read_to_string(format!("src/{}.nyx", file_name)).expect("cannot read file");
    match parser::parse(&input) {
        Err(err) => panic!("{:#?}", err),
        Ok(ast) => {
            let mut first_iteration = true;

            for node in ast {
                match node.print() {
                    Err(err) => panic!("{:#?}", err),
                    Ok(output) => {
                        let mut file = if first_iteration {
                            first_iteration = false;
                            OpenOptions::new()
                                .write(true)
                                .create(true)
                                .truncate(true)
                                .open(format!("src/{}.compact", file_name))
                                .expect("cannot open file")
                        } else {
                            OpenOptions::new()
                                .append(true)
                                .open(format!("src/{}.compact", file_name))
                                .expect("cannot open file")
                        };

                        writeln!(file, "{}", output).expect("cannot write to file");
                    }
                }
            }

            // light formatting
            let input =
                fs::read_to_string(format!("src/{}.compact", file_name)).expect("cannot read file");
            let formatted = regex::Regex::new(r"\n{3,}")
                .unwrap()
                .replace_all(&input, "\n\n")
                .to_string();
            fs::write(format!("src/{}.compact", file_name), formatted)
                .expect("cannot write to file");
        }
    }
}
