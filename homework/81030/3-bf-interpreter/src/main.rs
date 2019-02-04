use std::env::args;
use std::fmt::{self, Display, Formatter};
use std::fs::File;
use std::io::{self, stdin, Read};

const TAPE_SIZE: usize = 1024;

#[derive(Debug)]
enum Error {
    NoArgsProvidedError,
    UnexpectedEof,
    NegativeBracketNestingLevelError,
    NegativeInstructionPointerError,
    IoError(io::Error),
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(std::error::Error + 'static)> {
        match self {
            Error::IoError(error) => Some(error),
            _ => None,
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Error::NoArgsProvidedError => write!(f, "no arguments were provided"),
            Error::UnexpectedEof => write!(f, "unexpected EOF has been reached"),
            Error::NegativeInstructionPointerError => {
                write!(f, "instruction pointer has been decremented beyond zero")
            }
            Error::NegativeBracketNestingLevelError => {
                write!(f, "bracket nesting level has become negative")
            }
            Error::IoError(error) => write!(f, "I/O error: {}", error),
        }
    }
}

impl From<io::Error> for Error {
    fn from(error: io::Error) -> Self {
        Error::IoError(error)
    }
}

type Result<T> = std::result::Result<T, Error>;

fn interpret<T: Read>(mut input: T) -> Result<()> {
    let mut program = Vec::new();
    input.read_to_end(&mut program)?;

    let stdin = stdin();
    let mut stdin = stdin.lock().bytes();

    let mut tape: [u8; TAPE_SIZE] = [0; TAPE_SIZE];
    let mut tape_pointer: usize = 0;

    let mut bracket_nesting_level: usize = 0;
    let mut bracket_nesting_level_to_skip_to: usize = 0;
    let mut is_reverse_direction = false;

    let mut instruction_pointer: usize = 0;
    while instruction_pointer < program.len() {
        if bracket_nesting_level_to_skip_to != 0 {
            if is_reverse_direction {
                match program[instruction_pointer] {
                    b'[' => {
                        if bracket_nesting_level == bracket_nesting_level_to_skip_to {
                            bracket_nesting_level_to_skip_to = 0;
                            is_reverse_direction = false;
                        } else {
                            bracket_nesting_level = bracket_nesting_level
                                .checked_sub(1)
                                .ok_or_else(|| Error::NegativeBracketNestingLevelError)?;
                        }
                    }
                    b']' => bracket_nesting_level += 1,
                    _ => {}
                }
            } else {
                match program[instruction_pointer] {
                    b'[' => bracket_nesting_level += 1,
                    b']' => {
                        if bracket_nesting_level == bracket_nesting_level_to_skip_to {
                            bracket_nesting_level_to_skip_to = 0;
                        }
                        bracket_nesting_level = bracket_nesting_level
                            .checked_sub(1)
                            .ok_or_else(|| Error::NegativeBracketNestingLevelError)?;
                    }
                    _ => {}
                }
            }
        } else {
            match program[instruction_pointer] {
                b'>' => {
                    if tape_pointer == TAPE_SIZE - 1 {
                        tape_pointer = 0;
                    } else {
                        tape_pointer += 1;
                    }
                }
                b'<' => {
                    if tape_pointer == 0 {
                        tape_pointer = TAPE_SIZE - 1;
                    } else {
                        tape_pointer -= 1;
                    }
                }
                b'+' => tape[tape_pointer] = tape[tape_pointer].wrapping_add(1),
                b'-' => tape[tape_pointer] = tape[tape_pointer].wrapping_sub(1),
                b'.' => print!("{}", tape[tape_pointer] as char),
                b',' => {
                    if let Some(input) = stdin.next() {
                        tape[tape_pointer] = input?;
                    } else {
                        return Err(Error::UnexpectedEof);
                    }
                }
                b'[' => {
                    bracket_nesting_level += 1;
                    if tape[tape_pointer] == 0 {
                        bracket_nesting_level_to_skip_to = bracket_nesting_level;
                    }
                }
                b']' => {
                    if tape[tape_pointer] != 0 {
                        bracket_nesting_level_to_skip_to = bracket_nesting_level;
                        is_reverse_direction = true;
                    } else {
                        bracket_nesting_level = bracket_nesting_level
                            .checked_sub(1)
                            .ok_or_else(|| Error::NegativeBracketNestingLevelError)?;
                    }
                }
                _ => {}
            }
        }

        if is_reverse_direction {
            instruction_pointer = instruction_pointer
                .checked_sub(1)
                .ok_or_else(|| Error::NegativeInstructionPointerError)?
        } else {
            instruction_pointer += 1;
        }
    }

    Ok(())
}

fn main() -> Result<()> {
    let mut args = args().skip(1);
    if args.len() > 0 {
        args.try_for_each(|filename| interpret(File::open(filename)?))
    } else {
        Err(Error::NoArgsProvidedError)
    }
}
