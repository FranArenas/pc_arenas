use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Identifier(String),

    // Constants
    IntLiteral(i64),

    // Keywords
    IntKeyword,
    VoidKeyword,
    ReturnKeyword,

    // Assignment operator
    Assignment, // =

    // Compound assignment operators
    AddAssignment,        // +=
    SubtractAssignment,   // -=
    MultiplyAssignment,   // *=
    DivideAssignment,     // /=
    ModulusAssignment,    // %=
    BitwiseAndAssignment, // &=
    BitwiseOrAssignment,  // |=
    BitwiseXorAssignment, // ^=
    ShiftLeftAssignment,  // <<=
    ShiftRightAssignment, // >>=

    // Increment and decrement operators
    Increment, // ++
    Decrement, // --

    // Unary operators
    BitwiseComplement, // ~
    Negation,          // - Note: It can be unary or binary

    // Binary arithmetical operators
    Add,      // +
    Multiply, // *
    Divide,   // /
    Modulus,  // %

    // bitwise operators
    BitwiseAnd, // &
    BitwiseOr,  // |
    BitwiseXor, // ^

    // shift operators
    ShiftLeft,  // <<
    ShiftRight, // >>

    // Logical operators
    LogicalAnd,         // &&
    LogicalOr,          // ||
    LogicalNot,         // !
    Equal,              // ==
    NotEqual,           // !=
    LessThan,           // <
    GreaterThan,        // >
    LessThanOrEqual,    // <=
    GreaterThanOrEqual, // >=

    // Others
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Semicolon,

    // Control flow
    IfKeyword,
    ElseKeyword,
    QuestionMark,
    Colon,

    // Loops
    DoKeyword,
    WhileKeyword,
    ForKeyword,
    BreakKeyword,
    ContinueKeyword,

    // End of file
    EndOfFile,
}

impl TokenType {
    pub fn is_binary_operator(&self) -> bool {
        matches!(
            self,
            TokenType::Add
                | TokenType::Multiply
                | TokenType::Divide
                | TokenType::Modulus
                | TokenType::Negation // Note: Negation can also be unary
                | TokenType::BitwiseAnd
                | TokenType::BitwiseOr
                | TokenType::BitwiseXor
                | TokenType::ShiftLeft
                | TokenType::ShiftRight
                | TokenType::LogicalAnd
                | TokenType::LogicalOr
                | TokenType::Equal
                | TokenType::NotEqual
                | TokenType::LessThan
                | TokenType::GreaterThan
                | TokenType::LessThanOrEqual
                | TokenType::GreaterThanOrEqual
                | TokenType::Assignment // Note: Assignment is treated as a binary operator even though it is handled individually in the parser
        )
    }

    pub fn is_compound_assignment(&self) -> bool {
        matches!(
            self,
            TokenType::AddAssignment
                | TokenType::SubtractAssignment
                | TokenType::MultiplyAssignment
                | TokenType::DivideAssignment
                | TokenType::ModulusAssignment
                | TokenType::BitwiseAndAssignment
                | TokenType::BitwiseOrAssignment
                | TokenType::BitwiseXorAssignment
                | TokenType::ShiftLeftAssignment
                | TokenType::ShiftRightAssignment
        )
    }

    pub fn get_precedence(&self) -> Option<u8> {
        match self {
            TokenType::Multiply | TokenType::Divide | TokenType::Modulus => Some(12),
            TokenType::Add | TokenType::Negation => Some(11),
            TokenType::ShiftLeft | TokenType::ShiftRight => Some(10),

            // Relational (higher than bitwise ops)
            TokenType::GreaterThan
            | TokenType::LessThan
            | TokenType::GreaterThanOrEqual
            | TokenType::LessThanOrEqual => Some(9),

            // Equality
            TokenType::Equal | TokenType::NotEqual => Some(8),

            // Bitwise
            TokenType::BitwiseAnd => Some(7),
            TokenType::BitwiseXor => Some(6),
            TokenType::BitwiseOr => Some(5),

            // Logical
            TokenType::LogicalAnd => Some(4),
            TokenType::LogicalOr => Some(3),
            // Ternary conditional operator
            TokenType::QuestionMark => Some(2),

            // Assignment and compound assignment have the lowest precedence
            TokenType::Assignment
            | TokenType::AddAssignment
            | TokenType::SubtractAssignment
            | TokenType::MultiplyAssignment
            | TokenType::DivideAssignment
            | TokenType::ModulusAssignment
            | TokenType::BitwiseAndAssignment
            | TokenType::BitwiseOrAssignment
            | TokenType::BitwiseXorAssignment
            | TokenType::ShiftLeftAssignment
            | TokenType::ShiftRightAssignment => Some(1),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub column: usize,
}

impl Token {
    pub fn new(token_type: TokenType, line: usize, column: usize) -> Self {
        Token {
            token_type,
            line,
            column,
        }
    }
}

#[derive(Debug, Clone)]
pub enum LexError {
    InvalidCharacter {
        ch: char,
        line: usize,
        column: usize,
    },
    InvalidIDentifier {
        ch: char,
        line: usize,
        column: usize,
    },
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexError::InvalidCharacter { ch, line, column } => {
                write!(
                    f,
                    "Invalid character '{}' at line {}, column {}",
                    ch, line, column
                )
            }
            LexError::InvalidIDentifier { ch, line, column } => {
                write!(
                    f,
                    "Invalid identifier '{}' at line {}, column {}",
                    ch, line, column
                )
            }
        }
    }
}

struct Lexer<'a> {
    column: usize,
    row: usize,
    input: std::iter::Peekable<std::str::Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            column: 0,
            row: 1, // Most editors start line numbering at 1, unfortunately
            input: input.chars().peekable(),
        }
    }

    pub fn next(&mut self) -> Option<char> {
        if let Some(ch) = self.input.next() {
            self.column += 1;
            if ch == '\n' {
                self.row += 1;
                self.column = 0; // Reset column on new line
            }
            Some(ch)
        } else {
            None
        }
    }
    pub fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }
}

pub fn tokenize(input: &str) -> (Vec<Token>, Vec<LexError>) {
    let mut tokens: Vec<Token> = Vec::new();
    let mut errors: Vec<LexError> = Vec::new();
    let mut lexer = Lexer::new(input);

    while let Some(c) = lexer.next() {
        match c {
            'a'..='z' | 'A'..='Z' => {
                // Identifiers and keywords
                let mut identifier = String::new();
                identifier.push(c);
                while let Some(&next) = lexer.peek() {
                    if next.is_alphanumeric() || next == '_' {
                        identifier.push(lexer.next().unwrap());
                    } else {
                        break;
                    }
                }

                let start_column = lexer.column - identifier.len();
                // Check if the identifier is a keyword or a normal identifier
                match identifier.as_str() {
                    "void" => {
                        tokens.push(Token::new(TokenType::VoidKeyword, lexer.row, start_column))
                    }
                    "int" => {
                        tokens.push(Token::new(TokenType::IntKeyword, lexer.row, start_column))
                    }
                    "return" => tokens.push(Token::new(
                        TokenType::ReturnKeyword,
                        lexer.row,
                        start_column,
                    )),
                    "if" => tokens.push(Token::new(TokenType::IfKeyword, lexer.row, start_column)),
                    "else" => {
                        tokens.push(Token::new(TokenType::ElseKeyword, lexer.row, start_column))
                    }
                    "do" => tokens.push(Token::new(TokenType::DoKeyword, lexer.row, start_column)),
                    "while" => {
                        tokens.push(Token::new(TokenType::WhileKeyword, lexer.row, start_column))
                    }
                    "for" => {
                        tokens.push(Token::new(TokenType::ForKeyword, lexer.row, start_column))
                    }
                    "break" => {
                        tokens.push(Token::new(TokenType::BreakKeyword, lexer.row, start_column))
                    }
                    "continue" => tokens.push(Token::new(
                        TokenType::ContinueKeyword,
                        lexer.row,
                        start_column,
                    )),
                    _ => tokens.push(Token::new(
                        TokenType::Identifier(identifier),
                        lexer.row,
                        start_column,
                    )),
                }
            }
            '0'..='9' => {
                // Integer literals
                let mut number = String::new();
                let mut error = false;
                number.push(c);
                while let Some(&next) = lexer.peek() {
                    if next.is_digit(10) {
                        number.push(lexer.next().unwrap());
                    } else {
                        if next.is_alphabetic() {
                            lexer.next(); // Consume the invalid character
                            errors.push(LexError::InvalidIDentifier {
                                ch: next,
                                line: lexer.row,
                                column: lexer.column + 1, // +1 because we haven't consumed it yet
                            });
                            error = true;
                        }
                        break;
                    }
                }
                if !error {
                    let value = number.parse::<i64>().unwrap();
                    let start_column = lexer.column - number.len();
                    tokens.push(Token::new(
                        TokenType::IntLiteral(value),
                        lexer.row,
                        start_column,
                    ));
                }
            }
            // Assignment and equality operator
            '=' => {
                match lexer.peek() {
                    Some(&'=') => {
                        lexer.next(); // Consume the second '='
                        tokens.push(Token::new(TokenType::Equal, lexer.row, lexer.column - 1));
                    }
                    _ => {
                        tokens.push(Token::new(TokenType::Assignment, lexer.row, lexer.column));
                    }
                }
            }
            // Unary operators
            '~' => tokens.push(Token::new(
                TokenType::BitwiseComplement,
                lexer.row,
                lexer.column,
            )),
            '-' => {
                match lexer.peek() {
                    Some(&'-') => {
                        lexer.next(); // Consume the second '-'
                        tokens.push(Token::new(
                            TokenType::Decrement,
                            lexer.row,
                            lexer.column - 1,
                        ));
                    }
                    Some(&'=') => {
                        lexer.next(); // Consume the '='
                        tokens.push(Token::new(
                            TokenType::SubtractAssignment,
                            lexer.row,
                            lexer.column - 1,
                        ));
                    }
                    _ => tokens.push(Token::new(TokenType::Negation, lexer.row, lexer.column)),
                }
            }
            // Binary operators
            '+' => {
                match lexer.peek() {
                    Some(&'+') => {
                        lexer.next(); // Consume the second '+'
                        tokens.push(Token::new(
                            TokenType::Increment,
                            lexer.row,
                            lexer.column - 1,
                        ));
                    }
                    Some(&'=') => {
                        lexer.next(); // Consume the '='
                        tokens.push(Token::new(
                            TokenType::AddAssignment,
                            lexer.row,
                            lexer.column - 1,
                        ));
                    }
                    _ => tokens.push(Token::new(TokenType::Add, lexer.row, lexer.column)),
                }
            }
            '*' => {
                match lexer.peek() {
                    Some(&'=') => {
                        lexer.next(); // Consume the '='
                        tokens.push(Token::new(
                            TokenType::MultiplyAssignment,
                            lexer.row,
                            lexer.column - 1,
                        ));
                    }
                    _ => tokens.push(Token::new(TokenType::Multiply, lexer.row, lexer.column)),
                }
            }
            '/' => {
                match lexer.peek() {
                    Some(&'=') => {
                        lexer.next(); // Consume the '='
                        tokens.push(Token::new(
                            TokenType::DivideAssignment,
                            lexer.row,
                            lexer.column - 1,
                        ));
                    }
                    _ => tokens.push(Token::new(TokenType::Divide, lexer.row, lexer.column)),
                }
            }
            '%' => {
                match lexer.peek() {
                    Some(&'=') => {
                        lexer.next(); // Consume the '='
                        tokens.push(Token::new(
                            TokenType::ModulusAssignment,
                            lexer.row,
                            lexer.column - 1,
                        ));
                    }
                    _ => tokens.push(Token::new(TokenType::Modulus, lexer.row, lexer.column)),
                }
            }
            // Bitwise operators
            '^' => {
                match lexer.peek() {
                    Some(&'=') => {
                        lexer.next(); // Consume the '='
                        tokens.push(Token::new(
                            TokenType::BitwiseXorAssignment,
                            lexer.row,
                            lexer.column - 1,
                        ));
                    }
                    _ => tokens.push(Token::new(TokenType::BitwiseXor, lexer.row, lexer.column)),
                }
            }
            // Bitwise/Logical operators
            '&' => {
                match lexer.peek() {
                    Some(&'&') => {
                        lexer.next(); // Consume the second '&'
                        tokens.push(Token::new(
                            TokenType::LogicalAnd,
                            lexer.row,
                            lexer.column - 1,
                        ));
                    }
                    Some(&'=') => {
                        lexer.next(); // Consume the '='
                        tokens.push(Token::new(
                            TokenType::BitwiseAndAssignment,
                            lexer.row,
                            lexer.column - 1,
                        ));
                    }
                    _ => tokens.push(Token::new(TokenType::BitwiseAnd, lexer.row, lexer.column)),
                }
            }
            '|' => {
                match lexer.peek() {
                    Some(&'|') => {
                        lexer.next(); // Consume the second '|'
                        tokens.push(Token::new(
                            TokenType::LogicalOr,
                            lexer.row,
                            lexer.column - 1,
                        ));
                    }
                    Some(&'=') => {
                        lexer.next(); // Consume the '='
                        tokens.push(Token::new(
                            TokenType::BitwiseOrAssignment,
                            lexer.row,
                            lexer.column - 1,
                        ));
                    }
                    _ => tokens.push(Token::new(TokenType::BitwiseOr, lexer.row, lexer.column)),
                }
            }
            // Shift/Relational operators
            '<' => {
                match lexer.peek() {
                    Some(&'<') => {
                        lexer.next(); // Consume the second '<'
                        // Check for <<=
                        match lexer.peek() {
                            Some(&'=') => {
                                lexer.next(); // Consume the '='
                                tokens.push(Token::new(
                                    TokenType::ShiftLeftAssignment,
                                    lexer.row,
                                    lexer.column - 2,
                                ));
                            }
                            _ => {
                                tokens.push(Token::new(
                                    TokenType::ShiftLeft,
                                    lexer.row,
                                    lexer.column - 1,
                                ));
                            }
                        }
                    }
                    Some(&'=') => {
                        lexer.next(); // Consume the '='
                        tokens.push(Token::new(
                            TokenType::LessThanOrEqual,
                            lexer.row,
                            lexer.column - 1,
                        ));
                    }
                    _ => tokens.push(Token::new(TokenType::LessThan, lexer.row, lexer.column)),
                }
            }
            '>' => {
                match lexer.peek() {
                    Some(&'>') => {
                        lexer.next(); // Consume the second '>'
                        // Check for >>=
                        match lexer.peek() {
                            Some(&'=') => {
                                lexer.next(); // Consume the '='
                                tokens.push(Token::new(
                                    TokenType::ShiftRightAssignment,
                                    lexer.row,
                                    lexer.column - 2,
                                ));
                            }
                            _ => {
                                tokens.push(Token::new(
                                    TokenType::ShiftRight,
                                    lexer.row,
                                    lexer.column - 1,
                                ));
                            }
                        }
                    }
                    Some(&'=') => {
                        lexer.next(); // Consume the '='
                        tokens.push(Token::new(
                            TokenType::GreaterThanOrEqual,
                            lexer.row,
                            lexer.column - 1,
                        ));
                    }
                    _ => tokens.push(Token::new(TokenType::GreaterThan, lexer.row, lexer.column)),
                }
            }
            '!' => {
                match lexer.peek() {
                    Some(&'=') => {
                        lexer.next(); // Consume the '='
                        tokens.push(Token::new(TokenType::NotEqual, lexer.row, lexer.column - 1));
                    }
                    _ => tokens.push(Token::new(TokenType::LogicalNot, lexer.row, lexer.column)),
                }
            }
            // Control flow
            '?' => tokens.push(Token::new(TokenType::QuestionMark, lexer.row, lexer.column)),
            ':' => tokens.push(Token::new(TokenType::Colon, lexer.row, lexer.column)),
            // Others
            '(' => tokens.push(Token::new(TokenType::OpenParen, lexer.row, lexer.column)),
            ')' => tokens.push(Token::new(TokenType::CloseParen, lexer.row, lexer.column)),
            '{' => tokens.push(Token::new(TokenType::OpenBrace, lexer.row, lexer.column)),
            '}' => tokens.push(Token::new(TokenType::CloseBrace, lexer.row, lexer.column)),
            ';' => tokens.push(Token::new(TokenType::Semicolon, lexer.row, lexer.column)),
            _ if c.is_whitespace() || c == '\n' => {} // \n is handled in next()
            _ => {
                errors.push(LexError::InvalidCharacter {
                    ch: c,
                    line: lexer.row,
                    column: lexer.column,
                });
            }
        }
    }

    // End of file token
    tokens.push(Token::new(TokenType::EndOfFile, lexer.row, lexer.column));

    (tokens, errors)
}
