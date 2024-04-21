#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    _End,
    Or,
    Lp,
    Letter,
    Plus,
    Question,
    Cap,
    Ls,
    Rs,
    Rp,
    Dash,
    Star,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    Token(TokenType, String),
    Garbage(String),
}

impl Token {
    pub fn get_type(&self) -> Option<TokenType> {
        match self {
            Self::Token(t, _) => Some(t.clone()),
            _ => None,
        }
    }
}
pub struct Lexer {
    cur: usize,
    cur_word: String,
    start: usize,
}
impl Default for Lexer {
    fn default() -> Self {
        Self::new()
    }
}

impl Lexer {
    // Tries to move from node on letter c, panics if no way to move
    fn step(&self, node: usize, c: char) -> usize {
        let d = c as u32;
        match node {
            1 if (0..=39).contains(&d)
                || d == 44
                || (46..=62).contains(&d)
                || (64..=90).contains(&d)
                || (95..=123).contains(&d)
                || (125..=4294967295).contains(&d)
                || false =>
            {
                68
            }
            1 if d == 92 || false => 70,
            1 if d == 40 || false => 71,
            1 if d == 41 || false => 72,
            1 if d == 42 || false => 73,
            1 if d == 43 || false => 74,
            1 if d == 45 || false => 75,
            1 if d == 63 || false => 76,
            1 if d == 91 || false => 77,
            1 if d == 93 || false => 78,
            1 if d == 94 || false => 79,
            1 if d == 124 || false => 80,
            68 if (0..=4294967295).contains(&d) || false => 3,
            70 if (0..=39).contains(&d)
                || d == 44
                || (46..=62).contains(&d)
                || (64..=90).contains(&d)
                || (95..=123).contains(&d)
                || (125..=4294967295).contains(&d)
                || false =>
            {
                3
            }
            70 if (40..=43).contains(&d)
                || d == 45
                || d == 63
                || (91..=94).contains(&d)
                || d == 124
                || false =>
            {
                68
            }
            71 if (0..=4294967295).contains(&d) || false => 3,
            72 if (0..=4294967295).contains(&d) || false => 3,
            73 if (0..=4294967295).contains(&d) || false => 3,
            74 if (0..=4294967295).contains(&d) || false => 3,
            75 if (0..=4294967295).contains(&d) || false => 3,
            76 if (0..=4294967295).contains(&d) || false => 3,
            77 if (0..=4294967295).contains(&d) || false => 3,
            78 if (0..=4294967295).contains(&d) || false => 3,
            79 if (0..=4294967295).contains(&d) || false => 3,
            80 if (0..=4294967295).contains(&d) || false => 3,
            3 if (0..=4294967295).contains(&d) || false => 3,
            x => panic!("No way to continue: {x}"),
        }
    }

    // Checks if a node is a dead state
    fn is_dead_state(&self, node: usize) -> bool {
        match node {
            3 => true,
            _ => false,
        }
    }

    // Checks if a node is an end node and returns the associated token type if so
    fn is_end(&self, node: usize) -> Option<TokenType> {
        match node {
            5 => Some(TokenType::Star),
            28 => Some(TokenType::Or),
            20 => Some(TokenType::Question),
            23 => Some(TokenType::Lp),
            77 => Some(TokenType::Ls),
            21 => Some(TokenType::Or),
            42 => Some(TokenType::Plus),
            48 => Some(TokenType::Lp),
            8 => Some(TokenType::Plus),
            47 => Some(TokenType::Rs),
            11 => Some(TokenType::Question),
            27 => Some(TokenType::Cap),
            71 => Some(TokenType::Lp),
            29 => Some(TokenType::Rp),
            67 => Some(TokenType::Letter),
            45 => Some(TokenType::Or),
            58 => Some(TokenType::Lp),
            24 => Some(TokenType::Star),
            80 => Some(TokenType::Or),
            36 => Some(TokenType::Or),
            55 => Some(TokenType::Or),
            52 => Some(TokenType::Question),
            16 => Some(TokenType::Cap),
            39 => Some(TokenType::Lp),
            18 => Some(TokenType::Star),
            43 => Some(TokenType::Question),
            73 => Some(TokenType::Star),
            15 => Some(TokenType::Or),
            49 => Some(TokenType::Rp),
            32 => Some(TokenType::Star),
            13 => Some(TokenType::Star),
            40 => Some(TokenType::Rp),
            37 => Some(TokenType::Ls),
            7 => Some(TokenType::Plus),
            25 => Some(TokenType::Plus),
            50 => Some(TokenType::Star),
            79 => Some(TokenType::Cap),
            78 => Some(TokenType::Rs),
            19 => Some(TokenType::Plus),
            6 => Some(TokenType::Or),
            54 => Some(TokenType::Cap),
            41 => Some(TokenType::Star),
            65 => Some(TokenType::Cap),
            34 => Some(TokenType::Question),
            66 => Some(TokenType::Or),
            26 => Some(TokenType::Question),
            76 => Some(TokenType::Question),
            53 => Some(TokenType::Ls),
            72 => Some(TokenType::Rp),
            63 => Some(TokenType::Ls),
            74 => Some(TokenType::Plus),
            22 => Some(TokenType::Lp),
            12 => Some(TokenType::Question),
            60 => Some(TokenType::Star),
            46 => Some(TokenType::Rs),
            9 => Some(TokenType::Star),
            2 => Some(TokenType::Or),
            62 => Some(TokenType::Question),
            64 => Some(TokenType::Rs),
            57 => Some(TokenType::Dash),
            51 => Some(TokenType::Plus),
            75 => Some(TokenType::Dash),
            68 => Some(TokenType::Letter),
            10 => Some(TokenType::Or),
            4 => Some(TokenType::Star),
            14 => Some(TokenType::Plus),
            33 => Some(TokenType::Plus),
            17 => Some(TokenType::Cap),
            31 => Some(TokenType::Lp),
            59 => Some(TokenType::Rp),
            56 => Some(TokenType::Dash),
            44 => Some(TokenType::Cap),
            38 => Some(TokenType::Ls),
            61 => Some(TokenType::Plus),
            30 => Some(TokenType::Rp),
            35 => Some(TokenType::Cap),
            _ => None,
        }
    }
    // Eats a character c and possibly returns the next token
    pub fn eat_char(&mut self, c: char) -> Option<Token> {
        let mut res = None;
        let next = self.step(self.cur, c);

        if self.is_dead_state(next) {
            let restart = self.step(self.start, c);
            if let Some(tok) = self.is_end(self.cur) {
                res = Some(Token::Token(tok, std::mem::take(&mut self.cur_word)));
            } else if !self.is_dead_state(restart) {
                res = Some(Token::Garbage(std::mem::take(&mut self.cur_word)));
            }
            self.cur = restart;
        } else {
            self.cur = next;
        }
        self.cur_word.push(c);

        res
    }
    // When the text ends, returns the last token
    pub fn finalize(&mut self) -> Token {
        if let Some(tok) = self.is_end(self.cur) {
            Token::Token(tok, std::mem::take(&mut self.cur_word))
        } else {
            Token::Garbage(std::mem::take(&mut self.cur_word))
        }
    }
    // Create a new Lexer
    pub fn new() -> Self {
        Self {
            cur: 1,
            cur_word: String::new(),
            start: 1,
        }
    }
} // impl Lexer
