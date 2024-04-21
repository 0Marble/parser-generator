#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
_End,

  or,
  lp,
  letter,
  plus,
  question,
  cap,
  ls,
  rs,
  rp,
  dash,
  star,
}

impl TokenType {
pub fn from_str(s: &str) -> Self {
match s {
"or" => Self::or,
"lp" => Self::lp,
"letter" => Self::letter,
"plus" => Self::plus,
"question" => Self::question,
"cap" => Self::cap,
"ls" => Self::ls,
"rs" => Self::rs,
"rp" => Self::rp,
"dash" => Self::dash,
"star" => Self::star,
x => panic!("Not a token: {x}"),}
}
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
  Token(TokenType, String),
  Garbage(String)
}

impl Token { pub fn get_type(&self) -> Option<TokenType> { match self { Self::Token(t, _) => Some(t.clone()), _ => None } } pub fn get_text(&self) -> &str { match self { Self::Token(_, s) => s, Self::Garbage(s) => s } } }
pub struct Lexer {
  cur: usize,
  cur_word: String,  start: usize,
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

    1 if (0..=39).contains(&d) ||d == 44 ||(46..=62).contains(&d) ||(64..=90).contains(&d) ||(95..=123).contains(&d) ||(125..=4294967295).contains(&d) ||false => 68,
    1 if d == 92 ||false => 70,
    1 if d == 40 ||false => 71,
    1 if d == 41 ||false => 72,
    1 if d == 42 ||false => 73,
    1 if d == 43 ||false => 74,
    1 if d == 45 ||false => 75,
    1 if d == 63 ||false => 76,
    1 if d == 91 ||false => 77,
    1 if d == 93 ||false => 78,
    1 if d == 94 ||false => 79,
    1 if d == 124 ||false => 80,
    68 if (0..=4294967295).contains(&d) ||false => 3,
    70 if (0..=39).contains(&d) ||d == 44 ||(46..=62).contains(&d) ||(64..=90).contains(&d) ||(95..=123).contains(&d) ||(125..=4294967295).contains(&d) ||false => 3,
    70 if (40..=43).contains(&d) ||d == 45 ||d == 63 ||(91..=94).contains(&d) ||d == 124 ||false => 68,
    71 if (0..=4294967295).contains(&d) ||false => 3,
    72 if (0..=4294967295).contains(&d) ||false => 3,
    73 if (0..=4294967295).contains(&d) ||false => 3,
    74 if (0..=4294967295).contains(&d) ||false => 3,
    75 if (0..=4294967295).contains(&d) ||false => 3,
    76 if (0..=4294967295).contains(&d) ||false => 3,
    77 if (0..=4294967295).contains(&d) ||false => 3,
    78 if (0..=4294967295).contains(&d) ||false => 3,
    79 if (0..=4294967295).contains(&d) ||false => 3,
    80 if (0..=4294967295).contains(&d) ||false => 3,
    3 if (0..=4294967295).contains(&d) ||false => 3,
    x => panic!("No way to continue: {x}")
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
    5 => Some(TokenType::star),
    28 => Some(TokenType::or),
    20 => Some(TokenType::question),
    23 => Some(TokenType::lp),
    77 => Some(TokenType::ls),
    21 => Some(TokenType::or),
    42 => Some(TokenType::plus),
    48 => Some(TokenType::lp),
    8 => Some(TokenType::plus),
    47 => Some(TokenType::rs),
    11 => Some(TokenType::question),
    27 => Some(TokenType::cap),
    71 => Some(TokenType::lp),
    29 => Some(TokenType::rp),
    67 => Some(TokenType::letter),
    45 => Some(TokenType::or),
    58 => Some(TokenType::lp),
    24 => Some(TokenType::star),
    80 => Some(TokenType::or),
    36 => Some(TokenType::or),
    55 => Some(TokenType::or),
    52 => Some(TokenType::question),
    16 => Some(TokenType::cap),
    39 => Some(TokenType::lp),
    18 => Some(TokenType::star),
    43 => Some(TokenType::question),
    73 => Some(TokenType::star),
    15 => Some(TokenType::or),
    49 => Some(TokenType::rp),
    32 => Some(TokenType::star),
    13 => Some(TokenType::star),
    40 => Some(TokenType::rp),
    37 => Some(TokenType::ls),
    7 => Some(TokenType::plus),
    25 => Some(TokenType::plus),
    50 => Some(TokenType::star),
    79 => Some(TokenType::cap),
    78 => Some(TokenType::rs),
    19 => Some(TokenType::plus),
    6 => Some(TokenType::or),
    54 => Some(TokenType::cap),
    41 => Some(TokenType::star),
    65 => Some(TokenType::cap),
    34 => Some(TokenType::question),
    66 => Some(TokenType::or),
    26 => Some(TokenType::question),
    76 => Some(TokenType::question),
    53 => Some(TokenType::ls),
    72 => Some(TokenType::rp),
    63 => Some(TokenType::ls),
    74 => Some(TokenType::plus),
    22 => Some(TokenType::lp),
    12 => Some(TokenType::question),
    60 => Some(TokenType::star),
    46 => Some(TokenType::rs),
    9 => Some(TokenType::star),
    2 => Some(TokenType::or),
    62 => Some(TokenType::question),
    64 => Some(TokenType::rs),
    57 => Some(TokenType::dash),
    51 => Some(TokenType::plus),
    75 => Some(TokenType::dash),
    68 => Some(TokenType::letter),
    10 => Some(TokenType::or),
    4 => Some(TokenType::star),
    14 => Some(TokenType::plus),
    33 => Some(TokenType::plus),
    17 => Some(TokenType::cap),
    31 => Some(TokenType::lp),
    59 => Some(TokenType::rp),
    56 => Some(TokenType::dash),
    44 => Some(TokenType::cap),
    38 => Some(TokenType::ls),
    61 => Some(TokenType::plus),
    30 => Some(TokenType::rp),
    35 => Some(TokenType::cap),
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

