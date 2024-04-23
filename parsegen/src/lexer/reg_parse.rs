use crate::lexer::reg_lexer::TokenType;
use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Symbol {
    Cap,
    Dash,
    Letter,
    Lp,
    Ls,
    Or,
    Plus,
    Question,
    Rp,
    Rs,
    Star,
    ConcatEnd(usize),
    ElemEnd(usize),
    LetterListEnd(usize),
    VariantEnd(usize),
}

pub struct Parser {
    cur: usize,
    stack: Vec<usize>,
    buf: VecDeque<TokenType>,
}
impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

impl Parser {
    // Checks the buffer contents, return 0 in case of a match, 1 if buf is a prefix of toks, and 2 otherwise
    fn check_buf(&self, toks: &[TokenType]) -> u8 {
        for (i, tok) in toks.iter().cloned().enumerate() {
            if self.buf.len() <= i {
                return 1;
            }
            if self.buf[i] != tok {
                return 2;
            }
        }
        0
    }
    // Checks if the parser has reached a valid end state
    pub fn is_end(&self) -> bool {
        if !self.stack.is_empty() || !self.buf.is_empty() {
            return false;
        }
        match self.cur {
            855 => true,
            _ => false,
        }
    }

    // Traverses the Lgraph while it can, before stopping at a point where eating a token is required
    fn traverse(&mut self) -> Vec<Symbol> {
        let mut res = vec![];
        loop {
            let mut had_pref = false;
            match self.cur {
                30 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(0);
                        self.cur = 0;
                        continue;
                    }
                }
                0 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        res.push(Symbol::Letter);
                        self.cur = 4;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Ls]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(5);
                        self.buf.pop_front();
                        res.push(Symbol::Ls);
                        self.cur = 5;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Lp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(6);
                        self.buf.pop_front();
                        res.push(Symbol::Lp);
                        self.cur = 6;
                        continue;
                    }
                }
                4 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 142;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Ls]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 144;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Lp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 143;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Or]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 145;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Star]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 150;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Plus]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 146;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Question]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 147;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Rp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 148;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::_End]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 151;
                        continue;
                    }
                }
                5 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(9);
                        self.buf.pop_front();
                        res.push(Symbol::Letter);
                        self.cur = 9;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Cap]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(10);
                        self.buf.pop_front();
                        res.push(Symbol::Cap);
                        self.cur = 10;
                        continue;
                    }
                }
                6 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        res.push(Symbol::Letter);
                        self.cur = 4;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Ls]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(5);
                        self.buf.pop_front();
                        res.push(Symbol::Ls);
                        self.cur = 5;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Lp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(6);
                        self.buf.pop_front();
                        res.push(Symbol::Lp);
                        self.cur = 6;
                        continue;
                    }
                }
                142 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(4));
                        self.cur = 814;
                        continue;
                    }
                }
                144 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(4));
                        self.cur = 815;
                        continue;
                    }
                }
                143 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(4));
                        self.cur = 816;
                        continue;
                    }
                }
                145 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(4));
                        self.cur = 749;
                        continue;
                    }
                }
                150 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(4));
                        self.cur = 817;
                        continue;
                    }
                }
                146 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(4));
                        self.cur = 818;
                        continue;
                    }
                }
                147 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(4));
                        self.cur = 819;
                        continue;
                    }
                }
                148 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(4));
                        self.cur = 750;
                        continue;
                    }
                }
                151 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(4));
                        self.cur = 751;
                        continue;
                    }
                }
                9 => {
                    let r = self.check_buf(&[TokenType::Dash]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        res.push(Symbol::Dash);
                        self.cur = 21;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(18);
                        self.buf.pop_front();
                        res.push(Symbol::Letter);
                        self.cur = 18;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Rs]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 269;
                        continue;
                    }
                }
                10 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(13);
                        self.buf.pop_front();
                        res.push(Symbol::Letter);
                        self.cur = 13;
                        continue;
                    }
                }
                814 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(3);
                        self.cur = 629;
                        continue;
                    }
                }
                815 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(3);
                        self.cur = 630;
                        continue;
                    }
                }
                816 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(3);
                        self.cur = 631;
                        continue;
                    }
                }
                749 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ConcatEnd(3));
                        self.cur = 37;
                        continue;
                    }
                }
                817 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::Star);
                        self.cur = 25;
                        continue;
                    }
                }
                818 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::Plus);
                        self.cur = 26;
                        continue;
                    }
                }
                819 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::Question);
                        self.cur = 27;
                        continue;
                    }
                }
                750 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ConcatEnd(3));
                        self.cur = 40;
                        continue;
                    }
                }
                751 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ConcatEnd(3));
                        self.cur = 43;
                        continue;
                    }
                }
                21 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        res.push(Symbol::Letter);
                        self.cur = 22;
                        continue;
                    }
                }
                18 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(18);
                        self.buf.pop_front();
                        res.push(Symbol::Letter);
                        self.cur = 18;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Rs]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 269;
                        continue;
                    }
                }
                269 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::LetterListEnd(14));
                            self.cur = 65;
                            continue;
                        }
                    }
                    _ => {}
                },
                13 => {
                    let r = self.check_buf(&[TokenType::Dash]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        res.push(Symbol::Dash);
                        self.cur = 16;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(18);
                        self.buf.pop_front();
                        res.push(Symbol::Letter);
                        self.cur = 18;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Rs]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 269;
                        continue;
                    }
                }
                629 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::Letter);
                        self.cur = 4;
                        continue;
                    }
                }
                630 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(5);
                        res.push(Symbol::Ls);
                        self.cur = 5;
                        continue;
                    }
                }
                631 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(6);
                        res.push(Symbol::Lp);
                        self.cur = 6;
                        continue;
                    }
                }
                37 => match self.stack.last() {
                    Some(0) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 465;
                            continue;
                        }
                    }
                    Some(6) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 499;
                            continue;
                        }
                    }
                    Some(3) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 820;
                            continue;
                        }
                    }
                    Some(28) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 576;
                            continue;
                        }
                    }
                    _ => {}
                },
                25 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 695;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Ls]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 696;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Lp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 697;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Or]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 698;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Star]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 699;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Plus]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 700;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Question]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 701;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Rp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 702;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::_End]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 703;
                        continue;
                    }
                }
                26 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 704;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Ls]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 705;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Lp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 706;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Or]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 707;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Star]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 708;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Plus]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 709;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Question]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 710;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Rp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 711;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::_End]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 712;
                        continue;
                    }
                }
                27 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 713;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Ls]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 714;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Lp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 715;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Or]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 716;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Star]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 717;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Plus]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 718;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Question]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 719;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Rp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 720;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::_End]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 721;
                        continue;
                    }
                }
                40 => match self.stack.last() {
                    Some(0) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 468;
                            continue;
                        }
                    }
                    Some(6) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 502;
                            continue;
                        }
                    }
                    Some(3) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 821;
                            continue;
                        }
                    }
                    Some(28) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 579;
                            continue;
                        }
                    }
                    _ => {}
                },
                43 => match self.stack.last() {
                    Some(0) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 470;
                            continue;
                        }
                    }
                    Some(6) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 504;
                            continue;
                        }
                    }
                    Some(3) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 822;
                            continue;
                        }
                    }
                    Some(28) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 581;
                            continue;
                        }
                    }
                    _ => {}
                },
                22 => {
                    let r = self.check_buf(&[TokenType::Rs]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        res.push(Symbol::Rs);
                        self.cur = 23;
                        continue;
                    }
                }
                65 => match self.stack.last() {
                    Some(5) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 823;
                            continue;
                        }
                    }
                    Some(10) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 824;
                            continue;
                        }
                    }
                    Some(13) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 858;
                            continue;
                        }
                    }
                    Some(18) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 858;
                            continue;
                        }
                    }
                    Some(9) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 858;
                            continue;
                        }
                    }
                    _ => {}
                },
                16 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        res.push(Symbol::Letter);
                        self.cur = 19;
                        continue;
                    }
                }
                465 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(0);
                        self.cur = 747;
                        continue;
                    }
                }
                499 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(6);
                        self.cur = 747;
                        continue;
                    }
                }
                820 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ConcatEnd(2));
                        self.cur = 37;
                        continue;
                    }
                }
                576 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(28);
                        self.cur = 747;
                        continue;
                    }
                }
                695 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(10));
                        self.cur = 814;
                        continue;
                    }
                }
                696 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(10));
                        self.cur = 815;
                        continue;
                    }
                }
                697 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(10));
                        self.cur = 816;
                        continue;
                    }
                }
                698 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(10));
                        self.cur = 749;
                        continue;
                    }
                }
                699 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(10));
                        self.cur = 817;
                        continue;
                    }
                }
                700 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(10));
                        self.cur = 818;
                        continue;
                    }
                }
                701 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(10));
                        self.cur = 819;
                        continue;
                    }
                }
                702 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(10));
                        self.cur = 750;
                        continue;
                    }
                }
                703 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(10));
                        self.cur = 751;
                        continue;
                    }
                }
                704 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(11));
                        self.cur = 814;
                        continue;
                    }
                }
                705 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(11));
                        self.cur = 815;
                        continue;
                    }
                }
                706 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(11));
                        self.cur = 816;
                        continue;
                    }
                }
                707 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(11));
                        self.cur = 749;
                        continue;
                    }
                }
                708 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(11));
                        self.cur = 817;
                        continue;
                    }
                }
                709 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(11));
                        self.cur = 818;
                        continue;
                    }
                }
                710 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(11));
                        self.cur = 819;
                        continue;
                    }
                }
                711 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(11));
                        self.cur = 750;
                        continue;
                    }
                }
                712 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(11));
                        self.cur = 751;
                        continue;
                    }
                }
                713 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(12));
                        self.cur = 814;
                        continue;
                    }
                }
                714 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(12));
                        self.cur = 815;
                        continue;
                    }
                }
                715 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(12));
                        self.cur = 816;
                        continue;
                    }
                }
                716 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(12));
                        self.cur = 749;
                        continue;
                    }
                }
                717 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(12));
                        self.cur = 817;
                        continue;
                    }
                }
                718 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(12));
                        self.cur = 818;
                        continue;
                    }
                }
                719 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(12));
                        self.cur = 819;
                        continue;
                    }
                }
                720 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(12));
                        self.cur = 750;
                        continue;
                    }
                }
                721 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(12));
                        self.cur = 751;
                        continue;
                    }
                }
                468 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(0);
                        self.cur = 691;
                        continue;
                    }
                }
                502 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(6);
                        self.cur = 691;
                        continue;
                    }
                }
                821 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ConcatEnd(2));
                        self.cur = 40;
                        continue;
                    }
                }
                579 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(28);
                        self.cur = 691;
                        continue;
                    }
                }
                470 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(0);
                        self.cur = 693;
                        continue;
                    }
                }
                504 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(6);
                        self.cur = 693;
                        continue;
                    }
                }
                822 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ConcatEnd(2));
                        self.cur = 43;
                        continue;
                    }
                }
                581 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(28);
                        self.cur = 693;
                        continue;
                    }
                }
                23 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 826;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Ls]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 827;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Lp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 828;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Or]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 829;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Star]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 830;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Plus]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 831;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Question]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 832;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Rp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 833;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::_End]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 834;
                        continue;
                    }
                }
                823 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::Rs);
                        self.cur = 12;
                        continue;
                    }
                }
                824 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::Rs);
                        self.cur = 15;
                        continue;
                    }
                }
                858 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::LetterListEnd(13));
                        self.cur = 65;
                        continue;
                    }
                }
                19 => {
                    let r = self.check_buf(&[TokenType::Rs]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        res.push(Symbol::Rs);
                        self.cur = 20;
                        continue;
                    }
                }
                747 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(2);
                        self.cur = 648;
                        continue;
                    }
                }
                691 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::VariantEnd(1));
                        self.cur = 88;
                        continue;
                    }
                }
                693 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::VariantEnd(1));
                        self.cur = 91;
                        continue;
                    }
                }
                826 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 396;
                            continue;
                        }
                    }
                    _ => {}
                },
                827 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 400;
                            continue;
                        }
                    }
                    _ => {}
                },
                828 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 404;
                            continue;
                        }
                    }
                    _ => {}
                },
                829 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 408;
                            continue;
                        }
                    }
                    _ => {}
                },
                830 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 412;
                            continue;
                        }
                    }
                    _ => {}
                },
                831 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 416;
                            continue;
                        }
                    }
                    _ => {}
                },
                832 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 420;
                            continue;
                        }
                    }
                    _ => {}
                },
                833 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 424;
                            continue;
                        }
                    }
                    _ => {}
                },
                834 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 428;
                            continue;
                        }
                    }
                    _ => {}
                },
                12 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 779;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Ls]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 780;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Lp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 781;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Or]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 782;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Star]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 783;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Plus]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 784;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Question]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 785;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Rp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 786;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::_End]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 787;
                        continue;
                    }
                }
                15 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 844;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Ls]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 845;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Lp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 846;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Or]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 847;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Star]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 848;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Plus]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 849;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Question]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 850;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Rp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 851;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::_End]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 852;
                        continue;
                    }
                }
                20 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 835;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Ls]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 836;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Lp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 837;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Or]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 838;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Star]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 839;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Plus]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 840;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Question]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 841;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Rp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 842;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::_End]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 843;
                        continue;
                    }
                }
                648 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(28);
                        res.push(Symbol::Or);
                        self.cur = 28;
                        continue;
                    }
                }
                88 => match self.stack.last() {
                    Some(6) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 853;
                            continue;
                        }
                    }
                    Some(28) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 854;
                            continue;
                        }
                    }
                    _ => {}
                },
                91 => match self.stack.last() {
                    Some(0) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 855;
                            continue;
                        }
                    }
                    Some(28) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 856;
                            continue;
                        }
                    }
                    _ => {}
                },
                396 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(5));
                            self.cur = 814;
                            continue;
                        }
                    }
                    _ => {}
                },
                400 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(5));
                            self.cur = 815;
                            continue;
                        }
                    }
                    _ => {}
                },
                404 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(5));
                            self.cur = 816;
                            continue;
                        }
                    }
                    _ => {}
                },
                408 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(5));
                            self.cur = 749;
                            continue;
                        }
                    }
                    _ => {}
                },
                412 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(5));
                            self.cur = 817;
                            continue;
                        }
                    }
                    _ => {}
                },
                416 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(5));
                            self.cur = 818;
                            continue;
                        }
                    }
                    _ => {}
                },
                420 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(5));
                            self.cur = 819;
                            continue;
                        }
                    }
                    _ => {}
                },
                424 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(5));
                            self.cur = 750;
                            continue;
                        }
                    }
                    _ => {}
                },
                428 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(5));
                            self.cur = 751;
                            continue;
                        }
                    }
                    _ => {}
                },
                779 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(7));
                        self.cur = 814;
                        continue;
                    }
                }
                780 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(7));
                        self.cur = 815;
                        continue;
                    }
                }
                781 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(7));
                        self.cur = 816;
                        continue;
                    }
                }
                782 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(7));
                        self.cur = 749;
                        continue;
                    }
                }
                783 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(7));
                        self.cur = 817;
                        continue;
                    }
                }
                784 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(7));
                        self.cur = 818;
                        continue;
                    }
                }
                785 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(7));
                        self.cur = 819;
                        continue;
                    }
                }
                786 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(7));
                        self.cur = 750;
                        continue;
                    }
                }
                787 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(7));
                        self.cur = 751;
                        continue;
                    }
                }
                844 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(8));
                            self.cur = 814;
                            continue;
                        }
                    }
                    _ => {}
                },
                845 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(8));
                            self.cur = 815;
                            continue;
                        }
                    }
                    _ => {}
                },
                846 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(8));
                            self.cur = 816;
                            continue;
                        }
                    }
                    _ => {}
                },
                847 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(8));
                            self.cur = 749;
                            continue;
                        }
                    }
                    _ => {}
                },
                848 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(8));
                            self.cur = 817;
                            continue;
                        }
                    }
                    _ => {}
                },
                849 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(8));
                            self.cur = 818;
                            continue;
                        }
                    }
                    _ => {}
                },
                850 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(8));
                            self.cur = 819;
                            continue;
                        }
                    }
                    _ => {}
                },
                851 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(8));
                            self.cur = 750;
                            continue;
                        }
                    }
                    _ => {}
                },
                852 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(8));
                            self.cur = 751;
                            continue;
                        }
                    }
                    _ => {}
                },
                835 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 351;
                            continue;
                        }
                    }
                    _ => {}
                },
                836 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 356;
                            continue;
                        }
                    }
                    _ => {}
                },
                837 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 361;
                            continue;
                        }
                    }
                    _ => {}
                },
                838 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 366;
                            continue;
                        }
                    }
                    _ => {}
                },
                839 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 371;
                            continue;
                        }
                    }
                    _ => {}
                },
                840 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 376;
                            continue;
                        }
                    }
                    _ => {}
                },
                841 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 381;
                            continue;
                        }
                    }
                    _ => {}
                },
                842 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 386;
                            continue;
                        }
                    }
                    _ => {}
                },
                843 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 391;
                            continue;
                        }
                    }
                    _ => {}
                },
                28 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        res.push(Symbol::Letter);
                        self.cur = 4;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Ls]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(5);
                        self.buf.pop_front();
                        res.push(Symbol::Ls);
                        self.cur = 5;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Lp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.stack.push(6);
                        self.buf.pop_front();
                        res.push(Symbol::Lp);
                        self.cur = 6;
                        continue;
                    }
                }
                853 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::Rp);
                        self.cur = 8;
                        continue;
                    }
                }
                854 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::VariantEnd(0));
                            self.cur = 88;
                            continue;
                        }
                    }
                    _ => {}
                },
                855 => {}
                856 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::VariantEnd(0));
                            self.cur = 91;
                            continue;
                        }
                    }
                    _ => {}
                },
                351 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 352;
                            continue;
                        }
                    }
                    _ => {}
                },
                356 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 357;
                            continue;
                        }
                    }
                    _ => {}
                },
                361 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 362;
                            continue;
                        }
                    }
                    _ => {}
                },
                366 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 367;
                            continue;
                        }
                    }
                    _ => {}
                },
                371 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 372;
                            continue;
                        }
                    }
                    _ => {}
                },
                376 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 377;
                            continue;
                        }
                    }
                    _ => {}
                },
                381 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 382;
                            continue;
                        }
                    }
                    _ => {}
                },
                386 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 387;
                            continue;
                        }
                    }
                    _ => {}
                },
                391 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            self.cur = 392;
                            continue;
                        }
                    }
                    _ => {}
                },
                8 => {
                    let r = self.check_buf(&[TokenType::Letter]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 804;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Ls]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 805;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Lp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 806;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Or]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 807;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Star]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 808;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Plus]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 809;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Question]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 810;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::Rp]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 811;
                        continue;
                    }
                    let r = self.check_buf(&[TokenType::_End]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        self.buf.pop_front();
                        self.cur = 812;
                        continue;
                    }
                }
                352 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(6));
                            self.cur = 814;
                            continue;
                        }
                    }
                    _ => {}
                },
                357 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(6));
                            self.cur = 815;
                            continue;
                        }
                    }
                    _ => {}
                },
                362 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(6));
                            self.cur = 816;
                            continue;
                        }
                    }
                    _ => {}
                },
                367 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(6));
                            self.cur = 749;
                            continue;
                        }
                    }
                    _ => {}
                },
                372 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(6));
                            self.cur = 817;
                            continue;
                        }
                    }
                    _ => {}
                },
                377 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(6));
                            self.cur = 818;
                            continue;
                        }
                    }
                    _ => {}
                },
                382 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(6));
                            self.cur = 819;
                            continue;
                        }
                    }
                    _ => {}
                },
                387 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(6));
                            self.cur = 750;
                            continue;
                        }
                    }
                    _ => {}
                },
                392 => match self.stack.last() {
                    Some(_) if self.check_buf(&[]) != 2 => {
                        let r = self.check_buf(&[]);
                        if r == 1 {
                            had_pref = true;
                        } else if r == 0 {
                            self.stack.pop();
                            res.push(Symbol::ElemEnd(6));
                            self.cur = 751;
                            continue;
                        }
                    }
                    _ => {}
                },
                804 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(9));
                        self.cur = 814;
                        continue;
                    }
                }
                805 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(9));
                        self.cur = 815;
                        continue;
                    }
                }
                806 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(9));
                        self.cur = 816;
                        continue;
                    }
                }
                807 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(9));
                        self.cur = 749;
                        continue;
                    }
                }
                808 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(9));
                        self.cur = 817;
                        continue;
                    }
                }
                809 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(9));
                        self.cur = 818;
                        continue;
                    }
                }
                810 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(9));
                        self.cur = 819;
                        continue;
                    }
                }
                811 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(9));
                        self.cur = 750;
                        continue;
                    }
                }
                812 => {
                    let r = self.check_buf(&[]);
                    if r == 1 {
                        had_pref = true;
                    } else if r == 0 {
                        res.push(Symbol::ElemEnd(9));
                        self.cur = 751;
                        continue;
                    }
                }
                _ => {}
            }
            if had_pref || self.is_end() {
                return res;
            } else {
                panic!(
                    "Could not continue from node {}, stack {:?}, buf {:?}",
                    self.cur, self.stack, self.buf
                );
            }
        }
    }
    // Eats a token, produces a vector of corresponding symbols
    pub fn eat_tok(&mut self, tok: TokenType) -> Vec<Symbol> {
        let mut res = self.traverse();
        self.buf.push_back(tok);
        res.append(&mut self.traverse());
        res
    }

    // Create a new Parser
    pub fn new() -> Self {
        Self {
            cur: 30,
            stack: vec![],
            buf: VecDeque::new(),
        }
    }
} // impl Parser
