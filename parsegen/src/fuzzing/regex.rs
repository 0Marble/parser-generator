use crate::lexer::regex::Regex;

use super::Rng;

impl Regex {
    pub fn fuzz(&self, rng: &mut dyn Rng) -> String {
        match self {
            Regex::Empty => String::new(),
            Regex::Base(c) => c.to_string(),
            Regex::Concat(regs) => regs
                .iter()
                .map(|r| r.fuzz(rng))
                .reduce(|a, b| a + &b)
                .unwrap_or_default(),
            Regex::Variant(regs) => {
                let pick = rng.gen() % regs.len();
                regs[pick].fuzz(rng)
            }
            Regex::Star(r) => {
                let count = rng.gen() % 50;
                (0..count)
                    .map(|_| r.fuzz(rng))
                    .reduce(|a, b| a + &b)
                    .unwrap_or_default()
            }
            Regex::Range(a, b) => {
                let len = *b as u32 - *a as u32 + 1;
                let mut pick = *a as usize + rng.gen() % len as usize;
                loop {
                    assert!(pick >= *a as _);
                    if let Some(c) = char::from_u32(pick as u32) {
                        return c.to_string();
                    }
                    pick -= 1;
                }
            }
            Regex::Option(r) => {
                if rng.gen() % 2 == 1 {
                    r.fuzz(rng)
                } else {
                    String::new()
                }
            }
            Regex::NotChar(chars) => 'OUTER: loop {
                let mut c = rng.gen() as u32;
                loop {
                    if c == 0 {
                        continue 'OUTER;
                    }
                    c -= 1;
                    if let Some(c) = char::from_u32(c as u32) {
                        if chars.contains(&c) {
                            continue;
                        }
                        return c.to_string();
                    }
                }
            },
            Regex::NotRange(a, b) => 'OUTER: loop {
                let mut c = rng.gen() as u32;
                loop {
                    if c == 0 {
                        continue 'OUTER;
                    }
                    c -= 1;
                    if let Some(c) = char::from_u32(c as u32) {
                        if (*a as u32..=*b as u32).contains(&(c as u32)) {
                            continue;
                        }
                        return c.to_string();
                    }
                }
            },
        }
    }
}
