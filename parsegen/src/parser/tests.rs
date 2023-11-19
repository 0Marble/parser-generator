use std::{io::Cursor, io::Write, string::FromUtf8Error};

use crate::tokenizer::Token;

use super::{
    grammar::Grammar,
    lgraph::{Lgraph, Stack},
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum TraverseError {
    FromUtf8(FromUtf8Error),
    ConflictOn(usize, Option<Token>, Option<usize>),
    NoWayToContinue(usize, Option<Token>, Option<usize>),
    NotAnEndState(usize),
    StackNotEmptied(Stack),
}

impl From<FromUtf8Error> for TraverseError {
    fn from(v: FromUtf8Error) -> Self {
        Self::FromUtf8(v)
    }
}

fn traverse<T: ToString>(
    g: &Lgraph,
    grammar: &Grammar,
    toks: impl IntoIterator<Item = T>,
) -> Result<String, TraverseError> {
    let mut res = vec![];
    let mut w = Cursor::new(&mut res);
    let w = &mut w;

    let mut state = g.start_nodes().next().unwrap();
    let mut stack = Stack::new();
    for tok in toks
        .into_iter()
        .map(Token::new)
        .map(Some)
        .chain(std::iter::once(None))
    {
        let mut consumed = false;

        while !consumed {
            let mut next = None;
            let top = stack.top();
            let mut bracket = None;
            for (_, letter, to) in g.edges_from(state) {
                if letter.tok().is_some() {
                    if letter.tok() != tok {
                        continue;
                    }
                    consumed = true;
                } else if letter.look_ahead().all(|t| t.as_ref() != tok.as_ref()) {
                    continue;
                }
                if let Some(b) = letter.bracket() {
                    if !b.is_open() && Some(b.index()) != top {
                        continue;
                    }
                }

                if next.is_some() || bracket.is_some() {
                    return Err(TraverseError::ConflictOn(state, tok, top));
                }
                next = Some(to);
                bracket = letter.bracket();
            }
            if let Some(next) = next {
                state = next;
            } else {
                return Err(TraverseError::NoWayToContinue(state, tok, top));
            }
            if consumed {
                write!(w, "{}, ", tok.as_ref().unwrap()).unwrap();
            }
            if let Some(b) = bracket {
                let (next_stack, ok) = stack.try_accept(b);
                stack = next_stack;
                assert!(ok);
                if !b.is_open() {
                    if let Some((j, nt)) = grammar
                        .non_terminals()
                        .enumerate()
                        .find(|(j, _)| b.index() == *j)
                    {
                        write!(w, "{nt}[{j}], ").unwrap();
                    }
                }
            }
        }
    }

    if !g.is_end_node(state) {
        return Err(TraverseError::NotAnEndState(state));
    }
    if stack.top().is_some() {
        return Err(TraverseError::StackNotEmptied(stack));
    }

    Ok(String::from_utf8(res)?)
}
