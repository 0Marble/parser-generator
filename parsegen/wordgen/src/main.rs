use std::str::FromStr;

use parsegen::parser::{grammar::Grammar, lgraph::Lgraph};

fn expr_grammar_ll1() -> Grammar {
    let g = "
S -> E;
E -> T Ea;
Ea -> add T Ea;
Ea -> ;
T -> F Ta;
Ta -> mul F Ta;
Ta -> ;
F -> lp E rp;
F -> id;";
    Grammar::from_str(g).unwrap()
}

fn expr_grammar() -> Grammar {
    Grammar::from_str(" E -> T add E; E -> T; T -> F mul T; T -> F; F -> id; F -> lp E rp;")
        .unwrap()
}

fn main() {
    let grammar = expr_grammar();
    let lg = Lgraph::slr(&grammar);

    for (_, tree) in lg.possible_words(&grammar).take(5) {
        println!("{tree}")
    }
}
