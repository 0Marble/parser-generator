mod regex;

use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use regex::{Nfa, Regex};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Letter {
    idx: u32,
}

impl Letter {
    fn new(idx: u32) -> Self {
        Self { idx }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Bracket {
    Open(u32),
    Closed(u32),
}

impl Bracket {
    fn is_closing(&self) -> bool {
        match self {
            Bracket::Open(_) => false,
            Bracket::Closed(_) => true,
        }
    }

    fn idx(&self) -> u32 {
        match self {
            Bracket::Open(i) => *i,
            Bracket::Closed(i) => *i,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
struct Node {
    idx: usize,
}

impl Node {
    fn new(idx: usize) -> Self {
        Self { idx }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
struct Edge {
    idx: usize,
    from: Node,
    to: Node,
    letter: Option<Letter>,
    bracket: Option<Bracket>,
}

impl Display for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]{}-", self.idx, self.from.idx)?;
        if let Some(l) = self.letter {
            write!(f, "{}", l.idx)?;
        }
        if let Some(b) = self.bracket {
            match b {
                Bracket::Open(i) => write!(f, "({}", i)?,
                Bracket::Closed(i) => write!(f, "){}", i)?,
            }
        }

        write!(f, "->{}", self.to.idx)
    }
}

impl Edge {
    fn new(
        idx: usize,
        from: Node,
        to: Node,
        letter: Option<Letter>,
        bracket: Option<Bracket>,
    ) -> Self {
        Self {
            idx,
            from,
            to,
            letter,
            bracket,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Lgraph {
    edges: Vec<Edge>,
    node_count: usize,
    start: usize,
    end_nodes: Vec<usize>,
}

impl Lgraph {
    fn nodes(&self) -> impl Iterator<Item = Node> + '_ {
        (0..self.node_count).map(Node::new)
    }
    fn edges(&self) -> impl Iterator<Item = Edge> + '_ {
        self.edges.iter().cloned()
    }
    fn edges_from(&self, node: Node) -> impl Iterator<Item = Edge> + '_ {
        self.edges().filter(move |e| e.from == node)
    }
    fn start(&self) -> Node {
        Node::new(self.start)
    }
    fn end_nodes(&self) -> impl Iterator<Item = Node> + '_ {
        self.end_nodes.iter().cloned().map(Node::new)
    }
    fn is_end_node(&self, node: Node) -> bool {
        self.end_nodes.contains(&node.idx)
    }
    fn nth_edge(&self, n: usize) -> Option<Edge> {
        self.edges().find(|e| e.idx == n)
    }
    fn nth_node(&self, n: usize) -> Option<Node> {
        if self.node_count > n {
            return Some(Node::new(n));
        }
        None
    }
    fn from_bnf(rules: &[(char, Regex)]) -> (Self, Vec<(Node, Node, char)>) {
        // for each rule, create a DFA (use regex rules?),
        // with each edge marked with the letter from rule.
        // Say we have DFAs d1,d2,..dn, associated with rules r1,r2,...rn.
        // Connect the DFAs in the following way:
        //    for all edges in all dfas:
        //      if edge=(p, x, q), add it directly.
        //      if edge=(p, ri, q), add edges (p, -, (, beg(ri)), (end(ri), -, ), q)

        let mut between_edges = vec![];
        let mut edges = vec![];
        let mut node_count = 0;
        let mut skip_count = 0;
        let mut start_node = None;
        let mut end_nodes = vec![];
        let mut starts_and_ends = vec![];

        for (_, reg) in rules {
            let nfa = reg.clone().into_nfa();
            let (state_machine, _) = nfa.into_min_dfa(true);
            let (state_machine, _) = state_machine
                .into_nfa()
                .concat(Nfa::new(0).add_end_node(0))
                .rename();

            if start_node.is_none() {
                start_node = Some(state_machine.start_node().clone() + node_count);
                end_nodes = state_machine.end_nodes().map(|n| n + node_count).collect();
            } else {
            }

            starts_and_ends.push((
                state_machine.start_node() + node_count,
                state_machine
                    .end_nodes()
                    .next()
                    .expect("Always has at least one node")
                    + node_count,
            ));

            for (_, edge) in state_machine.edges().enumerate() {
                let letter = edge.item().clone();

                if let Some(letter) = edge.item() {
                    if letter.is_uppercase() {
                        let (rule_idx, _) = rules
                            .iter()
                            .enumerate()
                            .find(|(_, (name, _))| name == letter)
                            .expect(&format!("No rule '{}' given", letter));
                        between_edges.push((
                            edge.beg() + node_count,
                            edge.end() + node_count,
                            skip_count,
                            rule_idx,
                        ));
                        skip_count += 1;
                        continue;
                    }
                }
                edges.push((edge.beg() + node_count, letter, edge.end() + node_count));
            }

            node_count += state_machine.nodes().count();
        }

        let mut g = Lgraph {
            edges: vec![],
            node_count,
            start: start_node.expect("No rules given"),
            end_nodes,
        };

        for (i, edge) in edges.into_iter().enumerate() {
            let (from, letter, to) = edge;
            g.edges.push(Edge::new(
                i,
                Node::new(from),
                Node::new(to),
                letter.map(|c| Letter::new(c as u32)),
                None,
            ));
        }
        for (_, connection) in between_edges.into_iter().enumerate() {
            let (start, end, bracket_idx, rule) = connection;
            let (from, to) = &starts_and_ends[rule];
            g.edges.push(Edge::new(
                g.edges.len(),
                Node::new(start),
                Node::new(*from),
                None,
                Some(Bracket::Open(bracket_idx)),
            ));

            g.edges.push(Edge::new(
                g.edges.len(),
                Node::new(*to),
                Node::new(end),
                None,
                Some(Bracket::Closed(bracket_idx)),
            ));
        }

        (
            g,
            starts_and_ends
                .into_iter()
                .enumerate()
                .map(|(i, (start, end))| (Node::new(start), Node::new(end), rules[i].0))
                .collect(),
        )
    }
}

impl Display for Lgraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "digraph {{")?;
        writeln!(
            f,
            "node [shape=circle];\n  Q1 [style=invisible, height=0, width=0, fixedsize=true];"
        )?;
        writeln!(f, "Q1->{};", self.start().idx)?;
        for n in self.end_nodes() {
            writeln!(f, "{} [shape=doublecircle];", n.idx)?;
        }

        for e in self.edges() {
            write!(f, "{} -> {} [label=\"", e.from.idx, e.to.idx)?;
            if let Some(letter) = e.letter {
                write!(f, "{}", char::from_u32(letter.idx).unwrap())?;
            }
            if let Some(bracket) = e.bracket {
                match bracket {
                    Bracket::Open(i) => write!(f, "({}", i)?,
                    Bracket::Closed(i) => write!(f, "){}", i)?,
                }
            }
            writeln!(f, "\", idx={}];", e.idx)?;
        }

        writeln!(f, "}}")
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
struct Stack {
    s: Vec<u32>,
}

impl Stack {
    fn new() -> Self {
        Self { s: vec![] }
    }
    fn push(&mut self, b: Bracket) -> bool {
        match b {
            Bracket::Open(idx) => self.s.push(idx),
            Bracket::Closed(idx) => {
                if let Some(cur_idx) = self.s.pop() {
                    if cur_idx != idx {
                        self.s.push(cur_idx);
                        return false;
                    }
                } else {
                    return false;
                }
            }
        }
        true
    }
    fn top(&self) -> Option<u32> {
        self.s.last().cloned()
    }
    fn is_balanced(&self) -> bool {
        self.s.is_empty()
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Parser {
    starts_and_ends: Vec<(Node, Node, char)>,
    end_letter: Letter,
    g: Lgraph,
    edge_direct: HashMap<usize, HashSet<(Letter, Option<u32>)>>,
}

impl Parser {
    fn new(mut g: Lgraph, starts_and_ends: Vec<(Node, Node, char)>) -> Self {
        let next_bracket = g
            .edges()
            .flat_map(|e| e.bracket)
            .map(|b| b.idx())
            .max()
            .unwrap_or_default()
            + 1;
        let next_letter = g
            .edges()
            .flat_map(|e| e.letter)
            .map(|l| l.idx)
            .max()
            .unwrap_or_default()
            + 1;

        g.edges.push(Edge::new(
            g.edges.len(),
            Node::new(g.node_count),
            g.start(),
            None,
            Some(Bracket::Open(next_bracket)),
        ));
        g.start = g.node_count;
        g.node_count += 1;
        let old_end_nodes = std::mem::take(&mut g.end_nodes);
        for end in old_end_nodes {
            g.edges.push(Edge::new(
                g.edges.len(),
                Node::new(end),
                Node::new(g.node_count),
                Some(Letter::new(next_letter)),
                Some(Bracket::Closed(next_bracket)),
            ));
        }
        g.end_nodes = vec![g.node_count];
        g.node_count += 1;

        let mut edge_direct: HashMap<_, HashSet<_>> = HashMap::new();

        for node in g.nodes() {
            let mut local_direct: HashMap<_, HashSet<_>> = HashMap::new();

            for start_edge in g.edges_from(node) {
                let mut next = vec![(start_edge, None, Stack::new(), 0, HashSet::new())];

                while let Some((edge, mut bracket, mut stack, depth, visited)) = next.pop() {
                    if let Some(b) = edge.bracket {
                        if !stack.push(b) && bracket.is_none() {
                            assert!(b.is_closing(), "just in case");
                            bracket = Some(b.idx());
                        }
                    }
                    if let Some(l) = edge.letter {
                        local_direct
                            .entry(start_edge.idx)
                            .or_default()
                            .insert((l, bracket));
                    } else {
                        for next_edge in g.edges_from(edge.to) {
                            let mut visited_clone = visited.clone();
                            if visited_clone.insert(next_edge.idx) {
                                // [TODO] this should actually allow for 1 loop to be taken
                                // for example in a section 1-)1->2; 2->3; 3->1; 1-a->4;
                                // if we are in 2 it will not find the option (a, 1)
                                next.push((
                                    next_edge,
                                    bracket,
                                    stack.clone(),
                                    depth + 1,
                                    visited_clone,
                                ));
                            }
                        }
                    }
                }
            }

            // check for conflicts
            for (e1, dir1) in &local_direct {
                for (e2, dir2) in &local_direct {
                    if e1 == e2 {
                        continue;
                    }

                    for (l1, b1) in dir1 {
                        for (l2, b2) in dir2 {
                            assert!(
                                !(l1 == l2 && (b1 == b2 || b1.is_none() || b2.is_none())),
                                "Conflict for node={:?} between edges {} and {}",
                                node,
                                e1,
                                e2
                            );
                        }
                    }
                }
            }

            edge_direct.extend(local_direct);
        }

        Self {
            g,
            edge_direct,
            end_letter: Letter::new(next_letter),
            starts_and_ends,
        }
    }

    fn print_direct(&self) -> String {
        let mut s = String::new();
        for (edge, direct) in &self.edge_direct {
            s += &format!(
                "{}: {:?}\n",
                self.g.nth_edge(*edge).unwrap(),
                &direct.iter().map(|(l, b)| (l.idx, b)).collect::<Vec<_>>()
            );
        }

        s
    }

    fn parse(&self, s: &[Letter]) -> Vec<String> {
        let mut cur_node = self.g.start();
        let mut res = vec![];
        let mut stack = Stack::new();

        'LETTERS: for letter in s.iter().chain(std::iter::once(&self.end_letter)) {
            'EDGES: loop {
                for e in self.g.edges_from(cur_node) {
                    let direct = self
                        .edge_direct
                        .get(&e.idx)
                        .expect(&format!("No direct computed for {:?}", e));

                    for (l, b) in direct {
                        if l == letter && (stack.top() == *b || b.is_none()) {
                            cur_node = e.to;
                            for node in self.edges_nodes(e) {
                                res.push(node);
                            }

                            if let Some(b) = e.bracket {
                                assert!(
                                    stack.push(b),
                                    "Unbalanced brackets, letter={:?}, path={:?}",
                                    letter,
                                    res
                                );
                            }

                            if let Some(l) = e.letter {
                                assert_eq!(l, *letter);
                                continue 'LETTERS;
                            } else {
                                continue 'EDGES;
                            }
                        }
                    }
                }

                unreachable!(
                    "Can not continue: letter={:?}, path={:?}, node={:?}, stack={:?}",
                    letter, res, cur_node, stack
                );
            }
        }

        assert!(
            self.g.is_end_node(cur_node),
            "Finished in not an end node {:?}, path={:?}, input={:?}",
            cur_node,
            res,
            s
        );
        assert!(stack.is_balanced());
        assert_eq!(
            res.pop(),
            Some(char::from_u32(self.end_letter.idx).unwrap().to_string())
        );
        res
    }

    fn edges_nodes(&self, edge: Edge) -> Vec<String> {
        let term = edge.letter.map(|l| char::from_u32(l.idx).unwrap());

        let mut start_node = None;
        let mut end_node = None;
        for (from, to, rule) in &self.starts_and_ends {
            if to.idx == edge.to.idx {
                end_node = Some(format!("{}]", rule));
            }
            if from.idx == edge.from.idx {
                start_node = Some(format!("{}[", rule));
            }
        }

        let mut res = vec![];
        if let Some(start) = start_node {
            res.push(start);
        }
        if let Some(term) = term {
            res.push(term.to_string());
        }
        if let Some(end) = end_node {
            res.push(end);
        }

        res
    }

    fn write_js(&self, w: &mut dyn std::io::Write) -> std::io::Result<()> {
        writeln!(
            w,
            r#"
// File generated automatically
// Graph: 
/*{}*/ 
// Table:
/*{}*/

class Parser {{
    constructor() {{}}
    static stack_push(stack, bracket_idx, bracket_open) {{
        if (bracket_open) {{ stack.push(bracket_idx); return true; }}
        if (stack.length === 0) {{ return false; }}
        if (stack[stack.length - 1] !== bracket_idx) {{ return false; }}
        stack.pop();
        return true;
    }}
    static parse(str) {{
        var state = {}
        var stack = []
        var path = []
        str+="{}";
        
        LETTERS: for (let c of str) {{
            EDGES: while(true) {{
                var stack_top = stack.slice(-1)[0];
                switch (state) {{    
        "#,
            self.g,
            self.print_direct(),
            self.g.start().idx,
            char::from_u32(self.end_letter.idx).unwrap()
        )?;
        for cur_node in self.g.nodes() {
            writeln!(w, "case {}:", cur_node.idx)?;

            for e in self.g.edges_from(cur_node) {
                for (l, b) in self
                    .edge_direct
                    .get(&e.idx)
                    .expect(&format!("No direct generated for {}", e))
                {
                    let b_condition = match b {
                        Some(b) => format!(" && stack_top === {}", b),
                        None => String::new(),
                    };
                    writeln!(
                        w,
                        "if (c === '{}' {}) {{",
                        char::from_u32(l.idx).unwrap(),
                        b_condition,
                    )?;

                    writeln!(w, "state={};", e.to.idx)?;
                    for node in self.edges_nodes(e) {
                        writeln!(w, "path.push(\"{}\");", node)?;
                    }

                    if let Some(b) = e.bracket {
                        writeln!(w, "if (!Parser.stack_push(stack, {}, {})) {{ throw new Error(\"Unbalanced brackets\"); }}", b.idx(), !b.is_closing())?;
                    }
                    if e.letter.is_some() {
                        writeln!(w, "continue LETTERS;")?;
                    } else {
                        writeln!(w, "continue EDGES;")?;
                    }
                    writeln!(w, "}} else ")?;
                }
            }

            writeln!(
                w,
                "{{ throw new Error(`No way to continue on letter '${{c}}'`); }}\nbreak;"
            )?;
        }

        writeln!(
            w,
            r#"
        default: throw new Error(`Unreachable state: ${{state}}`); }} 
        }} 
        }} 
        
        if (stack.length !== 0) {{
            throw new Error(`Unbalanced brackets`);    
        }}
        const end_states = [ {} ];
        var is_end_state = false;
        for (let i=0;i<end_states.length; i++) {{
            if (state === end_states[i]) {{ is_end_state = true; break; }} 
        }}
        if (!is_end_state) {{ throw new Error("Did not reach an end state"); }}
        
        path.pop();
        return path  
        }} 
        }}"#,
            self.g
                .end_nodes()
                .fold(String::new(), |acc, state| acc + &format!("{},", state.idx))
        )
    }
}

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;
    use std::io::Write;
    use std::process::Command;
    use std::process::Stdio;

    use super::*;

    fn expr_lgraph() -> (Lgraph, Vec<(Node, Node, char)>) {
        let bnf = [
            (
                'S',
                Regex::Or(vec![
                    Regex::Seq(vec![Regex::Char('T'), Regex::Char('+'), Regex::Char('S')]),
                    Regex::Char('T'),
                ]),
            ),
            (
                'T',
                Regex::Or(vec![
                    Regex::Seq(vec![Regex::Char('F'), Regex::Char('*'), Regex::Char('T')]),
                    Regex::Char('F'),
                ]),
            ),
            (
                'F',
                Regex::Or(vec![
                    Regex::Char('a'),
                    Regex::Char('b'),
                    Regex::Seq(vec![Regex::Char('('), Regex::Char('S'), Regex::Char(')')]),
                ]),
            ),
        ];

        Lgraph::from_bnf(&bnf)
    }

    #[test]
    fn bnf() {
        let (g, starts_and_ends) = expr_lgraph();
        println!("{}", g);

        let p = Parser::new(g, starts_and_ends.clone());
        println!("{}", p.g);
        println!("{}", p.print_direct());

        let ast = parse_tree(&p, "a+b+a".chars());
        assert_eq!(ast, "S[T[F[aF]T]+S[T[F[bF]T]+S[T[F[aF]T]S]S]S]");

        let ast = parse_tree(&p, "a*(a+b)".chars());
        assert_eq!(ast, "S[T[F[aF]*T[F[(S[T[F[aF]T]+S[T[F[bF]T]S]S])F]T]T]S]");

        let ast = parse_tree(&p, "a+a+(a*(a+b))".chars());
        assert_eq!(ast, "S[T[F[aF]T]+S[T[F[aF]T]+S[T[F[(S[T[F[aF]*T[F[(S[T[F[aF]T]+S[T[F[bF]T]S]S])F]T]T]S])F]T]S]S]S]");
    }

    fn parse_tree<I>(p: &Parser, s: impl IntoIterator<Item = I>) -> String
    where
        I: Into<u32>,
    {
        let l: Vec<_> = s.into_iter().map(|l| Letter::new(l.into())).collect();
        let path = p.parse(&l);

        path.into_iter().fold(String::new(), |acc, n| acc + &n)
    }

    #[test]
    fn js_parser() {
        let (g, starts_and_ends) = expr_lgraph();
        let p = Parser::new(g, starts_and_ends);

        let mut node_process = Command::new("node")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();
        let Some(mut node_stdin) = node_process.stdin.take() else { unreachable!() };
        p.write_js(&mut node_stdin).unwrap();

        for input in ["a+b+a", "a*(a+b)", "a+a+(a*(a+b))"] {
            node_stdin
                .write_all(
                    format!(
                        r#"
        
        // TESTING CODE 
            {{
                        
            const ast = Parser.parse("{}");
            var s = "";
            for (const node of ast) {{
                s+=node;
            }}
            console.log(s); }}
                "#,
                        input
                    )
                    .as_bytes(),
                )
                .unwrap();
        }

        drop(node_stdin);
        let output = String::from_utf8(node_process.wait_with_output().unwrap().stdout).unwrap();

        for (out, actual_out) in output.split('\n').zip([
            "S[T[F[aF]T]+S[T[F[bF]T]+S[T[F[aF]T]S]S]S]",
            "S[T[F[aF]*T[F[(S[T[F[aF]T]+S[T[F[bF]T]S]S])F]T]T]S]",
            "S[T[F[aF]T]+S[T[F[aF]T]+S[T[F[(S[T[F[aF]*T[F[(S[T[F[aF]T]+S[T[F[bF]T]S]S])F]T]T]S])F]T]S]S]S]"
        ]) {
            assert_eq!(out, actual_out);
        }
    }

    #[test]
    fn bug_loop_error() {
        let g = Lgraph {
            edges: vec![
                Edge::new(
                    0,
                    Node::new(0),
                    Node::new(0),
                    Some(Letter::new('a' as _)),
                    Some(Bracket::Open(0)),
                ),
                Edge::new(1, Node::new(1), Node::new(0), None, Some(Bracket::Open(1))),
                Edge::new(2, Node::new(0), Node::new(2), None, None),
                Edge::new(
                    3,
                    Node::new(2),
                    Node::new(3),
                    None,
                    Some(Bracket::Closed(0)),
                ),
                Edge::new(4, Node::new(3), Node::new(4), None, None),
                Edge::new(5, Node::new(4), Node::new(2), None, None),
                Edge::new(
                    6,
                    Node::new(2),
                    Node::new(5),
                    Some(Letter::new('b' as _)),
                    Some(Bracket::Closed(1)),
                ),
            ],
            node_count: 6,
            start: 1,
            end_nodes: vec![5],
        };

        let p = Parser::new(g, vec![(Node::new(1), Node::new(5), 'S')]);
        println!("{}", p.g);
        println!("{}", p.print_direct());
        let ast = parse_tree(&p, ['a', 'b']);
        assert_eq!(ast, "S[abS]");

        assert_eq!(parse_tree(&p, ['a', 'a', 'b']), "S[aabS]", "This fails because we dont allow for any cycles to happen when generating the `direct` table");
    }
}
