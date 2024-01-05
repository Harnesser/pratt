//! Expression Parsing Experimentation
//!
//! Supported:
//! * Addition       : a + b
//! * Multiplication : a * b 
//!
//! Not Supported:
//! * Unary Minus/Plus : -a +a
//! * Division         : a / b
//! * Subtraction      : a - b
//! * Exponentials     : a ** b ** c
//! * Brackets         : (a + b) * c
//!
//! Precedence Parsing:
//! * Naive shunting algorithm
//! * TODO: Pratt or maybe Precedence Climbing

use std::path::Path;
use std::fs::File;
use std::io::Write;

use std::iter::Peekable;

#[derive(Debug, Copy, Clone, PartialEq)]
enum Token {
    Literal(i32),
    Add,
    Mul,

    EOF,
    Sentinel,
}

impl Token {
    #[allow(dead_code)]
    fn precedence(&self) -> u32 {
        match self {
            Token::EOF => 0,
            Token::Sentinel => 1,
            Token::Literal(_) => 10,
            Token::Add => 20,
            Token::Mul => 30,
        }
    }

    #[allow(dead_code)]
    fn is_binary(&self) -> bool {
        match self {
            Token::Add | Token::Mul => true,
            _ => false
        }
    }
}

enum TokeniserState {
    Int,
    Operator,
}

fn tokenise(expr: &str) -> Vec<Token> {

    let mut tokens: Vec<Token> = vec![];
    let mut text_iter = expr.chars();
    let mut buf = "".to_string();
    let mut state = TokeniserState::Int;
    let mut nxt;

    let mut c: char = ' ';
    let mut eat = true;

    'things: loop {

        dbg!(eat, c, &buf);

        if eat {
            if let Some(c_) = text_iter.next() {
                c = c_;
            } else {
                break 'things;
            }

            if c == ' ' {
                continue;
            }
        }

        match state {

            TokeniserState::Int => {
                match c {
                    '0' ..= '9' => {
                        buf.push(c);
                        nxt = TokeniserState::Int;
                    },

                    _ => {
                        let val = buf.parse::<i32>().unwrap();
                        buf.clear();
                        let token = Token::Literal(val);
                        tokens.push(token);
                        eat = false;
                        nxt = TokeniserState::Operator;
                    }
                }
            },

            TokeniserState::Operator => {
                match c {
                    '+' => {
                        tokens.push(Token::Add);
                        nxt = TokeniserState::Int;
                    },

                    '*' => {
                        tokens.push(Token::Mul);
                        nxt = TokeniserState::Int;
                    },

                    _ => {
                        println!("Here with my friend '{}'", c);
                        unimplemented!()
                    }
                }
                eat = true;
            },

            //_ => { unimplemented!() }

        } // match state

        state = nxt;

    } // loop

    if !buf.is_empty() {
        let val = buf.parse::<i32>().unwrap();
        let token = Token::Literal(val);
        tokens.push(token);
    }
    tokens.push(Token::EOF);

    dbg!(&tokens);
    tokens
}


// adapted from start of:
// https://recursion.wtf/posts/rust_schemes/

#[derive(Debug)]
enum ExprBoxed {
    LiteralInt { literal: i32 },
    Add {
        a: Box<ExprBoxed>,
        b: Box<ExprBoxed>,
    },
    Mul {
        a: Box<ExprBoxed>,
        b: Box<ExprBoxed>,
    },
}

impl ExprBoxed {

    fn eval(&self) -> i32 {

        match &self {

            ExprBoxed::LiteralInt {literal} => {
                *literal
            },
            ExprBoxed::Add {a, b} => {
                let av = a.eval();
                let bv = b.eval();
                av + bv
            },
            ExprBoxed::Mul {a, b} => {
                let av = a.eval();
                let bv = b.eval();
                av * bv
            }
        }
    }

    // i can't use mem addr of boxes cos i'm allocating all over the 
    // place it seems.
    fn dot(&self, labels: &mut Vec<String>, edges: &mut Vec<(usize, usize)>) -> usize {
        match &self {
            ExprBoxed::LiteralInt {literal} => {
                labels.push(literal.to_string());
                labels.len()-1
            },
            ExprBoxed::Add {a, b} => {
                labels.push('+'.to_string());
                let u = labels.len() - 1;
                let v1 = a.dot(labels, edges);
                let v2 = b.dot(labels, edges);
                edges.push((u, v1));
                edges.push((u, v2));
                u
            },

            ExprBoxed::Mul {a, b} => {
                labels.push('*'.to_string());
                let u = labels.len() - 1;
                let v1 = a.dot(labels, edges);
                let v2 = b.dot(labels, edges);
                edges.push((u, v1));
                edges.push((u, v2));
                u
            }

        }
    }

}

fn write_dot(expr: &ExprBoxed) {
    let mut lines: Vec<String> = vec![];
    lines.push(r#"digraph "expression" {"#.to_string());

    let mut labels: Vec<String> = vec![];
    let mut edges: Vec<(usize,usize)> = vec![];

    expr.dot(&mut labels, &mut edges);

    for (i, label) in labels.iter().enumerate() {
        lines.push( format!("{} [label = \"{}\"];", i, label) );
    }

    for (u,v) in edges {
        lines.push( format!("{} -> {}", u, v) );
    }

    lines.push("}".to_string());

    // Create a path to the desired file
    let path = Path::new("expression.dot");
    let display = path.display();

    // Open a file in write-only mode, returns `io::Result<File>`
    let mut file = match File::create(path) {
        Err(why) => panic!("couldn't create {}: {}", display, why),
        Ok(file) => file,
    };

    match file.write_all(lines.join("\n").as_bytes()) {
        Err(why) => panic!("couldn't write to {}: {}", display, why),
        Ok(_) => println!("successfully wrote to {}", display),
    }
}


struct ShuntingYard<'a> {
    expr_stack: Vec<ExprBoxed>,
    operator_stack: Vec<Token>,
    token_iter: Peekable<Box<dyn Iterator<Item=Token> + 'a>>,
    next: Token,
}

impl ShuntingYard<'_> {

    fn new(tokens: Vec<Token>) -> ShuntingYard<'static> {
        let mut p = ShuntingYard {
            expr_stack: Default::default(),
            operator_stack: Default::default(),
            token_iter: (Box::new(tokens.into_iter()) as Box<dyn Iterator<Item=Token>>).peekable(),
            next: Token::Sentinel,
        };
        p.operator_stack.push(Token::Sentinel);
        p.next = *p.token_iter.peek().unwrap();
        p
    }

    fn consume(&mut self) {
        self.token_iter.next();
        self.next = *self.token_iter.peek().unwrap();
        println!("Next: {:?}", self.next);
        dbg!(&self.operator_stack);
        dbg!(&self.expr_stack);
    }

    fn expect(&mut self, t: Token) {
        if self.next != t {
            panic!("Expected token {:?}, got {:?}", t, self.next);
        } else {
            if t != Token::EOF {
                self.consume();
            }
        }
    }

    fn expression(&mut self) {
        println!("In expression()");
        println!(" - before while 1");
        self.precedence();
        while self.next.is_binary() {
            self.push_operator(self.next);
            self.consume();
            self.precedence();
        }
        println!(" - before while 2");
        loop {
            let top_op = self.operator_stack[self.operator_stack.len()-1];
            println!("   - {:?}", top_op);
            if top_op == Token::Sentinel {
                println!("Done");
                break;
            } else {
                self.pop_operator();
            }
        }
        println!("leaving expression()");
    }

    fn precedence(&mut self) {
        println!("In precedence()");
        match self.next {
            Token::Literal(v) => {
                println!("Pushing literal");
                self.expr_stack.push(ExprBoxed::LiteralInt {literal: v});
                self.consume();
            },
            _ => { todo!() }
/*
            Token::Add | Token::Mul => {
                self.operator_stack.push(*t);
            },
            Token::EOF | Token::Sentinel => todo!()
*/

        }
        println!("Leaving precedence()");
    }

    fn push_operator(&mut self, t: Token) {
        // pop operators until t has the lowest precedence
        let t_prec = t.precedence();

        loop {
            let top_prec = self.operator_stack[self.operator_stack.len()-1].precedence();
            println!("prec {} vs {}", t_prec, top_prec);
            if top_prec > t_prec {
                self.pop_operator();
            } else {
                break;
            }
        }
        self.operator_stack.push(t);
    }

    fn pop_operator(&mut self) {
        let top_op = &self.operator_stack[self.operator_stack.len()-1];
        println!("Popping {:?}", top_op);
        if top_op.is_binary() {
            let b = self.expr_stack.pop().unwrap();
            let a = self.expr_stack.pop().unwrap();
            match top_op {
                Token::Add => {
                    let expr = ExprBoxed::Add{a: Box::new(a), b: Box::new(b)};
                    self.expr_stack.push(expr);
                },
                Token::Mul => {
                    let expr = ExprBoxed::Mul{a: Box::new(a), b: Box::new(b)};
                    self.expr_stack.push(expr);
                },
                _ => {}
            }
            let _ = self.operator_stack.pop();
        } else {
            panic!("ASDFASDF");
        }

    }

}

// parse using a naive shunting algorithm
// this does not do operator precedence
// Keeps operators on a stack until both their operands have been parsed.
fn parse_shunting<'a>(tokens: Vec<Token>) -> ExprBoxed {

    dbg!(&tokens);
    let mut parser = ShuntingYard::new(tokens.clone());

    parser.expression();
    parser.expect(Token::EOF);
    parser.expr_stack.pop().unwrap()
}


fn eval_expression(expr: &str) -> i32 {
    println!("Parsing: '{}'", expr);
    let tokens = tokenise(expr);
    let expr = parse_shunting(tokens);
    write_dot(&expr);
    expr.eval()
}

fn main() {
    // cos of the ordering, the dotfile will be of the failing testcase,
    // or the last one
    assert_eq!(42, eval_expression("10 + 16 * 2"));
    assert_eq!(162, eval_expression("10 * 16 + 2"));
    assert_eq!(102, eval_expression("10 * 3 + 9 * 8"));
}
