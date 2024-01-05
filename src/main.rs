//! Expression Parsing Experimentation
//!
//! # Supported
//!
//! Parsing Algorithms
//! * Shunting Yard
//!
//! Arithmetic Operators
//! * Addition         : a + b
//! * Multiplication   : a * b
//! * Brackets         : (a + b) * c
//!
//!
//! # Not Supported
//!
//! Parsing Algorithms
//! * TODO: Precedence Climbing
//! * TODO: Pratt
//!
//! Arithmetic Operators
//! * TODO: Unary Minus/Plus : -a, +a
//! * TODO: Division         : a / b
//! * TODO: Subtraction      : a - b
//! * TODO: Exponentials     : a ** b ** c
//!
//! Logic Operators    :
//! * TODO: Not              : !a
//! * TODO: And              : a && b
//! * TODO: Or               : a || b
//! * TODO: Xor              : a ^ b
//!
//! Bitwise Operators :
//! * TODO: BwNot            : ~a
//! * TODO: BwAnd            : a & b
//! * TODO: BwOr             : a | b
//! * TODO: BwXor            : a ^ b
//!
//! Reduction Operators
//! * TODO: RedAnd           : &a
//! * TODO: RedOr            : |a
//! * TODO: RedXor           : ^a
//!
//! Precedence Parsing:

use std::path::Path;
use std::fs::File;
use std::io::Write;

use std::iter::Peekable;

#[derive(Debug, Copy, Clone, PartialEq)]
enum Token {
    Literal(i32),
    Add,
    Mul,

    Bopen,
    Bclose,

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
            Token::Bopen => 50,
            Token::Bclose => 50,
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

/// Tokeniser
/// Should this even have state?
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

                    '(' => {
                        tokens.push(Token::Bopen);
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

                    ')' => {
                        tokens.push(Token::Bclose);
                        nxt = TokeniserState::Operator;
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

/// Shunting-Yard Algorithm
///
/// Implementation taken from ["Parsing Expressions by Recursive Descent"][1]
///
/// This was painful to implement in this style cos i needed lookahead in the
/// parser, and i needed to store an interator in the struct.
///
/// [1]: https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#shunting_yard
struct ShuntingYard<'a> {
    /// Stack of subexpressions
    expr_stack: Vec<ExprBoxed>,
    /// Stack of lower-precedence operators
    operator_stack: Vec<Token>,
    /// Iterator on the token list
    token_iter: Peekable<Box<dyn Iterator<Item=Token> + 'a>>,
    /// Peek into next token
    next: Token,
}

impl ShuntingYard<'_> {

    /// Create a new Shunting Yard parser datastructure.
    ///
    /// Initialises the token iterator and puts a sentinel token on the stack.
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

    /// Consume the current token and update the lookahead
    fn consume(&mut self) {
        self.token_iter.next();
        self.next = *self.token_iter.peek().unwrap();
        println!("Next: {:?}", self.next);
        dbg!(&self.operator_stack);
        dbg!(&self.expr_stack);
    }

    /// Expect and consume a token
    fn expect(&mut self, t: Token) {
        if self.next != t {
            panic!("Expected token {:?}, got {:?}", t, self.next);
        } else {
            if t != Token::EOF {
                self.consume();
            }
        }
    }

    /// Parse a subexpression
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

    /// Precdence?
    fn precedence(&mut self) {
        println!("In precedence()");
        match self.next {
            Token::Literal(v) => {
                println!("Pushing literal '{}'", v);
                self.expr_stack.push(ExprBoxed::LiteralInt {literal: v});
                self.consume();
            },
            Token::Bopen => {
                self.consume();
                self.operator_stack.push(Token::Sentinel);
                self.expression();
                self.expect(Token::Bclose);
                assert_eq!(self.operator_stack.pop(), Some(Token::Sentinel));
            },

            _ => { unreachable!() }

        }
        println!("Leaving precedence()");
    }

    /// Push an operator on to the stack
    ///
    /// Will pop the stack until the token at the top has an equal or lower precedence
    /// to the token we want to push. Then we push.
    fn push_operator(&mut self, t: Token) {
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

    /// Pop an operator on the stack
    ///
    /// Also pops some operands from the subexpression stack and pushes
    /// a new subexpression on that stack.
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

/// Parse using a naive shunting algorithm
///
/// Return an AST
fn parse_shunting<'a>(tokens: Vec<Token>) -> ExprBoxed {

    dbg!(&tokens);
    let mut parser = ShuntingYard::new(tokens.clone());

    parser.expression();
    parser.expect(Token::EOF);
    parser.expr_stack.pop().unwrap()
}

/// Parse and evaluate the expression in the string.
///
/// Also writes a Graphviz file of the resulting AST.
fn eval_expression(expr: &str) -> i32 {
    println!("Parsing: '{}'", expr);
    let tokens = tokenise(expr);
    let expr = parse_shunting(tokens);
    write_dot(&expr);
    expr.eval()
}

/// Mostly tests for now
fn main() {
    // cos of the ordering, the dotfile will be of the failing testcase,
    // or the last one
    assert_eq!(42, eval_expression("10 + 16 * 2"));
    assert_eq!(162, eval_expression("10 * 16 + 2"));
    assert_eq!(102, eval_expression("10 * 3 + 9 * 8"));
    assert_eq!(102, eval_expression("(10 * 3) + 9 * 8"));
    assert_eq!(102, eval_expression("10 * 3 + (9 * 8)"));
    assert_eq!(960, eval_expression("10 * ( 3 + 9 ) * 8"));
    assert_eq!(960, eval_expression("10*(3+9)*8"));
}
