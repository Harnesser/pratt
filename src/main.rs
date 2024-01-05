//! Expression Parsing Experimentation
//!
//! # Supported
//!
//! Parsing Algorithms
//! * Shunting Yard
//!
//! Arithmetic Operators
//! * Addition         : `a + b`
//! * Subtraction      : `a - b`
//! * Multiplication   : `a * b`
//! * Division         : `a / b`
//! * Brackets         : `(a + b) * c`
//! * Unary Minus/Plus : `-a`, `+a`
//! * Exponentials     : `a ** b ** c`
//!     - right associative, 2-char symbol
//!     - keeping `^` for XOR
//!     - negative exponents not supported, cos rust doesn't support
//!       them for integers.
//!
//! # Not Supported
//!
//! Parsing Algorithms
//! * TODO: Precedence Climbing
//! * TODO: Pratt
//!
//! Arithmetic Operators
//! * (nothing planned unimplemented)
//!
//! # Stretch Goals
//!
//! Logic Operators    :
//! * TODO: Not              : `!a`
//! * TODO: And              : `a && b`
//! * TODO: Or               : `a || b`
//! * TODO: Xor              : `a ^ b` (this just bitwise?)
//!
//! Bitwise Operators :
//! * TODO: BwNot            : `~a`
//! * TODO: BwAnd            : `a & b`
//! * TODO: BwOr             : `a | b`
//! * TODO: BwXor            : `a ^ b`
//!
//! Reduction Operators
//! * TODO: RedAnd           : `&a`
//! * TODO: RedOr            : `|a`
//! * TODO: RedXor           : `^a`
//!
//! Precedence Parsing:

use std::path::Path;
use std::fs::File;
use std::io::Write;

use std::iter::Peekable;

#[derive(Debug)]
#[derive(Copy, Clone, PartialEq)]
#[allow(clippy::upper_case_acronyms)]
enum Token {
    Literal(i32),
    Add,
    Sub,
    Mul,
    Div,

    Exp,

    UnAdd,
    UnSub,

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
            Token::Sub => 20,
            Token::Mul => 30,
            Token::Div => 30,
            Token::UnAdd => 40,
            Token::UnSub => 40,
            Token::Exp => 45,
            Token::Bopen => 50,
            Token::Bclose => 50,
        }
    }

    fn is_binary(&self) -> bool {
        matches!(self,
                 Token::Add | Token::Sub |
                 Token::Mul | Token::Div |
                 Token::Exp)
    }

    fn is_unary(&self) -> bool {
        matches!(self,
            Token::UnAdd | Token::UnSub |
            Token::Add | Token::Sub
        )
    }

    /// return true if this token beats token `t2` in a precedence fight
    fn wins_over(&self, t2: Token) -> bool {
        let p_this = self.precedence();
        let p_t2 = t2.precedence();
        println!("prec {} vs {}", p_this, p_t2);

        if p_this == p_t2 {
            // if == and left-assoc, true
            // only Exp is right-assoc
            !matches!(self, Token::Exp)
        } else {
            p_this > p_t2
        }

    }
}


/// Tokeniser
fn tokenise(expr: &str) -> Vec<Token> {

    let mut tokens: Vec<Token> = vec![];
    let mut text_iter = expr.chars().peekable();
    let mut buf = "".to_string();

    let mut c: char;
    let mut c_next: Option<char>;

    'things: loop {

        c = text_iter.next().unwrap();
        c_next = text_iter.peek().copied();

        dbg!(c, c_next, &buf);

        match c {
            ' ' => {
                continue;
            },

            '0' ..= '9' => {
                buf.push(c);
                match c_next {
                    Some('0' ..= '9') => {
                    },
                    _ => {
                        // can't figure out how to put this in a closure
                        let val = buf.parse::<i32>().unwrap();
                        buf.clear();
                        let token = Token::Literal(val);
                        tokens.push(token);
                    }
                }
            },

            '(' => {
                tokens.push(Token::Bopen);
            },

            '+' => {
                tokens.push(Token::Add);
            },

            '-' => {
                tokens.push(Token::Sub);
            },

            '/' => {
                tokens.push(Token::Div);
            },

            '*' => {
                if let Some('*') = c_next {
                    println!("exponential");
                    tokens.push(Token::Exp);
                    // consume
                    _ = text_iter.next().unwrap();
                    c_next = text_iter.peek().copied();
                } else {
                    tokens.push(Token::Mul);
                }
            },

            ')' => {
                tokens.push(Token::Bclose);
            },

            _ => {
                println!("Here with my friend '{}'", c);
                unimplemented!()
            }
        }

        if c_next.is_none() {
            break 'things;
        }

    } // loop

    if !buf.is_empty() {
        println!("nonempty buffer");
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
    Sub {
        a: Box<ExprBoxed>,
        b: Box<ExprBoxed>,
    },
    UnAdd {
        a: Box<ExprBoxed>,
    },
    UnSub {
        a: Box<ExprBoxed>,
    },
    Mul {
        a: Box<ExprBoxed>,
        b: Box<ExprBoxed>,
    },
    Div {
        a: Box<ExprBoxed>,
        b: Box<ExprBoxed>,
    },
    Exp {
        m: Box<ExprBoxed>,
        e: Box<ExprBoxed>,
    },
}

impl ExprBoxed {

    fn eval(&self) -> i32 {

        match &self {

            ExprBoxed::LiteralInt {literal} => {
                *literal
            },

            ExprBoxed::Exp {m, e} => {
                let mv = m.eval();
                let ev = e.eval();
                mv.pow(ev.try_into().unwrap())
            },

            ExprBoxed::UnAdd {a} => {
                a.eval()
            },
            ExprBoxed::UnSub {a} => {
                -a.eval()
            },

            ExprBoxed::Mul {a, b} => {
                let av = a.eval();
                let bv = b.eval();
                av * bv
            },

            ExprBoxed::Div {a, b} => {
                let av = a.eval();
                let bv = b.eval();
                av / bv
            },

            ExprBoxed::Add {a, b} => {
                let av = a.eval();
                let bv = b.eval();
                av + bv
            },
            ExprBoxed::Sub {a, b} => {
                let av = a.eval();
                let bv = b.eval();
                av - bv
            },

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

            ExprBoxed::UnAdd {a} => {
                labels.push('+'.to_string());
                let u = labels.len() - 1;
                let v1 = a.dot(labels, edges);
                edges.push((u, v1));
                u
            },
            ExprBoxed::UnSub {a} => {
                labels.push('-'.to_string());
                let u = labels.len() - 1;
                let v1 = a.dot(labels, edges);
                edges.push((u, v1));
                u
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
            ExprBoxed::Sub {a, b} => {
                labels.push('-'.to_string());
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

            ExprBoxed::Div {a, b} => {
                labels.push('/'.to_string());
                let u = labels.len() - 1;
                let v1 = a.dot(labels, edges);
                let v2 = b.dot(labels, edges);
                edges.push((u, v1));
                edges.push((u, v2));
                u
            }

            ExprBoxed::Exp {m, e} => {
                labels.push("**".to_string());
                let u = labels.len() - 1;
                let v1 = m.dot(labels, edges);
                let v2 = e.dot(labels, edges);
                edges.push((u, v1));
                edges.push((u, v2));
                u
            }

        }
    }

}

fn write_dot(expr: &ExprBoxed, filename: &str) {
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
    let path = Path::new(filename);
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
        } else if t != Token::EOF {
            self.consume();
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

            // is unary
            Token::Add=> {
                println!("Pushing unary plus");
                self.operator_stack.push(Token::UnAdd);
                self.consume();
                self.precedence();
            }
            Token::Sub => {
                println!("Pushing unary minus");
                self.operator_stack.push(Token::UnSub);
                self.consume();
                self.precedence();
            }

            // tokeniser doesn't emit these
            Token::UnSub | Token::UnAdd => {
            },

            // should not be here
            _ => { unreachable!() }

        }
        println!("Leaving precedence()");
    }

    /// Push an operator on to the stack
    ///
    /// Will pop the stack until the token at the top has an equal or lower precedence
    /// to the token we want to push. Then we push.
    fn push_operator(&mut self, t: Token) {
        loop {
            let top_op = self.operator_stack[self.operator_stack.len()-1];
            if top_op.wins_over(t) {
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
                Token::Sub => {
                    let expr = ExprBoxed::Sub{a: Box::new(a), b: Box::new(b)};
                    self.expr_stack.push(expr);
                },
                Token::Mul => {
                    let expr = ExprBoxed::Mul{a: Box::new(a), b: Box::new(b)};
                    self.expr_stack.push(expr);
                },
                Token::Div => {
                    let expr = ExprBoxed::Div{a: Box::new(a), b: Box::new(b)};
                    self.expr_stack.push(expr);
                },
                Token::Exp => {
                    let expr = ExprBoxed::Exp{m: Box::new(a), e: Box::new(b)};
                    self.expr_stack.push(expr);
                },
                _ => {}
            }
            let _ = self.operator_stack.pop();
        } else if top_op.is_unary() {
            let a = self.expr_stack.pop().unwrap();
            match top_op {
                Token::UnAdd => {
                    let expr = ExprBoxed::UnAdd{a: Box::new(a) };
                    self.expr_stack.push(expr);
                },
                Token::UnSub => {
                    let expr = ExprBoxed::UnSub{a: Box::new(a) };
                    self.expr_stack.push(expr);
                },
                _ => {
                    panic!("asdfasdfasdf asdfasdf");
                }
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
fn parse_shunting(tokens: Vec<Token>) -> ExprBoxed {

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
    let filename = expr_to_filename(expr);
    let tokens = tokenise(expr);
    let expr = parse_shunting(tokens);
    write_dot(&expr, &filename);
    expr.eval()
}


/// Replacements so expr is a valid filename
fn expr_to_filename(expr: &str) -> String {
    let mut s = expr.to_string();
    s = s.replace(' ', "_");
    s = s.replace('+', "ADD");
    s = s.replace('-', "SUB");
    s = s.replace('*', "MUL");
    s = s.replace('(', "BRO");
    s = s.replace(')', "BRC");
    s = s.replace('/', "DIV");
    s += ".dot";
    s = "dot/".to_string() + &s;
    s
}

// Read from args?
fn main() {
    assert_eq!(10,  eval_expression("-(-23 - -10) - -(-5 - -2)"));
    println!("4^(3^2) = {:?}", 4i32.checked_pow(3i32.pow(2) as u32) );
    println!("(4^3)^2 = {:?}", (4i32.pow(3)).checked_pow(2) );

    // TODO? interesting to catch this in this parser?
    //eval_expression("2**-3");
}



#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_basic_precedence() {
        assert_eq!(42, eval_expression("10 + 16 * 2"));
        assert_eq!(162, eval_expression("10 * 16 + 2"));
        assert_eq!(102, eval_expression("10 * 3 + 9 * 8"));
    }

    #[test]
    fn test_brackets() {
        assert_eq!(102, eval_expression("(10 * 3) + 9 * 8"));
        assert_eq!(102, eval_expression("10 * 3 + (9 * 8)"));
        assert_eq!(960, eval_expression("10 * ( 3 + 9 ) * 8"));
        assert_eq!(3,  eval_expression("11 - (3 - 1) * 4"));
    }

    #[test]
    fn test_unary_arith() {
        assert_eq!(32,  eval_expression("-(-24 + -10) + -2"));
        assert_eq!(10,  eval_expression("-(-23 - -10) - -(-5 - -2)"));
        assert_eq!(40,  eval_expression("-(-23 - +10) - +(-5 - +2)"));
    }

    #[test]
    fn test_misc() {
        assert_eq!(23,  eval_expression("+23"));
        assert_eq!(-23,  eval_expression("-23"));
        assert_eq!(960, eval_expression("10*(3+9)*8"));
    }

    #[test]
    fn test_exponents() {
        assert_eq!(  2i32.pow(3),  eval_expression("2**3"));
        assert_eq!( -16, eval_expression("-2**4"));
        assert_eq!(  16, eval_expression("(-2)**4"));
        //assert_eq!( 2i32.pow(-3), eval_expression("2**-3")); // rust barfs
        assert_eq!( 2i32.pow(3), eval_expression("2**-(-2 -1)"));
        assert_eq!(4i32.pow(3i32.pow(2).try_into().unwrap()), eval_expression("4**3**2"));
    }

    #[test]
    fn test_division() {
        assert_eq!(5, eval_expression("10/2"));
        assert_eq!(5, eval_expression("-10/-2"));
        assert_eq!((10/2)*3, eval_expression("(10/2)*3"));
        assert_eq!(10/(2*3), eval_expression("10/(2*3)"));
        assert_eq!((10/2)*3, eval_expression("10/2*3"));
        assert_eq!((10*2)/3, eval_expression("10*2/3"));
    }

}

