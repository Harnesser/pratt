use std::path::Path;
use std::fs::File;
use std::io::Write;

#[derive(Debug)]
enum Token {
    Literal(i32),
    Add,
    Mul,
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


// parse using a naive shunting algorithm
// this does not do operator precedence
fn parse_shunting<'a>(tokens: Vec<Token>) -> ExprBoxed {

    let mut expr_stack: Vec<ExprBoxed> = Default::default();
    let mut operator_stack: Vec<Token> = Default::default();

    // marshall
    for t in tokens {
        match t {
            Token::Add | Token::Mul => {
                operator_stack.push(t);
            },
            Token::Literal(v) => {
                expr_stack.push(ExprBoxed::LiteralInt {literal: v});
            }
        }
    }

    // i have no intuition for what all this boxing is doing allocation-wise

    // unpack
    for op in operator_stack.iter().rev() {
        println!("Op: {:?}", op);
        let b = expr_stack.pop().unwrap();
        let a = expr_stack.pop().unwrap();
        match op {
            Token::Add => {
                let expr = ExprBoxed::Add{a: Box::new(a), b: Box::new(b)};
                expr_stack.push(expr);
            },
            Token::Mul => {
                let expr = ExprBoxed::Mul{a: Box::new(a), b: Box::new(b)};
                expr_stack.push(expr);
            },
            _ => {}
        }
    }

    expr_stack.pop().unwrap()
}


fn eval_expression(expr: &str) -> i32 {
    let tokens = tokenise(expr);
    let expr = parse_shunting(tokens);
    write_dot(&expr);
    expr.eval()
}

fn main() {
    //assert_eq!(42, eval_expression("10 + 16 * 2"));
    //assert_eq!(372, eval_expression("10 * 3 + 9 * 8"));
    assert_eq!(162, eval_expression("10 * 16 + 2"));
}
