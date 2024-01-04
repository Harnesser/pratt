
enum Operator {

}

#[derive(Debug)]
enum Token {
    Literal(i32),
    Plus,
}

#[derive(Debug)]
struct Var {
    name: String,
    value: i32,
}

#[derive(Debug)]
enum Expr<'a> {
    Var(&'a Var),
    Plus(&'a Expr<'a>, &'a Expr<'a>),
}

impl Expr<'_> {

    fn eval(&self) -> i32 {

        match self {

            Expr::Var(a) => {
                a.value
            },
            Expr::Plus(a, b) => {
                let av = a.eval();
                let bv = b.eval();
                av + bv
            }
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
                        tokens.push(Token::Plus);
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

    if buf.len() > 0 {
        let val = buf.parse::<i32>().unwrap();
        let token = Token::Literal(val);
        tokens.push(token);
    }


    dbg!(&tokens);
    tokens
}

fn eval_expression(expr: &str) -> i32 {
    let tokens = tokenise(expr);
    return 0;
}

fn main() {
    assert_eq!(26, eval_expression("10 + 16"));
}
