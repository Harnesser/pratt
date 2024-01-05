use crate::Token;
use crate::ExprBoxed;

use std::iter::Peekable;

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
pub fn parse_shunting(tokens: Vec<Token>) -> ExprBoxed {

    dbg!(&tokens);
    let mut parser = ShuntingYard::new(tokens.clone());

    parser.expression();
    parser.expect(Token::EOF);
    parser.expr_stack.pop().unwrap()
}

