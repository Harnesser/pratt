/// Precedence Climbing Algorithm
///
/// Implementation taken from ["Parsing Expressions by Recursive Descent"][1]
///
/// [1]: https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing

use crate::Token;
use crate::ExprBoxed;

use std::iter::Peekable;

struct Climber<'a> {
    /// Final AST
    tree: Option<ExprBoxed>,
    /// Iterator on the token list
    token_iter: Peekable<Box<dyn Iterator<Item=Token> + 'a>>,
    /// Peek into next token
    next: Token,
}

impl Climber<'_> {

    /// Create a new Precedence Climber parser datastructure.
    fn new(tokens: Vec<Token>) -> Climber<'static> {
        let mut p = Climber {
            tree: None,
            token_iter: (Box::new(tokens.into_iter()) as Box<dyn Iterator<Item=Token>>).peekable(),
            next: Token::Sentinel,
        };
        p.next = *p.token_iter.peek().unwrap();
        p
    }

    /// Consume the current token and update the lookahead
    fn consume(&mut self) {
        self.token_iter.next();
        self.next = *self.token_iter.peek().unwrap();
        println!("Next: {:?}", self.next);
    }

    /// Expect and consume a token
    fn expect(&mut self, t: Token) {
        if self.next != t {
            panic!("Expected token {:?}, got {:?}", t, self.next);
        } else if t != Token::EOF {
            self.consume();
        }
    }

    fn go(&mut self) {
        self.tree = Some(self.expression(0));
        self.expect(Token::EOF);
    }

    /// Parse a subexpression
    fn expression(&mut self, p: usize) -> ExprBoxed {
        println!("In expression() with {}", p);

        let mut tree = self.precedence();
        while self.next.is_binary() && self.next.height() >= p {
            let op = self.next;
            self.consume();
            let incr = !(op == Token::Exp); // the only right-ass
            let p_op = op.height();
            let q = if incr {p_op + 1} else { p_op };
            let tree1 = self.expression(q);
            tree = match op {
                Token::Add => {
                    ExprBoxed::Add{a: Box::new(tree), b: Box::new(tree1)}
                },
                Token::Sub => {
                    ExprBoxed::Sub{a: Box::new(tree), b: Box::new(tree1)}
                },
                Token::Mul => {
                    ExprBoxed::Mul{a: Box::new(tree), b: Box::new(tree1)}
                },
                Token::Div => {
                    ExprBoxed::Div{a: Box::new(tree), b: Box::new(tree1)}
                },
                Token::Exp => {
                    ExprBoxed::Exp{m: Box::new(tree), e: Box::new(tree1)}
                },
                _ => {
                    panic!("should be creating a binary op here");
                }
            };

        }
        println!("tree {:?}", tree);
        tree
    }

    fn precedence(&mut self) -> ExprBoxed{
        println!("In precedence()");
        let mut tree: ExprBoxed;
        if self.next.is_unary() {
            let op = self.next;
            self.consume();

            // this height is wrong, because the tokeniser doesn't know
            // about unary or binary ops.
            // let q = op.height();
            let q = match op {
                Token::Add => Token::UnAdd.height(),
                Token::Sub => Token::UnSub.height(),
                _ => {
                    panic!("there's no unary precedence for {:?}", op);
                }
            };

            tree = self.expression(q);
            tree = match op {
                Token::Add => {
                    ExprBoxed::UnAdd{a: Box::new(tree) }
                },
                Token::Sub => {
                    ExprBoxed::UnSub{a: Box::new(tree) }
                },
                _ => {
                    panic!("This should be a unary op: {:?}", op);
                }
            };
        } else if self.next == Token::Bopen {
            self.consume();
            tree = self.expression(0);
            self.expect(Token::Bclose);
        } else if let Token::Literal(v) = self.next {
            println!("Literal '{}'", v);
            tree = ExprBoxed::LiteralInt {literal: v};
            self.consume();
        } else {
            panic!("Should not have got to here in precedence()");
        }
        println!("tree {:?}", tree);
        tree
    }

}

/// Parse using precedence climbing
///
/// Return an AST
pub fn parse(tokens: Vec<Token>) -> ExprBoxed {

    dbg!(&tokens);
    let mut parser = Climber::new(tokens.clone());

    parser.go();
    parser.tree.unwrap()
}
