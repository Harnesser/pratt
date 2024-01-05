/// Precedence Climbing Algorithm
///
/// Implementation taken from ["Parsing Expressions by Recursive Descent"][1]
///
/// [1]: https://www.engr.mun.ca/~theo/Misc/exp_parsing.htm#climbing

use crate::Token;
use crate::ExprBoxed;

struct Climber {
}

impl Climber {

    /// Create a new Precedence Climber parser datastructure.
    fn new(_tokens: Vec<Token>) -> Climber {
        Climber {}
    }

}

/// Parse using precedence climbing
///
/// Return an AST
pub fn parse(tokens: Vec<Token>) -> ExprBoxed {

    dbg!(&tokens);
    let mut parser = Climber::new(tokens.clone());

    //parser.expression();
    //parser.expect(Token::EOF);
    //parser.expr_stack.pop().unwrap()
    ExprBoxed::LiteralInt {literal: -1}
}
