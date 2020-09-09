//! `chain_cmp` lets you chain comparison operators like
//! you would in mathematics.

use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{parse_macro_input, spanned::Spanned, Expr, ExprBinary, Token};

/// Use the `chomp` macro to chain comparison operators.
///
/// # Examples
///
/// ## Basic usage
///
/// ```
/// use chain_cmp::chomp;
///
/// let (a, b, c) = (1, 2, 3);
///
/// assert!(chomp!(a < b < c));
///
/// // You can use equality operators as well:
/// assert!(chomp!(a != b != c));
///
/// // And you can even chain more than three operators:
/// assert!(chomp!(a != b != c != a)); // making sure these values are pairwise distinct
/// ```
///
/// ## Comparing arbitrary expressions
///
/// As long as the comparison operators have the lowest precedence,
/// `chomp` will evaluate any expression, like variables, blocks,
/// function calls, etc.
///
/// ```
/// use chain_cmp::chomp;
///
/// const ANSWER: u32 = 42;
///
/// assert!(chomp!({
///     println!("Life, the Universe, and Everything");
///     ANSWER
/// } != 6 * 9 == 54));
/// ```
#[proc_macro]
pub fn chomp(tokens: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(tokens as ExprBinary);
    match cmp_tree_to_conjunction_tree(ast) {
        Ok(expr) => expr.into_token_stream(),
        Err(err) => err.to_compile_error(),
    }
    .into()
}

/// `cmp_tree_to_conjunction_tree` turns a tree of chained
/// comparisons that would normally not be valid rust into
/// a valid tree of conjunctions.
///
/// In other words, it turns something like `a < b < c` into
/// `a < b && b < c`.
fn cmp_tree_to_conjunction_tree(cmp_tree: ExprBinary) -> Result<Expr, syn::Error> {
    let mut exprs = Vec::new();
    flatten_tree(cmp_tree, &mut exprs)?;
    Ok(build_conjunction_tree(exprs))
}

/// `is_comparison_op` returns `true` if `op` is one of
/// `<`, `<=`, `>`, `>=`, `==`, `!=`.
fn is_comparison_op(op: &syn::BinOp) -> bool {
    use syn::BinOp::*;
    match op {
        Ne(_) | Eq(_) | Le(_) | Ge(_) | Lt(_) | Gt(_) => true,
        _ => false,
    }
}

/// `is_comparison` returns `true` if `expr` is a
/// comparison, i.e. any operation that is supported
/// by types that implement `PartialEq` or `PartialOrd`.
fn is_comparison(expr: &ExprBinary) -> bool {
    is_comparison_op(&expr.op)
}

/// `flatten_tree` takes a `tree` of binary expressions and flattens it,
/// appending each individual expression to `container`.
///
/// For example, this tree of comparison expressions
/// (where `en` is an arbitrary expression)
///
/// ```nocompile
///          <
///         / \
///        <=  e4
///       /  \
///      <=   e3
///     /  \
///    e1  e2
/// ```
///
/// becomes this flattened list:
///
/// ```nocompile
/// [e4, e3, e2, e1]
/// ```
fn flatten_tree(mut tree: ExprBinary, container: &mut Vec<Expr>) -> Result<(), syn::Error> {
    let op = tree.op;
    if !is_comparison_op(&op) {
        let err = syn::Error::new_spanned(
            op,
            format!(
                "Expected one of `<`, `<=`, `>`, `>=`, `==`, `!=`, found: `{}`",
                op.to_token_stream()
            ),
        );
        return Err(err);
    }

    match &*tree.left {
        Expr::Binary(rest) if is_comparison(rest) => {
            let lhs = rest.right.clone();
            let rest = match *std::mem::replace(&mut tree.left, lhs) {
                Expr::Binary(expr) => expr,
                _ => unreachable!(),
            };
            container.push(tree.into());
            flatten_tree(rest, container)
        }
        _ => {
            container.push(tree.into());
            Ok(())
        }
    }
}

/// `build_conjunction_tree` turns a list of `Expr`s into
/// a tree of conjunctions where the last element of the list
/// is the root of the resulting tree.
///
/// For example, this list of four expressions
///
/// ```nocompile
/// [e4, e3, e2, e1]
/// ```
///
/// becomes this tree of conjunctions:
///
/// ```nocompile
///     &&
///    /  \
///   e1   &&
///       /  \
///      e2   &&
///          /  \
///         e3   e4
/// ```
fn build_conjunction_tree(mut exprs: Vec<Expr>) -> Expr {
    let expr = exprs
        .pop()
        .expect("need at least one expression to build tree");

    if exprs.is_empty() {
        expr
    } else {
        new_conjuction(expr, build_conjunction_tree(exprs)).into()
    }
}

/// `new_conjunction` returns a new binary expression of the form
/// `left && right`.
fn new_conjuction(left: Expr, right: Expr) -> ExprBinary {
    let (left_span, right_span) = (left.span(), right.span());

    ExprBinary {
        attrs: vec![],
        left: Box::new(left),
        op: syn::BinOp::And(Token![&&]([left_span, right_span])),
        right: Box::new(right),
    }
}
