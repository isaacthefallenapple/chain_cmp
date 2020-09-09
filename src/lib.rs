use proc_macro::TokenStream;
use quote::ToTokens;
use syn::{parse_macro_input, spanned::Spanned, Expr, ExprBinary, Token};

#[proc_macro]
pub fn chomp(tokens: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(tokens as ExprBinary);
    match cmp_tree_to_conjunction_tree(ast) {
        Ok(expr) => expr.into_token_stream(),
        Err(err) => err.to_compile_error(),
    }
    .into()
}

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

fn is_comparison(expr: &ExprBinary) -> bool {
    is_comparison_op(&expr.op)
}

fn flatten_tree(tree: ExprBinary, container: &mut Vec<Expr>) -> Result<(), syn::Error> {
    let ExprBinary {
        right,
        op,
        left: rest,
        ..
    } = tree;

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

    return match *rest {
        Expr::Binary(rest) if is_comparison(&rest) => {
            let expr = ExprBinary {
                attrs: vec![],
                right,
                op,
                left: rest.right.clone(),
            };
            container.push(expr.into());
            flatten_tree(rest, container)
        }
        _ => {
            let expr = ExprBinary {
                attrs: vec![],
                right,
                op,
                left: rest,
            };
            container.push(expr.into());
            Ok(())
        }
    };
}

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

fn new_conjuction(left: Expr, right: Expr) -> ExprBinary {
    let (left_span, right_span) = (left.span(), right.span());

    ExprBinary {
        attrs: vec![],
        left: Box::new(left),
        op: syn::BinOp::And(Token![&&]([left_span, right_span])),
        right: Box::new(right),
    }
}
