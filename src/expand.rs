use pest::Parser;
use proc_macro2::TokenStream;
use quote::quote;
use syn::Token;

use crate::sexpr::{
    emitter::emit_datum,
    error::Error,
    parser::{Rule, SExpr},
};

pub fn sexpr(input: TokenStream) -> TokenStream {
    let sexpr = syn::parse2::<SExprMacro>(input);
    let sexpr = match sexpr {
        Ok(sexpr) => sexpr,
        Err(err) => return err.to_compile_error(),
    };
    let input = sexpr.input.value();
    let pairs = SExpr::parse(Rule::sexpr, &input);
    let pairs = match pairs {
        Ok(pairs) => pairs,
        Err(e) => return syn::Error::new(sexpr.input.span(), e).to_compile_error(),
    };
    let mut pairs = pairs.into_iter();
    let mut out = TokenStream::new();

    // First pair is the datum
    if let Some(pair) = pairs.next() {
        if let Err(err) = emit_datum(sexpr.mutator, pair, &mut out) {
            return syn::Error::new(sexpr.input.span(), err).to_compile_error();
        }
    } else {
        return syn::Error::new(sexpr.input.span(), Error::EmptySExpr).to_compile_error();
    }

    // Second pair is the end of input
    if let Some(pair) = pairs.next() {
        if pair.as_rule() != Rule::EOI {
            return syn::Error::new(sexpr.input.span(), Error::ExpectedEndOfInput)
                .to_compile_error();
        }
    } else {
        return syn::Error::new(sexpr.input.span(), Error::ExpectedEndOfInput).to_compile_error();
    }

    out
}

pub fn sexpr_file(input: TokenStream) -> TokenStream {
    let sexpr = syn::parse2::<SExprMacro>(input);
    let sexpr = match sexpr {
        Ok(sexpr) => sexpr,
        Err(err) => return err.to_compile_error(),
    };
    let input = sexpr.input.value();
    let input = match std::fs::read_to_string(input) {
        Ok(input) => input,
        Err(err) => return syn::Error::new(sexpr.input.span(), err).to_compile_error(),
    };
    let pairs = SExpr::parse(Rule::script, &input);
    let pairs = match pairs {
        Ok(pairs) => pairs,
        Err(e) => return syn::Error::new(sexpr.input.span(), e).to_compile_error(),
    };
    let mut exprs = vec![];

    for pair in pairs {
        match pair.as_rule() {
            Rule::shadowed => continue,
            Rule::EOI => break,
            _ => {
                let mut expr = TokenStream::new();

                if let Err(err) = emit_datum(sexpr.mutator.to_owned(), pair, &mut expr) {
                    return syn::Error::new(sexpr.input.span(), err).to_compile_error();
                }
                exprs.push(expr);
            }
        }
    }
    quote! { [#(#exprs),*] }
}

pub fn sexpr_script(input: TokenStream) -> TokenStream {
    let sexpr = syn::parse2::<SExprMacro>(input);
    let sexpr = match sexpr {
        Ok(sexpr) => sexpr,
        Err(err) => return err.to_compile_error(),
    };
    let input = sexpr.input.value();
    let pairs = SExpr::parse(Rule::script, &input);
    let pairs = match pairs {
        Ok(pairs) => pairs,
        Err(e) => return syn::Error::new(sexpr.input.span(), e).to_compile_error(),
    };
    let mut exprs = vec![];

    for pair in pairs {
        match pair.as_rule() {
            Rule::shadowed => continue,
            Rule::EOI => break,
            _ => {
                let mut expr = TokenStream::new();

                if let Err(err) = emit_datum(sexpr.mutator.to_owned(), pair, &mut expr) {
                    return syn::Error::new(sexpr.input.span(), err).to_compile_error();
                }
                exprs.push(expr);
            }
        }
    }
    quote! { [#(#exprs),*] }
}

struct SExprMacro {
    mutator: Option<syn::Ident>,
    input: syn::LitStr,
}

impl syn::parse::Parse for SExprMacro {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mutator = if input.peek(syn::Ident) && input.peek2(Token![,]) {
            let mutator = Some(input.parse::<syn::Ident>()?);

            input.parse::<Token![,]>()?;
            mutator
        } else {
            None
        };
        let input = input.parse::<syn::LitStr>()?;

        Ok(Self { mutator, input })
    }
}

#[cfg(test)]
mod tests {
    use quote::quote;

    use super::*;

    #[test]
    fn sexpr_success() {
        let sexprs = [
            (
                quote! { m, "#(1)" },
                quote! { Value::new_vector(m.clone(), vec![Value::new_int(1i64)]) },
            ),
            (
                quote! { m, " #( 1 ) " },
                quote! { Value::new_vector(m.clone(), vec![Value::new_int(1i64)]) },
            ),
            (
                quote! { m, r#"
                ; line comment
                #( 1 #| value comment |# ) ; inline comment
                #| block comment |#
                ; last line comment"# },
                quote! { Value::new_vector(m.clone(), vec![Value::new_int(1i64)]) },
            ),
            (
                quote! { m, r#"
                #| ; line comment
                #| nested comment |#
                |#
                #( 1 )
                "# },
                quote! { Value::new_vector(m.clone(), vec![Value::new_int(1i64)]) },
            ),
        ];

        for (i, (input, expected)) in sexprs.iter().enumerate() {
            let i = i + 1;
            let output = sexpr(input.clone());

            assert_eq!(
                output.to_string(),
                expected.to_string(),
                "sexpr #{} failed",
                i
            );
        }
    }

    #[test]
    fn sexpr_file_success() {
        let sexprs = [
            (
                quote! { m, "tests/sexpr/empty.scm" },
                quote! { [] },
            ),
            (
                quote! { m, "tests/sexpr/values.scm" },
                quote! { [Value::new_nil(), Value::new_int(100i64), Value::new_bool(true)] },
            ),
        ];

        for (i, (input, expected)) in sexprs.iter().enumerate() {
            let i = i + 1;
            let output = sexpr_file(input.clone());

            assert_eq!(
                output.to_string(),
                expected.to_string(),
                "sexpr_file #{} failed",
                i
            );
        }
    }

    #[test]
    fn sexpr_script_success() {
        let sexprs = [
            (
                quote! { m, r#"
                ; line comment
                #( 1 #| value comment |# ) ; inline comment
                #| block comment |#
                ; last line comment"# },
                quote! { [Value::new_vector(m.clone(), vec![Value::new_int(1i64)])] },
            ),
            (
                quote! { m, r#"
                #| ; line comment
                #| nested comment |#
                |#
                #( 1 )
                "# },
                quote! { [Value::new_vector(m.clone(), vec![Value::new_int(1i64)])] },
            ),
        ];

        for (i, (input, expected)) in sexprs.iter().enumerate() {
            let i = i + 1;
            let output = sexpr_script(input.clone());

            assert_eq!(
                output.to_string(),
                expected.to_string(),
                "sexpr_script #{} failed",
                i
            );
        }
    }
}
