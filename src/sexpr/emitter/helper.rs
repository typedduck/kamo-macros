use pest::iterators::Pair;

use crate::sexpr::{error::Error, parser::Rule};

pub fn get_escape(pair: Pair<'_, Rule>) -> Result<Option<char>, Error<'_>> {
    if matches!(pair.as_rule(), Rule::symbol_escape | Rule::string_escape) {
        let mut pairs = pair.into_inner();
        let pair = pairs.next().expect("inner pair missing");

        let c = match pair.as_rule() {
            Rule::character_code => {
                let code = pair.as_str();
                let code = u32::from_str_radix(code, 16).expect("invalid character code");

                char::from_u32(code).ok_or_else(|| Error::InvalidCodePoint(pair.as_span(), code))?
            }
            Rule::symbol_mnemonic => match pair.as_str() {
                "|" => '|',
                "\"" => '"',
                "\\" => '\\',
                "a" => '\x07',
                "b" => '\x08',
                "t" => '\t',
                "n" => '\n',
                "r" => '\r',
                _ => return Err(Error::ExpectedEscape(pair.as_span())),
            },
            Rule::string_mnemonic => match pair.as_str() {
                "\"" => '"',
                "\\" => '\\',
                "a" => '\x07',
                "b" => '\x08',
                "t" => '\t',
                "n" => '\n',
                "r" => '\r',
                _ => return Err(Error::ExpectedEscape(pair.as_span())),
            },
            Rule::string_ilws => return Ok(None),
            _ => unreachable!(),
        };
        assert!(pairs.next().is_none(), "too many inner pairs");
        Ok(Some(c))
    } else {
        Err(Error::ExpectedEscape(pair.as_span()))
    }
}
