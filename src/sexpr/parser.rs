use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "sexpr/sexpr.pest"]
pub struct SExpr;

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Parser;

    include!("../../tests/atoms.in");

    #[test]
    fn parse_atoms() {
        for (i, (value, rule)) in ATOMS.iter().enumerate() {
            let i = i + 1;
            let pairs = SExpr::parse(Rule::sexpr, value);
            let pairs = match pairs {
                Ok(pairs) => pairs,
                Err(e) => panic!("unsuccessful parse {}: {}", i, e),
            };

            for pair in pairs {
                if pair.as_rule() == Rule::EOI {
                    continue;
                }
                assert_eq!(pair.as_rule(), *rule, "atom value {}", i);
                if i > 1000 {
                    println!("Rule:    {:?}", pair.as_rule());
                    println!("Tokens:  {:?}", pair.into_inner());
                    println!();
                }
            }
        }
    }

    include!("../../tests/compounds.in");

    #[test]
    fn parse_compounds() {
        for (i, (input, rule)) in COMPOUNDS.iter().enumerate() {
            let i = i + 1;
            let pairs = SExpr::parse(Rule::sexpr, input);
            let pairs = match pairs {
                Ok(pairs) => pairs,
                Err(e) => panic!("unsuccessful parse {}: {}", i, e),
            };

            for pair in pairs {
                if pair.as_rule() == Rule::EOI {
                    continue;
                }
                assert_eq!(pair.as_rule(), *rule, "compound value {}", i);
                if i > 1000 {
                    println!("Rule:    {:?}", pair.as_rule());
                    println!("Tokens:  {:?}", pair.into_inner());
                    println!();
                }
            }
        }
    }
}
