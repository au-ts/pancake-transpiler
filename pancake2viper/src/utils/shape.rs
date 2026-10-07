use std::fmt::Display;

use pest::Parser as _;
use pest_derive::Parser;

use super::errors::ShapeError;
use super::traits::ToType;
#[cfg(feature = "viper")]
use super::{traits::ToViperType, ViperEncodeCtx};
use crate::ir::Type;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Shape {
    Simple,
    Nested(Vec<Self>),
}

impl Shape {
    pub fn len(&self) -> usize {
        match self {
            Self::Simple => 1,
            Self::Nested(l) => l.iter().map(Self::len).sum(),
        }
    }

    pub fn is_simple(&self) -> bool {
        match self {
            Self::Simple => true,
            Self::Nested(_) => false,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn access(&self, idx: usize) -> Result<(usize, usize), ShapeError> {
        match self {
            Self::Simple => Err(ShapeError::SimpleShapeAccess(self.clone())),
            Self::Nested(elems) => {
                assert!(idx < elems.len());
                let size = elems[idx].len();
                let offset = elems.iter().take(idx).map(Self::len).sum();
                Ok((offset, size))
            }
        }
    }
}

#[cfg(feature = "viper")]
impl<'a> ToViperType<'a> for Shape {
    fn to_viper_type(&self, ctx: &ViperEncodeCtx<'a>) -> viper::Type<'a> {
        match self {
            Self::Simple => ctx.ast.int_type(),
            Self::Nested(_) => ctx.ast.seq_type(ctx.ast.int_type()),
        }
    }
}

impl Display for Shape {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Simple => write!(f, "1"),
            Self::Nested(inner) => {
                write!(
                    f,
                    "{{{}}}",
                    inner
                        .iter()
                        .map(Self::to_string)
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Shape::{self, *};

    #[test]
    fn parse_simple() {
        let s = "1";
        let shape = Shape::parse(s).unwrap();
        assert_eq!(shape, Shape::Simple);
        assert_eq!(shape.len(), 1);
    }

    #[test]
    fn parse_nested1() {
        let s = "<1>";
        let shape = Shape::parse(s).unwrap();
        assert_eq!(shape, Nested(vec![Simple]));
        assert_eq!(shape.len(), 1);
    }

    #[test]
    fn parse_nested2() {
        let s = "<1,<1,1>>";
        let shape = Shape::parse(s).unwrap();
        assert_eq!(shape, Nested(vec![Simple, Nested(vec![Simple, Simple])]));
        assert_eq!(shape.len(), 3);
    }

    #[test]
    fn parse_nested3() {
        let s = "<1,<1,1,<1>>,<1,1>,1>";
        let shape = Shape::parse(s).unwrap();
        assert_eq!(
            shape,
            Nested(vec![
                Simple,
                Nested(vec![Simple, Simple, Nested(vec![Simple])]),
                Nested(vec![Simple, Simple]),
                Simple
            ])
        );
        assert_eq!(shape.len(), 7);
    }
}

#[derive(Parser)]
#[grammar = "src/pancake/shape.pest"]
struct ShapeParser;

impl Shape {
    fn parse_term(pair: pest::iterators::Pair<'_, Rule>) -> anyhow::Result<Self> {
        let n = pair.as_str().parse::<u64>()?;
        Ok(if n == 1 {
            Self::Simple
        } else {
            Self::Nested((0..n).map(|_| Self::Simple).collect())
        })
    }

    pub fn parse(s: &str) -> anyhow::Result<Self> {
        let top = ShapeParser::parse(Rule::top, s)?
            .next()
            .unwrap()
            .into_inner()
            .next()
            .unwrap();
        match top.as_rule() {
            Rule::term => Self::parse_term(top),
            Rule::shape => {
                let inner = top
                    .into_inner()
                    .map(|pair| match pair.as_rule() {
                        Rule::term => Self::parse_term(pair),
                        Rule::shape => Self::parse(pair.as_str()),
                        _ => unreachable!(),
                    })
                    .collect::<Result<Vec<_>, _>>();
                Ok(Shape::Nested(inner?))
            }
            _ => unreachable!(),
        }
    }
}

impl ToType for Shape {
    fn to_type(&self, _is_annot: bool) -> Type {
        match self {
            Self::Simple => Type::Int,
            Self::Nested(inner) => Type::Struct(inner.clone()),
        }
    }
}
