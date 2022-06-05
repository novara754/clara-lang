use std::ops::Range;

use serde_json::json;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FileId(pub usize);

pub trait Spanned {
    fn span(&self) -> Span;
}

#[derive(Debug, Copy, Clone)]
pub struct Span {
    pub source: FileId,
    pub start: usize,
    pub len: usize,
}

impl Span {
    pub fn new(source: FileId, start: usize, len: usize) -> Self {
        Self { source, start, len }
    }

    pub fn to(self, other: Self) -> Self {
        Self {
            start: self.start,
            len: other.start - self.start + other.len,
            ..self
        }
    }

    pub fn json(self) -> serde_json::Value {
        json!({
            "start": self.start,
            "len": self.len,
        })
    }
}

impl Spanned for Span {
    fn span(&self) -> Span {
        *self
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start..(span.start + span.len)
    }
}
