use serde_json::json;

pub trait Spanned {
    fn span(&self) -> Span;
}

#[derive(Debug, Copy, Clone)]
pub struct Span {
    pub start: usize,
    pub len: usize,
}

impl Span {
    pub fn new(start: usize, len: usize) -> Self {
        Self { start, len }
    }

    pub fn to(self, other: Self) -> Self {
        Self {
            start: self.start,
            len: other.start - self.start + other.len,
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

impl ariadne::Span for Span {
    type SourceId = ();

    fn source(&self) -> &Self::SourceId {
        &()
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.start + self.len
    }
}
