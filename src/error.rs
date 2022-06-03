use crate::span::Span;

pub trait ReportError {
    fn report(&self) -> ariadne::Report<Span>;
}

pub trait JsonError {
    fn json(&self) -> serde_json::Value;
}
