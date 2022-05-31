use crate::span::Span;

pub trait ReportError {
    fn report(&self) -> ariadne::Report<Span>;
}
