use crate::parser::{Literal, ParsedExpression, ParsedProgram, ParsedStatement};

pub fn generate_c(w: &mut impl std::io::Write, program: &ParsedProgram) -> std::io::Result<()> {
    writeln!(w, "#include <lib.h>")?;

    for func in &program.functions {
        if func.name == "main" {
            writeln!(w, "int main()\n{{")?;
        } else {
            writeln!(w, "void {}()\n{{", func.name)?;
        }

        for stmt in &func.body.statements {
            write!(w, "\t")?;
            write_stmt(w, stmt)?;
        }

        writeln!(w, "}}")?;
    }

    Ok(())
}

fn write_stmt(w: &mut impl std::io::Write, stmt: &ParsedStatement) -> std::io::Result<()> {
    match stmt {
        ParsedStatement::Expression(expr) => write_expr(w, expr)?,
    }
    writeln!(w, ";")
}

fn write_expr(w: &mut impl std::io::Write, expr: &ParsedExpression) -> std::io::Result<()> {
    match expr {
        ParsedExpression::FunctionCall(func_call) => {
            write!(w, "{}(", func_call.name)?;
            for (i, arg) in func_call.args.iter().enumerate() {
                if i > 0 {
                    write!(w, ",")?;
                }
                write_expr(w, arg)?;
            }
            write!(w, ")")
        }
        ParsedExpression::Literal(literal) => match literal {
            Literal::String(string, _) => write!(
                w,
                "(StringSlice){{ .data = \"{}\", .length = {} }}",
                string,
                string.len()
            ),
            Literal::Int(int, _) => write!(w, "{}", int),
        },
        ParsedExpression::Invalid => {
            panic!();
        }
    }
}
