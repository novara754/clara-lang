use crate::{
    parser::Literal,
    typechecker::{CheckedExpression, CheckedProgram, CheckedStatement},
};

pub fn generate_c(w: &mut impl std::io::Write, program: &CheckedProgram) -> std::io::Result<()> {
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

fn write_stmt(w: &mut impl std::io::Write, stmt: &CheckedStatement) -> std::io::Result<()> {
    match stmt {
        CheckedStatement::Expression(expr) => write_expr(w, expr)?,
        CheckedStatement::LetAssign(name, expr) => {
            write!(w, "{} {} = ", expr.ttype().to_c(), name)?;
            write_expr(w, expr)?;
        }
    }
    writeln!(w, ";")
}

fn write_expr(w: &mut impl std::io::Write, expr: &CheckedExpression) -> std::io::Result<()> {
    write!(w, "(")?;
    match expr {
        CheckedExpression::FunctionCall(func_call) => {
            write!(w, "{}(", func_call.name)?;
            for (i, arg) in func_call.args.iter().enumerate() {
                if i > 0 {
                    write!(w, ",")?;
                }
                write_expr(w, arg)?;
            }
            write!(w, ")")?;
        }
        CheckedExpression::Literal(literal, _type) => match literal {
            Literal::String(string, _) => {
                write!(
                    w,
                    "(string){{ .data = \"{}\", .length = {} }}",
                    string,
                    string.len()
                )?;
            }
            Literal::Int(int, _) => {
                write!(w, "{}", int)?;
            }
        },
        CheckedExpression::Variable(name, _type) => write!(w, "{}", name)?,
    }
    write!(w, ")")?;
    Ok(())
}
