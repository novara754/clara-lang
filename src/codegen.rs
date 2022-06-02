use crate::{
    parser::Literal,
    typechecker::{CheckedExpression, CheckedProgram, CheckedStatement},
};

macro_rules! write_indented {
    ($w:expr, $indent:expr) => {{
        for _ in 0..$indent {
            let _ = $w.write(&[b'\t'])?;
        }
        ::std::io::Result::<()>::Ok(())
    }};

    ($w:expr, $indent:expr, $($args:tt)*) => {{
        write_indented!($w, $indent)?;
        write!($w, $($args)*)
    }};
}

pub fn generate_c(w: &mut impl std::io::Write, program: &CheckedProgram) -> std::io::Result<()> {
    writeln!(w, "#include <lib.h>")?;

    for func in &program.functions {
        if func.name == "main" {
            writeln!(w, "int main()\n{{")?;
        } else {
            writeln!(w, "void {}()\n{{", func.name)?;
        }

        for stmt in &func.body.statements {
            write_stmt(w, 1, stmt)?;
        }

        writeln!(w, "}}")?;
    }

    Ok(())
}

fn write_stmt(
    w: &mut impl std::io::Write,
    indent: usize,
    stmt: &CheckedStatement,
) -> std::io::Result<()> {
    match stmt {
        CheckedStatement::Expression(expr) => {
            write_indented!(w, indent)?;
            write_expr(w, expr)?;
        }
        CheckedStatement::LetAssign(name, expr) => {
            write_indented!(w, indent, "{} {} = ", expr.ttype().to_c(), name)?;
            write_expr(w, expr)?;
        }
        CheckedStatement::WhileLoop(while_loop) => {
            write_indented!(w, indent, "while (")?;
            write_expr(w, &while_loop.condition)?;
            writeln!(w, ") {{")?;
            for stmt in &while_loop.body.statements {
                write_stmt(w, indent + 1, stmt)?;
            }
            write_indented!(w, indent, "}}\n")?;
        }
    }
    write_indented!(w, indent, ";\n")
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
            Literal::Bool(bool_value, _) => {
                write!(w, "{}", bool_value)?;
            }
        },
        CheckedExpression::Variable(name, _type) => write!(w, "{}", name)?,
        CheckedExpression::BinaryOp(lhs, rhs, op, _type) => {
            write_expr(w, lhs)?;
            write!(w, " {} ", op.to_c())?;
            write_expr(w, rhs)?;
        }
    }
    write!(w, ")")?;
    Ok(())
}
