use crate::typechecker::{
    CheckedBlock, CheckedExpression, CheckedLiteral, CheckedProgram, CheckedStatement,
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

    for r#struct in &program.structs {
        write!(w, "typedef struct {}", r#struct.name)?;
        if !r#struct.is_opaque {
            writeln!(w, "{{")?;
            for (field_name, field_type) in &r#struct.fields {
                writeln!(w, "\t{} {};", field_type.to_c(), field_name)?;
            }
            write!(w, "}} {}", r#struct.name)?;
        }
        writeln!(w, ";\n")?;
    }

    for func in &program.functions {
        if func.name == "main" {
            writeln!(w, "int main()")?;
        } else {
            writeln!(w, "void {}()", func.name)?;
        }

        write_block(w, 0, &func.body)?;
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
            write_block(w, indent, &while_loop.body)?;
            write_indented!(w, indent, "}}")?;
        }
        CheckedStatement::IfElse(if_else) => {
            write_indented!(w, indent, "if (")?;
            write_expr(w, &if_else.condition)?;
            writeln!(w, ")")?;
            write_block(w, indent, &if_else.if_body)?;
            write_indented!(w, indent, "else\n")?;
            if let Some(ref else_body) = if_else.else_body {
                write_block(w, indent, else_body)?;
            }
        }
    }
    writeln!(w, ";")
}

fn write_block(
    w: &mut impl std::io::Write,
    indent: usize,
    block: &CheckedBlock,
) -> std::io::Result<()> {
    write_indented!(w, indent, "{{\n")?;
    for stmt in &block.statements {
        write_stmt(w, indent + 1, stmt)?;
    }
    write_indented!(w, indent, "}}\n")
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
        CheckedExpression::Literal(literal) => match literal {
            CheckedLiteral::String(string, _) => {
                write!(
                    w,
                    "(string){{ .data = \"{}\", .length = {} }}",
                    string,
                    string.len()
                )?;
            }
            CheckedLiteral::Int(int, _) => {
                write!(w, "{}", int)?;
            }
            CheckedLiteral::Bool(bool_value, _) => {
                write!(w, "{}", bool_value)?;
            }
            CheckedLiteral::Struct(struct_literal, _) => {
                write!(w, "({}){{", struct_literal.name)?;
                for (field_name, field_value) in &struct_literal.fields {
                    write!(w, ".{} = ", field_name)?;
                    write_expr(w, field_value)?;
                    write!(w, ", ")?;
                }
                write!(w, "}}")?;
            }
        },
        CheckedExpression::Variable(name, _type) => write!(w, "{}", name)?,
        CheckedExpression::BinaryOp(lhs, rhs, op, _type) => {
            write_expr(w, lhs)?;
            write!(w, " {} ", op.to_c())?;
            write_expr(w, rhs)?;
        }
        CheckedExpression::FieldAccess(field_access, _type) => {
            write_expr(w, &field_access.object)?;
            write!(w, ".{}", field_access.field_name)?;
        }
    }
    write!(w, ")")?;
    Ok(())
}
