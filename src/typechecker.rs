use std::collections::{hash_map::Entry, HashMap};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use serde_json::json;

use crate::{
    parser::{
        CompareOperation, FunctionParameter, Literal, MathOperation, ParsedBlock, ParsedExpression,
        ParsedFunction, ParsedProgram, ParsedStatement, ParsedStruct,
    },
    span::{Span, Spanned},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    UserDefined(String),
    Pointer(Box<Type>, bool),
    GenericInt,
    String,
    Int,
    Bool,
    Unit,
    CChar,
    CInt,
    GenericEmptyArray,
    Array(Box<Type>, usize),
    Incomplete,
}

impl Type {
    pub fn element_type(&self) -> Option<Type> {
        match self {
            Type::Pointer(elem_type, _) => Some(*elem_type.clone()),
            _ => None,
        }
    }

    pub fn is_mut_pointer(&self) -> bool {
        match self {
            Type::Pointer(_, is_mut) => *is_mut,
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, Type::Pointer(_, _))
    }

    pub fn is_integer_type(&self) -> bool {
        matches!(self, Self::GenericInt | Self::CInt | Self::Int)
    }

    pub fn matches(&self, other: &Type) -> bool {
        if self == other {
            true
        } else {
            match (self, other) {
                (Self::GenericInt, _) => other.is_integer_type(),
                (_, Self::GenericInt) => self.is_integer_type(),
                (Self::Pointer(this, this_mut), Self::Pointer(other, other_mut)) => {
                    this_mut == other_mut && this.matches(other)
                }
                _ => false,
            }
        }
    }

    pub fn from_string(typename: &str) -> Self {
        match typename {
            "string" => Type::String,
            "int" => Type::Int,
            "bool" => Type::Bool,
            "c_char" => Type::CChar,
            "c_int" => Type::CInt,
            "()" => Type::Unit,
            _ => Type::UserDefined(typename.to_string()),
        }
    }

    pub fn to_str(&self) -> String {
        match self {
            Self::GenericInt => "{integer}".to_string(),
            Self::Pointer(ty, true) => format!("->mut {}", ty.to_str()),
            Self::Pointer(ty, false) => format!("->{}", ty.to_str()),
            Self::String => "string".to_string(),
            Self::Int => "int".to_string(),
            Self::Bool => "bool".to_string(),
            Self::Unit => "unit".to_string(),
            Self::CChar => "c_char".to_string(),
            Self::CInt => "c_int".to_string(),
            Self::Incomplete => "incomplete type".to_string(),
            Self::UserDefined(name) => name.clone(),
            Self::GenericEmptyArray => "[_; 0]".to_string(),
            Self::Array(elem_type, size) => format!("[{}; {size}]", elem_type.to_str()),
        }
    }
}

pub enum TypeCheckError {
    WrongNumArgs(Span, usize, usize),
    WrongArgType(Span, Type, Type),
    WrongConditionType(Span, Type),
    UnknownFunction(String, Span),
    UnknownVariable(String, Option<String>, Span),
    BinaryOpMismatch(Type, Type, Span, Span),
    UnknownType(String, Span),
    OpaqueStructFieldAccess(Type, Span),
    FieldAccessInvalidField(Type, String, Span),
    ObjectIsNotAStruct(Type, Span),
    StructFieldWrongType(String, String, Type, Type, Span),
    StructMissingField(String, String, Span),
    StructSuperfluousField(String, String, Span),
    InvalidReturnType(Type, Type, Span),
    DuplicateParameterName(String, Span),
    DuplicateVariableName(String, Span),
    DuplicateFuncStructName(String, Span),
    WrongElementTypeInArray(Type, Type, Span),
    InvalidIterableInForIn(Type, Span),
    AssignmentTypeMismatch(Type, Type, Span, Span),
    AssignmentToNonLValue(Span),
    AssignmentToImmutable(Span),
    MutablePointerToImmutableVariable(Span),
    DerefNonPointer(Type, Span),
}

impl TypeCheckError {
    pub fn report(&self) -> Diagnostic<usize> {
        match *self {
            Self::WrongNumArgs(span, actual, expected) => Diagnostic::error()
                .with_message("incorrect number of arguments to function call")
                .with_labels(vec![Label::primary(span.source.0, span)])
                .with_notes(vec![format!(
                    "function expects {} arguments but {} were provided",
                    expected, actual
                )]),
            Self::WrongArgType(span, ref actual, ref expected) => Diagnostic::error()
                .with_message("incorrect argument type in function call")
                .with_labels(vec![Label::primary(span.source.0, span).with_message(
                    format!(
                        "argument has type {} but function expects {}",
                        actual.to_str(),
                        expected.to_str()
                    ),
                )]),
            Self::UnknownFunction(ref function_name, span) => Diagnostic::error()
                .with_message(format!("reference to unknown function `{}`", function_name))
                .with_labels(vec![Label::primary(span.source.0, span)]),
            Self::UnknownVariable(ref variable_name, ref function_name, span) => {
                let report = Diagnostic::error();
                if let Some(function_name) = function_name {
                    report.with_message(format!(
                        "reference to unknown variable `{}` in function `{}`",
                        variable_name, function_name
                    ))
                } else {
                    report
                        .with_message(format!("reference to unknown variable `{}`", variable_name))
                }
                .with_labels(vec![Label::primary(span.source.0, span)])
            }
            Self::WrongConditionType(span, ref actual) => Diagnostic::error()
                .with_message("incorrect type in condition")
                .with_labels(vec![Label::primary(span.source.0, span)
                    .with_message(format!("expression has type {}", actual.to_str()))])
                .with_notes(vec![format!(
                    "expression in condition has to be of type {}",
                    Type::Bool.to_str()
                )]),
            Self::BinaryOpMismatch(ref lhs_type, ref rhs_type, lhs_span, rhs_span) => {
                Diagnostic::error()
                    .with_message("type mismatch in binary operator")
                    .with_labels(vec![
                        Label::primary(lhs_span.source.0, lhs_span)
                            .with_message(format!("left operand has type {}", lhs_type.to_str())),
                        Label::primary(rhs_span.source.0, rhs_span)
                            .with_message(format!("right operand has type {}", rhs_type.to_str())),
                    ])
                    .with_notes(vec![
                        "Both sides of the operator need to have the same type".to_string(),
                    ])
            }
            Self::UnknownType(ref type_name, span) => Diagnostic::error()
                .with_message(format!("reference to unknown type `{}`", type_name))
                .with_labels(vec![
                    Label::primary(span.source.0, span).with_message("type is referenced here")
                ]),
            Self::OpaqueStructFieldAccess(ref object_type, span) => Diagnostic::error()
                .with_message("field access on opaque struct")
                .with_labels(vec![Label::primary(span.source.0, span).with_message(
                    format!(
                        "cannot access fields of opaque struct type {}",
                        object_type.to_str()
                    ),
                )]),
            Self::FieldAccessInvalidField(ref object_type, ref field_name, span) => {
                Diagnostic::error()
                    .with_message("invalid struct field")
                    .with_labels(vec![Label::primary(span.source.0, span).with_message(
                        format!(
                            "struct type {} has no field by the name of `{}`",
                            object_type.to_str(),
                            field_name
                        ),
                    )])
            }
            Self::ObjectIsNotAStruct(ref object_type, span) => Diagnostic::error()
                .with_message("object is not a struct")
                .with_labels(vec![Label::primary(span.source.0, span).with_message(
                    format!(
                        "trying to access field on non-struct type {}",
                        object_type.to_str()
                    ),
                )]),
            Self::StructMissingField(ref struct_name, ref missing_field_name, span) => {
                Diagnostic::error()
                    .with_message("missing field in struct literal")
                    .with_labels(vec![Label::primary(span.source.0, span).with_message(
                        format!(
                            "missing field `{}` in literal for struct `{}`",
                            missing_field_name, struct_name
                        ),
                    )])
            }
            Self::StructFieldWrongType(
                ref struct_name,
                ref field_name,
                ref actual,
                ref expected,
                span,
            ) => Diagnostic::error()
                .with_message("wrong type for field in struct literal")
                .with_labels(vec![Label::primary(span.source.0, span).with_message(
                    format!(
                        "expression has type {} but struct expects type {}",
                        actual.to_str(),
                        expected.to_str()
                    ),
                )])
                .with_notes(vec![format!(
                    "Field `{}` on struct `{}` has type `{}`",
                    field_name,
                    struct_name,
                    expected.to_str()
                )]),
            Self::StructSuperfluousField(ref struct_name, ref field_name, span) => {
                Diagnostic::error()
                    .with_message("incorrect field in struct literal")
                    .with_labels(vec![Label::primary(span.source.0, span).with_message(
                        format!("no field `{}` on struct `{}`", field_name, struct_name),
                    )])
            }
            Self::InvalidReturnType(ref actual, ref expected, span) => Diagnostic::error()
                .with_message("type of return value does not match expected return type")
                .with_labels(vec![Label::primary(span.source.0, span).with_message(
                    format!(
                        "expression has type `{}` but function expected type `{}`",
                        actual.to_str(),
                        expected.to_str(),
                    ),
                )]),
            Self::DuplicateParameterName(ref name, span) => Diagnostic::error()
                .with_message("duplicate parameter name in function")
                .with_labels(vec![Label::primary(span.source.0, span).with_message(
                    format!("parameter name `{name}` has already been used in this function"),
                )]),
            Self::DuplicateVariableName(ref name, span) => Diagnostic::error()
                .with_message("duplicate variable name in function")
                .with_labels(vec![Label::primary(span.source.0, span).with_message(
                    format!("variable name `{name}` has already been used in this function"),
                )]),
            Self::DuplicateFuncStructName(ref name, span) => Diagnostic::error()
                .with_message("duplicate function or struct name")
                .with_labels(vec![Label::primary(span.source.0, span).with_message(
                    format!("function or struct name `{name}` has already been used"),
                )]),
            Self::WrongElementTypeInArray(ref actual, ref expected, span) => Diagnostic::error()
                .with_message("wrong type for element in array literal")
                .with_labels(vec![Label::primary(span.source.0, span).with_message(
                    format!(
                        "expression has type `{}` but array expected type `{}`",
                        actual.to_str(),
                        expected.to_str()
                    ),
                )])
                .with_notes(vec![
                    "All elements of an array must have the same type".to_string()
                ]),
            Self::InvalidIterableInForIn(ref actual, span) => Diagnostic::error()
                .with_message("wrong type for iterable in for-in loop")
                .with_labels(vec![Label::primary(span.source.0, span).with_message(
                    format!(
                        "expression has type `{}` expected array type",
                        actual.to_str()
                    ),
                )])
                .with_notes(vec![format!(
                    "For-in loops currently only support arrays as iterables"
                )]),
            Self::AssignmentTypeMismatch(ref actual, ref expected, lhs_span, rhs_span) => {
                Diagnostic::error()
                    .with_message("wrong type in assignment")
                    .with_labels(vec![
                        Label::primary(lhs_span.source.0, lhs_span)
                            .with_message(format!("lhs has type {}", expected.to_str())),
                        Label::secondary(rhs_span.source.0, rhs_span)
                            .with_message(format!("rhs has type {}", actual.to_str())),
                    ])
            }
            Self::AssignmentToNonLValue(span) => Diagnostic::error()
                .with_message("lhs of assignment is not an l-value")
                .with_labels(vec![Label::primary(span.source.0, span)]),
            Self::AssignmentToImmutable(span) => Diagnostic::error()
                .with_message("lhs of assignment is not mutable")
                .with_labels(vec![Label::primary(span.source.0, span)]),
            Self::MutablePointerToImmutableVariable(span) => Diagnostic::error()
                .with_message("cannot create mutable pointer to immutable variable")
                .with_labels(vec![Label::primary(span.source.0, span)]),
            Self::DerefNonPointer(ref ttype, span) => Diagnostic::error()
                .with_message("cannot dereference value of non-pointer type")
                .with_labels(vec![Label::primary(span.source.0, span)
                    .with_message(format!("value has type `{}`", ttype.to_str()))]),
        }
    }
}

impl TypeCheckError {
    pub fn json(&self) -> serde_json::Value {
        match *self {
            Self::WrongNumArgs(span, actual, expected) => json!({
                "message":
                format!(
                    "incorrect number of arguments to function call, expected {} but found {}",
                    expected,
                    actual
                ),
                "span": span.json(),
            }),
            Self::WrongArgType(span, ref actual, ref expected) => json!({
                "message":
                    format!(
                        "incorrect argument type in function call, expected type {} but found type {}",
                        expected.to_str(),
                        actual.to_str()
                    ),
                "span": span.json(),
            }),
            Self::UnknownFunction(ref function_name, span) => json!({
                "message": format!("reference to unknown function `{}`", function_name),
                "span": span.json(),
            }),
            Self::UnknownVariable(ref variable_name, ref function_name, span) => {
                let message = if let Some(functio_name) = function_name {
                    format!(
                        "reference to unknown variable `{}` in function `{}`",
                        variable_name, functio_name
                    )
                } else {
                    format!("unknown variable `{}`", variable_name)
                };
                json!({
                    "message": message,
                    "span": span.json(),
                })
            }
            Self::WrongConditionType(span, ref actual) => json!({
                "message":
                    format!(
                        "wrong type in condition, expected `{}` but found `{}`",
                        Type::Bool.to_str(),
                        actual.to_str()
                    ),
                "span": span.json(),
            }),
            Self::BinaryOpMismatch(ref lhs_type, ref rhs_type, lhs_span, rhs_span) => json!({
                "message":
                    format!(
                        "type mismatch in binary operator, type `{}` on the left and type `{}` on the right",
                        lhs_type.to_str(),
                        rhs_type.to_str()
                    ),
                "span": lhs_span.to(rhs_span).json(),
            }),
            Self::UnknownType(ref type_name, span) => json!({
                "message": format!("reference to unknown type `{}`", type_name),
                "span": span.json(),
            }),
            Self::OpaqueStructFieldAccess(ref object_type, span) => json!({
                "message": format!("field access on opaque struct type `{}`", object_type.to_str()),
                "span": span.json(),
            }),
            Self::FieldAccessInvalidField(ref object_type, ref field_name, span) => json!({
                "message":
                    format!(
                        "struct type `{}` has no field by the name of `{}",
                        object_type.to_str(),
                        field_name
                    ),
                "span": span.json(),
            }),

            Self::ObjectIsNotAStruct(ref object_type, span) => json!({
                "message": format!("object of type `{}` is not a struct", object_type.to_str()),
                "span": span.json(),
            }),
            Self::StructMissingField(ref struct_name, ref missing_field_name, span) => json!({
                "message":
                    format!(
                        "initializer for field `{}` missing in struct literal for struct type `{}`",
                        missing_field_name,
                        struct_name
                    ),
                "span": span.json(),
            }),
            Self::StructFieldWrongType(
                ref _struct_name,
                ref field_name,
                ref actual,
                ref expected,
                span,
            ) => json!({
                "message":
                    format!(
                        "field `{}` has incorrect type, expected `{}` but found `{}`",
                        field_name,
                        expected.to_str(),
                        actual.to_str()
                    ),
                "span": span.json(),
            }),
            Self::StructSuperfluousField(ref struct_name, ref field_name, span) => json!({
                "message":
                    format!(
                        "struct type `{struct_name}` has no field by the name of `{field_name}`"
                    ),
                "span": span.json(),
            }),
            Self::InvalidReturnType(ref actual, ref expected, span) => json!({
                "message":
                    format!(
                        "return value has type `{}` but function expected type `{}`",
                        actual.to_str(),
                        expected.to_str(),
                    ),
                "span": span.json(),
            }),
            Self::DuplicateParameterName(ref name, span) => json!({
                "message": format!("parameter name `{name}` used more than once"),
                "span": span.json(),
            }),
            Self::DuplicateVariableName(ref name, span) => json!({
                "message": format!("variable name `{name}` used more than once in this function"),
                "span": span.json(),
            }),
            Self::DuplicateFuncStructName(ref name, span) => json!({
                "message": format!("function or struct name `{name}` used more than once"),
                "span": span.json(),
            }),
            Self::WrongElementTypeInArray(ref actual, ref expected, span) => json!({
                "message":
                    format!(
                        "element has type `{}` but array expected type `{}`",
                        actual.to_str(),
                        expected.to_str()
                    ),
                    "span": span.json(),
            }),
            Self::InvalidIterableInForIn(ref actual, span) => json!({
                "message":
                    format!(
                        "element has type `{}` but expected array in for-in loop",
                        actual.to_str(),
                    ),
                "span": span.json(),
            }),
            Self::AssignmentTypeMismatch(ref _actual, ref _expected, _lhs_span, rhs_span) => {
                json!({
                    "message": "type mismatch in assignment",
                    "json": rhs_span.json(),
                })
            }
            Self::AssignmentToNonLValue(span) => json!({
                "message": "lhs of assignment is not an l-value",
                "span": span.json(),
            }),
            Self::AssignmentToImmutable(span) => json!({
                "message": "lhs of assignment is not mutable",
                "span": span.json(),
            }),
            Self::MutablePointerToImmutableVariable(span) => json!({
                "message": "cannot create mutable pointer to immutable variable",
                "span": span.json(),
            }),
            Self::DerefNonPointer(ref _ttype, span) => json!({
                "message": "cannot dereference value of non-pointer type",
                "span": span.json(),
            }),
        }
    }
}

#[derive(Debug)]
pub struct CheckedFunctionCall {
    pub name: String,
    pub args: Vec<CheckedExpression>,
    pub ttype: Type,
}

#[derive(Debug)]
pub struct CheckedStructLiteral {
    pub name: String,
    pub fields: HashMap<String, CheckedExpression>,
}

#[derive(Debug)]
pub struct CheckedArrayLiteral {
    pub elements: Vec<CheckedExpression>,
    pub element_type: Option<Type>,
}

#[derive(Debug)]
pub enum CheckedLiteral {
    String(String, Type),
    Int(i32, Type),
    Bool(bool, Type),
    Struct(CheckedStructLiteral, Struct, Type),
    Array(CheckedArrayLiteral, Type),
}

#[derive(Debug)]
pub struct CheckedFieldAccess {
    pub object: Box<CheckedExpression>,
    pub object_is_ptr: bool,
    pub field_name: String,
    pub is_mut: bool,
}

#[derive(Debug)]
pub struct CheckedPointerTo {
    pub inner: Box<CheckedExpression>,
    pub ttype: Type,
}

#[derive(Debug)]
pub struct CheckedDeref {
    pub inner: Box<CheckedExpression>,
    pub ttype: Type,
}

#[derive(Debug)]
pub enum CheckedExpression {
    Literal(CheckedLiteral),
    FunctionCall(CheckedFunctionCall),
    Variable(String, Type, bool),
    CompareOp(
        Box<CheckedExpression>,
        Box<CheckedExpression>,
        CompareOperation,
        Type,
    ),
    MathOp(
        Box<CheckedExpression>,
        Box<CheckedExpression>,
        MathOperation,
        Type,
    ),
    FieldAccess(CheckedFieldAccess, Struct, Type),
    Assignment(Box<CheckedExpression>, Box<CheckedExpression>),
    PointerTo(CheckedPointerTo),
    Deref(CheckedDeref),
}

impl CheckedExpression {
    pub fn ttype(&self) -> Type {
        match self {
            Self::Literal(literal) => match literal {
                CheckedLiteral::String(_, ttype) => ttype,
                CheckedLiteral::Int(_, ttype) => ttype,
                CheckedLiteral::Bool(_, ttype) => ttype,
                CheckedLiteral::Struct(_, _, ttype) => ttype,
                CheckedLiteral::Array(_, ttype) => ttype,
            }
            .clone(),
            Self::FunctionCall(func_call) => func_call.ttype.clone(),
            Self::Variable(_name, ttype, _is_mut) => ttype.clone(),
            Self::CompareOp(_lhs, _rhs, _op, ttype) => ttype.clone(),
            Self::MathOp(_lhs, _rhs, _op, ttype) => ttype.clone(),
            Self::FieldAccess(_field_access, _struct, ttype) => ttype.clone(),
            Self::Assignment(_lhs, _rhs) => Type::Unit,
            Self::PointerTo(pointer_to) => pointer_to.ttype.clone(),
            Self::Deref(deref) => deref.ttype.clone(),
        }
    }

    pub fn is_mut(&self) -> bool {
        match self {
            Self::Variable(_, _, is_mut) => *is_mut,
            Self::FieldAccess(field_acces, _, _) => field_acces.is_mut,
            _ => true,
        }
    }
}

#[derive(Debug)]
pub struct CheckedWhileLoop {
    pub condition: CheckedExpression,
    pub body: CheckedBlock,
}

#[derive(Debug)]
pub struct CheckedIfElse {
    pub condition: CheckedExpression,
    pub if_body: CheckedBlock,
    pub else_body: CheckedBlock,
}

#[derive(Debug)]
pub struct CheckedForInLoop {
    pub elem_var_name: String,
    pub elem_var_type: Type,
    pub index_var: Option<String>,
    pub iterable: CheckedExpression,
    pub body: CheckedBlock,
}

#[derive(Debug)]
pub enum CheckedStatement {
    Expression(CheckedExpression),
    LetAssign(String, CheckedExpression),
    WhileLoop(CheckedWhileLoop),
    IfElse(CheckedIfElse),
    ForInLoop(CheckedForInLoop),
    Return(CheckedExpression),
}

#[derive(Debug)]
pub struct CheckedBlock {
    pub statements: Vec<CheckedStatement>,
}

#[derive(Debug)]
pub struct CheckedFunction {
    pub name: String,
    pub body: CheckedBlock,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Type,
}

#[derive(Debug)]
pub struct CheckedExternFunction {
    pub name: String,
    pub parameters: Vec<FunctionParameter>,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub fields: Vec<(String, Type)>,
    pub is_opaque: bool,
}

impl Struct {
    fn get_field(&self, field_name: &str) -> Option<&Type> {
        for (delcared_field_name, declared_field_type) in &self.fields {
            if delcared_field_name == field_name {
                return Some(declared_field_type);
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct CheckedProgram {
    pub extern_functions: Vec<CheckedExternFunction>,
    pub functions: Vec<CheckedFunction>,
    pub structs: Vec<Struct>,
}

#[derive(Debug)]
struct Function {
    parameters: Vec<FunctionParameter>,
    return_type: Type,
}

#[derive(Debug)]
struct KnownVariable {
    ttype: Type,
    is_mut: bool,
}

#[derive(Debug, Default)]
struct ScopeStack {
    stack: Vec<(Option<String>, HashMap<String, KnownVariable>)>,
}

impl ScopeStack {
    fn push_scope(&mut self, function_name: Option<String>) {
        self.stack.push((function_name, HashMap::default()));
    }

    fn pop_scope(&mut self) {
        self.stack.pop();
    }

    fn add_variable(&mut self, variable_name: &str, ttype: Type, is_mut: bool) -> bool {
        let entry = self
            .stack
            .last_mut()
            .unwrap()
            .1
            .entry(variable_name.to_string());

        match entry {
            Entry::Occupied(_) => true,
            _ => {
                entry.or_insert(KnownVariable { ttype, is_mut });
                false
            }
        }
    }

    fn get_variable_type(&self, variable_name: &str) -> Option<&KnownVariable> {
        for scope in self.stack.iter().rev() {
            if let Some(var) = scope.1.get(variable_name) {
                return Some(var);
            }
        }
        None
    }

    fn get_current_function(&self) -> Option<&str> {
        for scope in self.stack.iter().rev() {
            if let Some(ref function_name) = scope.0 {
                return Some(function_name);
            }
        }
        None
    }
}

#[derive(Debug)]
struct Context<'a> {
    known_structs: HashMap<String, Struct>,
    known_functions: HashMap<String, Function>,
    scope_stack: ScopeStack,
    current_function: Option<&'a ParsedFunction>,
}

impl<'a> Context<'a> {
    fn type_is_defined(&self, ttype: &Type) -> bool {
        match ttype {
            Type::Pointer(ref subtype, _is_mut) => self.type_is_defined(subtype),
            Type::UserDefined(ref name) => self.known_structs.contains_key(name),
            _ => true,
        }
    }
}

pub fn typecheck_program(program: &ParsedProgram) -> (CheckedProgram, Vec<TypeCheckError>) {
    let mut errors = vec![];

    let mut context = Context {
        known_structs: HashMap::new(),
        known_functions: HashMap::new(),
        scope_stack: ScopeStack::default(),
        current_function: None,
    };

    for func in &program.extern_functions {
        let name = func.name.clone();

        if context.known_functions.contains_key(&name) {
            errors.push(TypeCheckError::DuplicateFuncStructName(
                name.clone(),
                func.name_span,
            ));
            continue;
        }

        context.known_functions.insert(
            name,
            Function {
                parameters: func.parameters.clone(),
                return_type: func.return_type.clone(),
            },
        );
    }
    for func in &program.functions {
        let name = func.name.clone();

        if context.known_functions.contains_key(&name) {
            errors.push(TypeCheckError::DuplicateFuncStructName(
                name.clone(),
                func.name_span,
            ));
            continue;
        }

        context.known_functions.insert(
            name,
            Function {
                parameters: func.parameters.clone(),
                return_type: func.return_type.clone(),
            },
        );
    }
    for r#struct in &program.structs {
        match r#struct {
            ParsedStruct::Opaque(name, name_span) => {
                if context.known_functions.contains_key(name) {
                    errors.push(TypeCheckError::DuplicateFuncStructName(
                        name.clone(),
                        *name_span,
                    ));
                    continue;
                }

                context.known_structs.insert(
                    name.clone(),
                    Struct {
                        name: name.clone(),
                        fields: vec![],
                        is_opaque: true,
                    },
                );
            }
            ParsedStruct::Transparent(name, name_span, fields) => {
                if context.known_functions.contains_key(name) {
                    errors.push(TypeCheckError::DuplicateFuncStructName(
                        name.clone(),
                        *name_span,
                    ));
                    continue;
                }

                context.known_structs.insert(
                    name.clone(),
                    Struct {
                        name: name.clone(),
                        fields: fields.clone(),
                        is_opaque: false,
                    },
                );
            }
        };
    }

    let extern_functions = program
        .extern_functions
        .iter()
        .map(|func| {
            let mut seen_param_names: Vec<&str> = vec![];

            for param in &func.parameters {
                if !context.type_is_defined(&param.ttype) {
                    errors.push(TypeCheckError::UnknownType(
                        param.ttype.to_str(),
                        param.type_span,
                    ));
                }

                if seen_param_names.contains(&param.name.as_str()) {
                    errors.push(TypeCheckError::DuplicateParameterName(
                        param.name.clone(),
                        param.name_span,
                    ));
                }

                seen_param_names.push(&param.name);
            }

            if !context.type_is_defined(&func.return_type) {
                errors.push(TypeCheckError::UnknownType(
                    func.return_type.to_str(),
                    func.return_type_span,
                ));
            }

            CheckedExternFunction {
                name: func.name.clone(),
                parameters: func.parameters.clone(),
                return_type: func.return_type.clone(),
            }
        })
        .collect();

    let functions = program
        .functions
        .iter()
        .map(|func| {
            let mut seen_param_names: Vec<&str> = vec![];

            for param in &func.parameters {
                if !context.type_is_defined(&param.ttype) {
                    errors.push(TypeCheckError::UnknownType(
                        param.ttype.to_str(),
                        param.type_span,
                    ));
                }

                if seen_param_names.contains(&param.name.as_str()) {
                    errors.push(TypeCheckError::DuplicateParameterName(
                        param.name.clone(),
                        param.name_span,
                    ));
                }

                seen_param_names.push(&param.name);
            }

            if !context.type_is_defined(&func.return_type) {
                errors.push(TypeCheckError::UnknownType(
                    func.return_type.to_str(),
                    func.return_type_span,
                ));
            }

            context.scope_stack.push_scope(Some(func.name.clone()));
            context.current_function = Some(func);

            for param in &func.parameters {
                context
                    .scope_stack
                    .add_variable(&param.name, param.ttype.clone(), false);
            }

            let (body, mut errs) = typecheck_block(&mut context, &func.body);
            errors.append(&mut errs);

            context.current_function.take();
            context.scope_stack.pop_scope();

            CheckedFunction {
                name: func.name.clone(),
                parameters: func.parameters.clone(),
                return_type: func.return_type.clone(),
                body,
            }
        })
        .collect();

    (
        CheckedProgram {
            functions,
            extern_functions,
            structs: context.known_structs.into_values().collect(),
        },
        errors,
    )
}

fn typecheck_block(
    context: &mut Context,
    block: &ParsedBlock,
) -> (CheckedBlock, Vec<TypeCheckError>) {
    let mut errors = vec![];

    context.scope_stack.push_scope(None);
    let statements = block
        .statements
        .iter()
        .map(|stmt| {
            let (checked_stmt, mut errs) = typecheck_statement(context, stmt);
            errors.append(&mut errs);
            checked_stmt
        })
        .collect();
    context.scope_stack.pop_scope();

    (CheckedBlock { statements }, errors)
}

fn typecheck_statement(
    context: &mut Context,
    statement: &ParsedStatement,
) -> (CheckedStatement, Vec<TypeCheckError>) {
    match statement {
        ParsedStatement::Expression(expr) => {
            let (checked_expr, errors) = typecheck_expression(context, expr);
            (CheckedStatement::Expression(checked_expr), errors)
        }
        ParsedStatement::LetAssign(let_assign) => {
            let (checked_value, mut errors) = typecheck_expression(context, &let_assign.value);
            if context.scope_stack.add_variable(
                &let_assign.name,
                checked_value.ttype(),
                let_assign.is_mut,
            ) {
                errors.push(TypeCheckError::DuplicateVariableName(
                    let_assign.name.clone(),
                    let_assign.name_span,
                ));
            }
            (
                CheckedStatement::LetAssign(let_assign.name.clone(), checked_value),
                errors,
            )
        }
        ParsedStatement::WhileLoop(while_loop) => {
            let mut errors = vec![];

            let (checked_condition, mut errs) =
                typecheck_expression(context, &while_loop.condition);
            errors.append(&mut errs);

            if checked_condition.ttype() != Type::Bool {
                errors.push(TypeCheckError::WrongConditionType(
                    while_loop.condition.span(),
                    checked_condition.ttype(),
                ));
            }

            let (checked_body, mut errs) = typecheck_block(context, &while_loop.body);
            errors.append(&mut errs);

            (
                CheckedStatement::WhileLoop(CheckedWhileLoop {
                    condition: checked_condition,
                    body: checked_body,
                }),
                errors,
            )
        }
        ParsedStatement::IfElse(if_else) => {
            let mut errors = vec![];

            let (checked_condition, mut errs) = typecheck_expression(context, &if_else.condition);
            errors.append(&mut errs);

            if checked_condition.ttype() != Type::Bool {
                errors.push(TypeCheckError::WrongConditionType(
                    if_else.condition.span(),
                    checked_condition.ttype(),
                ));
            }

            let (checked_if_body, mut errs) = typecheck_block(context, &if_else.if_body);
            errors.append(&mut errs);

            let checked_else_body = if let Some(ref else_body) = if_else.else_body {
                let (checked_else_body, mut errs) = typecheck_block(context, else_body);
                errors.append(&mut errs);
                checked_else_body
            } else {
                CheckedBlock { statements: vec![] }
            };

            (
                CheckedStatement::IfElse(CheckedIfElse {
                    condition: checked_condition,
                    if_body: checked_if_body,
                    else_body: checked_else_body,
                }),
                errors,
            )
        }
        ParsedStatement::ForInLoop(for_in) => {
            let mut errors = vec![];

            let (checked_iterable, mut errs) =
                typecheck_expression(context, &for_in.iterable_value);
            errors.append(&mut errs);

            let elem_type = if let Type::Array(elem_type, _) = checked_iterable.ttype() {
                *elem_type
            } else {
                errors.push(TypeCheckError::InvalidIterableInForIn(
                    checked_iterable.ttype(),
                    for_in.iterable_value.span(),
                ));
                Type::Incomplete
            };

            context.scope_stack.push_scope(None);

            if context
                .scope_stack
                .add_variable(&for_in.elem_var_name, elem_type.clone(), false)
            {
                errors.push(TypeCheckError::DuplicateVariableName(
                    for_in.elem_var_name.clone(),
                    for_in.elem_var_name_span,
                ));
            }

            if let Some((ref index_var_name, index_var_name_span)) = for_in.index_var {
                if context
                    .scope_stack
                    .add_variable(index_var_name, Type::Int, false)
                {
                    errors.push(TypeCheckError::DuplicateVariableName(
                        for_in.elem_var_name.clone(),
                        index_var_name_span,
                    ));
                }
            }

            let (checked_body, mut errs) = typecheck_block(context, &for_in.body);
            errors.append(&mut errs);

            context.scope_stack.pop_scope();

            (
                CheckedStatement::ForInLoop(CheckedForInLoop {
                    elem_var_name: for_in.elem_var_name.clone(),
                    elem_var_type: elem_type,
                    index_var: for_in.index_var.clone().map(|(name, _)| name),
                    iterable: checked_iterable,
                    body: checked_body,
                }),
                errors,
            )
        }
        ParsedStatement::Return(return_value) => {
            let (checked_return_value, mut errors) = typecheck_expression(context, return_value);

            let function_return_type = &context.current_function.unwrap().return_type;
            if !function_return_type.matches(&checked_return_value.ttype()) {
                errors.push(TypeCheckError::InvalidReturnType(
                    checked_return_value.ttype(),
                    function_return_type.clone(),
                    return_value.span(),
                ));
            }

            (CheckedStatement::Return(checked_return_value), errors)
        }
    }
}

/// Typecheck an expression and return the type of the expression.
fn typecheck_expression(
    context: &mut Context,
    expression: &ParsedExpression,
) -> (CheckedExpression, Vec<TypeCheckError>) {
    match expression {
        ParsedExpression::Literal(literal) => match literal {
            Literal::String(value, _) => (
                CheckedExpression::Literal(CheckedLiteral::String(
                    value.clone(),
                    Type::Pointer(Box::new(Type::CChar), false),
                )),
                vec![],
            ),
            Literal::Int(value, _) => (
                CheckedExpression::Literal(CheckedLiteral::Int(*value, Type::GenericInt)),
                vec![],
            ),
            Literal::Bool(value, _) => (
                CheckedExpression::Literal(CheckedLiteral::Bool(*value, Type::Bool)),
                vec![],
            ),
            Literal::Struct(struct_literal, _) => {
                let mut errors = vec![];

                let checked_fields: HashMap<_, _> = struct_literal
                    .fields
                    .iter()
                    .map(|(field_name, _, field_value)| {
                        let (checked_field_value, mut errs) =
                            typecheck_expression(context, field_value);
                        errors.append(&mut errs);
                        (field_name.clone(), checked_field_value)
                    })
                    .collect();

                let r#struct =
                    if let Some(r#struct) = context.known_structs.get(&struct_literal.name) {
                        for (field_name, _) in &r#struct.fields {
                            if !checked_fields.iter().any(|(name, _)| name == field_name) {
                                errors.push(TypeCheckError::StructMissingField(
                                    struct_literal.name.clone(),
                                    field_name.clone(),
                                    struct_literal.span,
                                ));
                            }
                        }

                        for ((field_name, checked_field), (_, field_name_span, parsed_field)) in
                            checked_fields.iter().zip(struct_literal.fields.iter())
                        {
                            if let Some(field_type) = r#struct.get_field(field_name) {
                                if !checked_field.ttype().matches(field_type) {
                                    errors.push(TypeCheckError::StructFieldWrongType(
                                        struct_literal.name.clone(),
                                        field_name.clone(),
                                        checked_field.ttype(),
                                        field_type.clone(),
                                        parsed_field.span(),
                                    ))
                                }
                            } else {
                                errors.push(TypeCheckError::StructSuperfluousField(
                                    struct_literal.name.clone(),
                                    field_name.clone(),
                                    *field_name_span,
                                ));
                            }
                        }

                        r#struct.clone()
                    } else {
                        errors.push(TypeCheckError::UnknownType(
                            struct_literal.name.clone(),
                            struct_literal.name_span,
                        ));

                        Struct {
                            name: String::new(),
                            fields: vec![],
                            is_opaque: true,
                        }
                    };

                (
                    CheckedExpression::Literal(CheckedLiteral::Struct(
                        CheckedStructLiteral {
                            name: struct_literal.name.clone(),
                            fields: checked_fields,
                        },
                        r#struct,
                        Type::UserDefined(struct_literal.name.clone()),
                    )),
                    errors,
                )
            }
            Literal::Array(array_literal, _) => {
                let mut errors = vec![];

                let mut checked_elements = Vec::with_capacity(array_literal.elements.len());

                let array_elem_type = if let Some(first_elem) = array_literal.elements.get(0) {
                    let (checked_elem, mut errs) = typecheck_expression(context, first_elem);
                    errors.append(&mut errs);

                    let array_elem_type = checked_elem.ttype();
                    checked_elements.push(checked_elem);
                    Some(array_elem_type)
                } else {
                    None
                };

                checked_elements.extend(array_literal.elements.iter().skip(1).map(|elem| {
                    let (checked_elem, mut errs) = typecheck_expression(context, elem);
                    errors.append(&mut errs);

                    if !checked_elem
                        .ttype()
                        .matches(array_elem_type.as_ref().unwrap())
                    {
                        errors.push(TypeCheckError::WrongElementTypeInArray(
                            checked_elem.ttype(),
                            array_elem_type.clone().unwrap(),
                            elem.span(),
                        ));
                    }

                    checked_elem
                }));

                let array_type = if let Some(array_elem_type) = &array_elem_type {
                    Type::Array(Box::new(array_elem_type.clone()), checked_elements.len())
                } else {
                    Type::GenericEmptyArray
                };

                (
                    CheckedExpression::Literal(CheckedLiteral::Array(
                        CheckedArrayLiteral {
                            elements: checked_elements,
                            element_type: array_elem_type,
                        },
                        array_type,
                    )),
                    errors,
                )
            }
        },
        ParsedExpression::FunctionCall(func_call) => {
            let mut errors = vec![];

            let checked_args: Vec<CheckedExpression> = func_call
                .args
                .iter()
                .map(|arg| {
                    let (checked_arg, mut errs) = typecheck_expression(context, arg);
                    errors.append(&mut errs);
                    checked_arg
                })
                .collect();

            let return_type = if let Some(func) = context.known_functions.get(&func_call.name) {
                if checked_args.len() != func.parameters.len() {
                    errors.push(TypeCheckError::WrongNumArgs(
                        func_call.span,
                        checked_args.len(),
                        func.parameters.len(),
                    ));
                }

                for ((checked_arg, arg), param) in checked_args
                    .iter()
                    .zip(func_call.args.iter())
                    .zip(func.parameters.iter())
                {
                    if !checked_arg.ttype().matches(&param.ttype) {
                        errors.push(TypeCheckError::WrongArgType(
                            arg.span(),
                            checked_arg.ttype(),
                            param.ttype.clone(),
                        ));
                    }
                }

                func.return_type.clone()
            } else {
                errors.push(TypeCheckError::UnknownFunction(
                    func_call.name.clone(),
                    func_call.name_span,
                ));
                Type::Incomplete
            };

            (
                CheckedExpression::FunctionCall(CheckedFunctionCall {
                    name: func_call.name.clone(),
                    args: checked_args,
                    ttype: return_type,
                }),
                errors,
            )
        }
        ParsedExpression::Variable(variable_name, span) => {
            if let Some(KnownVariable { ttype, is_mut }) =
                context.scope_stack.get_variable_type(variable_name)
            {
                (
                    CheckedExpression::Variable(variable_name.clone(), ttype.clone(), *is_mut),
                    vec![],
                )
            } else {
                (
                    CheckedExpression::Variable(variable_name.clone(), Type::Incomplete, false),
                    vec![TypeCheckError::UnknownVariable(
                        variable_name.clone(),
                        context
                            .scope_stack
                            .get_current_function()
                            .map(|name| name.to_string()),
                        *span,
                    )],
                )
            }
        }
        ParsedExpression::CompareOp(lhs, rhs, op) => {
            let (checked_lhs, mut errors) = typecheck_expression(context, lhs);
            let (checked_rhs, mut errs) = typecheck_expression(context, rhs);
            errors.append(&mut errs);

            if !checked_lhs.ttype().matches(&checked_rhs.ttype()) {
                errors.push(TypeCheckError::BinaryOpMismatch(
                    checked_lhs.ttype(),
                    checked_rhs.ttype(),
                    lhs.span(),
                    rhs.span(),
                ))
            }

            let ttype = match op {
                CompareOperation::Equality => Type::Bool,
                CompareOperation::GreaterThan => Type::Bool,
                CompareOperation::GreaterThanEqual => Type::Bool,
                CompareOperation::LessThan => Type::Bool,
                CompareOperation::LessThanEqual => Type::Bool,
            };

            (
                CheckedExpression::CompareOp(
                    Box::new(checked_lhs),
                    Box::new(checked_rhs),
                    *op,
                    ttype,
                ),
                errors,
            )
        }
        ParsedExpression::MathOp(lhs, rhs, op) => {
            let (checked_lhs, mut errors) = typecheck_expression(context, lhs);
            let (checked_rhs, mut errs) = typecheck_expression(context, rhs);
            errors.append(&mut errs);

            if !checked_lhs.ttype().matches(&checked_rhs.ttype()) {
                errors.push(TypeCheckError::BinaryOpMismatch(
                    checked_lhs.ttype(),
                    checked_rhs.ttype(),
                    lhs.span(),
                    rhs.span(),
                ))
            }

            let ttype = checked_lhs.ttype();
            (
                CheckedExpression::MathOp(Box::new(checked_lhs), Box::new(checked_rhs), *op, ttype),
                errors,
            )
        }
        ParsedExpression::FieldAccess(field_access) => {
            let (checked_object, mut errors) = typecheck_expression(context, &field_access.object);

            let (object_is_ptr, obj_type, is_mut) =
                if let Type::Pointer(ref element_type, is_mut) = checked_object.ttype() {
                    (true, *element_type.clone(), is_mut)
                } else {
                    (false, checked_object.ttype(), checked_object.is_mut())
                };

            let (ttype, r#struct) =
                if let Some(r#struct) = context.known_structs.get(&obj_type.to_str()) {
                    let ttype = if r#struct.is_opaque {
                        errors.push(TypeCheckError::OpaqueStructFieldAccess(
                            obj_type,
                            field_access.field_name_span,
                        ));
                        Type::Incomplete
                    } else if let Some(field_type) = r#struct.get_field(&field_access.field_name) {
                        field_type.clone()
                    } else {
                        errors.push(TypeCheckError::FieldAccessInvalidField(
                            obj_type,
                            field_access.field_name.clone(),
                            field_access.field_name_span,
                        ));
                        Type::Incomplete
                    };
                    (ttype, r#struct.clone())
                } else {
                    errors.push(TypeCheckError::ObjectIsNotAStruct(
                        obj_type,
                        field_access.object_span,
                    ));
                    (
                        Type::Incomplete,
                        Struct {
                            name: String::new(),
                            fields: vec![],
                            is_opaque: true,
                        },
                    )
                };

            (
                CheckedExpression::FieldAccess(
                    CheckedFieldAccess {
                        object: Box::new(checked_object),
                        object_is_ptr,
                        field_name: field_access.field_name.clone(),
                        is_mut,
                    },
                    r#struct,
                    ttype,
                ),
                errors,
            )
        }
        ParsedExpression::Assignment(lhs, rhs) => {
            let (checked_lhs, mut errors) = typecheck_expression(context, lhs);
            let (checked_rhs, mut errs) = typecheck_expression(context, rhs);
            errors.append(&mut errs);

            let (is_l_value, is_mut, target_type) = match &checked_lhs {
                CheckedExpression::Variable(_name, ttype, is_mut) => (true, *is_mut, ttype.clone()),
                CheckedExpression::Deref(deref) => (
                    true,
                    deref.inner.ttype().is_mut_pointer(),
                    deref
                        .inner
                        .ttype()
                        .element_type()
                        .unwrap_or(Type::Incomplete),
                ),
                _ => (false, false, Type::Incomplete),
            };

            if !is_l_value {
                errors.push(TypeCheckError::AssignmentToNonLValue(lhs.span()));
            }

            if is_l_value && !is_mut {
                errors.push(TypeCheckError::AssignmentToImmutable(lhs.span()));
            }

            if !target_type.matches(&checked_rhs.ttype()) {
                errors.push(TypeCheckError::AssignmentTypeMismatch(
                    checked_rhs.ttype(),
                    target_type,
                    lhs.span(),
                    rhs.span(),
                ));
            }

            (
                CheckedExpression::Assignment(Box::new(checked_lhs), Box::new(checked_rhs)),
                errors,
            )
        }
        expr @ ParsedExpression::PointerTo(pointer_to) => {
            let (checked_inner, mut errors) = typecheck_expression(context, &pointer_to.inner);

            if pointer_to.is_mut && !checked_inner.is_mut() {
                errors.push(TypeCheckError::MutablePointerToImmutableVariable(
                    expr.span(),
                ));
            }

            let ttype = Type::Pointer(Box::new(checked_inner.ttype()), pointer_to.is_mut);
            (
                CheckedExpression::PointerTo(CheckedPointerTo {
                    inner: Box::new(checked_inner),
                    ttype,
                }),
                errors,
            )
        }
        expr @ ParsedExpression::Deref(deref) => {
            let (checked_inner, mut errors) = typecheck_expression(context, &deref.inner);

            if !checked_inner.ttype().is_pointer() {
                errors.push(TypeCheckError::DerefNonPointer(
                    checked_inner.ttype(),
                    expr.span(),
                ))
            }

            let ttype = checked_inner
                .ttype()
                .element_type()
                .unwrap_or(Type::Incomplete);
            (
                CheckedExpression::Deref(CheckedDeref {
                    inner: Box::new(checked_inner),
                    ttype,
                }),
                errors,
            )
        }
    }
}
