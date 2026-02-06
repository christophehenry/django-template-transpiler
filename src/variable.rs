use crate::utils::{EngineMethods, SimplerAstMethods};
use dtl_lexer::TemplateContent;
use dtl_lexer::types::{At, TemplateString};
use dtl_lexer::variable::{Argument, VariableToken, lex_variable_or_filter};
use num_bigint::Sign;
use oxc::ast::AstBuilder;
use oxc::ast::ast::{Expression, NumberBase};
use oxc::span::SPAN;

pub(crate) fn parse_variable<'t>(
    ast_builder: &AstBuilder<'t>,
    template: TemplateString<'t>,
    at: At,
) -> Expression<'t> {
    let variable = template.content(at);
    let Some((variable_token, at, filter_lexer)) = lex_variable_or_filter(variable, at.0).unwrap()
    else {
        return ast_builder.expression_string_literal(SPAN, "", None);
    };

    let variable_content = template.content(at);

    let var_expr = match variable_token {
        VariableToken::Variable => match variable_content {
            "None" => ("literal", ast_builder.expression_symbol("None")),
            "True" => (
                "literal",
                ast_builder.expression_boolean_literal(SPAN, true),
            ),
            "False" => (
                "literal",
                ast_builder.expression_boolean_literal(SPAN, false),
            ),
            _ => (
                "varName",
                ast_builder.expression_string_literal(SPAN, template.content(at), None),
            ),
        },
        VariableToken::Int(num) => {
            let (sign, digits) = num.to_u64_digits();
            let num = match sign {
                Sign::Minus => -(digits[0] as f64),
                _ => digits[0] as f64,
            };
            (
                "literal",
                ast_builder.expression_numeric_literal(SPAN, num, None, NumberBase::Decimal),
            )
        }
        VariableToken::Float(num) => (
            "literal",
            ast_builder.expression_numeric_literal(SPAN, num, None, NumberBase::Decimal),
        ),
    };

    let filters = filter_lexer
        .map(|it| {
            let it = it.unwrap();
            let mut properties = vec![(
                "filterName",
                ast_builder.expression_string_literal(SPAN, it.content(template), None),
            )];

            if let Some(filter_arg) = it.argument {
                let arg_expr = match filter_arg {
                    Argument::Numeric(_) => ast_builder.expression_numeric_literal(
                        SPAN,
                        filter_arg.content(template).parse::<f64>().unwrap(),
                        None,
                        NumberBase::Decimal,
                    ),
                    Argument::Text(_) => ast_builder.expression_string_literal(
                        SPAN,
                        filter_arg.content(template),
                        None,
                    ),
                    Argument::TranslatedText(_) => {
                        ast_builder.get_translation_fn(filter_arg.content(template))
                    }
                    Argument::Variable(at) => parse_variable(ast_builder, template, at),
                };
                properties.push(("argument", arg_expr));
            }
            ast_builder.expression_object_simple(properties)
        })
        .collect();
    ast_builder.get_variable_fn(var_expr, filters)
}
