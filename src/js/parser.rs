use crate::js::utils::{SimplerAstMethods, SomeWrap};
use crate::lex::START_TAG_LEN;
use crate::lex::core::{Lexer, TokenType};
use crate::lex::variable::{ArgumentType, lex_translation, lex_variable};
use crate::types::TemplateString;
use oxc::allocator::Allocator;
use oxc::ast::ast::{BinaryOperator, Expression, FunctionType, NumberBase, SourceType, Statement};
use oxc::ast::{AstBuilder, NONE};
use oxc::codegen::Codegen;
use oxc::span::SPAN;

pub struct Parser<'t> {
    template: TemplateString<'t>,
    lexer: Lexer<'t>,
}

impl<'t> Parser<'t> {
    pub fn new(template: TemplateString<'t>) -> Self {
        Self {
            template,
            lexer: Lexer::new(template),
        }
    }

    pub fn render(&mut self) -> String {
        let allocator = Allocator::default();
        let ast_builder = AstBuilder::new(&allocator);

        let mut expressions = Vec::<Expression>::new();
        while let Some(token) = self.lexer.next() {
            match token.token_type {
                TokenType::Text => expressions.push(ast_builder.expression_string_literal(
                    SPAN,
                    self.template.content(token.at),
                    None,
                )),
                TokenType::Comment => continue,
                TokenType::Variable => expressions.push(*self.parse_variable(
                    token.content(self.template),
                    token.at.0 + START_TAG_LEN,
                    &ast_builder,
                )),
                TokenType::Tag => continue,
            };
        }

        let result = expressions
            .into_iter()
            .reduce(|acc, expr| {
                ast_builder.expression_binary(SPAN, acc, BinaryOperator::Addition, expr)
            })
            .unwrap_or_else(|| ast_builder.expression_string_literal(SPAN, "", None));

        let program = ast_builder.program(
            SPAN,
            SourceType::mjs(),
            "",
            ast_builder.vec(),
            None,
            ast_builder.vec(),
            ast_builder.vec1(*self.get_export_fn(&ast_builder, result)),
        );

        Codegen::new().build(&program).code
    }

    fn parse_variable(
        &self,
        variable: &str,
        start: usize,
        ast_builder: &AstBuilder<'t>,
    ) -> Box<Expression<'t>> {
        if let Some((variable_token, filter_lexer)) = lex_variable(variable, start).unwrap() {
            let variable_name = self.template.content(variable_token.at);
            let filters = filter_lexer
                .map(|it| {
                    let it = it.unwrap();
                    let mut properties = vec![(
                        "filterName",
                        ast_builder.expression_string_literal(
                            SPAN,
                            self.template.content(it.at),
                            None,
                        ),
                    )];

                    if let Some(filter_arg) = it.argument {
                        let arg_expr = match filter_arg.argument_type {
                            ArgumentType::Numeric => ast_builder.expression_numeric_literal(
                                SPAN,
                                self.template.content(filter_arg.at).parse::<f64>().unwrap(),
                                None,
                                NumberBase::Decimal,
                            ),
                            ArgumentType::Text => ast_builder.expression_string_literal(
                                SPAN,
                                self.template.content(filter_arg.at),
                                None,
                            ),
                            ArgumentType::TranslatedText => *self.get_translation_fn(
                                ast_builder,
                                self.template.content(lex_translation(filter_arg.at)),
                            ),
                            ArgumentType::Variable => *self.get_variable_fn(
                                ast_builder,
                                self.template.content(filter_arg.at),
                                Vec::new(),
                            ),
                        };
                        properties.push(("argument", arg_expr));
                    }
                    ast_builder.expression_object_simple(properties)
                })
                .collect();
            self.get_variable_fn(ast_builder, variable_name, filters)
        } else {
            ast_builder.expression_string_literal(SPAN, "", None).into()
        }
    }

    fn get_variable_fn(
        &self,
        ast_builder: &AstBuilder<'t>,
        variable_name: &'t str,
        filter_exprs: Vec<Expression<'t>>,
    ) -> Box<Expression<'t>> {
        ast_builder
            .expression_call_simple(
                ["engine", "variable"],
                vec![ast_builder.expression_object_simple(vec![
                    (
                        "varName",
                        ast_builder.expression_string_literal(SPAN, variable_name, None),
                    ),
                    (
                        "context",
                        ast_builder.expression_identifier(SPAN, "context"),
                    ),
                    ("filters", ast_builder.expression_array_simple(filter_exprs)),
                ])],
            )
            .into()
    }

    fn get_translation_fn(
        &self,
        ast_builder: &AstBuilder<'t>,
        text_to_translate: &'t str,
    ) -> Box<Expression<'t>> {
        ast_builder
            .expression_call_simple(
                ["engine", "translate"],
                vec![ast_builder.expression_string_literal(SPAN, text_to_translate, None)],
            )
            .into()
    }

    /** Generates ``export default function(engine, context) { … }`` */
    fn get_export_fn(
        &self,
        ast_builder: &AstBuilder<'t>,
        result_to_return: Expression<'t>,
    ) -> Box<Statement<'t>> {
        let params = ast_builder.formal_parameters_simple(["engine", "context"]);
        let return_statement = ast_builder.statement_return(SPAN, result_to_return.wrap());
        let export = ast_builder.export_default_declaration_kind_function_declaration(
            SPAN,
            FunctionType::FunctionDeclaration,
            None,
            false,
            false,
            false,
            NONE,
            NONE,
            params,
            NONE,
            ast_builder
                .alloc_function_body(SPAN, ast_builder.vec(), ast_builder.vec1(return_statement))
                .wrap(),
        );
        Statement::from(ast_builder.module_declaration_export_default_declaration(SPAN, export))
            .into()
    }
}

#[cfg(test)]
mod tests {
    use crate::js::parser::Parser;
    use crate::types::TemplateString;
    use regex::Regex;

    fn assert_template(expected: &str, template: &str) {
        let rendered = Parser::new(TemplateString(template)).render();
        let actual = Regex::new(r"\s*\n\t?\s*")
            .unwrap()
            .replace_all(rendered.trim(), "");
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_empty() {
        assert_template(
            r#"export default function(engine, context) {return "";}"#,
            "",
        );
    }

    #[test]
    fn test_simple_text() {
        assert_template(
            r#"export default function(engine, context) {return "<div>Some text here</div>";}"#,
            r#"<div>Some text here</div>"#,
        );
    }
}
