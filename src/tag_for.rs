use crate::parser::Parser;
use crate::utils::{EngineMethods, SimplerAstMethods, SomeWrap};
use crate::variable::parse_variable;
use dtl_lexer::TemplateContent;
use dtl_lexer::common::text_content_at;
use dtl_lexer::tag::Tag;
use dtl_lexer::tag::forloop::{ForLexer, ForTokenType, ForVariableToken};
use dtl_lexer::types::TemplateString;
use oxc::ast::ast::{Expression, FunctionType, PropertyKind, Statement, VariableDeclarationKind};
use oxc::ast::{AstBuilder, NONE};
use oxc::span::SPAN;

const EMPTY_TAG: &str = "empty";

fn parse_iter_expr<'t>(
    ast_builder: &AstBuilder<'t>,
    template: TemplateString<'t>,
    expression_token: ForVariableToken,
) -> Expression<'t> {
    let expression = match expression_token.token_type {
        ForTokenType::Numeric => panic!("Numeric type are not iterable"),
        ForTokenType::Text => ast_builder.expression_string_literal(
            SPAN,
            template.content(text_content_at(expression_token.at)),
            None,
        ),
        ForTokenType::TranslatedText => {
            ast_builder.get_translation_fn(template.content(text_content_at(expression_token.at)))
        }
        ForTokenType::Variable => parse_variable(ast_builder, template, expression_token.at),
    };

    ast_builder.expression_function(
        SPAN,
        FunctionType::FunctionExpression,
        None,
        false,
        false,
        false,
        NONE,
        NONE,
        ast_builder.formal_parameters_simple([]),
        NONE,
        Some(ast_builder.function_body(
            SPAN,
            ast_builder.vec(),
            ast_builder.vec1(ast_builder.statement_return(
                SPAN,
                Some(ast_builder.expression_parenthesized(SPAN, expression)),
            )),
        )),
    )
}

pub(crate) trait ParseFor<'t> {
    fn parse_for(&mut self, tag: Tag) -> Expression<'t>;
    fn variable_unpacker_function(&self, variable_names: Vec<String>) -> Expression<'t>;
}

impl<'t> ParseFor<'t> for Parser<'t> {
    fn parse_for(&mut self, tag: Tag) -> Expression<'t> {
        let mut lexer = ForLexer::new(self.template, tag.parts);

        let mut variable_names = Vec::new();
        while let Some(token) = lexer.lex_variable_name() {
            variable_names.push(token.unwrap());
        }
        if variable_names.is_empty() {
            panic!("Missing variable")
        }

        lexer.lex_in().unwrap();

        let expression_token = lexer.lex_expression().unwrap();
        let reversed = lexer.lex_reversed().unwrap();
        let variable_names: Vec<String> = variable_names
            .iter()
            .map(|token| self.template.content(token.at).to_string())
            .collect();

        let iter_getter = parse_iter_expr(&self.ast_builder, self.template, expression_token);
        let (end_tag, forloop_expr) = self.parse_until([EMPTY_TAG, "endfor"]);

        let render_expr = self.ast_builder.expression_arrow_function_simple(
            true,
            false,
            ["engine", "context"],
            [forloop_expr],
        );

        let empty_expr = if end_tag.content(self.template) == EMPTY_TAG {
            let (_, lhs) = self.parse_until(["endfor"]);
            self.ast_builder.expression_arrow_function_simple(
                true,
                false,
                ["engine", "context"],
                [lhs],
            )
        } else {
            self.ast_builder.expression_null_literal(SPAN)
        };

        self.ast_builder.expression_call_simple(
            ["engine", "tag"],
            vec![self.ast_builder.expression_object_simple(vec![
                (
                    "tagName",
                    PropertyKind::Init,
                    self.ast_builder
                        .expression_string_literal(SPAN, "for", None),
                ),
                (
                    "context",
                    PropertyKind::Init,
                    self.ast_builder.expression_identifier(SPAN, "context"),
                ),
                (
                    "args",
                    PropertyKind::Init,
                    self.ast_builder.expression_object_simple(vec![
                        ("iter", PropertyKind::Get, iter_getter),
                        ("render", PropertyKind::Init, render_expr),
                        ("emptyRender", PropertyKind::Init, empty_expr),
                        (
                            "reversed",
                            PropertyKind::Init,
                            self.ast_builder.expression_boolean_literal(SPAN, reversed),
                        ),
                        (
                            "forloopVariables",
                            PropertyKind::Init,
                            self.variable_unpacker_function(variable_names),
                        ),
                    ]),
                ),
            ])],
        )
    }

    fn variable_unpacker_function(&self, variable_names: Vec<String>) -> Expression<'t> {
        let init_identifier = "item";
        let variable_names: Vec<&'t str> = variable_names
            .iter()
            .map(|x| self.ast_builder.atom(x.as_str()).as_str())
            .collect();

        if variable_names.len() == 1 {
            return self.ast_builder.expression_function(
                SPAN,
                FunctionType::FunctionExpression,
                None,
                false,
                false,
                false,
                NONE,
                NONE,
                self.ast_builder.formal_parameters_simple([init_identifier]),
                NONE,
                self.ast_builder
                    .function_body(
                        SPAN,
                        self.ast_builder.vec(),
                        self.ast_builder.vec1(
                            self.ast_builder.statement_return(
                                SPAN,
                                self.ast_builder
                                    .expression_object_simple(vec![(
                                        variable_names[0],
                                        PropertyKind::Init,
                                        self.ast_builder
                                            .expression_identifier(SPAN, init_identifier),
                                    )])
                                    .wrap(),
                            ),
                        ),
                    )
                    .wrap(),
            );
        }

        let unpack_decl = self.ast_builder.declaration_variable(
            SPAN,
            VariableDeclarationKind::Const,
            self.ast_builder.vec1(
                self.ast_builder.variable_declarator(
                    SPAN,
                    VariableDeclarationKind::Const,
                    self.ast_builder.binding_pattern(
                        self.ast_builder.binding_pattern_kind_array_pattern(
                            SPAN,
                            self.ast_builder
                                .vec_from_iter(variable_names.iter().map(|&x| {
                                    self.ast_builder
                                        .binding_pattern(
                                            self.ast_builder
                                                .binding_pattern_kind_binding_identifier(SPAN, x),
                                            NONE,
                                            false,
                                        )
                                        .wrap()
                                })),
                            NONE,
                        ),
                        NONE,
                        false,
                    ),
                    self.ast_builder
                        .expression_identifier(SPAN, init_identifier)
                        .wrap(),
                    false,
                ),
            ),
            false,
        );

        self.ast_builder.expression_function(
            SPAN,
            FunctionType::FunctionExpression,
            None,
            false,
            false,
            false,
            NONE,
            NONE,
            self.ast_builder.formal_parameters_simple([init_identifier]),
            NONE,
            self.ast_builder
                .function_body(
                    SPAN,
                    self.ast_builder.vec(),
                    self.ast_builder.vec_from_array([
                        Statement::from(unpack_decl),
                        self.ast_builder.statement_return(
                            SPAN,
                            self.ast_builder
                                .expression_object_simple(
                                    variable_names
                                        .iter()
                                        .map(|&x| {
                                            (
                                                x,
                                                PropertyKind::Init,
                                                self.ast_builder.expression_identifier(SPAN, x),
                                            )
                                        })
                                        .collect(),
                                )
                                .wrap(),
                        ),
                    ]),
                )
                .wrap(),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::assert_template;

    #[test]
    fn test_empty() {
        assert_template(
            r#"export default function(engine, _context) {
                const context = engine.context(_context);
                return "";
            }"#,
            "{% for key , value in items %}{{ key }}:{{ value }}/{% endfor %}",
        );
    }
}
