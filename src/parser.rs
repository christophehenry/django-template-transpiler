use crate::tag_if::ParseIf;
use crate::utils::{SimplerAstMethods, SomeWrap};
use crate::variable::parse_variable;
use dtl_lexer::core::{Lexer, TokenType};
use dtl_lexer::tag::{Tag, lex_tag};
use dtl_lexer::types::TemplateString;
use dtl_lexer::{START_TAG_LEN, TemplateContent};
use oxc::allocator::Allocator;
use oxc::ast::ast::{
    BinaryOperator, Expression, FunctionType, SourceType, Statement, VariableDeclarationKind,
};
use oxc::ast::{AstBuilder, NONE};
use oxc::codegen::Codegen;
use oxc::span::SPAN;

pub struct Parser<'t> {
    pub(crate) template: TemplateString<'t>,
    pub(crate) lexer: Lexer<'t>,
    pub(crate) ast_builder: AstBuilder<'t>,
}

impl<'t> Parser<'t> {
    pub(crate) fn new(allocator: &'t Allocator, template: TemplateString<'t>) -> Self {
        Self {
            template,
            lexer: Lexer::new(template),
            ast_builder: AstBuilder::new(allocator),
        }
    }

    pub fn render(&mut self) -> String {
        let result = self.parse_all();

        let program = self.ast_builder.program(
            SPAN,
            SourceType::mjs(),
            "",
            self.ast_builder.vec(),
            None,
            self.ast_builder.vec(),
            self.ast_builder
                .vec1(*self.get_export_fn(&self.ast_builder, result)),
        );

        Codegen::new().build(&program).code
    }

    pub(crate) fn parse_all(&mut self) -> Expression<'t> {
        let (_, result) = self.parse_until_tag_or_end([]);
        result
    }

    pub(crate) fn parse_until<const N: usize>(
        &mut self,
        closing_tags: [&'t str; N],
    ) -> (Tag, Expression<'t>) {
        let (tag, result) = self.parse_until_tag_or_end(closing_tags);
        (tag.unwrap(), result)
    }

    fn parse_until_tag_or_end<const N: usize>(
        &mut self,
        closing_tags: [&'t str; N],
    ) -> (Option<Tag>, Expression<'t>) {
        let mut result: Option<Expression<'t>> = None;

        while let Some(token) = self.lexer.next() {
            let expr = match token.token_type {
                TokenType::Text => self.ast_builder.expression_string_literal(
                    SPAN,
                    token.content(self.template),
                    None,
                ),
                TokenType::Comment => continue,
                TokenType::Variable => parse_variable(
                    &self.ast_builder,
                    self.template,
                    (token.at.0 + START_TAG_LEN, token.at.1 - 2 * START_TAG_LEN),
                ),
                TokenType::Tag => {
                    let tag =
                        lex_tag(token.content(self.template), token.at.0 + START_TAG_LEN).unwrap();
                    let tag_name = self.template.content(tag.at);
                    if closing_tags.contains(&tag_name) {
                        return (
                            Some(tag),
                            result.unwrap_or(self.ast_builder.expression_empty_string_literal()),
                        );
                    }
                    self.parse_tag(tag)
                }
            };
            result = match result {
                None => Some(expr),
                Some(previous) => self
                    .ast_builder
                    .expression_binary(SPAN, previous, BinaryOperator::Addition, expr)
                    .wrap(),
            }
        }

        if N > 0 {
            panic!(
                "Unmet {}: {}",
                if closing_tags.len() == 1 {
                    "closing tag"
                } else {
                    "any of the closing tags"
                },
                closing_tags.join(",")
            );
        };
        (
            None,
            result.unwrap_or(self.ast_builder.expression_empty_string_literal()),
        )
    }

    pub(crate) fn parse_tag(&mut self, tag: Tag) -> Expression<'t> {
        let tag_name = tag.content(self.template);

        match tag_name {
            "if" => self.parse_if(tag),
            "autoescape" | "block" | "comment" | "csrf_token" | "cycle" | "debug" | "filter"
            | "firstof" | "for" | "ifchanged" | "" | "load" | "lorem" | "now" | "partial"
            | "partialdef" | "querystring" | "regroup" | "resetcycle" | "spaceless"
            | "templatetag" | "url" | "verbatim" | "widthratio" | "with" => {
                todo!("{tag_name} not implemented yet")
            }
            // Implementation is not planned for these tags
            "extends" | "include" => panic!("{tag_name} is unimplemented",),
            _ => todo!("Custom tags not supported yet ({tag_name})"),
        }
    }

    /** Generates ``export default function(engine, context) { … }`` */
    fn get_export_fn(
        &self,
        ast_builder: &AstBuilder<'t>,
        result_to_return: Expression<'t>,
    ) -> Box<Statement<'t>> {
        let params = ast_builder.formal_parameters_simple(["engine", "_context"]);
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
                .alloc_function_body(
                    SPAN,
                    ast_builder.vec(),
                    ast_builder.vec_from_array([
                        Statement::from(ast_builder.declaration_variable_simple(
                            VariableDeclarationKind::Const,
                            "context",
                            ast_builder.expression_call_simple(
                                ["engine", "context"],
                                vec![ast_builder.expression_identifier(SPAN, "_context")],
                            ),
                        )),
                        return_statement,
                    ]),
                )
                .wrap(),
        );
        Statement::from(ast_builder.module_declaration_export_default_declaration(SPAN, export))
            .into()
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use dtl_lexer::types::TemplateString;
    use oxc::allocator::Allocator;
    use regex::Regex;

    fn render_template(template: &str) -> String {
        let allocator = Allocator::default();
        Parser::new(&allocator, TemplateString(template)).render()
    }

    fn assert_template(expected: &str, template: &str) {
        let process_regex = Regex::new(r"[\s\n]+").unwrap();
        assert_eq!(
            process_regex.replace_all(expected.trim(), " "),
            process_regex.replace_all(render_template(template).trim(), " ")
        );
    }

    #[test]
    fn test_empty() {
        assert_template(
            r#"export default function(engine, _context) {
                const context = engine.context(_context);
                return "";
            }"#,
            "",
        );
    }

    #[test]
    fn test_simple_text() {
        assert_template(
            r#"export default function(engine, _context) {
                const context = engine.context(_context);
                return "<div>Some text here</div>";
            }"#,
            r#"<div>Some text here</div>"#,
        );
    }

    #[test]
    fn test_variable_with_filters() {
        assert_ne!(
            0,
            render_template(
                r#"<p>
                {{ foo|default:1 }}
                {{ foo|default:1.0 }}
                {{ foo|default:"bar" }}
                {{ foo|default:'baz' }}
                {{ foo|default:_('hello') }}
                Some text
                {{ bar|lower }}
                {{ bar|cut:"foo" }}
                {{ foo.bar }}
            </p>"#
            )
            .len()
        )
    }

    #[test]
    fn test_variable_literal() {
        assert_template(
            r#"export default function(engine, _context) {
                const context = engine.context(_context);
                return "<div>" + engine.variable({ literal: Symbol.for("None"), context, filters: [] }) + "</div>";
            }"#,
            r#"<div>{{ None }}</div>"#,
        );
        assert_template(
            r#"export default function(engine, _context) {
                const context = engine.context(_context);
                return "<div>" + engine.variable({ literal: true, context, filters: [] }) + "</div>";
            }"#,
            r#"<div>{{ True }}</div>"#,
        );
        assert_template(
            r#"export default function(engine, _context) {
                const context = engine.context(_context);
                return "<div>" + engine.variable({ literal: false, context, filters: [] }) + "</div>";
            }"#,
            r#"<div>{{ False }}</div>"#,
        );
        assert_template(
            r#"export default function(engine, _context) {
                const context = engine.context(_context);
                return "<div>" + engine.variable({ literal: 1.5, context, filters: [] }) + "</div>";
            }"#,
            r#"<div>{{ 1.5 }}</div>"#,
        );
    }

    #[test]
    fn test_if_tag() {
        assert_template(
            r#"export default function(engine, _context) {
                const context = engine.context(_context);
                return "<div>" + (engine.variable({ literal: true, context, filters: [] }) ? "This is true" : "") + "</div>";
            }"#,
            r#"<div>{% if True %}This is true{% endif %}</div>"#,
        );
        assert_template(
            r#"export default function(engine, _context) {
                const context = engine.context(_context);
                return "<div>" + (engine.variable({ literal: true, context, filters: [] })
                    ? "This is true"
                    : "This is false")
                + "</div>";
            }"#,
            r#"<div>{% if True %}This is true{% else %}This is false{% endif %}</div>"#,
        );
        assert_template(
            r#"export default function(engine, _context) {
                const context = engine.context(_context);
                return "<div>" + (engine.variable({ literal: true, context, filters: [] })
                    ? "This is true"
                    : engine.variable({ varName: "var", context, filters: [] }) === 1.5
                        ? "Hmm…"
                        : "")
                    + "</div>";
            }"#,
            r#"<div>{% if True %}This is true{% elif var == 1.5 %}Hmm…{% endif %}</div>"#,
        );
        assert_template(
            r#"export default function(engine, _context) {
                const context = engine.context(_context);
                return "<div>" + (engine.variable({ literal: true, context, filters: [] })
                    ? "This is true"
                    : engine.variable({ varName: "var", context, filters: [] }) === 1.5
                        ? "Hmm…"
                        : "This is false")
                    + "</div>";
            }"#,
            r#"<div>{% if True %}This is true{% elif var == 1.5 %}Hmm…{% else %}This is false{% endif %}</div>"#,
        );
        assert_template(
            r#"export default function(engine, _context) {
                const context = engine.context(_context);
                return "<div>" + (engine.variable({ varName: "var", context, filters: [{ filterName: "len" }] }) === 0
                    && engine.variable({ varName: "test.empty", context, filters: [] })
                        ? "Some condition"
                        : engine.variable({ varName: "var", context, filters: [{ filterName: "len" }] }) === 2
                            ? "Some other condition"
                            : engine.variable({ varName: "test.not_empty", context, filters: [] })
                                ? "Some third condition"
                                : "Else branch")
                    + "</div>";
                }"#,
            r#"<div>{% if var|len == 0 and test.empty %}Some condition{% elif var|len == 2 %}Some other condition{% elif test.not_empty %}Some third condition{% else %}Else branch{% endif %}</div>"#,
        );
    }
}
