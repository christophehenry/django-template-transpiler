use crate::parser::Parser;
use crate::utils::{EngineMethods, SimplerAstMethods};
use crate::variable::parse_variable;
use dtl_lexer::TemplateContent;
use dtl_lexer::tag::ifcondition::{
    IfConditionAtom, IfConditionLexer, IfConditionOperator, IfConditionToken, IfConditionTokenType,
};
use dtl_lexer::tag::{Tag, TagParts};
use dtl_lexer::types::TemplateString;
use oxc::ast::AstBuilder;
use oxc::ast::ast::{BinaryOperator, Expression, LogicalOperator, NumberBase, UnaryOperator};
use oxc::span::SPAN;
use std::iter::Peekable;

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
enum ConditionTokenType {
    Atom,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEqual,
    GreaterThanEqual,
    In,
    NotIn,
    Not,
    Is,
    IsNot,
}

struct ConditionToken<'t> {
    expr: Expression<'t>,
    condition_type: ConditionTokenType,
}

pub(crate) trait ParseIf<'t> {
    fn parse_if(&mut self, tag: Tag) -> Expression<'t>;
}

#[derive(Debug)]
enum ConditionTree<'t> {
    If {
        #[allow(unused)]
        tag: Tag,
        condition: Expression<'t>,
        lhs: Expression<'t>,
    },
    Else {
        #[allow(unused)]
        tag: Tag,
        rhs: Expression<'t>,
    },
}

impl<'t> ParseIf<'t> for Parser<'t> {
    fn parse_if(&mut self, tag: Tag) -> Expression<'t> {
        let mut condition_tree: Vec<ConditionTree> = vec![];
        let condition = parse_if_condition(&self.ast_builder, self.template, tag.parts.clone());
        let (mut end_tag, mut lhs) = self.parse_until(["elif", "else", "endif"]);
        condition_tree.push(ConditionTree::If {
            tag,
            condition,
            lhs,
        });

        loop {
            match end_tag.content(self.template) {
                "elif" => {
                    let tag = end_tag;
                    let condition =
                        parse_if_condition(&self.ast_builder, self.template, tag.parts.clone());
                    (end_tag, lhs) = self.parse_until(["elif", "else", "endif"]);
                    condition_tree.push(ConditionTree::If {
                        tag,
                        condition,
                        lhs,
                    })
                }
                "else" => {
                    let tag = end_tag;
                    (end_tag, lhs) = self.parse_until(["endif"]);
                    condition_tree.push(ConditionTree::Else { tag, rhs: lhs })
                }
                "endif" => break,
                &_ => unreachable!(),
            }
        }

        let last_branch = condition_tree.pop().unwrap();
        let rhs = match last_branch {
            ConditionTree::If { condition, lhs, .. } => {
                self.ast_builder.expression_conditional_safe(
                    condition,
                    lhs,
                    self.ast_builder.expression_empty_string_literal(),
                )
            }
            ConditionTree::Else { rhs, .. } => {
                let ConditionTree::If { condition, lhs, .. } = condition_tree.pop().unwrap() else {
                    panic!("{{% else %}} branch already met")
                };
                self.ast_builder
                    .expression_conditional_safe(condition, lhs, rhs)
            }
        };
        condition_tree.into_iter().rfold(rhs, |rhs, item| {
            let ConditionTree::If { condition, lhs, .. } = item else {
                panic!("{{% else %}} branch already met")
            };
            self.ast_builder
                .expression_conditional_safe(condition, lhs, rhs)
        })
    }
}

fn parse_if_condition<'t>(
    ast_builder: &AstBuilder<'t>,
    template: TemplateString<'t>,
    tag_parts: TagParts,
) -> Expression<'t> {
    let mut parser = IfConditionLexer::new(template, tag_parts).peekable();
    let Some(item) = parser.next() else {
        panic!("Unfinished condition expression");
    };
    let token = item.unwrap();
    if !matches!(token.token_type, IfConditionTokenType::Atom(_)) {
        panic!("Bad {{% if %}} first token {}", token.content(template))
    }

    let mut lhs = parse_condition_token(ast_builder, template, token, None).expr;

    loop {
        let Some(item) = parser.next() else {
            return ast_builder.expression_parenthesized(SPAN, lhs);
        };

        let token = parse_condition_token(ast_builder, template, item.unwrap(), Some(lhs));

        lhs = match &token.condition_type {
            ConditionTokenType::Atom => token.expr,
            ConditionTokenType::And => ast_builder.expression_logical(
                SPAN,
                token.expr,
                LogicalOperator::And,
                render_condition_atom(ast_builder, template, parser.next().unwrap().unwrap()),
            ),
            ConditionTokenType::Or => ast_builder.expression_logical(
                SPAN,
                token.expr,
                LogicalOperator::Or,
                render_condition_atom(ast_builder, template, parser.next().unwrap().unwrap()),
            ),
            ConditionTokenType::Equal => ast_builder.expression_binary(
                SPAN,
                token.expr,
                BinaryOperator::StrictEquality,
                render_condition_atom(ast_builder, template, parser.next().unwrap().unwrap()),
            ),
            ConditionTokenType::NotEqual => ast_builder.expression_binary(
                SPAN,
                token.expr,
                BinaryOperator::StrictInequality,
                render_condition_atom(ast_builder, template, parser.next().unwrap().unwrap()),
            ),
            ConditionTokenType::LessThan => ast_builder.expression_binary(
                SPAN,
                token.expr,
                BinaryOperator::LessThan,
                render_condition_atom(ast_builder, template, parser.next().unwrap().unwrap()),
            ),
            ConditionTokenType::GreaterThan => ast_builder.expression_binary(
                SPAN,
                token.expr,
                BinaryOperator::GreaterThan,
                render_condition_atom(ast_builder, template, parser.next().unwrap().unwrap()),
            ),
            ConditionTokenType::LessThanEqual => ast_builder.expression_binary(
                SPAN,
                token.expr,
                BinaryOperator::LessEqualThan,
                render_condition_atom(ast_builder, template, parser.next().unwrap().unwrap()),
            ),
            ConditionTokenType::GreaterThanEqual => ast_builder.expression_binary(
                SPAN,
                token.expr,
                BinaryOperator::GreaterEqualThan,
                render_condition_atom(ast_builder, template, parser.next().unwrap().unwrap()),
            ),
            ConditionTokenType::In => {
                render_contains_function(ast_builder, template, &mut parser, token.expr)
            }
            ConditionTokenType::NotIn => ast_builder.expression_unary(
                SPAN,
                UnaryOperator::UnaryNegation,
                render_contains_function(ast_builder, template, &mut parser, token.expr),
            ),
            ConditionTokenType::Not => {
                ast_builder.expression_unary(SPAN, UnaryOperator::UnaryNegation, token.expr)
            }
            ConditionTokenType::Is => {
                render_is_function(ast_builder, template, &mut parser, token.expr)
            }
            ConditionTokenType::IsNot => ast_builder.expression_unary(
                SPAN,
                UnaryOperator::UnaryNegation,
                render_is_function(ast_builder, template, &mut parser, token.expr),
            ),
        }
    }
}

fn parse_condition_token<'t>(
    ast_builder: &AstBuilder<'t>,
    template: TemplateString<'t>,
    token: IfConditionToken,
    lhs: Option<Expression<'t>>,
) -> ConditionToken<'t> {
    match token.token_type {
        IfConditionTokenType::Atom(_) => ConditionToken {
            expr: render_condition_atom(ast_builder, template, token),
            condition_type: ConditionTokenType::Atom,
        },
        IfConditionTokenType::Operator(op) => ConditionToken {
            expr: lhs.unwrap(),
            condition_type: match op {
                IfConditionOperator::And => ConditionTokenType::And,
                IfConditionOperator::Or => ConditionTokenType::Or,
                IfConditionOperator::Equal => ConditionTokenType::Equal,
                IfConditionOperator::NotEqual => ConditionTokenType::NotEqual,
                IfConditionOperator::LessThan => ConditionTokenType::LessThan,
                IfConditionOperator::GreaterThan => ConditionTokenType::GreaterThan,
                IfConditionOperator::LessThanEqual => ConditionTokenType::LessThanEqual,
                IfConditionOperator::GreaterThanEqual => ConditionTokenType::GreaterThanEqual,
                IfConditionOperator::In => ConditionTokenType::In,
                IfConditionOperator::NotIn => ConditionTokenType::NotIn,
                IfConditionOperator::Is => ConditionTokenType::Is,
                IfConditionOperator::IsNot => ConditionTokenType::IsNot,
            },
        },
        IfConditionTokenType::Not => ConditionToken {
            expr: lhs.unwrap(),
            condition_type: ConditionTokenType::Not,
        },
    }
}

fn render_condition_atom<'t>(
    ast_builder: &AstBuilder<'t>,
    template: TemplateString<'t>,
    token: IfConditionToken,
) -> Expression<'t> {
    let content = token.content(template);
    let IfConditionTokenType::Atom(atom) = &token.token_type else {
        panic!("Bad right-hand side of binary expression {}", content)
    };
    match atom {
        IfConditionAtom::Numeric => ast_builder.expression_numeric_literal(
            SPAN,
            content.parse().unwrap(),
            None,
            NumberBase::Float,
        ),
        IfConditionAtom::Text => ast_builder.expression_string_literal(SPAN, content, None),
        IfConditionAtom::TranslatedText => ast_builder.get_translation_fn(content),
        IfConditionAtom::Variable => parse_variable(ast_builder, template, token.content_at()),
    }
}

fn render_contains_function<'t>(
    ast_builder: &AstBuilder<'t>,
    template: TemplateString<'t>,
    parser: &mut Peekable<IfConditionLexer>,
    lhs: Expression<'t>,
) -> Expression<'t> {
    ast_builder.expression_call_simple(
        ["engine", "_", "contains"],
        vec![
            lhs,
            render_condition_atom(ast_builder, template, parser.next().unwrap().unwrap()),
        ],
    )
}

fn render_is_function<'t>(
    ast_builder: &AstBuilder<'t>,
    template: TemplateString<'t>,
    parser: &mut Peekable<IfConditionLexer>,
    lhs: Expression<'t>,
) -> Expression<'t> {
    ast_builder.expression_call_simple(
        ["engine", "_", "do_is"],
        vec![
            lhs,
            render_condition_atom(ast_builder, template, parser.next().unwrap().unwrap()),
        ],
    )
}
