use oxc::ast::ast::{
    Argument, ArrayExpressionElement, Declaration, Expression, FormalParameterKind,
    FormalParameters, PropertyKind, VariableDeclarationKind,
};
use oxc::ast::{AstBuilder, NONE};
use oxc::span::SPAN;

pub(super) trait SomeWrap<T> {
    fn wrap(self) -> Option<T>;
}

impl<T> SomeWrap<T> for T {
    fn wrap(self) -> Option<T> {
        Some(self)
    }
}

impl<T> SomeWrap<T> for Option<T> {
    fn wrap(self) -> Self {
        self
    }
}

pub(super) trait SimplerAstMethods<'a> {
    fn expression_empty_string_literal(&self) -> Expression<'a>;

    /**
     * Produces ``obj.fn_name(arg1, arg2, arg3)``
     *
     * Example:
     *
     *   ast_builder
     *   .expression_call_simple(
     *       ["obj", "fn_name"],
     *       vec![
     *           ast_builder.expression_identifier(SPAN, "arg1"),
     *           ast_builder.expression_identifier(SPAN, "arg2"),
     *           ast_builder.expression_identifier(SPAN, "arg3"),
     *       ]
     *   )
     */
    fn expression_call_simple<const N: usize>(
        &self,
        names: [&'a str; N],
        args: Vec<Expression<'a>>,
    ) -> Expression<'a>;

    fn formal_parameters_simple<A>(&self, params: A) -> FormalParameters<'a>
    where
        A: Into<Vec<&'a str>>;

    fn expression_object_simple(
        &self,
        properties: Vec<(&'a str, Expression<'a>)>,
    ) -> Expression<'a>;

    fn expression_array_simple(&self, items: Vec<Expression<'a>>) -> Expression<'a>;
    fn expression_symbol(&self, name: &'a str) -> Expression<'a>;

    fn declaration_variable_simple(
        &self,
        kind: VariableDeclarationKind,
        name: &'a str,
        init: Expression<'a>,
    ) -> Declaration<'a>;
    fn expression_conditional_safe(
        &self,
        test: Expression<'a>,
        consequent: Expression<'a>,
        alternate: Expression<'a>,
    ) -> Expression<'a>;
}
impl<'a> SimplerAstMethods<'a> for AstBuilder<'a> {
    fn expression_empty_string_literal(&self) -> Expression<'a> {
        self.expression_string_literal(SPAN, "", None)
    }

    fn expression_call_simple<const N: usize>(
        &self,
        names: [&'a str; N],
        args: Vec<Expression<'a>>,
    ) -> Expression<'a> {
        assert!(N > 0);

        let expr = match names.len() {
            1 => self.expression_identifier(SPAN, names[0]),
            _ => names.iter().enumerate().fold(
                self.expression_string_literal(SPAN, "", None),
                |acc, (idx, &it)| {
                    if idx == 0 {
                        self.expression_identifier(SPAN, it)
                    } else {
                        Expression::from(self.member_expression_static(
                            SPAN,
                            acc,
                            self.identifier_name(SPAN, it),
                            false,
                        ))
                    }
                },
            ),
        };
        self.expression_call(
            SPAN,
            expr,
            NONE,
            self.vec_from_iter(args.into_iter().map(Argument::from)),
            false,
        )
    }

    fn formal_parameters_simple<A>(&self, params: A) -> FormalParameters<'a>
    where
        A: Into<Vec<&'a str>>,
    {
        self.formal_parameters(
            SPAN,
            FormalParameterKind::FormalParameter,
            self.vec_from_iter(params.into().iter().map(|&param| {
                self.plain_formal_parameter(
                    SPAN,
                    self.binding_pattern(
                        self.binding_pattern_kind_binding_identifier(SPAN, param),
                        NONE,
                        false,
                    ),
                )
            })),
            NONE,
        )
    }

    fn expression_object_simple(
        &self,
        properties: Vec<(&'a str, Expression<'a>)>,
    ) -> Expression<'a> {
        self.expression_object(
            SPAN,
            self.vec_from_iter(properties.into_iter().map(|(name, expr)| {
                self.object_property_kind_object_property(
                    SPAN,
                    PropertyKind::Init,
                    self.property_key_static_identifier(SPAN, name),
                    expr,
                    false,
                    false,
                    false,
                )
            })),
        )
    }

    fn expression_array_simple(&self, items: Vec<Expression<'a>>) -> Expression<'a> {
        self.expression_array(
            SPAN,
            self.vec_from_iter(items.into_iter().map(ArrayExpressionElement::from)),
        )
    }

    fn expression_symbol(&self, name: &'a str) -> Expression<'a> {
        self.expression_call_simple(
            ["Symbol", "for"],
            vec![self.expression_string_literal(SPAN, name, None)],
        )
    }

    fn declaration_variable_simple(
        &self,
        kind: VariableDeclarationKind,
        name: &'a str,
        init: Expression<'a>,
    ) -> Declaration<'a> {
        self.declaration_variable(
            SPAN,
            VariableDeclarationKind::Const,
            self.vec_from_array([self.variable_declarator(
                SPAN,
                kind,
                self.binding_pattern(
                    self.binding_pattern_kind_binding_identifier(SPAN, name),
                    NONE,
                    false,
                ),
                init.wrap(),
                false,
            )]),
            false,
        )
    }

    fn expression_conditional_safe(
        &self,
        test: Expression<'a>,
        consequent: Expression<'a>,
        alternate: Expression<'a>,
    ) -> Expression<'a> {
        self.expression_parenthesized(
            SPAN,
            self.expression_conditional(
                SPAN,
                self.expression_parenthesized(SPAN, test),
                consequent,
                alternate,
            ),
        )
    }
}

pub(super) trait EngineMethods<'a> {
    fn get_translation_fn(&self, text_to_translate: &'a str) -> Expression<'a>;
    fn get_variable_fn(
        &self,
        var_expr: (&'a str, Expression<'a>),
        filter_exprs: Vec<Expression<'a>>,
    ) -> Expression<'a>;
}

impl<'a> EngineMethods<'a> for AstBuilder<'a> {
    fn get_translation_fn(&self, text_to_translate: &'a str) -> Expression<'a> {
        self.expression_call_simple(
            ["engine", "translate"],
            vec![self.expression_string_literal(SPAN, text_to_translate, None)],
        )
    }

    fn get_variable_fn(
        &self,
        var_expr: (&'a str, Expression<'a>),
        filter_exprs: Vec<Expression<'a>>,
    ) -> Expression<'a> {
        self.expression_call_simple(
            ["engine", "variable"],
            vec![self.expression_object_simple(vec![
                var_expr,
                ("context", self.expression_identifier(SPAN, "context")),
                ("filters", self.expression_array_simple(filter_exprs)),
            ])],
        )
    }
}
