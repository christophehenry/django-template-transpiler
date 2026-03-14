mod parser;
mod tag_for;
mod tag_if;
mod utils;
mod variable;

use crate::parser::Parser;
use dtl_lexer::types::IntoTemplateString;
use oxc::allocator::Allocator;
use pyo3::pymodule;

pub fn transpile(raw_template: &str) -> String {
    let allocator = Allocator::default();
    Parser::new(&allocator, raw_template.into_template_string()).render()
}

#[pymodule]
pub mod django_template_transpiler {
    use pyo3::{PyResult, Python, pyfunction};

    #[pyfunction]
    #[pyo3(signature = (raw_template))]
    pub fn transpile(_py: Python<'_>, raw_template: &str) -> PyResult<String> {
        Ok(crate::transpile(raw_template))
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use crate::parser::Parser;
    use dtl_lexer::types::TemplateString;
    use oxc::allocator::Allocator;
    use regex::Regex;

    pub(crate) fn render_template(template: &str) -> String {
        let allocator = Allocator::default();
        Parser::new(&allocator, TemplateString(template)).render()
    }

    pub(crate) fn assert_template(expected: &str, template: &str) {
        let process_regex = Regex::new(r"[\s\n]+").unwrap();
        assert_eq!(
            process_regex.replace_all(expected.trim(), " "),
            process_regex.replace_all(render_template(template).trim(), " ")
        );
    }
}
