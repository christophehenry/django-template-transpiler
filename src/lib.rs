mod parser;
mod utils;

use crate::parser::Parser;
use dtl_lexer::types::IntoTemplateString;
use pyo3::pymodule;

pub fn transpile(raw_template: &str) -> String {
    Parser::new(raw_template.into_template_string()).render()
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
