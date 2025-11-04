mod parser;
mod utils;

use pyo3::pymodule;

#[pymodule]
pub mod django_rusty_templates {
    use crate::js::parser::Parser;
    use crate::types::TemplateString;
    use pyo3::types::PyAnyMethods;
    use pyo3::{PyResult, Python, import_exception, pyfunction};
    use std::fs::{File, create_dir_all};
    use std::io::prelude::*;
    use std::path::PathBuf;

    import_exception!(django.core.exceptions, ImproperlyConfigured);

    #[pyfunction]
    pub fn transpile(_py: Python<'_>, _filename: PathBuf) -> PyResult<()> {
        Ok(())
    }

    #[pyfunction]
    pub fn transpile_from_string(
        py: Python<'_>,
        content: &str,
        target_filename: PathBuf,
    ) -> PyResult<()> {
        let static_root = py
            .import("django.conf")?
            .getattr("settings")?
            .getattr("STATIC_ROOT")?;

        if static_root.is_none() {
            return Err(ImproperlyConfigured::new_err(
                "django.conf.settings.STATIC_ROOT should be set",
            ));
        }

        let static_root = static_root.str()?.extract::<String>()?;

        let mut dest: PathBuf = static_root.into();
        if !dest.is_absolute() {
            dest = std::env::current_dir()?.join(dest);
        };
        dest.push(target_filename);

        create_dir_all(dest.parent().unwrap())?;
        let mut file = File::create(dest.as_path())?;
        let script_content = Parser::new(TemplateString(content)).render();
        file.write_all(script_content.as_bytes())?;

        Ok(())
    }
}
