use std::process::Output;
use std::sync::LazyLock;
use lazy_format::lazy_format;

pub fn compile_base_package(path_env: &str) {
    run_r_script("compile_base_package", &[path_env]);
}

#[cfg(test)]
pub fn write_baseenv(path_env: &str) {
    run_r_script("baseenv", &[path_env])
}

#[cfg(test)]
pub fn create_serdata(code: &str, path: &str) {
    run_r_script("create_serdata", &["-d", code, path])
}

#[cfg(test)]
pub fn create_testdata(code: &str, path: &str, path_comp: &str, opt: bool) {
    // Create test data using R script
    let opt_flag = if opt { "-opt" } else { "-noopt" };
    run_r_script("create_testdata", &[code, path, path_comp, opt_flag])
}

fn run_r_script(name: &str, args: &[&str]) {
    static RSCRIPT: LazyLock<String> = LazyLock::new(|| {
        std::env::var("RSCRIPT").unwrap_or_else(|_| "Rscript".to_string())
    });
    
    let Output {
        status,
        stdout,
        stderr,
    } = std::process::Command::new(&*RSCRIPT)
        .arg(format!("./scripts/{name}.R"))
        .args(args)
        .output()
        .unwrap_or_else(|e| panic!("failed to run R script \"{name}\": {e}"));
    assert!(
        status.success(),
        "`{name}.R{args}` {status}\n=== STDOUT ===\n{stdout}\n=== STDERR ===\n{stderr}\n===",
        args = lazy_format!(" {arg:?}" for arg in args),
        stdout = String::from_utf8_lossy(&stdout),
        stderr = String::from_utf8_lossy(&stderr),
    );
}