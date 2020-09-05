use std::os::unix::process::CommandExt;

fn main() {
    let mut expr = String::new();
    expr.push_str("(user-open-files (list ");

    for arg in std::env::args().skip(1) {
        let mut path = std::path::PathBuf::from(arg);
        if !path.is_absolute() {
            path = std::env::current_dir().unwrap().join(path);
        }

        expr.push('"');
        expr.push_str(
            &path
                .to_str()
                .unwrap()
                .replace("\\", "\\\\")
                .replace("\"", "\\\""),
        );
        expr.push('"');
        expr.push(' ');
    }

    expr.push_str("))");

    let err = std::process::Command::new("emacsclient")
        .args(&["--no-wait", "-e", &expr])
        .stdout(std::process::Stdio::null())
        .exec();
    panic!("exec failed: {:?}", err);
}
