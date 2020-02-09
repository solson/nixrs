extern crate lalrpop;

fn main() {
    println!("rerun-if-changed=src/parse.lalrpop");
    lalrpop::process_root().unwrap();
}
