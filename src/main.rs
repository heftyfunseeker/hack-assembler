extern crate hack_assembler;

use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();

    let source_file_name = &args[1];
    let dest_file_name = &args[2];

    let source = fs::read_to_string(source_file_name)
        .expect("Something went wrong reading source file");

    let mut assembler_output: String = String::new();
    hack_assembler::assemble(&source, &mut assembler_output);

    let _ = fs::write(dest_file_name, &assembler_output);

    let mut binary = Vec::new();
    hack_assembler::generate_binary(assembler_output, &mut binary);

    println!("generated assembly at file: {}", dest_file_name);
}
