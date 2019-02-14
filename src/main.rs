use std::env;
use std::fs;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
enum ParserState {
    Start,
    ReadingComment,
    LookingForNewline,
    AssertEndOfInstruction,
    AInstructionStart,
    AInstructionNumeric,
    AInstructionSymbol,
    LabelStart,
    LabelEnd,
    CInstructionDestOrCmp,
    CInstructionCmp,
    CInstructionJmp,
}

// used for dest and jmp
const BITS_3: &'static [&'static str] = &[
    "000", // null
    "001", // m
    "010", // d
    "011", // md
    "100", // a
    "101", // am
    "110", // ad
    "111", // amd
];

// use for comp
const BITS_7: &'static [&'static str] = &[
    "0101010", // 0
    "0111111", // 1
    "0111010", // -1
    "0001100", // d
    "0110000", // a, m
    "1110000", // a, m
    "0001101", // !d
    "0110001", // !a, !m
    "1110001", // !a, !m
    "0001111", // -d
    "0110011", // -a
    "1110011", // -m
    "0011111", // d+1
    "0110111", // a+1, m+1
    "1110111", // a+1, m+1
    "0001110", // d-1
    "0110010", // a-1, m-1
    "1110010", // a-1, m-1
    "0000010", // d+a, d+m
    "1000010", // d+a, d+m
    "0010011", // d-a, d-m
    "1010011", // d-a, d-m
    "0000111", // a-d, m-d
    "1000111", // a-d, m-d
    "0000000", // d&a, d&m
    "1000000", // d&a, d&m
    "0010101", // d|a, d|m
    "1010101", // d|a, d|m
];
#[derive(Default)]
struct SymbolPatchInfo {
    address: i32, // this symbol's adress
    patch_indices: Vec<usize>, // the lines we need to patch
}

static FNV_OFFSET:u64 = 14695981039346656037;
static FNV_PRIME:u64 = 1099511628211;

fn adjust_hash(hash: u64, c: char) -> u64 {
    return hash.wrapping_mul(FNV_PRIME) ^ c as u64;
}

fn fnv_64(input: &str) -> u64 {
    let mut hash = FNV_OFFSET;
    for c in input.chars() {
        hash = hash.wrapping_mul(FNV_PRIME);
        hash = hash ^ c as u64;
    }
    return hash;
}

fn assemble(source: &str, assembler_output: &mut String) {
    let mut comp_lut: HashMap<u64, &str> = HashMap::new();
    comp_lut.insert(fnv_64("0"),   &BITS_7[0]);
    comp_lut.insert(fnv_64("1"),   &BITS_7[1]);
    comp_lut.insert(fnv_64("-1"),  &BITS_7[2]);
    comp_lut.insert(fnv_64("D"),   &BITS_7[3]);
    comp_lut.insert(fnv_64("A"),   &BITS_7[4]);
    comp_lut.insert(fnv_64("M"),   &BITS_7[5]);
    comp_lut.insert(fnv_64("!D"),  &BITS_7[6]);
    comp_lut.insert(fnv_64("!A"),  &BITS_7[7]);
    comp_lut.insert(fnv_64("!M"),  &BITS_7[8]);
    comp_lut.insert(fnv_64("-D"),  &BITS_7[9]);
    comp_lut.insert(fnv_64("-A"),  &BITS_7[10]);
    comp_lut.insert(fnv_64("-M"),  &BITS_7[11]);
    comp_lut.insert(fnv_64("D+1"), &BITS_7[12]);
    comp_lut.insert(fnv_64("A+1"), &BITS_7[13]);
    comp_lut.insert(fnv_64("M+1"), &BITS_7[14]);
    comp_lut.insert(fnv_64("D-1"), &BITS_7[15]);
    comp_lut.insert(fnv_64("A-1"), &BITS_7[16]);
    comp_lut.insert(fnv_64("M-1"), &BITS_7[17]);
    comp_lut.insert(fnv_64("D+A"), &BITS_7[18]);
    comp_lut.insert(fnv_64("D+M"), &BITS_7[19]);
    comp_lut.insert(fnv_64("D-A"), &BITS_7[20]);
    comp_lut.insert(fnv_64("D-M"), &BITS_7[21]);
    comp_lut.insert(fnv_64("A-D"), &BITS_7[22]);
    comp_lut.insert(fnv_64("M-D"), &BITS_7[23]);
    comp_lut.insert(fnv_64("D&A"), &BITS_7[24]);
    comp_lut.insert(fnv_64("D&M"), &BITS_7[25]);
    comp_lut.insert(fnv_64("D|A"), &BITS_7[26]);
    comp_lut.insert(fnv_64("D|M"), &BITS_7[27]);

    let mut jump_lut: HashMap<u64, &str> = HashMap::new();
    jump_lut.insert(fnv_64("JGT"), &BITS_3[1]);
    jump_lut.insert(fnv_64("JEQ"), &BITS_3[2]);
    jump_lut.insert(fnv_64("JGE"), &BITS_3[3]);
    jump_lut.insert(fnv_64("JLT"), &BITS_3[4]);
    jump_lut.insert(fnv_64("JNE"), &BITS_3[5]);
    jump_lut.insert(fnv_64("JLE"), &BITS_3[6]);
    jump_lut.insert(fnv_64("JMP"), &BITS_3[7]);

    let mut dest_lut: HashMap<u64, &str> = HashMap::new();
    dest_lut.insert(fnv_64("M"),   &BITS_3[1]);
    dest_lut.insert(fnv_64("D"),   &BITS_3[2]);
    dest_lut.insert(fnv_64("MD"),  &BITS_3[3]);
    dest_lut.insert(fnv_64("A"),   &BITS_3[4]);
    dest_lut.insert(fnv_64("AM"),  &BITS_3[5]);
    dest_lut.insert(fnv_64("AD"),  &BITS_3[6]);
    dest_lut.insert(fnv_64("AMD"), &BITS_3[7]);

    let mut patch_info: HashMap<u64, SymbolPatchInfo> = HashMap::new();
    patch_info.insert(fnv_64("SP"), SymbolPatchInfo { address: 0, patch_indices: Default::default() });
    patch_info.insert(fnv_64("LCL"), SymbolPatchInfo { address: 1, patch_indices: Default::default() });
    patch_info.insert(fnv_64("ARG"), SymbolPatchInfo { address: 2, patch_indices: Default::default() });
    patch_info.insert(fnv_64("THIS"), SymbolPatchInfo { address: 3, patch_indices: Default::default() });
    patch_info.insert(fnv_64("THAT"), SymbolPatchInfo { address: 4, patch_indices: Default::default() });
    patch_info.insert(fnv_64("SCREEN"), SymbolPatchInfo { address: 0x4000, patch_indices: Default::default() });
    patch_info.insert(fnv_64("KBD"), SymbolPatchInfo { address: 0x6000, patch_indices: Default::default() });

    patch_info.insert(fnv_64("R0"), SymbolPatchInfo { address: 0, patch_indices: Default::default() });
    patch_info.insert(fnv_64("R1"), SymbolPatchInfo { address: 1, patch_indices: Default::default() });
    patch_info.insert(fnv_64("R2"), SymbolPatchInfo { address: 2, patch_indices: Default::default() });
    patch_info.insert(fnv_64("R3"), SymbolPatchInfo { address: 3, patch_indices: Default::default() });
    patch_info.insert(fnv_64("R4"), SymbolPatchInfo { address: 4, patch_indices: Default::default() });
    patch_info.insert(fnv_64("R5"), SymbolPatchInfo { address: 5, patch_indices: Default::default() });
    patch_info.insert(fnv_64("R6"), SymbolPatchInfo { address: 6, patch_indices: Default::default() });
    patch_info.insert(fnv_64("R7"), SymbolPatchInfo { address: 7, patch_indices: Default::default() });
    patch_info.insert(fnv_64("R8"), SymbolPatchInfo { address: 8, patch_indices: Default::default() });
    patch_info.insert(fnv_64("R9"), SymbolPatchInfo { address: 9, patch_indices: Default::default() });
    patch_info.insert(fnv_64("R10"), SymbolPatchInfo { address: 10, patch_indices: Default::default() });
    patch_info.insert(fnv_64("R11"), SymbolPatchInfo { address: 11, patch_indices: Default::default() });
    patch_info.insert(fnv_64("R12"), SymbolPatchInfo { address: 12, patch_indices: Default::default() });
    patch_info.insert(fnv_64("R13"), SymbolPatchInfo { address: 13, patch_indices: Default::default() });
    patch_info.insert(fnv_64("R14"), SymbolPatchInfo { address: 14, patch_indices: Default::default() });
    patch_info.insert(fnv_64("R15"), SymbolPatchInfo { address: 15, patch_indices: Default::default() });

    let mut state: ParserState = ParserState::Start;
    let mut c_inst_hash: u64 = FNV_OFFSET;
    let mut symbol_hash: u64 = FNV_OFFSET;
    let mut literal:u64 = 0;
    let mut dest_bits: &str = BITS_3[0]; // set to null;
    let mut line_number: i32 = -1;
    let mut next_address = 16; // usable address (+1) for the symbol patcher to use
    for c in source.chars() {
        match state {
            ParserState::Start => {
                if c.is_ascii_whitespace() {
                    continue;
                }
                if c == '/' {
                    state = ParserState::ReadingComment;
                }
                else if c == '(' {
                    symbol_hash = FNV_OFFSET;
                    state = ParserState::LabelStart;
                }
                else if c == '@' { // a-instruction
                    line_number += 1;
                    symbol_hash = FNV_OFFSET;
                    if line_number != 0 {
                        assembler_output.push('\n');
                    }
                    state = ParserState::AInstructionStart;
                }
                else /* c-instrunction */ {
                    line_number += 1;
                    if line_number != 0 {
                        assembler_output.push('\n');
                    }
                    assembler_output.push_str("111");

                    dest_bits = BITS_3[0]; // set to null;
                    // c instr header
                    state = ParserState::CInstructionDestOrCmp;
                    c_inst_hash = adjust_hash(c_inst_hash, c);
                }
            }
            ParserState::CInstructionDestOrCmp => {
                if c == '=' {
                    dest_bits = dest_lut[&c_inst_hash];
                    c_inst_hash = FNV_OFFSET; // reset for comp
                    state = ParserState::CInstructionCmp;
                }
                else if c == ';' {
                    let comp_bits = comp_lut[&c_inst_hash];
                    assembler_output.push_str(comp_bits);
                    assembler_output.push_str(dest_bits);
                    state = ParserState::CInstructionJmp;
                    c_inst_hash = FNV_OFFSET; // reset for comp
                }
                else {
                    c_inst_hash = adjust_hash(c_inst_hash, c);
                }
            }
            ParserState::CInstructionCmp => {
                if c == ';' {
                    let comp_bits = comp_lut[&c_inst_hash];
                    assembler_output.push_str(comp_bits);
                    assembler_output.push_str(dest_bits);
                    c_inst_hash = FNV_OFFSET; // reset for comp
                    state = ParserState::CInstructionJmp;
                }
                else if c == '/' {
                    let comp_bits = comp_lut[&c_inst_hash];
                    assembler_output.push_str(comp_bits);
                    assembler_output.push_str(dest_bits);
                    // null jump
                    assembler_output.push_str(BITS_3[0]);
                    c_inst_hash = FNV_OFFSET; // reset for comp
                    state = ParserState::ReadingComment;
                }
                else if c.is_ascii_whitespace() {
                    if c == '\n' || c == '\r' {
                        let comp_bits = comp_lut[&c_inst_hash];
                        assembler_output.push_str(comp_bits);
                        assembler_output.push_str(dest_bits);
                        // null jump
                        assembler_output.push_str(BITS_3[0]);
                        c_inst_hash = FNV_OFFSET; // reset for comp
                        state = ParserState::Start;
                    }
                    continue;
                }
                else {
                    c_inst_hash = adjust_hash(c_inst_hash, c);
                }
            }
            ParserState::CInstructionJmp => {
                if c == '/' {
                    let jmp_bits = jump_lut[&c_inst_hash];
                    assembler_output.push_str(jmp_bits);
                    state = ParserState::ReadingComment;
                    c_inst_hash = FNV_OFFSET; // reset for comp
                }
                else if c.is_ascii_whitespace() {
                    if c == '\n' || c == '\r' {
                        let jmp_bits = jump_lut[&c_inst_hash];
                        assembler_output.push_str(jmp_bits);
                        c_inst_hash = FNV_OFFSET; // reset for comp
                        state = ParserState::Start;
                    }
                    continue;
                }
                else {
                    c_inst_hash = adjust_hash(c_inst_hash, c);
                }
            }
            ParserState::AInstructionStart => {
                if c.is_numeric() {
                    literal = c as u64 - '0' as u64;
                    state = ParserState::AInstructionNumeric;
                }
                else if c.is_ascii_alphabetic() {
                    symbol_hash = adjust_hash(symbol_hash, c);
                    state = ParserState::AInstructionSymbol;
                }
            }
            ParserState::AInstructionNumeric => {
                if c.is_numeric() == false {
                    let a_instruction = format!("0{:015b}", literal as u16);
                    assembler_output.push_str(&a_instruction);
                    state = ParserState::AssertEndOfInstruction;
                }
                else {
                    literal *= 10;
                    literal += c as u64 - '0' as u64;
                }
            }
            ParserState::AInstructionSymbol => {
                if c.is_ascii_alphanumeric() == false  && c != '_' && c != '.' && c != '$' && c != ':' {
                    let symbol_patch = patch_info.entry(symbol_hash).or_insert(SymbolPatchInfo { address: -1, patch_indices: Default::default() });
                    // allocate room for this to be patched;
                    symbol_patch.patch_indices.push(assembler_output.len());
                    assembler_output.push_str("ptchptchptchptch");
                    state = ParserState::AssertEndOfInstruction;
                }
                else {
                    symbol_hash = adjust_hash(symbol_hash, c);
                }
            }
            ParserState::LabelStart => {
                assert!(c.is_ascii_alphabetic() || c == '_' || c == '.' || c == '$' || c == ':');
                symbol_hash = adjust_hash(symbol_hash, c);
                state = ParserState::LabelEnd;
            }
            ParserState::LabelEnd => {
                if c == ')' {
                    let symbol_patch = patch_info.entry(symbol_hash).or_insert(SymbolPatchInfo { address: line_number + 1, patch_indices: Default::default() });
                    if symbol_patch.address == -1 {
                        symbol_patch.address = line_number + 1;
                    }
                    // allocate room for this to be patched;
                    state = ParserState::AssertEndOfInstruction;
                }
                else {
                    assert!(c.is_ascii_alphanumeric() || c == '_' || c == '.' || c == '$' || c == ':');
                    symbol_hash = adjust_hash(symbol_hash, c);
                }
            }
            ParserState::ReadingComment => {
               assert!(c == '/');
               state = ParserState::LookingForNewline;
            }
            ParserState::LookingForNewline => {
                if c == '\n' || c == '\r' {
                    state = ParserState::Start;
                }
            }
            ParserState::AssertEndOfInstruction => {
                if c == '/' {
                    state = ParserState::ReadingComment;
                }
                else if c == '\n' || c == '\r' {
                    state = ParserState::Start;
                }
                else {
                    assert!(c.is_ascii_whitespace());
                }
            }
        }
    }

    // patch symbols
    for (_, patch_info) in &patch_info {
        let mut address = next_address;
        if patch_info.address != -1 {
            address = patch_info.address;
        }
        else {
            next_address += 1;
        }
        for &patch_index in &patch_info.patch_indices {
            let a_instruction = format!("0{:015b}", address as u16);
            assembler_output.replace_range(patch_index..(patch_index+16), &a_instruction);
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    //@nicco: lets check this first
    let filename = &args[1];

    let source = fs::read_to_string(filename)
        .expect("Something went wrong reading file");

    println!("parsing file: {}", filename);

    let mut assembler_output: String = String::new();
    assemble(&source, &mut assembler_output);


    let _ = fs::write("pong.hack", &assembler_output);
    //print!("\ngenerated assembly: \n{}", assembler_output);
}
