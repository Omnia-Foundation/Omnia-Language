

pub mod string_utils {
    use std::slice::Iter;
    use std::str::{from_utf8_unchecked, Chars};

    pub struct StringBuilder {
        data: Vec<char>
    }
    impl StringBuilder {
        pub fn new() -> Self {
            Self {
                data: Vec::new()
            }
        }
        pub fn clear(&mut self) {
            self.data.clear()
        }
        pub fn push_c(&mut self, c: char) {
            self.data.push(c)
        }
        pub fn to_string(&self) -> String {
            String::from_iter(self.data.clone().into_iter())
        }
        pub fn push_string(&mut self, string: String) {
            self.data.append(&mut string.chars().collect::<Vec<char>>())
        }
        // pub unsafe fn to_str(&self) -> &str {
        //     from_utf8_unchecked(self.data.clone().into_iter().map(|x| x as u8).collect::<Vec<u8>>().as_slice())
        // }
    }

    pub struct Cursor {
        chars: Vec<char>,
        pos: usize
    }
    impl Cursor {
        pub fn new(input: String) -> Self {
            Self {
                chars: input.chars().collect(),
                pos: 0
            }
        }
        pub fn current(&self) -> Option<char> { self.chars.get(self.pos).copied() }
        pub fn next(&mut self) -> Option<char> {
            if self.pos < self.chars.len() {
                self.pos += 1;
            }
            self.current()
        }
        pub fn prev(&mut self) -> Option<char> {
            if self.pos > 0 {
                self.pos -= 1;
            }
            self.current()
        }
        pub fn get_current_line(&mut self) -> String {
            // println!("da {}", self.current().unwrap());
            let pos_revert = self.pos;
            loop {
                if self.prev().unwrap() == '\n' || self.pos == 0 { break }
            }
            self.next();
            let mut buffer = StringBuilder::new();
            while let Some(ch) = self.next() {
                if ch == '\n' || self.pos >= self.chars.len() {
                    break
                }
                buffer.push_c(self.current().unwrap())
            }
            self.pos = pos_revert;
            buffer.to_string()
        }
        pub fn skip(&mut self, n: usize) { self.pos = (self.pos + n).min(self.chars.len()) }
        pub fn seek(&mut self, n: usize) -> Option<char> { self.chars.get(self.pos + n).copied() }
        pub fn get_pos(&self) -> usize { self.pos }
    }
}

pub mod codegen_utils {
    use std::fs::File;
    use std::io::Write;
    use crate::core::utils::string_utils::StringBuilder;

    pub struct SectionText {
        imports: StringBuilder,
        procedures: Vec<String>
    }
    impl SectionText {
        pub fn new() -> Self {
            let mut imports = StringBuilder::new();
            imports.push_string("section .text\n".to_string());
            Self {
                imports,
                procedures: Vec::new()
            }
        }
        pub fn new_proc(&mut self, proc: String) { self.procedures.push(proc) }
        pub fn new_import(&mut self, name: String) { self.imports.push_string(name) }
        pub fn pack(self) -> String {
            let mut output = self.imports.to_string();

            for procedure in self.procedures {
                output.push_str(procedure.as_str())
            }
            output
        }

    }
    pub struct AssemblerWriter {
        sec_data: StringBuilder,
        sec_text: SectionText,
    }
    impl AssemblerWriter {
        pub fn init(data_init: String) -> Self {
            let mut sec_data = StringBuilder::new();
            sec_data.push_string("section .data\n".to_string());
            sec_data.push_string(data_init);
            let mut sec_text = SectionText::new();
            Self {
                sec_data,
                sec_text
            }
        }
        pub fn push_data(&mut self, data: String) { self.sec_data.push_string(data) }
        pub fn push_proc(&mut self, proc: String) { self.sec_text.new_proc(proc) }
        pub fn push_import(&mut self, import: String) { self.sec_text.new_import(import) }

        pub fn pack(self, output_file_name: String) -> std::io::Result<()> {
            let mut output = self.sec_data.to_string();
            output.push_str(self.sec_text.pack().as_str());
            let mut file = File::create(output_file_name)?;
            file.write_all(output.as_bytes())
        }
    }
}