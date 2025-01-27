


pub mod nasm_writer {
    use std::fs::File;
    use std::io;
    use std::io::{Error, Write};
    use crate::core::utils::stringutils::StringBuilder;

    pub struct Writer {
        file: File,
        data: StringBuilder,
        text: StringBuilder,
        procs: Vec<String>
    }
    impl Writer {
        pub fn new(filename: String) -> Self {
            let file = File::create(filename).expect("Unable to create file");
            let mut data = StringBuilder::new();
            data.push_str("section .data\n");
            let mut text = StringBuilder::new();
            text.push_str("section .text\n");
            Self {
                file,
                data,
                text,
                procs: Vec::new()
            }
        }
        pub fn new_proc(name: String) -> StringBuilder {
            let mut sb = StringBuilder::new();
            sb.push_string(format!("{name}:\n"));
            sb
        }
        pub fn take_proc(&mut self, proc: StringBuilder) {
            self.procs.push(proc.pack());
        }
        pub fn new_data(&mut self, name: String, r#type: String, data: String) {
            self.data.push_string(format!("{name} {} {data}\n", r#type))
        }
        pub fn new_text(&mut self, command: String, data: Vec<String>) {
            match command {
                String::from("global") => {
                    self.text.push_string(format!("{command} "));
                    for label in data {
                        self.text.push_string(label)
                    }
                }
                String::from("extern") => {
                    self.text.push_string(format!("{command} "));
                    for proc in data {
                        self.text.push_string(format!("{}, ", proc))
                    }
                }
                _ => ()
            }
        }
        pub fn pack_to_file(mut self) -> Vec<io::Result<usize>> {
            let data = self.data.pack();
            let text = self.text.pack();
            let res_data = self.file.write(data.as_bytes());
            let res_text = self.file.write(text.as_bytes());
            let mut res_procs = Vec::new();
            for proc in self.procs {
                res_procs.push(self.file.write(proc.as_bytes()))
            };
            res_procs.append(&mut vec![res_data, res_text]);
            res_procs
        }
    }
}