

pub struct StringBuilder {
    buffer: Vec<char>
}
impl StringBuilder {
    pub fn new() -> Self {
        Self {
            buffer: Vec::new()
        }
    }
    pub fn push_c(&mut self, c: char) {
        self.buffer.push(c)
    }
    pub fn push_str(&mut self, s: &str) {
        self.buffer.append(s.chars().collect())
    }
    pub fn push_string(&mut self, s: String) {
        self.buffer.append(s.chars().collect())
    }
    pub fn pack(self) -> String {
        String::from_iter(self.buffer)
    }
}