# The Omnia Language
***
![Logo](https://github.com/user-attachments/assets/d33b21a8-f273-429d-8b75-d1d17da10d36 "Omnia lang logo")

! **Work in progress** !

Omnia language is a **fast**, **secure** and **open-source** programming language

### Examples ###

```Omnia
    plug std::console.println|reader;
    
    visible func add(int a, int b) -> int {
        a + b
    }
    static func main(&string[] args) {
        int a, b;
        mk input = reader::new(std::stdin);
        input.next_int(&a);
        input.next_int(&b);
        println(add(a, b))
    }
```

###### Simple example - a + b
***

```Omnia
    plug stdmath::cos|sin;
    plug std::println|readln|stdin;
    
    visible func my_tan(decimal angle) -> decimal {
        sin(angle)/cos(angle)
    }
    static func main(&string[] args) {
        string input;
        readln(&input);
        decimal angle = decimal::from(input);
        println(my_tan(angle))
    }
```
###### Stdmath example - tan function

## Download and install ##
Currently, Omnia language is based on [Rust](https://www.rust-lang.org/ "Link to official rust website where you can download it"), so you need to install it before Omnia.

You can find latest release on [Releases](https://github.com/naydiYTomg/Omnia-Language/releases) page

## Contributing ##
Omnia language is open to your suggestions and improvements!

Please read contribution rules at <https://omnia.io/info/contributing>
