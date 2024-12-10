# Язык Omnia
***
![Logo](https://i.yapx.ru/YQOD8.png "логотип языка Omnia")

! **В разработке** !

Omnia это быстрый и безопасный язык программирования с открытым исходным кодом 

### Примеры ###

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

###### Простой пример - a + b
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
###### Пример использования стандартной библиотеки для математики(Stdmath) - функция тангенса

## Скачать и установить ##
В настоящее время язык Omnia основан на [Rust](https://www.rust-lang.org/ "Ссылка на оффициальный сайт языка Rust") так что вам нужно установить его перед этим

Последний релиз вы можете найти на странице [Релизов](https://github.com/naydiYTomg/Omnia-Language/releases)

## Вклад ##
Язык Omnia открыт для ваших предложений и улучшений! Вы можете внести свой вклад в развитие языка

Перед этим, пожалуйста, прочитайте правила контрибуции на <https://omnia.io/info/contributing>