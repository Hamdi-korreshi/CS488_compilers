class Animal {
    make_sound(): String {
        "generic animal sound"
    };
};

class Main inherits IO{
    dog: Animal <- new Animal;
    main(): Object {
        out_string(dog.make_sound())
    };
};
