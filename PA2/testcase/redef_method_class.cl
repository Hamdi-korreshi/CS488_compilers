class Animal {
    speak() : String {
        "Animal sound"
    };
};

class Dog inherits Animal {
    speak() : Int {
        42
    };
};

class Main inherits IO {

    main(): Object {
        {
            out_string("failed test");
        }
    };
};