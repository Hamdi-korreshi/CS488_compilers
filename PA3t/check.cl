class A {
    x : Int <- 3;
    j: String;
    myfunc() : Object {
        x + 1 + 2
    };
};

class B {
    y : String;
    myfunc() : String {
        y  <- "mystringing"
    };
};

class Main inherits IO {
    main() : Object {
        "A"
    };
};
