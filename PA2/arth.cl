class A {
    x : Int <- 5-true;
    y : String <- "x";
    z : Bool <- false;
};

class Main inherits IO {
    a: A;
    main() : Object {
        out_string("a")
    };
};