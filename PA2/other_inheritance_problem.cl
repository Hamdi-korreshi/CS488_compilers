class MyClass {
    my_method(x: Int, x: String) : Int {  -- Error: redefining formal parameter `x`.
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

