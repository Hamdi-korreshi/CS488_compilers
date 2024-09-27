class MyClass {
    myfunc() : Int {
        3
    };
};

class MyChildClass inherits MyClass {
    myfunc() : Int {
        4
    };
};

class Main inherits IO {
    x : MyClass;
    main(): Object {
        {
            x@MyChildClass.myfunc();
        }
    };
};