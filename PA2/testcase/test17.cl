class TempType inherits IO{
    myfunc(x : String, y : Int) : Object {
        {
        out_string(x);
        out_string(y);
        }
    };
};


class Main inherits IO {
    x : Int;
    y : Int;

    main() : Object {
        {
        x <- "teststring";
        y <- "three";
        myfunc(x,y);
        }
    };
};