class A {
    b : Int <- 3;
};

class Main inherits IO {
    y : Int;
    x : Int;
    main() : Object{
        {
        y <- 0;
        while (y < 3) loop {
            y <- y + 1;
            x <- y;
        } pool;
        out_string("A");
        }
    };
};
