class A {
	b : Int;
};

class Main inherits IO {
	x : A <- new A;
    y : Int;
    main() : Object{
        {
        y <- 0;
        while (y < 3) loop {
            y <- y + 1;
        } pool;
        3;
    }
    };
};
