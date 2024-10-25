class A {
    x: Int <- ~1;
    io : IO <- new IO;
	y : Int;
    myfuning() : Object {
		{
		io.out_string("xxxx");
		x <- 3;
		y <- 4;
		}
    	-- x <- io.in_int()
        
    };
};

class B {
    x: Int <- ~1;
    io : IO <- new IO;
	y : Int;
    myfunc() : Object {
        io.out_string("a")
    };
};

class Main inherits IO {
    c : A <- new A;
    y : Int;
    main() : Object{
        y <- 3
        -- if (not (y < ~3)) then {
        --     out_string("A");
        -- }
        -- else{
        --     out_string("B");
        -- }
        -- fi
        -- -- out_int(~3)
    };
};
