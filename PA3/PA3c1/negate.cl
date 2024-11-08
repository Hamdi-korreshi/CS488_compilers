class A {
    x: Int <- ~1;
    io : IO <- new IO;
    myfuning() : Object {
        x <- 3
    };
};

class B {
    x: Int <- ~1;
    io : IO <- new IO;
    -- myfunc() : Object {
    --     io.out_string("asdf")
    -- };
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
