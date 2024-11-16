class Main inherits IO {
    x : Int;
    y : String;
    main() : Object
    {
        {
        x <- 3;
        if (x < 2) then
        {
            out_string("a");
        }
        else {
            out_string("b");
        }
        fi;
        }
    };
};