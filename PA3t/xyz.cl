class Main inherits IO {
    x : Int <- 4;
    y : Int;

    main() : Object{
        {
        y <- 3;
        if (y = 3) then {
            let x : Int <- 3 in 
            out_int(x+2);
            out_string("\n");
        }
        else{
            out_string("B");
        }
        fi;
        out_int(3);
        out_string("\n");
        }
    };
};