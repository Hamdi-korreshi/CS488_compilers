class A {
	b : Int;
};

class Main inherits IO {
	x : A <- new A;
    y : Int;

    main() : Object{
        {
        y <- 3;
        if (y = 3) then {
            let x : Int <- 3 in 
            out_int(x+2);
        }
        else{
            out_string("B");
        }
        fi;
        -- out_int(~3)
        }
    };
};
