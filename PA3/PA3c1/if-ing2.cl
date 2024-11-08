class A {
    b : Int;
};

class Main inherits IO {
    x : A <- new A;
    y : Int;
    z : Bool; 

    main() : Object{
        {
        y <- 3;
        z <- true;
        if ((y = 3) = (z = false)) then {
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