class Main inherits IO {
    x : Int;
    y : Int;


    main() : Object {
        {
            x <- 5;
            if x then {
            x <- 6;
            }
            else
            {
                x <- 7;
            }
            fi;
        }
    };
};