class Main inherits IO {
    main() : Object {
        let x : Int <- 10 in
        if x < 20 then
            out_string("x is less than 20\n")
        else
            out_string("x is 20 or more\n")
        fi
    };
};

