class Main inherits IO {
    main() : Object {
        let x : Int <- in_int() in
        if x < 10 then
            out_int(x + 5)  -- Add 5 if x is less than 10
        else
            out_int(x * 2)  -- Double x if x is 10 or more
        fi
    };
};

