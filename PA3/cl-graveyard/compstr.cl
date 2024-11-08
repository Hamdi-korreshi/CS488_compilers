class Main inherits IO {
    main() : Object {
        let name : String <- "Alice" in
        if name = "Alice" then
            out_string("Hello, Alice!\n")
        else
            out_string("You are not Alice.\n")
        fi
    };
};

