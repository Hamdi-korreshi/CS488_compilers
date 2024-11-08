class Main inherits IO {
    main() : Object {
        let is_ready : Bool <- true in
        if is_ready then
            out_string("Ready to proceed!\n")
        else
            out_string("Not ready yet.\n")
        fi
    };
};

