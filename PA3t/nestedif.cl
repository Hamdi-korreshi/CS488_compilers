class Main inherits IO {
    main() : Object {
        let a : Int <- 5 in
        let b : Int <- 10 in
        if a < b then
		{
		if b < 20 then
                	out_string("a is less than b, and b is less than 20\n")
            	else
                	out_string("a is less than b, but b is 20 or more\n")
            	fi;
		}
        else
            out_string("a is not less than b\n")
        fi
    };
};

