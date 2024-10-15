class A inherits IO {
    myfunc() : SELF_TYPE{
        let b : Int in 
        {
            out_int(b);
            self;
        }
    };
};

class Main {
    main() : Object {
        {
	let
            i : Int <- 10,
            b : Bool <- true,
            s : String <- "Hello, Cool!",
            o : Object <- new Object,
            io : IO <- new IO
        in {
            -- Arithmetic operations
            i <- i + 5;
            i <- i - 3;
            i <- i * 2;
            i <- i / 2;

            -- Comparison operations
            b <- i < 20;
            b <- i <= 20;
            if b then
                {
                while not b < 20 loop
                    out_string("b")
                pool;
                }
            else {
                out_string("a");
            }
            fi;
            -- b <- i = 10;
            let x : Int <- 5 in {
            case x of
                x : Int => { out_string("Int\n"); 0; };
                y : String => { out_string("String\n"); "Str"; };
                z : Bool => { out_string("Bool\n"); true; };
                a : Object => { out_string("Object\n"); 2; };
            esac;
            };
            -- Basic class methods
            s <- s.concat(" World!");
            i <- s.length();
            s <- s.substr(0, 5);

            -- Input/Output methods
            io.out_string("Enter a number: ");
            i <- io.in_int();
            io.out_int(i);

            -- isvoid
            b <- isvoid o;

            -- Dynamic dispatch
            o <- (new Main).copy();

            -- Static dispatch
            o <- (new Main)@Main.copy();

            -- Self dispatch
            -- self.main();

            -- Return value
            o;
        };
    }
};
};
