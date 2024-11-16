class A {} ;
class B {};
class C {};
class D {};
class E {};
class F {};
class G {};
class Z {} ;

class Main {
    x : Int;
    y : Int;
    z : IO <- new IO;
    a : A <- new A;
    b : IO <- new IO;
    main() : Object
    {
        {
        x <- 3;
        y <- x;
        z.out_string("\n");
        z.out_int(3+5+8);
        z.out_string("ASDF");
        }
    };
};