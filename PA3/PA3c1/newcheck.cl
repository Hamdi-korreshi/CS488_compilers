-- class A {
--     x : A <- new A;
--     j: String;
--     myfunc() : Object {
--         x
--     };
-- };

-- class B {
--     y : String;
--     myfunc() : String {
--         y  <- "mystringing"
--     };
-- };

class Main inherits IO {
    main() : Object {
        {
        let jack : Object <- new Object in 
        out_string("jack");
        }
    };
};
