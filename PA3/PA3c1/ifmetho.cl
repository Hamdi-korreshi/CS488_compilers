class Person inherits IO {
    age : Int;

    init_age(new_age : Int) : Person {
        {
        age <- new_age;
        self;
        }
    };

    check_age() : Object {
        if age < 18 then
            out_string("Underage\n")
        else
            out_string("Adult\n")
        fi
    };
};

class Main inherits IO {
    main() : Object {
        let person : Person <- (new Person).init_age(16) in
        person.check_age()
    };
};
