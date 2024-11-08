class Animal {
    describe(): String {
        "I am an animal."
    };
};

class Dog inherits Animal {
    describe(): String {
        "I am a dog."
    };
};

class Main inherits IO{
    main(): Object {
        let
            myDog: Animal <- new Dog
        in {
            -- Dynamic dispatch: Calls Dog's describe method
            out_string(myDog.describe()); 
            out_string("\n");

            -- Static dispatch: Calls Animal's describe method
            out_string(myDog@Animal.describe());
            out_string("\n");
        }
    };
};

