-- redefine attribute error
lass StringList {
    car: String;
    cdr: StringList;

    init(head: String, tail: StringList): StringList {
        {
            car <- head;
            cdr <- tail;
            self;
        }
    };

    isNil(): Bool {cdr = null};

    cons(new_head: String) : StringList {
        (new StringList).init(new_head,self)
    };

};

class ConsStringList inherits StringList {
    car : String;
    cdr : StringList;

    isNil() : Bool { false };

    head() : StringList { cdr };

    tail() : StringList { cdr };

    init(s : String, rest : StringList) : StringList {
        {
            car <- s;
            cdr <- rest;
            self;
        }
    };

    -- Append recursively to add the string at the end of the list
    append(s : String) : StringList {
        (new ConsStringList).init(car, cdr.append(s))
    };

    getConnectedTasks(target : String) : StringList {
        if cdr.isNil() then
            -- End of the list, return empty
            new StringList
        else {
            -- Check if the current head matches the target
            if cdr.head() = target then
                -- If the next element matches the target, append this element (car) to the result
                (new ConsStringList).init(car, cdr.tail().getConnectedTasks(target))
            else
                -- Continue searching the rest of the list
                cdr.getConnectedTasks(target)
        fi;
        }
        fi
    };
};



class Main inherits IO {
    mylist : StringList;
    print_list(l: StringList): Object {
        if not (self.isNil()) then {
            out_string(l.car());
            out_string("\n");
            if not (isvoid l.cdr()) then {
                print_list(l.cdr());  -- Recursively print the rest of the list
            } else {
                self;  -- This 'self' here does nothing and can be removed
            } fi;
        } else {
            out_string("List is empty \n");
            self;
        } -- Ensure that `self` is returned in both cases
        fi
    };

    main(): Object {
        {
            mylist <- (new StringList ).init("5",mylist).cons("4").cons("3").cons("2").cons("1");
            mylist.print_list(mylist);
            while not mylist.isNil() loop {
                mylist.print_list(mylist);  -- Print the current list
                mylist <- mylist.tail();  -- Move to the next element (tail)
            } pool;
        }
    };
};