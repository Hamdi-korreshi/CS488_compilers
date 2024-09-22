class StringList {
    -- Base class for empty list
    isNil() : Bool { true };

    head() : String { { abort(); "" ; } };

    tail() : StringList { { abort(); self; } };

    append(s : String) : StringList {
        (new ConsStringList).init(s, self)  -- Append creates a new element at the end
    };

    getConnectedTasks(target : String) : StringList {
        new StringList  -- Return empty list if base class (empty list)
    };
};

class ConsStringList inherits StringList {
    car : String; 
    cdr : StringList; 

    isNil() : Bool { false };

    head() : String { car };

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

class InDeg {
    str: String;
    in_deg: Int;

    init(s: String, in_deg: Int) : InDeg {
        {
            str <- s;
            in_deg <- in_deg;
            self;
        }
    };

    getDeg() : Int {
        in_deg
    };

    updateDeg(deg: Int) : Int {
        in_deg <- deg
    };

};

class DepsStr {
    str: String;
    list: StringList;

    init(s: String, lst : StringList) : DepsStr {
        {
            str <- s;
            list <- lst;
            self;
        }
    };

    getString() : String {
        str
    };

    getList() : StringList {
        list
    };
};

class DepList {
    isNil() : Bool { true };

    head() : DepsStr { { abort(); new DepsStr; } };

    tail() : DepList { { abort(); self; } };

    append(t : DepsStr) : DepList {
        (new ConsDepList).init(t, self)  -- Append creates a new element at the end
    };
};

class ConsDepList inherits DepList {
    car : DepsStr; 
    cdr : DepList; 

    isNil() : Bool { false };

    head() : DepsStr { car };

    tail() : DepList { cdr };

    init(t : DepsStr, rest : DepList) : DepList {
        {
            car <- t;
            cdr <- rest;
            self;
        }
    };

    -- Append recursively to add the DepsStr at the end of the list
    append(t : DepsStr) : DepList {
        (new ConsDepList).init(car, cdr.append(t))
    };
};

class Main inherits IO {

    mylist: StringList <- new StringList;
    lst: DepList <- new DepList;
    dep: DepsStr <- new DepsStr;
    deps: DepsStr <- new DepsStr;

    

    -- Function to create a DepList using append
    create_deps_list(input : StringList) : DepList {
    {
        while (not input.isNil()) loop {
            if input.tail().isNil() then {
                out_string("Warning: Unmatched task found. Stopping.\n");
                input <- new StringList;  -- Empty the list and stop the loop
            } else {
                -- First, get the task and the dependency
                let task : String <- input.head() in

                -- Create a StringList with one dependency
                let depList : StringList <- new StringList in
                depList.append(input.getConnectedTasks(task));

                -- Create a new DepsStr for the task and dependencies
                let deps : DepsStr <- (new DepsStr).init(task, depList) in

                -- Check if the list is empty before appending
                if lst.isNil() then {
                    lst <- (new ConsDepList).init(deps, new DepList);  -- Initialize with the first item
                } else {
                    lst <- lst.append(deps);  -- Append new tasks and dependencies
                }
                fi;

                -- Move to the next pair of lines
                input <- input.tail().tail();  -- Move to the next task and dependency
            } fi;
        }
        pool;  -- Return the list of dependencies
        lst;
        }
    };

    print_deplist(lst : DepList) : Object {
        if lst.isNil() then
            self-- End of the list
        else {
            let dep : DepsStr <- lst.head() in {
                out_string("Task: ");
                out_string(dep.getString());  -- Get the task name (DepsStr)
                out_string("\nDependencies: ");
                print_stringlist(dep.getList()); 
            };
            print_deplist(lst.tail());
        }
        fi
    };

    print_stringlist(l : StringList) : Object {
        if l.isNil() then {
            self;
        } else {
            out_string(l.head());
            out_string("\n");
            print_stringlist(l.tail());  -- Recursively print the tail
        }
        fi
    };

    main() : Object {
    {
        let input_str : String <- in_string() in
        while not input_str = "" loop
            {
                mylist <- mylist.append(input_str);  -- Use append to build StringList
                input_str <- in_string();            -- Read next string
            }
        pool;

        -- Create DepList from input StringList
        lst <- create_deps_list(mylist);  
        print_deplist(lst);
    }
};

};
