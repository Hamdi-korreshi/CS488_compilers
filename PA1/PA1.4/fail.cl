-- class StringList {
--     car: String;
--     cdr: StringList;

--     init(head: String, tail: StringList): StringList {
--         {
--             car <- head;
--             cdr <- tail;
--             self;
--         }
--     };

--     isNil(): Bool {cdr = null};

--     cons(new_head: String) : StringList {
--         (new StringList).init(new_head,self)
--     };

-- };

-- class Main inherits IO {
--     mylist : StringList;
--     print_list(l: StringList): Object {
--         if not (self.isNil()) then {
--             out_string(l.car);
--             out_string("\n");
--             if not (l.cdr = null) then {
--                 print_list(l.cdr);  -- Recursively print the rest of the list
--             } else {
--                 self;  -- This 'self' here does nothing and can be removed
--             } fi;
--         } else {
--             out_string("List is empty \n");
--             self;
--         } -- Ensure that `self` is returned in both cases
--         fi
--     };

--     main(): Object {
--         {
--             mylist <- new StringList().init("5", nul).cons("4").cons("3").cons("2").cons("1");
--             mylist.print_list(mylist);
--             while not mylist.isNil() loop {
--                 mylist.print_list(mylist);  -- Print the current list
--                 mylist <- mylist.tail();  -- Move to the next element (tail)
--             } pool;
--         }
--     };
-- };