class StringList {
    -- lisp like lists thats where the manual got it from
    car : String;
    cdr : StringList;

    isNil() : Bool { {isvoid car;} };

    head() : String { { car; } };

    tail() : StringList { { cdr; } };

    init(s : String, rest : StringList) : StringList {
        {
            car <- s;
            cdr <- rest;
            self;
        }
    };

    prepend(s: String) : StringList {
        (new StringList).init(s, self)
    };

    append(s : String) : StringList {
        if s = "" then
            self  -- return normal
        else { 
            if isvoid cdr then  -- tail void for later
                (new StringList).init(s, new StringList) 
            else { 
                if not (s <= car) then 
                    (new StringList).init(s, self)
                else
                    (new StringList).init(car, cdr.append(s))  -- append (recurevisly) to the tail
                fi;
            } fi;
        } fi
    };
};

-- Dead class BFS is too ass, I am trying to do DFS
-- class InDeg {
--     str: String;
--     in_deg: Int;

--     isNil() : Bool { {isvoid self;} };

--     init(s: String, in_deg: Int) : InDeg {
--         {
--             str <- s;
--             in_deg <- in_deg;
--             self;
--         }
--     };

--     deg() : Int {
--         in_deg
--     };

--     updateDeg(deg: Int) : Int {
--         in_deg <- deg
--     };

--     str() : String {
--         str
--     };

-- };

-- class InDegList {
--     car : InDeg;
--     cdr : InDegList;

--     isNil() : Bool { { isvoid car; } };

--     head() : InDeg { { car; } };

--     tail() : InDegList { { cdr; } };

--     append(in_deg : InDeg) : InDegList {
--         if isNil() then
--             (new InDegList).init(in_deg, new InDegList)
--         else
--             (new InDegList).init(car, cdr.append(in_deg))
--         fi
--     };

--     find(str: String) : Int {
--     if car.str() = str then {
--         car.deg();
--     } else {
--         if not cdr.isNil() then {
--             cdr.find(str); 
--         } else {
--             20000; -- cant return negatives in cool use a big number
--         } fi;
--     } fi
-- };

--     -- PROBLEMO: DOES NOT WORK
--     io : IO;
--     update(str: String, new_deg: Int) : InDegList {
--         {
--     io <- (new IO);
--     io.out_string("str: ");
--     io.out_int(new_deg);
--     io.out_string("\n");
--     if car.str() = str then {
--         (new InDegList).init((new InDeg).init(str, new_deg), cdr);
--     } else {
--         if not self.isNil() then {
--             (new InDegList).init(car, cdr.update(str, new_deg));
--         } else {
--             self;
--         } fi;
--     } fi;
--         }
--     };


--     init(in_deg: InDeg, rest: InDegList) : InDegList {
--         {
--             car <- in_deg;
--             cdr <- rest;
--             self;
--         }
--     };
-- };

class EdgeList {
    parent: String;
    child: String;
    next: EdgeList;

    isNil() : Bool { { isvoid parent; } };
    parent() : String {
        parent
    };

    child() : String {
        child
    };

    next() : EdgeList {
        next
    };

    init(p: String, c: String, nxt: EdgeList) : EdgeList {
        {
            parent <- p;
            child <- c;
            next <- nxt;
            self;
        }
    };

    insert(p: String, c: String) : EdgeList {
        if not (parent <= c) then { -- HAS TO COMPARE WITH CHILD OTHERWISE WILL NOT WORK, WILL CAUSE HOURS OF HEADACHE, its so you can put traverse each and every path all the way down
            (new EdgeList).init(p,c,self);
        } else {
            if isvoid next then {
                next <- (new EdgeList).init(p,c,next);
                self;
            } else {
                (new EdgeList).init(parent,child,next.insert(p,c)); 
            } fi;
        } fi
    };
};

class Main inherits IO {

    vert: StringList <- new StringList;
    final: StringList <- new StringList; -- final output
    trace: StringList <- new StringList; -- trace for check cycle
    edging: EdgeList <- new EdgeList;
    task1: String;
    task2: String;
    cycle_check: Bool <- false;

    is_in(lst: StringList, s: String) : Bool {
        if (isvoid lst) then { -- use isvoid way easier to use than the isNil from manual
            false;
        }
        else 
        {
            if lst.head() = s then 
                {   
                    true;
                }
            else {
                is_in(lst.tail(), s);
            }
            fi;
        } fi
    };

    print_reverse(s: StringList) : Object {
        if not (isvoid s) then {
                print_reverse(s.tail());
                out_string(s.head());
                if (s.head() = "") then
                    out_string("")
                else
                    out_string("\n")
                fi;
            }
            else {
                self;
            } fi
    };

    print_stringlist(s: StringList ) : Object {
            if not (isvoid s ) then {
                out_string(s.head());
                if (s.head() = "") then
                    out_string("")
                else
                    out_string("\n")
                fi;
                print_stringlist(s.tail());
            }
            else {
                self;
            } fi
    };

    print_edges (e: EdgeList) : Object {
        if not (isvoid e) then {
                out_string(e.parent());
                out_string(" -> ");
                out_string(e.child());
                out_string("\n");
                print_edges(e.next());
            }
            else {
                self;
            } fi
    };

    -- v: the node being explored the vertices
    dfs(v: String, edges : EdgeList) : Object {
        {
			if is_in(final, v) then {
                -- out_string("visited: ");
                -- out_string(v);
                -- out_string("\n");
				self;
            }
			else {
				if is_in(trace, v) then {
					cycle_check <- true;
					self;
				} else {			
					trace <- (new StringList).init(v, trace);
					let edge_iter : EdgeList<- edges in
					while not(isvoid edge_iter) loop {
						if edge_iter.parent() = v then
							dfs(edge_iter.child(), edges)
						else
							self	
						fi;
						edge_iter <- edge_iter.next();
					} pool;
					final <- (new StringList).init(v, final);
                    -- out_string("Final: ");
                    -- print_stringlist(final);
                    -- out_string("\n");
				} fi;
				
			} fi;		
		}
    };

    topo_sort(vert: StringList, edges: EdgeList) : StringList {
        -- iter through the keys like in khans algo
        let iter : StringList <- vert in {
            while not (isvoid iter) loop {
                -- out_string("Checking head: ");
                -- out_string(iter.head());
                -- out_string("\n");
                if iter.head() = "" then
                    {out_string("");
                    iter <- iter.tail();}
                else {
                    dfs(iter.head(), edges);
                    iter <- iter.tail();}
                fi;
            } pool;

            if cycle_check then {
                new StringList;
            } else {
                final;
            } fi;
        }
    };

    main() : Object {
    {
        task1 <- in_string();
        task2 <- in_string();
        let loop_check : Bool <- true in
        while not (task1 = "") loop
            {
                -- out_string("Task1: ");
                -- out_string(task1);
                -- out_string("\n");
                -- out_string(task2);
                -- out_string("\n");
                if is_in(vert, task1) then {
                    -- out_string("is_in vert: ");
                    -- print_stringlist(vert);
                    out_string("");
                    -- out_string("checking: \n");
                    -- out_string(task1);
                    -- out_string("\n");
                }
                else {
                    if isvoid vert then {
                        vert <- (new StringList).init(task1,vert);
                        -- out_string("isvoid vert: ");
                        -- print_stringlist(vert);
                    }
                    else {
                        vert <- vert.append(task1);
                        -- out_string("append task1 vert: ");
                        -- print_stringlist(vert);
                        out_string("");
                        -- out_string(task1);
                        -- out_string("\n");
                    } fi;
                } fi;
                if is_in(vert,task2) then {
                        -- out_string("is_in task2 vert: ");
                        -- print_stringlist(vert);
                        out_string("");
                    }
                    else {
                        vert <- vert.append(task2);
                        -- out_string("append task2 vert: ");
                        -- print_stringlist(vert);
                        -- out_string("\n");
                        -- out_string("Task1: \n");
                        -- out_string(task1);
                        -- out_string("\n");
                        -- out_string("Task2: \n");
                        -- out_string(task2);
                        -- out_string("\n");
                        -- out_string("vert:\n");
                        -- print_stringlist(vert);
                    } fi;
                    if isvoid edging then {
                        edging <- (new EdgeList).init(task2,task1,edging);
                    }
                    else {
                        edging <- edging.insert(task2,task1);
                    } fi;
                task1 <- in_string();
                task2 <- in_string();
            }
        pool;
        -- out_string("edges:\n");
        -- print_edges(edging);
        -- out_string("vert:\n");
        -- print_stringlist(vert);
        topo_sort(vert, edging);
        if cycle_check then {
            out_string("cycle\n");
        } else {
            print_stringlist(final);
        } fi;
    }
};

};
