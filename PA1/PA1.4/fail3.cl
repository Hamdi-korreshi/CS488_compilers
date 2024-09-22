class StringList {
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

    sort_insert(s: String) : StringList {
        if not (car <= s ) then {
            (new StringList).init(s,self);
        }
        else {
            -- add at the end
            if isvoid cdr then {
                cdr <- (new StringList).init(s,cdr);
                self; -- has to return self otehrwise fails
            }
            else {
                (new StringList).init(car, cdr.sort_insert(s));
            } fi;
        } fi
    };

    preend(s: String) : StringList {
            if s = "" then
                self  -- If the string is empty, return the list as-is without appending
            else { 
                if isvoid cdr then  -- If the tail is void, append as the last element
                    (new StringList).init(s, new StringList)  -- Create a new node with an empty tail
                else { 
                    if not (s <= car) then  -- Insert the new element in lexicographical order
                        (new StringList).init(s, self)
                    else
                        (new StringList).init(car, cdr.preend(s))  -- Recursively append to the tail
                    fi;
                } fi;
            } fi
    };

    append(s : String) : StringList {
        if s = "" then
            self  -- If the string is empty, return the list as-is without appending
        else { 
            if isvoid cdr then  -- If the tail is void, append as the last element
                (new StringList).init(s, new StringList)  -- Create a new node with an empty tail
            else { 
                if not (s <= car) then  -- Insert the new element in lexicographical order
                    (new StringList).init(s, self)
                else
                    (new StringList).init(car, cdr.append(s))  -- Recursively append to the tail
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
--             (new InDegList).init(in_deg, new InDegList)  -- Add the element and initialize the list
--         else
--             (new InDegList).init(car, cdr.append(in_deg))  -- Recursively append to the tail
--         fi
--     };

--     find(str: String) : Int {
--     if car.str() = str then {
--         car.deg();  -- Return the degree if found
--     } else {
--         if not cdr.isNil() then {
--             cdr.find(str);  -- Recursively search in the tail
--         } else {
--             20000;  -- Return a large default value if the task is not found
--         } fi;
--     } fi
-- };

--     -- PROBLEMO
--     io : IO;
--     update(str: String, new_deg: Int) : InDegList {
--         {
--     io <- (new IO);
--     io.out_string("str: ");
--     io.out_int(new_deg);
--     io.out_string("\n");
--     if car.str() = str then {
--         (new InDegList).init((new InDeg).init(str, new_deg), cdr);  -- Return updated list with modified head
--     } else {
--         if not self.isNil() then {
--             (new InDegList).init(car, cdr.update(str, new_deg));  -- Recursively update the tail
--         } else {
--             self;  -- If not found, return the current list unchanged
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
        (new EdgeList).init(p,c,self)
    };
};

class Main inherits IO {

    vert: StringList <- new StringList;
    rec_stack : StringList <- new StringList;
    visited : StringList <- new StringList;
    edging: EdgeList <- new EdgeList;
    task1: String;
    task2: String;
    cycle: Bool <- false;
    final: StringList <- new StringList;

    is_in(lst: StringList, s: String) : Bool {
        if (isvoid lst) then {
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

    im_edging (e: EdgeList) : Object {
        if not (isvoid e) then {
                out_string(e.parent());
                out_string(" -> ");
                out_string(e.child());
                out_string("\n");
                im_edging(e.next());
            }
            else {
                self;
            } fi
    };

    -- print_indeglist(lst: InDegList) : Object {
    --     if not (lst.isNil()) then {
    --         out_string("Task: ");
    --         out_string(lst.head().str());
    --         out_string(", In-degree: ");
    --         out_int(lst.head().deg());
    --         out_string("\n");
    --         print_indeglist(lst.tail());  
    --     } else {
    --         out_string("The in-degree list is empty.\n");
    --     } fi
    -- };

    -- Function to initialize in-degree for all vertices
    -- init_in_degree() : InDegList {
    --     let temp_vert : StringList <- vert in {
    --         while not (isvoid temp_vert) loop {
    --             -- if (temp_vert.head() = "") then {
    --             --     out_string("");
    --             --     temp_vert <- temp_vert.tail();
    --             -- }
    --             -- else {
    --                 let task : String <- temp_vert.head() in {
    --                     indeglist <- indeglist.append((new InDeg).init(task, 0));  -- Start with 0 in-degree
    --                     temp_vert <- temp_vert.tail();
    --                 };
    --             -- } fi;
    --         } pool;
    --         indeglist;
    --     }
    -- };

    -- Calculate in-degrees based on edges
--     calculate_in_degree() : InDegList { {
--     print_indeglist(indeglist);  -- Debugging print to see the list before updates
--     let temp_edge : EdgeList <- edging in {
--         while not (isvoid temp_edge) loop {
--             let parent_task : String <- temp_edge.parent() in
--             let child_task : String <- temp_edge.child() in {
--                 let child_deg : Int <- indeglist.find(child_task) in {
--                     if child_deg = 20000 then {
--                         out_string("Task not found: ");  -- Debugging output
--                         out_string("\n");
--                     } else {
--                         out_string("the Int ");
--                         out_int(child_deg);
--                         out_string("\n");
--                         out_string("Updating in-degree for task: ");  -- Debugging output
--                         out_string(child_task);
--                         out_string("\n");
--                         indeglist <- indeglist.update(child_task, (child_deg + 1));  -- Increment in-degree
--                         out_string("Updated in-degree for ");
--                         out_int(indeglist.find(child_task));
--                         out_string("\n");
--                     } fi;
--                 };
--                 temp_edge <- temp_edge.next();
--             };
--         } pool;
--     };
--     indeglist; }
-- };

-- Kahn's Algorithm for topological sorting with cycle detection
    -- kahns_algorithm() : StringList {
    --         let topo_sort : StringList <- new StringList in {
    --             init_in_degree();  -- Initialize in-degrees
    --             calculate_in_degree();  -- Calculate in-degrees based on edges
    --             -- out_string("got to top\n");
    --             -- Add nodes with in-degree 0 to the queue
    --             -- out_string("InDegList: \n");
    --             -- print_indeglist(indeglist);
    --             let temp_vert : InDegList <-indeglist in {
    --                 while not temp_vert.isNil() loop {
    --                     let node : InDeg <- temp_vert.head() in {
    --                         if (node.deg() = 0) then { 
    --                             if queue.isNil() then {
    --                                 queue <- queue.append(node.str());
    --                             }
    --                             else {
    --                                 queue <- queue.append(node.str());
    --                             } fi;
    --                         }
    --                         else
    --                             self
    --                         fi;
    --                         temp_vert <- temp_vert.tail();
    --                     };
    --                 } pool;
    --             };
    --             -- out_string("got to queue\n");
    --             -- Process the queue
    --             -- out_string("completed queue \n");
    --             -- print_stringlist(queue);
    --             while not (isvoid queue) loop {
    --                 let curr_task : String <- queue.head() in {
    --                     topo_sort <- topo_sort.append(curr_task);  -- Add task to the sorted list
    --                     queue <- queue.tail();

    --                     -- Reduce the in-degree of each child of curr_task
    --                     let temp_edge : EdgeList <- edging in {
    --                         while not (isvoid temp_edge) loop {
    --                             if temp_edge.parent() = curr_task then {
    --                                 let child_task : String <- temp_edge.child() in {
    --                                     let child_deg : Int <- indeglist.find(child_task) in
    --                                     indeglist <- indeglist.update(child_task, child_deg - 1);
    --                                     let child_deg : Int <- indeglist.find(child_task) in
    --                                     if (child_deg - 1 = 0) then
    --                                         queue.append(child_task)
    --                                     else
    --                                         self
    --                                     fi;
    --                                 };
    --                             }
    --                             else
    --                                 self
    --                             fi;
    --                             temp_edge <- temp_edge.next();
    --                         } pool;
    --                     };
    --                 };
    --             } pool;

    --             -- Check for cycle detection
    --             let cycle_detected : Bool <- false in {
    --                 let temp_indeglist : InDegList <- indeglist in {
    --                     while not temp_indeglist.isNil() loop {
    --                         let node : InDeg <- temp_indeglist.head() in {
    --                             if not (node.deg() <= 0) then
    --                                 cycle_detected <- true
    --                             else
    --                                 self
    --                             fi;
    --                             temp_indeglist <- temp_indeglist.tail();
    --                         };
    --                     } pool;
    --                 };

    --                 if cycle_detected then {
    --                     out_string("Cycle detected! Topological sort is not possible.\n");
    --                     (new StringList);  -- Return an empty list if a cycle is detected
    --                 } else {
    --                     topo_sort;  -- Return the topologically sorted list
    --                 } fi;
    --             };
    --         }
    -- };
    dfs(v: String, edges : EdgeList, t_visited : StringList, p_visited : StringList) : Object {
    if is_in(p_visited, v) then {
        self;
    } else {
        if is_in(t_visited, v) then {
            cycle <- true;
            self;
        } else {
            t_visited <- t_visited.prepend(v);

            let edge_ptr : EdgeList <- edges in
            while not (isvoid edge_ptr) loop {
                if edge_ptr.parent() = v then {
                    dfs(edge_ptr.child(), edges, t_visited, p_visited);
                } else {
                    self;
                } fi;
                edge_ptr <- edge_ptr.next();
            } pool;

            p_visited <- p_visited.prepend(v);
            final <- final.prepend(v);
        } fi;
    } fi
};


dfs(v: String) : Object {
    if is_in(rec_stack, v) then {
        out_string("cycle");
        cycle <- true;
    } else {
        if not is_in(visited, v) then {
            visited <- visited.append(v);
            rec_stack <- rec_stack.append(v);

            let edge : EdgeList <- edging in {
                while not (isvoid edge) loop {
                    if edge.parent() = v then {
                        let child : String <- edge.child() in {
                            dfs(child);
                        };
                        } else {
                            out_string("");
                        } fi;
                    edge <- edge.next();
                } pool;
            };
            -- out_string("Task to be appended: ");
            -- out_string(v);
            -- out_string("\n");
            final <- (new StringList).init(v, final);
            rec_stack <- rec_stack.tail();
        } else {
            out_string("");
        } fi;
    } fi
};

topo_sort() : Object {
    let temp_vert : StringList <- vert in {
        while not (isvoid temp_vert) loop {
            if not is_in(visited, temp_vert.head()) then {
                dfs(temp_vert.head());
            } else {
                out_string("");
            } fi;
            temp_vert <- temp_vert.tail();
        } pool;

        if cycle then {
            new StringList;
        } else {
            final;
        } fi;
    }
};



    -- -- v : vertice or node
    -- dfs(v: String) : Object {
    --     if is_in(rec_stack, v) then {
    --         out_string("hit cycle\n");
    --         cycle <- true;
    --     } else {
    --         if not is_in(vs, v) then {
    --             visited <- visited.append(v);
    --             rec_stack <- rec_stack.append(v);

    --             let edge : EdgeList <- edging in {
    --                 while not (isvoid edge) loop {
    --                     if edge.parent() = v then {
    --                         let child : String <- edge.child() in {
    --                             dfs(child);
    --                         };
    --                     } else {
    --                         out_string("");
    --                     } fi;
    --                     edge <- edge.next();
    --                 } pool;
    --             };
    --         }
    --         else {
    --             final.append(v);
    --             rec_stack <- rec_stack.tail();
    --             out_string("");
    --         } fi;
    --     } fi
    -- };

    -- topo_sort() : Object {
    --     let temp_vert : StringList <- vert in {
    --         while not (isvoid temp_vert) loop {
    --             out_string("Task: ");
    --             out_string(temp_vert.head());
    --             out_string("\n");
    --             dfs(temp_vert.head());
    --             temp_vert <- temp_vert.tail();
    --         } pool;
    --     if cycle then {
    --         new StringList;
    --     } else {
    --         final;
    --     } fi;
    --     }
    -- };

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
                -- out_string("Task2: ");
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
                        vert <- (new StringList).append(task1);
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
                    if is_in(vert,task2) then {
                        -- out_string("is_in task2 vert: ");
                        -- print_stringlist(vert);
                        out_string("");
                    }
                    else {
                        vert <- vert.append(task2);
                        -- out_string("append task2 vert: ");
                        -- print_stringlist(vert);
                        out_string("");
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
                } fi;   
                task1 <- in_string();
                task2 <- in_string();
            }
        pool;
        -- out_string("edges:\n");
        -- im_edging(edging);
        -- im_edging(edging);
        -- out_string("vert:\n");
        -- print_stringlist(vert);
        -- let sorted_list : StringList <- topo_sort() in {
        --     if not sorted_list.isNil() then {
        --         out_string("Topologically sorted tasks:\n");
        --         print_stringlist(sorted_list);  -- Print sorted list
        --     } else {
        --         out_string("Cycle detected. Topological sort not possible.\n");
        --     } fi;
        -- };
        -- out_string("final \n");
        -- print_stringlist(final);
        topo_sort();
        print_stringlist(final);
    }
};

};
dfs(v: String) : Object {
    if is_in(rec_stack, v) then {
        out_string("cycle");
        cycle <- true;
    } else {
        if not is_in(visited, v) then {
            visited <- visited.append(v);
            rec_stack <- rec_stack.append(v);

            let edge : EdgeList <- edging in {
                while not (isvoid edge) loop {
                    if edge.parent() = v then {
                        let child : String <- edge.child() in {
                            dfs(child);
                        };
                        } else {
                            out_string("");
                        } fi;
                    edge <- edge.next();
                } pool;
            };
            -- out_string("Task to be appended: ");
            -- out_string(v);
            -- out_string("\n");
            final <- (new StringList).init(v, final);
            rec_stack <- rec_stack.tail();
        } else {
            out_string("");
        } fi;
    } fi
};

topo_sort() : Object {
    let temp_vert : StringList <- vert in {
        while not (isvoid temp_vert) loop {
            if not is_in(visited, temp_vert.head()) then {
                dfs(temp_vert.head());
            } else {
                out_string("");
            } fi;
            temp_vert <- temp_vert.tail();
        } pool;

        if cycle then {
            new StringList;
        } else {
            final;
        } fi;
    }
};
