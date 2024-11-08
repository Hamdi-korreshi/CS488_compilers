class Main inherits IO {
    main() : Object {
        let age : Int <- in_int() in
        if age < 18 then
            out_int(1)  -- Outputs 1 if age is less than 18 (considered a minor)
        else {if not (age <= 18) = (age < 65) then
            out_int(2)
        else
            out_int(3)
        fi;
          }  -- Outputs 3 if age is 65 or more (considered a senior)
        fi
    };
};

