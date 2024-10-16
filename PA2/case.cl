class Test {
  case_on_uninitialized(): Int {
    let x: Int <- 3 in 
    {
      case x of
        y: Test => 0;
        z: Test => 1;
      esac;
    }
  };
};

class Main inherits IO {
  y : String;
    main(): Object {
        {
            out_string("bruh");
        }
    };
};