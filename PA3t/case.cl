class Test {
  case_on_uninitialized(): Int {
    let x: Int <- 3 in 
    {
      case x of
        y: Test => 0;
      esac;
    }
  };
};

class Main inherits IO {
  x : Test;
  y : String;
    main(): Object {
        {
                let x: Int <- 3 in 
                {
                case x of
                    y: Int => 0;
                esac;
                };
            out_string("bruh");
        }
    };
};