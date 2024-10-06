class Test {
  case_on_uninitialized(): Int {
    let x: Test <- new Test in 
    {
      case x of
        y: Test => 0;
        z: Test => 1;
      esac;
    }
  };
};

class Main inherits IO {

    main(): Object {
        {
            out_string("bruh");
        }
    };
};