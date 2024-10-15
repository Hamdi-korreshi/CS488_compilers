class Parent {
    f(): Int { 1 };
};
class Child inherits Parent {
    f(): String { "error" };
};

class Main inherits IO {
	main() : Object {
	out_string("A")
};
};
