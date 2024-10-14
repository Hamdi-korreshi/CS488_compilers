class One inherits Two {
	x : String;
};

class Two inherits One {
	x : String;
};

class Main inherits IO {
	main () : Object{
	out_string("a")
};
};
