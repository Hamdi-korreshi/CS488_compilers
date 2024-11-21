class Main inherits IO {
	x: Int;
	y: String <- "Jack";
	z: Int;
	w: Int <- 56;
	main(): IO
	{
		{
		out_int(3);
		out_string("\n");
		x <- 5;
		out_int(x+5);
		out_string("\n");
		}
	};
};
