class Main inherits IO {
	x: Int;
	y: String <- "Jack";
	z: Int;
	w: Int <- 56;
	main(): IO
	{
		if (w < 100) then
			out_string("w is small\n")
		else 
			out_string("w is large\n")
		fi
	};
};

