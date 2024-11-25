class Main inherits IO {
	y : Int;
	x : Int;
	main() : Object
	{
		{
		y <- 4;
		x <- 3;
		if x < y then {
			out_int(5/3);
			x <- 4;
			x <- x + 1;	
		}
		else{
			out_int(2);
			x <- 10;
			x <- x + 1;
		}
		fi;
		}
	};
};
