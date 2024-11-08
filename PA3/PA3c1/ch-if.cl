class Main inherits IO {

	main() : Object {
		let x : Int <- 34 in 
		let y : Int <- 18 in 
		if (x < y*2) then
			out_string("x greater than\n")
		else {
			if (y < x/2) then 
				out_string("y greater than\n")
			else
				out_string("nothing is correct\n")
			fi;
		}
		fi
	};
};


