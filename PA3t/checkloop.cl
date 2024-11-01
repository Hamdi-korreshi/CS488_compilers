class Main inherits IO {
	main() : Object {
		{
		let x : Int <- 5 in
		while not (x = 0) loop
			{
				out_int(x);
				x <- x-1;
			}
		pool;
		}
	};
};
