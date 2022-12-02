f01:{(first;sum)@\:3#desc sum each where[null a]_a:0N,"J"$read0 x}

f02:{
	map0:{x!x}"ABC"cross" "cross"XYZ";
	map1:{$[a=1 2 3"ABC"?x;3;x="A";6*z="Y";6*z="XZ"x="B"]+a:1 2 3"XYZ"?z}.'map0;
	map2:{x,y,$[z="X";"ZXY";z="Y";"XYZ";"YZX"]"ABC"?x}.'map0;
	sum each map1@/:(::;map2)@\:read0 x}
