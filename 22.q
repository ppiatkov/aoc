f01:{(first;sum)@\:3#desc sum each where[null a]_a:0N,"J"$read0 x}

f02:{
	map0:{x!x}"ABC"cross" "cross"XYZ";
	map1:{(3*(1+a-1 2 3"ABC"?x)mod 3)+a:1 2 3"XYZ"?z}.'map0;
	map2:map1{x,y,rotate["XYZ"?z;"ZXY"]"ABC"?x}.'map0;
	sum each(map1;map2)@\:read0 x}

f03:{sum each(("c"$a+38+58*a<27)!a:1+til 52)('[first;inter/]'')({(count'[x]div 2)cut'x};3 cut)@\:read0 x}

f04:{sum each{(any[x=y]|(<>/)x>y;(x[0]<=y 1)&y[0]<=x 1)}. flip each"J"$"-"vs''flip","vs'read0 x}

f05:{
	n:(1+count first r:read0 x)div 4;
	m:0;while[count r m;m+:1];
	d:(1+t)!(flip[r reverse til m-1]1+4*t:til n)except'" ";
	j:{"J"$(" "vs x)1 3 5}each(m+1)_r;
	f:{a:(b:neg z 0)#y[z 1];y[z 1]:b _y[z 1];y[z 2],:$[x;reverse;]a;y};
	(value')(last''){x z/y}[d;j]each f@/:10b}

f06:{a+0{-1+last first x[z]/(1b;y)}[{(y>count distinct x z+til y;1+z@:1)}first read0 x]\a:4 14}

f07:{
	f:{v:last o:" "vs y;$[y like"$ cd *";@[;1;$[v~1#"/";1#;v~"..";-1_;,[;enlist last[x 1],v,"/"]]];any y like/:("$ ls";"dir *");;@[;0;@[;x 1;("J"$first o)+0^]]]x};
	d:asc value first(()!0#0;enlist 1#"/")f/read0 x;
	(sum(1+d bin 100000)#d;d d binr last[d]-40000000)}
