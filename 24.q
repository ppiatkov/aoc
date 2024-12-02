f01:{
	(a;b):asc each flip"J"$"   "vs/:read0 x;
	(sum abs a-b;sum c*(1+b bin c:a i)-j i:where count[a]>j:b?a)}

f02:{
	f:{ / Process a single report
		if[(c:count d)<2*sum 0>d:1_deltas x;d:neg d]; / Work with positive deltas
		b:$[a:all g:d in s:1 2 3;1b; / All safe
			1=e:sum n:not g;$[(j:n?1b)in(0;c-1);1b;any(d[j]+d j+1 -1)in s]; / If delta on edge or safe total with neighbour
			2=e;$[1<abs(-/)w:where n;0b;sum[d w]in s]; / If adjacent deltas with safe total
			0b]; / Can't fix more than two unsafe deltas
		(a;b)};
	sum f each"J"$" "vs'read0 x}

f03:{
	t:raze read0 x;
	f:{ / Finds and sums multiplications
		a:4_'ss[x;"mul("]_x;
		a:(1+a?'")")#'a;
		a:-1_'a where")"=last each a;
		a@:where 1=sum each a=",";
		a@:where all each a in",",.Q.n;
		sum prd each"J"$","vs'a};
	r:(0,asc ss[t;n:"don't()"],ss[t;"do()"])_t;
	r@:where not r like n,"*";
	(f t;sum f each enlist[r 0],4_'1_r)}
