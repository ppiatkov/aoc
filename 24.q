f01:{
	(a;b):asc each flip"J"$"   "vs/:read0 x;
	(sum abs a-b;sum c*(1+b bin c:a i)-j i:where count[a]>j:b?a)}

f02:{
	r:"J"$" "vs'read0 x;
	f:{d:1_deltas x;any[1_differ signum d]<all abs[d]in 1 2 3};
	a:f each r;
	b:any each(f''){x _/:til count x}each r;
	(sum a;sum a|b)}
