f01:{
	(s;n):50 100; / Starting position and total number of distinct positions
	v:"J"$1_'t:read0 x; / Absolute values of rotation "distances"
	d:1 -1"RL"?first each t; / Signs corresponding to rotation directions
	p:-1 1_\:sums s,v*d; / Positions before and after each rotation
	(sum 0=p[1]mod n;sum abs(-).(p*\:d)div n)} / Rest of the owl

f02:{
	t:"-"vs'","vs first read0 x; / Raw ID ranges (as strings)
	r:raze{0N 2#1 rotate(y;x),raze flip(0 1#'"1"),/:'(n+til count[y]-n:count x)#'/:"90"}.'t; / Split ranges so that each range has a fixed number of digits
	c:{[r]a where 0=n mod a:1_1+til n:count r 0}each r; / Possible counts of digit groups for each range
	h:{[r;c] / Calculates "invalid IDs" for a given range and digit group count
		d:"J"$raze 1_'((c;0N)#count[r 0]#"0"),'"1"; / Divisor (like 11, 1001, 1001001 etc) based on number of digits and group count
		d*{x+til 1+y-x}.(ceiling;floor)@'("J"$r)%d}; / Divide endpoints and find exact multiples in the range
	g:{[h;r;c]distinct raze r h/:c}h; / Calculates invalid IDs for a given range and all given group counts
	f:{[g;r;c]sum raze g .'flip(r;c)}g; / Sums invalid IDs for all ranges and all given group counts
	r f/:(c inter'2;c)}

f03:{
	f:{"J"$first{[(b;y);x](b,a;(1+y?a:max(1-x)_y)_y)}/[("";y);reverse 1+til x]};
	sum each 2 12 f/:\:read0 x}

f04:{
	e:enlist count[first t:read0 x]#" ";
	m:((').'v cross h),/(v;h):((e,-1_;,[;e]1_);(" ",'-1_';,'[;" "]1_'));
	r:{@[;;:;"x"]'[y;where each("@"=y)&4>sum"@"=x@\:y]}m;
	(sum/')"x"=(r;r/)@\:t}
