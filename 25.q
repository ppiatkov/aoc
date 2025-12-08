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

f05:{
	b:(0=count each i:read0 x)?1b;
	r:"J"$"-"vs'b#i; / Ranges
	ids:"J"$(b+1)_i;
	m:{[r] / Merges adjacent and overlapping end-exclusive ranges
		(s;e):flip r;
		p:0!update sums s from select sum s by e from([]s,e;1 -1 where 2#count s); / Accumulated +/- are zero at merged ranges' endpoints
		p[`e]@/:(0,-1_w+1;w:where 0=p`s)};
	(s;e):m r+\:0 1;
	(sum max ids within/:r;sum e-s)}

f06:{
	o:(prd;sum)"+"=l w:where" "<>l:last t:read0 x;
	sum each o@'/:"J"$'''(flip';::)@\:w _flip -1_t}

f07:{(::;sum)@'{[(n;s);t](n+sum 0<a;(s*not b)+(1_a,0)+0,-1_a:s*b:"^"=t)}/[(0;"S"=t 0);1_t:read0 x]}

f08:{
	r:"J"$","vs'read0 x; / Junction box coordinates
	p:r j:raze j,''til each j:til c:count r; / Coordinates of junction box pairs
	i:j iasc sum each v*v:(-).'p; / Pairs of junction box indexes sorted by distance
	f:{[i;n] / Returns grouped indexes of junction boxes for a given number of them connected
		(s;e):reverse[a],'a:flip n#i; / Start and end indexes of each connected pair
		d:e group s; / Direct connections of each box
		g:{[(u;d)]$[count d;(u,enlist x;(x:{distinct y,/x y}[d]/[1#key d])_d);(u;d)]}; / Groups connected boxes
		first(g/)(();d)}i;
	a1:{prd 3#desc count each x}f 1000;
	b:{[c;f;(s;e)]$[1=e-s;(s;e);c=count first f m:(s+e)div 2;(s;m);(m;e)]}[c;f]; / Binary search of threshold connection number
	a2:first prd r i first(b/)(0;count i);
	(a1;a2)}
