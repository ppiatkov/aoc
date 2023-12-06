f01:{
	a:raze b:string til 10;
	v:((a;b);(a,1_a;b,string`one`two`three`four`five`six`seven`eight`nine));
	f:{{"J"$raze where'[x=/:(min;max)@\:x]}(x[0]where count'[a])!raze a:y ss/:x 1};
	sum v f\:/:read0 x}

f02:{
	a:read0 x;
	b:{0^(exec max n by c from flip`n`c!flip raze({("J"$a 0;`$(a:" "vs x)1)}'')", "vs/:"; "vs(2+x?":")_x)`red`green`blue}each a;
	sum each(1+where all each 12 13 14>=/:b;prd each b)}

f03:{
	m:2+n:count first i:read0 x;
	t:raze".",/:(r,i,r:enlist n#"."),\:"."; / Add empty margins and flatten to 1D indexing
	b:0N 2#1_where differ t within"09"; / Start and end positions of numbers
	p:"J"$t c:{x+til y-x}.'b; / Numbers and all their positions
	f:m{a:(-1+first y),1+last y;raze a,(a,y)+/:x*1 -1}/:c; / Positions adjacent to numbers
	a1:sum p where any each not"."=t f;
	e:where 2=count each group raze f@'where each"*"=t f;
	a2:sum prd each p where each flip e in/:f;
	(a1;a2)}

f04:{
	n:count t:read0 x;
	m:{sum(in)."J"$0N 3#/:@[" | "vs x;0;last": "vs]}each t; / Number of matches in each card
	(sum prd each(0<m)*(m-1)#'2;sum{@[x;1+z+til y;x[z]+]}/[n#1;m;til n])}

f05:{
	s:value 7_first t:read0 x; / Seeds
	m:{flip`d1`s1`l!flip"J"$" "vs'x}each 2_'where[0=count each t]_t; / Maps
	m:{`s1 xasc select s1,s2:s1+l,d1,d2:d1+l,sh:d1-s1 from x}each m; / Source/destination start/end and shift
	m:{(0#x)upsert/((-0W;p;-0W;p:x[0;`s1];0);x;(p;0W;p:last[x]`s2;0W;0))}each m; / Fill before/after ranges
	m:{j:where x[`s2]<1_x[`s1],0W;x upsert flip`s1`s2`d1`d2`sh!(p;r;p:x[j;`s2];r:x[j+1;`s1];0)}each m; / Fill gaps
	c:{raze{ / Composition of two maps
		a:select from x where{(x[1]>y 0)&y[1]>x 0}[(y`d1`d2);(s1;s2)];
		d:y`sh;
		a:update s1:(s1|y`d1)-d,s2:(s2&y`d2)-d,sh:sh+d from a;
		update d1:s1+sh,d2:s2+sh from a}[y]each x};
	t1:([]s1:s;s2:s+1;d1:s;d2:s+1;sh:0); / Seed ranges for part 1
	t2:select s1,s2,d1:s1,d2:s2,sh:0 from update s2:s1+l from flip`s1`l!flip 0N 2#s; / Seed ranges for part 2
	m{exec min d1 from(x/)enlist[z],y}[c]/:(t1;t2)}

p06:{
	t:read0 x;
	p1:{"J"$last each 0N 2#where[differ" "=a]_a:last":"vs x}each t;
	p2:{"J"$last[":"vs x]except" "}each t;
	f:{-1+(-).(ceiling;floor)@'a+/:1 -1*\:sqrt((a*a:x[0]%2)-x 1)};
	(prd f p1;f p2)}
