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

f06:{
	t:read0 x;
	p1:{"J"$last each 0N 2#where[differ" "=a]_a:last":"vs x}each t;
	p2:{"J"$last[":"vs x]except" "}each t;
	f:{-1+(-).(ceiling;floor)@'a+/:1 -1*\:sqrt((a*a:x[0]%2)-x 1)};
	(prd f p1;f p2)}

f07:{
	t:flip`h`b!flip{@[" "vs x;1;"J"$]}each read0 x; / Hands and their bids
	c:("23456789TJQKA";"J23456789TQKA"); / Cards sorted by their strengths in parts 1 and 2
	s:{x desc value count each group y}(1 1 1 1 1;2 1 1 1;2 2 1;3 1 1;3 2;4 1;1#5)!til 7; / Hand strength
	f:{(cross/)?["J"=y;count[y]#1#x;(),/:y]}c; / Generates hands with all J substitutions
	w:update h:t[`h]n from ungroup enlist[`h]_update n:i,m:f each h from t; / Original and modified hands
	p1:update r:s each h,u:13 sv'c[0]?/:h from t; / Evaluate type and strength within type for part 1
	p2:select by n from`r`u xasc update r:s each m,u:13 sv'c[1]?/:h from w; / Same for part 2
	{exec sum b*i+1 from`r`u xasc x}each(p1;p2)}

f08:{
	d:"LR"?first t:read0 x; / Directions
	m:{(`u#first each x)!(1_'x)}`${(3#x;x 7 8 9;x 12 13 14)}each(2_t); / Node map
	a1:last{(x[1;y 0]d v mod count d:x 0;1+v:y 1)}[(d;m)]/['[`ZZZ<>;first];`AAA,0]; / Part 1 answer
	k@:where each"AZ"=\:string[k:key m][;2]; / Start and end nodes for part 2
	a2:count[d]*prd{last({({x[y]z}[x]/[z 0;y];1+z 1)}. x)/[not any in[;y]first@;(z;0)]}[(m;d);k 1]each k 0; / Magic formula for magic input
	(a1;a2)}

f09:{sum{last each(sums;({y-x}\))@'(last;first)@/:\:reverse any(1_deltas@)\"J"$" "vs x}each read0 x}

f10:{
	p:raze -1 rotate o,(2;n:count first o:".",/:read0[x],\:".")#"."; / Add margins and flatten
	m:(+[;1];-[;n];-[;1];+[;n]); / Move right/up/left/down
	d:("-J 7";"F|7 ";" L-F";"L J|"); / In->out connectors
	r:(0 1 0N -1;-1 0 1 0N;0N -1 0 1;1 0N -1 0); / Rotations
	c:(1001b;1100b); / Counter-clockwise/clockwise contour coordinate corrections
	v:((0 0 0 -1;1 1 0 0;0 1 0 0;0 0 -1 -1);(0 1 0 0;0 1 1 0;0 0 0 -1;-1 0 0 -1)); / Vector coefficients for two contour orientations
	g:{[v;c;n;x;a] (v .\:(x 1;a 1))*(c@'x 1)+x[0]mod n}[v;c;n]; / Integrand for the closed contour
	p[s]:d[t:(2+u 0)mod 4;last u:where 4>d?'p m@'s:p?"S"]; / Start position, direction and connector
	f:{[p;m;d;x](m[a]b;a:d[x 1]?p b:x 0)}[p;m;d]; / Iterator for (input direction;position) pair
	h:{[f;r;g;x]a:f 2#x;a,(1+x 2;r[x 1;a 1]+x 3;g[x;a]+x 4)}[f;r;g]/[{(0=y 2)|x<>y 0}s;(s;t;0;0;0)]; / Calculate area using Stokes' theorem
	(h[2]div 2;abs h[4]0>h 3)}

f11:{
	f:{[t;m]
		p:raze(til[count t]+sums(m-1)*all'["."=t]),/:'(til[count t 0]+sums(m-1)*all"."=t)where'["#"=t];
		n:exec 1+max y1 from r:flip`x1`y1`x2`y2!flip p cross p;
		r:update r1:y1+n*x1,r2:y2+n*x2 from r;
		r:select from r where r2>r1;
		exec sum(abs x1-x2)+abs y1-y2 from r};
	read0[x]f/:2 1000000}

f12:{
	f:{
		a:(a;"?"sv 5#enlist a:first b:" "vs x);
		b:(b;raze 5#enlist b:"J"$","vs last b);
		g:{[a;b;d;j] / Finds possible end positions of jth group for all initial offsets in d
			h:{[a;b;j;c] / Finds possible end positions of jth group for initial offset c
				o:c+til$[e:j=-1+n:count b;0;neg count[l]+sum l:(j+1)_b]+(m:count a)+1-c+k:b j; / Potential start positions
				o:(1+a[-1_o]?"#")#o; / Must not have any new "#" before this group
				o@:where$[e;o>m-1+k+reverse[a]?"#";"#"<>a o+k]; / Must not have "#"/"#"s after
				o@:where not any each"."=a o+\:til k; / Must not have any "."s within this group
				$[e;(1#m-k)!1#count o;count each group 1+o+k]};
			sum value[d]*/:'h[a;b;j]each key d}; / Group number of arrangements by accumulated offset
		{first value x[y;z]/[(1#0)!1#1;til count z]}'[g;a;b]};
	sum f each read0 x}

f13:{
	p:1_'where[0=count'[o]]_o:enlist[""],read0 x;
	f:{1+where y{x=sum sum(<>/)(min count'[a])#'a:(reverse z#y;z _y)}[x]/:1_til count y};
	(sum')(sum'')(raze'')100 1*'/:0 1 f/:/:\:(p;flip each p)
	}

f14:{
	p:flip read0 x;
	t:({"#"sv desc each"#"vs x}'); / Tilt
	l:{sum(1+til count a)*reverse a:sum"O"=x}; / Load
	c:(reverse flip t@)/[4;]; / Cycle
	f:{x{all all 1_(=':)3 0N#neg[3*y]#x}/:2_til count[x]div 3}; / Finds periodic pattern
	r:first{(z[0],x a;a:y z 1)}[l;c]/[not any f first@;(1#0;p)];
	o:1000000000 mod m:2+f[r]?1b;
	(l t p;r count[r]-1+(mod[count[r]-1;m]-o)mod m)}
