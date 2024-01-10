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
	f:{$[null w:last where -1_a=last a:x 0;1b;not b[w]~last b:x 1]}; / Finds periodic pattern
	r:first{(z[0],x a;b,enlist a:y last b:z 1)}[l;c]/[f;(1#0;enlist p)];
	o:1000000000 mod m:neg(-). -2#where r=last r;
	(l t p;r count[r]-1+(mod[count[r]-1;m]-o)mod m)}

f15:{
	h:{(17*x+"j"$y)mod 256}/[0;];
	a1:sum h each o:","vs first t:read0 x;
	f:{l:`$s:$[d:"-"=last z;-1_z;first a:"="vs z];@[y;x s;$[d;_[;l];,[;(1#l)!1#"J"$a 1]]]}h;
	r:f/[256#enlist(0#`)!0#0;o];
	a2:sum(1+til count r)*{sum(1+til count x)*x}each r;
	(a1;a2)}

f16:{
	o:raze t:e,t,e:enlist(n:count first t:" ",/:read0[x],\:" ")#""; / Add margins and flatten
	m:(1;neg n;-1;n); / Shift right/up/left/down
	g:(!/)flip( / Next direction
		(".";(1#0;1#1;1#2;1#3));
		("/";(1#1;1#0;1#3;1#2));
		("\\";(1#3;1#2;1#1;1#0));
		("-";(1#0;0 2;1#2;0 2));
		("|";(1 3;1#1;1 3;1#3)));
	s:{[o;m;g;x] / Finds next positions/directions and adds to visited list
		a:update p:p+m@/:d from ungroup select p,g[o p]@'d from x 1;
		(union;except).\:(select distinct from a where" "<>o p;x 0)}[o;m;g];
	f:{exec count distinct p from first x/[count last@;2#enlist([]p:1#y;d:z)]}s; / Repeats until there are no new positions/directions
	e:raze flip each((1+v;0);(u+n*k-2;1);(-2+n+v:1_n*til -1+k:count t;2);(n+u:1_til n-1;3)); / Edges
	(first;max)@\:f .'e}

f17:{
	n:count first p:0N,/:(-48+"j"$read0 x),\:0N;
	k:count r:e,p,e:enlist n#0N;
	o:raze r;
	m:(1;neg n;-1;n);
	st:([]p:n+1;d:0 3;l:0;h:0);
	v:update h:0W from(delete from(update p:i from([]l:o))where null l);
	v:raze v{y;update d:y from x}/:til 4;
	f:{[o;m;lmin;lmax;x]
		s1:select p:p+m@/:d,d,l:l+1,h from(s:x 0)where l<lmax;
		sx:select from s where l>=lmin;
		s2:select p:p+m@/:d,d,l:1,h from update d:mod[d+1;4]from sx;
		s3:select p:p+m@/:d,d,l:1,h from update d:mod[d-1;4]from sx;
		a:select distinct from(update h:h+o p from s1,s2,s3)where not null h;
		b:select min h by p,d,l from a;
		b:3!(0!b)where(value[b]`h)<x[1][key[b]]`h;
		(b;x[1]upsert b)}[o;m];
	w:{[f;st;n;k;v;lmin;lmax]
		a:`p`d`l xkey raze v{y;update l:y from x}/:1+til lmax;
		b:f[lmin;lmax]/[count first@;(st;a)];
		exec min h from last[b]where p=-2+n*k-1}[f;st;n;k;v];
	(w[0;3];w[4;10])}

f18:{
	o:{("RDLU"?x 0;"J"$a 1;"J"$b 5;256 sv"X"$'0N 2#"0",-1_b:-1_2_last a:" "vs x)}each read0 x;
	f:{(x[0]+s*(1 0;0 1;-1 0;0 -1)d;s+x 1;x[2]+(s:y 1)*x[0;0]*0 1 0 -1 d:y 0)}; / Stokes' theorem
	{1+div[x 1;2]+abs x 2}each f/[(0 0;0;0);]each 2 -2#'\:o}

f19:{
	o:(0,where 0=count each t)_t:read0 x;
	p:{(!/)"SJ"$'flip"="vs'","vs -1_1_x}each 1_o 1;
	f:(!/)flip{a:(0,x?"{")_x;(`$a 0;flip{c:"J"$2_b:first a:":"vs x;(@[;`$b 0;]/:[;(&;|)[d,1-d]@'c+d:">"=b 1];`$last a)}'[-1_b],e(e:enlist;`$last b:","vs -1_1_a 1))}'[o 0];
	r:enlist`x`m`a`s`w!(4#enlist 1 4001),`in;
	g:(1#`w)_{a:raze x{e:enlist y;$[`A=w:y`w;e;{(-1_x),y last x}/[(1#`w)_e;b 0],'([]w:last b:x w)]}/:y;select from a where w<>`R}[f]/[r];
	(sum sum p where(any')(all'')p within'/:\:0 -1+/:/:g;sum(prd')(-/'')g)}

f20:{
	p:flip`nm`typ`tgt!flip{a:" -> "vs x;(`$(b in"%&")_a 0;$["%"=b:first a 0;`f;"&"=b;`c;`b];`$", "vs a 1)}'[read0 x];
	s:update on:0b,src:count[i]#enlist 0#` from p;
	s:s lj select src:nm by nm:tgt from ungroup[p]where tgt in exec nm from p where typ=`c;
	s:1!update mem:count'[src]#'0b from s;
	g:{[s;n;pl;c] / Returns (state;targets;c received high pulse;pulses)
		if[null t:(d:s n)`typ;:(s;0#`;0b;0#0b)];
		o:$[`f=t;[if[pl;:(s;0#`;0b;0#0b)];s[n;`on]:not d`on];`c=t;not all d`mem;pl];
		(update mem:{[mem;src;s;v]@[mem;src?s;:;v]}'[mem;src;n;o]from s where typ=`c,nm in u;u;o&n=c;count[u:d`tgt]#o)};
	f:{[g;c;x;y;z] a:g[x 0;y;z;c];(a 0;x[1],a 1;x[2]|a 2;x[3],a 3)}g;
	h:{a:x[y]/[(z 0;0#`;z 2;0#0b);z 1;z 3];if[0=count a 1;:z];a,enlist z[4]+0^count'[group a 3]01b}f;
	k:{enlist[1+z 0],@[;0 2 4]x[y]/[(z 1;1#`broadcaster;0b;1#0b;1 0+z 3)]}h;
	a1:prd last k[`]/[1000;j:(0;s;0b;0 0)];
	u:first exec nm from s where`rx in/:tgt;
	a2:prd k{first y[z]/[{not x 2};x]}[j]/:exec nm from s where u in/:tgt;
	(a1;a2)}

f21:{
	if[(0=n mod 2)|count[first p]<>n:count p:read0 x;'"nyi"];
	o:(0#;::)@\:s:enlist(a w;w:(n>a:p?'"S")?1b);
	f:{distinct raze x+\:/:(0 1;0 -1;1 0;-1 0)};
	g:{z where"#"<>x ./:z mod y}[p;n];
	e:{y where all each y within(0;x)}n-1;
	h:{(a;x[a:y 1]except y 0)};
	a1:sum first each 0N 2#(count last@)each h[g e f@]\[64;o];
	r:{(c;-2#2 deltas/c:z[0],enlist b;-2#b:-1_raze z[2]+/:sums 0N 2#(count last@)'[a];last a:x\[y;z 3])}[h[g f@];n];
	j:r/[{$[2>count x 0;1b;not[(~/)x 1]]};(();();0 0;o)];
	c:{x[1],(x[2]-x 0)div 2}[-3#j[0;;k]],j[1;1;k:(N:26501365)mod n]div 2;
	a2:sum c*1,a,a*a:(N div n)-count[j 0]-2;
	(a1;a2)}

f22:{
	p:flip`x1`y1`z1`x2`y2`z2!flip{raze"J"$","vs'"~"vs x}each read0 x;
	p:update b:0b from p; / Add "bottom" flag
	p:`z1 xasc enlist[cols[p]!(min p`x1;min p`y1;0;max p`x2;max p`y2;0;1b)],p;
	f:{
		d:x y;
		s:select from x where x2>=d`x1,x1<=d`x2,y2>=d`y1,y1<=d`y2,z2<d`z2;
		x[y;`z1`z2]-:d[`z1]-1+m:0^exec max z2 from s;
		if[any exec b from s where z2=m;x[y;`b]:1b];
		x};
	g:{x/[y;exec i from y where not b]}f;
	r:update b:i=0 from g/[p];
	c:{a[`z1]=(x/[a:y _z])`z1}[g;r]each 1_til count r;
	(sum all each c;sum sum each not c)}

f23:{
	o:@[raze t;1+m:count first t:read0 x;:;"v"];
	n:w where 2<>sum o[(w:where o=".")+/:v:(1;neg m;-1;m)]in".",r:">^<v"; / Nodes
	d:raze n,/:'where each r=/:o n+\:v; / Directed graph
	f:{(1+z 0;a;(("#"<>x[b])&z[1]<>b:y+a:z[1]+y z 2)?1b)}[o;v];
	d:flip`s`e`c!flip d[;0],'{@[;1 0]x/[{(0=y 0)|not y[1]in x}y;0,z]}[f;n]each d;
	u:d,update s:e,e:s from d; / Undirected graph
	g:{[n;d;x]
		a:select s:e,h:(h,'s),l:l+c from ej[`s;x 0;d];
		a:delete from a where s in'h;
		b:last[n]=a`s;
		(a where not b;x[1]|exec max l from a where b)}n;
	g{last x[y]/[{count x 0};(([]s:1;h:enlist 0#0;l:0);0)]}/:(d;u)}

f24:{ / (p_a)_i + (v_a)_i*t_a = P_i +V_i*t_a  =>  P_i*(v_a)_j - V_i*(p_a)_j - (p_a)_i*(v_a)_j - (i<->j) - (a->b) = 0,  i,j=x,y,z, a,b=1,...,N
	o:"f"${`px`py`pz`vx`vy`vz!"J"$'raze[","vs'"@"vs x]}each read0 x;
	g:{inv[1 -1*/:x@\:`vy`vx]mmu{prd[x`px`vy]-prd[x`py`vx]}each x};
	f:{$[all within[s:x"f"$a:(y;z);200000000000000 400000000000000];all s{$[0<y`vx;>;<].(x 0;y`px)}/:a;0b]}g;
	q:{[o;x]
		a:1 -1 -1 1*/:(c:2*til 4){x[y;z]-x[y+1;z]}[o]/:\:x;
		b:sum each 1 -1*/:c{prd[x[y;z]]-prd[x[y+1;z]]}[o]/:\:x@/:(3 0;1 2);
		"j"$inv[a]mmu b}o;
	(sum raze{y[z]x/:(z+1)_y}[f;o]each til count o;sum(2#q`vy`vx`py`px),q[`vz`vx`pz`px]1)}

