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

f04:{
	n:count t:read0 x;
	m:{[e;x;y;t] / Move
		v:{[e;y;t]a:y _t;b:y#e;$[y>0;a,b;b,a]}e; / Vertical shift
		h:{[e;x;t]a:neg[x]_'t;b:x#" ";$[x>0;b,/:a;a,\:b]}e; / Horizontal shift
		h[x]v[y]t}enlist n#" ";
	s:{[m;t;w;d]flip[raze each(m .'d)@\:t]in(w;reverse w)}[m;t];
	a1:sum sum"XMAS"s/:(f,'f;f,'neg f;f,'0;0,'f:til 4);
	a2:sum all"MAS"s/:(r,'r;r,'reverse r:-1_f);
	(a1;a2)}

f05:{
	(a;b):"J"$"|,"vs/:'0 1_'(0,(0=count each t)?1b)_t:read0 x;
	m:count'[b]div 2;
	d:distinct raze a;
	e:a where@'(all'')(d in/:b)@\:d?a;
	a1:sum b[w]@'m w:where c:all each(<).''b?'e;
	g:b[n]{{y{$[(>/)j:x?y;x;@[x;j;:;x reverse j]]}/x}[y]/[x]}'e n:where not c;
	a2:sum g@'m n;
	(a1;a2)}

f06:{
	n:2+count first i:read0 x;
	r:raze r,(" ",/:i,\:" "),r:enlist n#" ";
	m:(1;neg n;-1;n);
	d:">^<v";
	j@:first w:where(n*n)>j:r?d;
	o:first w;
	v:{[r;m;(j;o;p;a)]
		q:j+m o;
		b:"#"=c:r q;
		h:$[" "=c;0N 0N;b;(j;(o-1)mod 4);(q;o)];
		h,$[b;(p,enlist h;h in p);(p;0b)]};
	a1:-1+count u:distinct first each e:v[r;m]\[{not null first x};(j;o;();0b)];
	u:u except(j;0N);
	a2:"j"$sum{[v;j;o;r;m;x]last v[@[r;x;:;"#"];m]/[{not[first last x]&not null first x};(j;o;();0b)]}[v;j;o;r;m]each u;
	(a1;a2)}

f07:{
	f:{[o;x]
		(a;b):"J"$@[": "vs x;1;" "vs];
		g:{[o;a;x;y]r where a>=r:raze o .\:(x;y)};
		a*any a=(1#b)g[o;a]/1_b};
	p:{"J"$string[x],\:string y};
	sum each((*;+);(*;+;p))f/:\:read0 x}

f08:{
	n:count t:read0 x;
	a:flip(div;mod).\:(value group w!r w:where"."<>r:raze t;n); / Antennas for each frequency
	p:{f c where not(=).'c:r cross r:til count f:flip x}each a; / Ordered antenna pairs
	b:raze{(2*y)-x}.''p; / First candidate antinode for each pair
	a1:count distinct b where all each b within\:(0;n-1);
	f:{$[0<z;ceiling(x-y)%z;0>z;1+floor y%abs z;0W]}n;
	g:{z+/:d*/:til min x'[z;d:z-y]}f; / All antinodes starting from the second antenna in each pair
	a2:count distinct raze raze g .''p;
	(a1;a2)}

f09:{
	t:0N 2#first read0 x;
	(f;s):0 -1_'("J"$''t)./:(::;)each 0 1; / Lengths of files and free space
	b:{(where differ x)_x}d:where f; / File blocks
	c:{sum x*til count x}; / Calculates checksum
	a1:c count[d]#raze b,'sums[0,s]_reverse d; / Fill free space with file blocks from the end
	g:s#'0N; / Initial free space blocks populated by nulls
	m:{[(b;g);j] / Tries to move a file block into a free space gap
		$[0=count w:where count[b j]<=sum each null j#g;(b;g); / Not enough free space
		(@[b;j;0N*];@[g;first w;{@[y;(til count x)+y?0N;:;x]}b j])]};
	(b;g):m/[(b;g);reverse til count b]; / Try to move every file block
	a2:c raze b,'g,enlist 0#0; / Merge blocks
	(a1;a2)}

f10:{
	t:read0 x;
	p:raze til[count t],/:'where each t="0";
	f:{[t;(p;h;j)] / Iterates current positions, corresponding trailheads and step #
		w:where j=t ./:r:raze p+/:\:(0 1;1 0;-1 0;0 -1);
		(r w;h[where count[p]#4]w;first string 1+"J"$j)
		}t;
	o:flip`e`s!2#f/[9;(p;til count p;"1")];
	a1:exec sum e from select count distinct e by s from o;
	a2:exec sum c from select c:count i by s from o;
	(a1;a2)}

f12:{
	n:count t:read0 x;
	l:flip each flip(div;mod).\:(value group raze t;n); / Lists of positions for each letter
	m:{[(g;l)](g,l where b;l where not b:l in raze g+\:/:(0 1;1 0;0 -1;-1 0))}; / Moves neighbours from list to a group
	c:{[m;(g;l)]if[count l;(p;l):m/[(1#l;1_l)];g,:enlist p];(g;l)}m; / Collects a region
	g:raze c{[c;l]first c/[(();l)]}'l; / Split every letter into regions
	a:count each g; / Area
	p:(4*a)-{sum 1=sum each abs raze x-\:/:x}each g; / Total perimeter is a sum of perimeters minus number of borders between neighbours
	s:{sum x{sum{(00b~2#b)|110b~b:(x+/:z*/:(1 0;0 1;1 1))in y}[x;y]each(1 1;1 -1;-1 1;-1 -1)}\:x}each g; / Number of sides = number of corners
	(sum a*p;sum a*s)}

f13:{
	t:"J"$(last''')"++="vs''/:","vs''3#'0N 4#read0 x;
	r:@[;2;10000000000000+]each t;
	f:{[((aX;aY);(bX;bY);(pX;pY))]
		e:(pX*bY;aX*pY)-(pY*bX;aY*pX);
		$[all 0=e mod d:(aX*bY)-aY*bX;e div d;0 0]};
	g:{[f;x]sum sum 3 1*'flip f each x}f;
	(g t;g r)}

f23:{
	s:asc each t:`$"-"vs'read0 x; / Sorted sets of two connected computers
	r:flip t,reverse each t;
	c:r[1]group r 0; / Mapping computer -> list of computers connected to it
	e:{[c;s] / Extends the fully connected sets adding one computer
		u:c last flip s; / Try all computers connected to the last in the set
		v:s[where count each u],'raze u;
		v@:where c{all(-2_y)in x last y}/:v; / Keep only fully connected sets
		distinct asc each v}c;
	a1:count a where any each(a:e s)like\:"t*";
	a2:","sv string first e/[{1<count x};s];
	(a1;a2)}

f25:{
	p:1_'(where 0=count each t)_t:enlist[""],read0 x;
	"j"$sum all each count[p 0]>=raze(+/:\:).(sum'')"#"=p group[p[;0;0]]"#."
	}
