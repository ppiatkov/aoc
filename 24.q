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

f11:{
	t:"J"$" "vs first read0 x;
	b:{[(x;m)]
		r:{$[x~0;1#1;0=(c:count s:string x)mod 2;"J"$(0;c div 2)_s;1#2024*x]}each x;
		(key;value)@\:sum each m[where count each r]group raze r};
	{[t;b;n]sum last b/[n;(t;count[t]#1)]}[t;b]each 25 75
	}

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

f14:{
	(p;v):("JJ";",")0:/:flip 2_''" "vs't:read0 x;
	d:101 103;
	c:{[d;p;v;n] / Calculates quadrant counts after n seconds
		f:(p+v*n)mod d; / Final positions
		"j"$(sum'')(&\:/:). flip(>;<).\:(f;d div 2)}[d;p;v];
	a1:prd raze c 100;
	i:abs(-/'')(sum'')d#'(q;flip each q:c each til max d); / Quadrant imbalances along each dimension
	o:{x?max x}each i; / Offsets of periodically repeating steps with anomalously large imbalances
	a2:first(inter/)o+d*til each reverse d;
	(a1;a2)}

f15:{
	c:(0=count each t:read0 x)?1b;
	w:c#t; / Map of the warehouse
	m:raze(c+1)_t; / List of movements
	d:">^<v"!(0 1;-1 0;0 -1;1 0); / Movement directions
	f:{raze til[count x],/:'where each x=y}; / Finds map positions of a given char
	w1:.[w;p1:first f[w;"@"];:;"."]; / Find initial robot position and mark it free
	s:{[d;b;(w;p);m]$[ / Attempts to make the next step (moving some boxes if necessary)
		"#"=o:w . n:p+d m;(w;p); / Wall ahead
		"."=o;(w;n); / Go to the free space ahead
		not first u:b[w;n;m];(w;p); / Immovable boxes ahead
		(last u;n)]}d; / Shift all boxes in front and go to the space ahead
	b1:{[d;w;p;m]$[ / Shifts a single box if possible, returns tuple (success; new map)
		"#"=o:w . n:p+d m;(0b;w); / Wall ahead of the box
		"."=o;(1b;.[;;:;]/[w;(p;n);".O"]); / Move the box to the free space ahead
		not first u:.z.s[d;w;n;m];(0b;w); / Immovable boxes ahead
		(1b;.[;;:;]/[last u;(p;n);".O"])]}d; / Shift all boxes in front and move this box
	r:{[f;s;m;b;w;p;c]sum sum 100 1*flip f[first s[b]/[(w;p);m];c]}[f;s;m];
	a1:r[b1;w1;p1;"O"];
	sw:ssr/[;("OO";"@@");("[]";"@.")]each w@\:where count[w 0]#2; / Stretched warehouse map
	w2:.[sw;p2:first f[sw;"@"];:;"."]; / Find initial robot position and mark it free
	b2:{[d;w;p;m] / Shifts a double box if possible, returns tuple (success; new map)
		$[m in"<>";
			$[ / Moving box horizontally
				"#"=o:w . n1:c+n:p+c:d m;(0b;w); / Wall ahead of the box
				"."=o;(1b;.[;;:;]/[w;(p;n&n1;n|n1);".[]"]); / Move the box to the free space ahead
				not first u:.z.s[d;w;n1;m];(0b;w); / Immovable boxes ahead
				(1b;.[;;:;]/[last u;(p;n&n1;n|n1);".[]"])]; / Shift all boxes in front and move this box
			$[ / Moving box vertically
				any"#"=(o;o1):w ./:(n:c+p;n1:(c:d m)+p1:p+0,$["]"=w . p;-1;1]);(0b;w); / Wall ahead of the box
				all"."=(o;o1);(1b;.[;;:;]/[w;(p;p1;n&n1;n|n1);"..[]"]);
				not first u:.z.s[d;w;$[o in"[]";n;n1];m];(0b;w); / Immovable boxes ahead
				not first v:.z.s[d;last u;p;m];(0b;w); / Immovable boxes ahead on the other side
				(1b;last v)]]}d; / Shift all boxes in front and move this box
	a2:r[b2;w2;p2;"["];
	(a1;a2)}

f16:{
	i:j,/:\:j:til count t:read0 x; / All map indices
	b:t{in[1_b;(1100b;0011b)]<first b:"#"<>x ./:y+/:(0 0;0 1;0 -1;1 0;-1 0)}/:/:i; / A node is not a wall or a path between walls
	n:10b cross raze j,/:'where each b; / Nodes (direction; vertical position; horizontal position)
	f:{[t;j;b] a:1_(,':)where b;j,''a where"#"<>t ./:j,'1+a[;1]}; / Finds horizontal links
	h:n?1b,''raze j f[t]'b; / Horizontal links
	v:n?0b,''(reverse'')raze j f[flip t]'flip b; / Vertical links
	s:flip 2 0N#til count n; / Same-position links (90 degree rotation)
	l:update`g#start from flip`start`end`score!flip(a,reverse each a:h,v,s),'a,a:raze((-).'(n@/:h)[;;2];(-).'(n@/:v)[;;1];count[s]#1000); / All links
	q:{[t;c](i;m i:(count'[t]>m:t?'c)?1b)}; / Finds a matching char on the map
	sn:n?1b,q[t;"S"]; / Start node
	en:where(1_'n)~\:q[t;"E"]; / End nodes (two possible directions)
	l:update score:0 from l where start in en,end in en; / Make end nodes equivalent to use only one of them
	d:{[n;l;(cur;visited;scores)] / Dijkstra
		if[null cur;:(cur;visited;scores)];
		r:select from l where cur=start,not end in visited;
		scores[r`end]&:r[`score]+scores cur;
		visited,:cur;
		w:where[scores<0W]except visited;
		cur:w{first where x=min x}scores w;
		(cur;visited;scores)};
	ns:last d[n;l]/[(sn;0#0;@[count[n]#0W;sn;:;0])]; / Calculate all node scores
	a1:ns en 0;
	p:{[n;l;ns;en;x] / Collects all optimal paths
		e:select from x where start=en;
		o:select from x where not start=en;
		e,select start:end,visited:(visited,'j)from ej[`start;o;l]where ns[end]=score+ns start};
	m:l distinct raze exec visited from p[n;update j:i from l;ns;en 0]/[([]start:1#sn;visited:enlist 0#0)];
	r:select 1_'n start,1_'n end from m;
	a2:count distinct raze exec start{x+/:signum[d]*/:til 1+abs sum d:y-x}'end from r;
	(a1;a2)}

f17:{
	reg:(`$'a[;1;0])!"J"$last each a:" "vs'3#t:read0 x;
	p:"J"$","vs last" "vs last t;
	instr:{[opc;lit;(ptr;reg;out)]
		cmb:$[lit within 0 3;lit;lit=4;reg`A;lit=5;reg`B;lit=6;reg`C;"err"];
		xor:{0b sv(<>). 0b vs'(x;y)};
		$[
			opc=0;(ptr+2;@[reg;`A;div[;prd cmb#2]];out);
			opc=1;(ptr+2;@[reg;`B;xor lit];out);
			opc=2;(ptr+2;@[reg;`B;:;cmb mod 8];out);
			opc=3;($[reg`A;lit;ptr+2];reg;out);
			opc=4;(ptr+2;@[reg;`B;xor reg`C];out);
			opc=5;(ptr+2;reg;out,cmb mod 8);
			opc=6;(ptr+2;@[reg;`B;:;reg[`A]div prd cmb#2];out);
			opc=7;(ptr+2;@[reg;`C;:;reg[`A]div prd cmb#2];out)]};
	step:{[instr;p;(ptr;reg;out)]instr[p ptr;p ptr+1;(ptr;reg;out)]}[instr;p];
	o:{[step;p;reg]last step/[{x>first y}count p;(0;reg;0#0)]}[step;p];
	a1:","sv string o reg;
	u:{[o;x]o`A`B`C!(8 sv x;0;0)}o;
	f:{[p;u;x] $[count[p]=count x 0;x;distinct a where(neg[count b 0]#p)~/:b:u each a:raze x,/:\:til 8]}[p;u];
	a2:min 8 sv'f/[enlist 0#0];
	(a1;a2)}

f18:{
	t:"J"$","vs'read0 x;
	d:{[n;t;(cur;visited;scores)] / Dijkstra
		if[()~cur;:(cur;visited;scores)];
		new:(cur+/:(1 0;-1 0;0 1;0 -1))except t,visited;
		new@:where all each new within(0;n-1);
		scores:.[;;(1+scores . cur)&]/[scores;new];
		visited,:enlist cur;
		w:raze[til[n],/:'where each scores<0W]except visited;
		cur:w{first where x=min x}scores ./:w;
		(cur;visited;scores)};
	f:{[d;n;t;m]a:d[n;m#t]/[(0 0;();.[(n;n)#0W;0 0;:;0])];last[a].(n-1;n-1)}[d;71;t];
	a1:f m:1024;
	b:{[f;(s;e)]$[e=s+1;(s;e);0W=f a:"j"$(s+e)%2;(s;a);(a;e)]}f; / Binary search
	a2:","sv string t first b/[(m;-1+count t)];
	(a1;a2)}

f19:{
	(towels;designs):(", "vs first@;2_)@\:read0 x;
	t:count'[towels]!towels,'"*";
	f:{[t;design;ways]
		if[count[design]<=offset:min key ways;:ways];
		ways[offset+where(offset _design)like/:t]+:ways offset;
		(k where offset<k:key ways)#ways}t;
	g:{[f;design]f[design]/[(1#0)!1#1]count design};
	("j"$sum not null a;sum a:f g/:designs)}

f20:{
	(start;end):"SE"{j,w j:(count[y 0]>w:y?'x)?1b}\:t:read0 x;
	step:{[t;(cur;prv)](first n[where"#"<>t ./:n:cur+/:(0 1;1 0;0 -1;-1 0)]except enlist prv;cur)};
	c:count path:first each step[t]\[{not x~first y}end;(start;0N 0N)];
	r:raze n{[path;n;x]b where n<=x-a+b:sum each abs path[x]-'/:path a:til 1+x-n}[path]/:n+til c-n:100;
	"j"$sum each r<=/:2 20}

f21:{
	// D(N,X,Y) - shortest distance between keys X an Y at level N.
	// It is defined as the minimal number of basic (level 0) key presses
	// required to be made after pressing key X before key Y can be pressed.
	// D(N,X,X)=0.
	// Assume D(N,X,Y) is known for all X and Y at a given N.
	// We need to calculate D(N+1,X,Y) for all X and Y.
	// Define a "node" as a tuple (key position at level N; key position at level N+1).
	// The links are:
	// (X;Z) -> (Y;Z), weight = D(N,X,Y)
	// (X;Z) -> (X;X[Z]), weight = 1 (if X[Z] exists)
	// D(N+1,X,Y) = shortest path from (A;X) to (A;Y).
	nextDists:{[dirs;map;nodes;allkeys;dists]
		f:{[dirs;map;nodes;allkeys;dists;x]
			neighbours:{[dirs;map;dists;node]
				n:dirs except x:node 0;
				w:dists x,'n;
				a:(n,'y:node 1;w);
				$[null b:map(y;x);a;(a[0],enlist(x;b);w,1)]}[dirs;map];
			d:{[neighbours;dists;nodes;(cur;visited;scores)] / Dijkstra
				if[null cur;:(cur;visited;scores)];
				(neighbs;weights):neighbours[dists;nodes cur];
				n:nodes?neighbs;
				n@:w:where not n in visited;weights@:w;
				scores[n]&:weights+scores cur;
				visited,:cur;
				w:where[scores<0W]except visited;
				cur:w{first where x=min x}scores w;
				(cur;visited;scores)}[neighbours;dists;nodes];
			r:d/[(j;0#0;@[count[nodes]#0W;j:nodes?"A",x;:;0])];
			(x,'allkeys)!(nodes!last r)"A",'allkeys}[dirs;map;nodes;allkeys;dists];
		raze f each allkeys};
	compl:{[nextDists;t;n]
		nk:("789";"456";"123";" 0A"); / Numeric keypad
		dk:(" ^A";"<v>"); / Directional keypad
		kd:{ / Creates dict (start_key;direction) -> destination_key for a keypad
			f:{(reverse each a;a@:where all each" "<>a:raze 1_'(,':')x)};
			o:raze p:raze f each(x;flip x);
			(o[;0],'"><v^"where count each p)!o[;1]};
		(dnum;ddir):kd each(nk;dk);
		dnodes:dirs cross dirs:raze[dk]except" ";
		nnodes:dirs cross nums:raze[nk]except" ";
		dists0:dnodes!count[dnodes]#0;
		dists:nextDists[dirs;ddir;dnodes;dirs]/[n;dists0];
		dists:nextDists[dirs;dnum;nnodes;nums;dists];
		sum dists{("J"$y except"A")*sum(1+x)@/:1_({y,x}':)"A",y}/:t}[nextDists;read0 x];
	compl each 2 25}

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
