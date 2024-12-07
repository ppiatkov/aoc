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
