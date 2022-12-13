f01:{(first;sum)@\:3#desc sum each where[null a]_a:0N,"J"$read0 x}

f02:{
	map0:{x!x}"ABC"cross" "cross"XYZ";
	map1:{(3*(1+a-1 2 3"ABC"?x)mod 3)+a:1 2 3"XYZ"?z}.'map0;
	map2:map1{x,y,rotate["XYZ"?z;"ZXY"]"ABC"?x}.'map0;
	sum each(map1;map2)@\:read0 x}

f03:{sum each(("c"$a+38+58*a<27)!a:1+til 52)('[first;inter/]'')({(count'[x]div 2)cut'x};3 cut)@\:read0 x}

f04:{sum each{(any[x=y]|(<>/)x>y;(x[0]<=y 1)&y[0]<=x 1)}. flip each"J"$"-"vs''flip","vs'read0 x}

f05:{
	n:(1+count first r:read0 x)div 4;
	m:0;while[count r m;m+:1];
	d:(1+t)!(flip[r reverse til m-1]1+4*t:til n)except'" ";
	j:{"J"$(" "vs x)1 3 5}each(m+1)_r;
	f:{a:(b:neg z 0)#y[z 1];y[z 1]:b _y[z 1];y[z 2],:$[x;reverse;]a;y};
	(value')(last''){x z/y}[d;j]each f@/:10b}

f06:{a+0{-1+last first x[z]/(1b;y)}[{(y>count distinct x z+til y;1+z@:1)}first read0 x]\a:4 14}

f07:{
	f:{v:last o:" "vs y;$[y like"$ cd *";@[;1;$[v~1#"/";1#;v~"..";-1_;,[;enlist last[x 1],v,"/"]]];any y like/:("$ ls";"dir *");;@[;0;@[;x 1;("J"$first o)+0^]]]x};
	d:asc value first(()!0#0;enlist 1#"/")f/read0 x;
	(sum(1+d bin 100000)#d;d d binr last[d]-40000000)}

f08:{
	f:{x>0N,-1_maxs x};
	g:{(x{(count[x]-y+1)&1+flip[x[y]>/:(1+y)_x]?'0b}/:til c-1),enlist(c:count x)#0};
	o:(a;reverse b;reverse a;b:flip a:read0 x);
	(sum sum max@;max max prd@)@'(::;'[flip;reverse];reverse;flip)@'/:(f;g)@/:\:o}

f09:{(count')(distinct'){{$[2>max abs d:y-x;x;x+1&-1|d]}\[0 0;x]}\[9;flip(sums')(raze')a[1]#'/:(1 -1 0 0;0 0 1 -1)@\:"RLUD"?first a:("CJ";" ")0:x]1 9}

f10:{
	b:sums -1_1,last a:('[1+;like[;"addx"]];0^)@'("SJ";" ")0:x;
	(sum d*c -1+d:20+40*til 6;".#"0N 40#2>abs c-til[count c:b where a 0]mod 40)}

f11:{
	b:(where 0=count each a)_a:enlist[""],read0 x;
	t:{`n`s`o`d`t`f!(0;"J"$", "vs 18_x 2;value"{[old]",(19_x 3),"}"),"J"$last'[" "vs'-3#x]}each b;
	r:{[m;t]t{[m;t;j]
		if[c:count v:m t[j;`o]t[j;`s];
			t[j;`n]+:c;t[j;`s]:0#0;
			g:group 0=v mod t[`d]j;
			t[t[j;`t];`s],:v g 1b;t[t[j;`f];`s],:v g 0b];
		t}[m]/til count t};
	{prd 2#desc(y/[z;x])`n}[t]'[r@/:(div[;3];mod[;prd t`d]);20 10000]}

f12:{
	m:count first a:read0 x;
	a:@[a;e:(a:raze a)?"SE";:;"az"];
	g:{[a;e;m;x]
		r:distinct raze o@'where each(2>a[x 1]-/:a o)&0W=x[0]o:x[1]+/:(1;-1;m;neg m);
		(@[x 0;r;:;j];r;j:1+x 2;e in r)}["j"$a;e 0;m];
	b:g/['[not;last];(@[count[a]#0W;e 1;:;0];-1#e;0;0b)];
	(b 2;min b[0]where"a"=a)
	}
