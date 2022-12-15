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
	t:{("J"$", "vs 18_x 2;value"{[old]",(19_x 3),"}"),"J"$last'[" "vs'-3#x]}each b;
	d:(m:til n:count t),'1_'t;
	r:{z{y[2;j]+:count w:where y[0]=j:z 0;y[1;w]:o:x z[1]y[1;w];y[0;w]:?[0=o mod z 2;z 3;z 4];y}[y]/x}d;
	f:(m where count'[e];raze e:t[;0];n#0);
	{prd 2#desc last y/[z;x]}[f]'[r@/:(div[;3];mod[;prd d[;2]]);20 10000]}

f12:{
	m:count first a:read0 x;
	a:@[a;e:(a:raze a)?"SE";:;"az"];
	g:{[a;e;m;x]
		r:distinct raze o@'where each(2>a[x 1]-/:a o)&0W=x[0]o:x[1]+/:(1;-1;m;neg m);
		(@[x 0;r;:;j];r;j:1+x 2;e in r)}["j"$a;e 0;m];
	b:g/['[not;last];(@[count[a]#0W;e 1;:;0];-1#e;0;0b)];
	(b 2;min b[0]where"a"=a)}

f13:{
	a:("[[2]]";"[[6]]";""),read0 x;
	b:{value ssr/[x;("[[]]";"[[]";",");("()";"enlist[";";")]}'[raze 2#'0N 3#a];
	c:{$[(0>tx:type x)&0>ty:type y;$[x<y;-1;x=y;0;1];
	(tx>=0)&ty<0;.z.s[x;enlist y];
	(tx<0)&ty>=0;.z.s[enlist x;y];
	last{[f;x;y;cx;cy;v]
		$[(ex:cx=j)|ey:cy=j:v 1;(1;j+1;$[ex>ey;-1;ex<ey;1;0]);
			($[r;1;0];j+1;r:f[x j;y j])]}[.z.s;x;y;count x;count y]/['[not;first];3#0]]};
	s:{{$[2>c:count z;z;{raze{(z[0],z[2-a;0];a _z 1;(1-a:x . y z[1 2;0])_z 2)
		}[x;y]/[{all count'[1_x]};enlist[0#z],z]}[x;y].z.s[x;y]each 2 0N#z]}[x;y;til count y]};
	i:iasc s['[-1=;c];b];
	(sum 1+where(</)flip 0N 2#2_i;prd 1+2#i)}

f14:{
	b:{value each" -> "vs ssr[x;",";" "]}each read0 x;
	c:distinct raze{(1#d),d[0]+\raze{signum[x]*/:abs[sum x]#1}each 1_d:deltas x}each b;
	p:500-o:min(first min c;-2+500-h:3+last max c);
	s:.[;;:;1b]/[(@[3+max d;0;(2*h+2)|])#0b;d:c-\:(o;0)];
	s[;a:-1+count s 0]:1b;
	f:{r:{v:y[1;0];w:y[1;1]+(y[1;1]_x v)?1b;$[not x[v-1;w];(1b;(v-1;w));x[v+1;w];(0b;(v;w-1));(1b;(v+1;w))]
		}[s:z 2]/[first;(1b;(x;0))];$[y=r[1;1];(0b;1+z 1;s);(1b;1+z 1;.[s;r 1;:;1b])]}p;
	-1 0+(1b;0;s){y/[first;x]1}/:f@/:(a-1;0)}
