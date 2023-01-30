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

f15:{
	t:{`SX`SY`BX`BY!"J"$@[2_'(" "vs x)2 3 8 9;0 1 2;-1_]}each read0 x;
	t:update d:sum abs(BX-SX;BY-SY)from t;
	a:{[t]
		Y:2000000;
		r:update h:abs SY-Y from t;
		r:update start:h+SX-d,end:1+SX+d-h from r;
		r:select start,end from r where start<=end;
		m:{flip c!r[j]@/:(0,-1_1+w;w:where 0=sums((2*count x)#1 -1)j:iasc r:raze flip x c:cols x)};
		(exec sum end-start from m r)-exec count distinct BX from t where BY=Y};
	b:{[t]
		s:update xpyt:SX+SY+d+1,xpyb:SX+SY-d+1,ymxt:(SY-SX)+d+1,ymxb:(SY-SX)-d+1 from t;
		xpy:first s[`xpyt]where count[s]>s[`xpyb]?s`xpyt;
		ymx:first s[`ymxt]where count[s]>s[`ymxb]?s`ymxt;
		sum 4000000 1*(xpy-ymx;xpy+ymx)div 2};
	(a;b)@\:t}

f17:{
	push::?[">"=first read0 x;1;-1];
	rocks::(0 1 2 3;1 9 10 11 19;0 1 2 11 20;0 9 18 27;0 1 9 10)+39;
	rounds::enlist`towerBlocks`floorHeight`totalHeight`nextRock`nextPush!(0#0;0;0;0;0);
	move::{[]
		if[all mod[movedRock:rock+push pushIdx;9]within 1 7;
			if[not any movedRock in state`towerBlocks;rock::movedRock]];
		pushIdx::(pushIdx+1)mod count push;
		$[9>min movedRock:rock-9;0b;any movedRock in state`towerBlocks;0b;[rock::movedRock;1b]]};
	fall::{[]
		state::last rounds;
		pushIdx::state`nextPush;
		rock::rocks[state`nextRock]+9*({$[count x;max x;0]}state`towerBlocks)div 9;
		(::)move/1b;
		newTowerBlocks:state[`towerBlocks],rock;
		shift:{0^last where 7=count each(union':)value(x mod 9)group x div 9}newTowerBlocks;
		if[shift;
			newTowerBlocks-:shift*9;
			newTowerBlocks@:where newTowerBlocks>8];
		newFloorHeight:shift+state`floorHeight;
		newTotalHeight:newFloorHeight+0^max[newTowerBlocks]div 9;
		newNextRock:(1+state`nextRock)mod count rocks;
		dejaVu:count[rounds]>j:(k#rounds)?(k:`towerBlocks`nextRock`nextPush)!(newTowerBlocks;newNextRock;pushIdx);
		`rounds upsert(newTowerBlocks;newFloorHeight;newTotalHeight;newNextRock;pushIdx);
		$[dejaVu;j;0N]};
	cycleStart::null fall/0N;
	cyclePeriod::-1+count[rounds]-cycleStart;
	heightPerCycle::(-/)rounds[`totalHeight]cycleStart+cyclePeriod,0;
	getHeight:{[n]
		$[n<cycleStart;rounds[n;`totalHeight];
			rounds[cycleStart+(n-cycleStart)mod cyclePeriod;`totalHeight]+heightPerCycle*(n-cycleStart)div cyclePeriod]};
	getHeight each 2022 1000000000000}

f18:{
	lava::flip`x`y`z!("JJJ";",")0:x;
	countSides:{[t]
		sides:{2*1+sum 1<1_deltas asc x};
		sx:exec sum s from select s:sides x by y,z from t;
		sy:exec sum s from select s:sides y by x,z from t;
		sz:exec sum s from select s:sides z by x,y from t;
		sx+sy+sz};
	vals:{x+til 1+y-x}.;
	cx:count xvals:vals xrange::(min;max)@\:lava`x;
	cy:count yvals:vals yrange::(min;max)@\:lava`y;
	cz:count zvals:vals zrange::(min;max)@\:lava`z;
	yzFaces:raze flip each`x`y`z!/:(;(cy*cz)#yvals;zvals where cz#cy)each xrange;
	xzFaces:raze flip each`x`y`z!/:((cx*cz)#xvals;;zvals where cz#cx)each yrange;
	xyFaces:raze flip each`x`y`z!/:((cx*cy)#xvals;yvals where cy#cx;)each zrange;
	surface:distinct xyFaces,yzFaces,xzFaces;
	frontier::externAir::surface except lava;
	propagateAir:{[b]
		`frontier upsert raze raze[`x`y`z{![;();0b;enlist[x]!enlist(y;x;1)]}\:/:(+;-)]@\:frontier;
		frontier::frontier except externAir,lava;
		if[last b;
			frontier::select from frontier where x within xrange,y within yrange,z within zrange];
		$[count frontier;[`externAir upsert frontier;10b];00b]};
	first propagateAir/11b;
	findGaps:{[t]
		g:{(m+til 1+max[x]-m:min x)except x};
		gx:ungroup `x`y`z#0!select g x by y,z from t;
		gy:ungroup `x`y`z#0!select g y by x,z from t;
		gz:ungroup `x`y`z#0!select g z by x,y from t;
		distinct gx,gy,gz};
	countSides each(lava;lava,findGaps[lava]except externAir)}

f19:{
	blueprints::{`id`m1r1`m1r2`m1r3`m2r3`m1r4`m3r4!"J"$@[" "vs x;1;-1_]1 6 12 18 21 27 30}each read0 x;
	initialStates::enlist`m1`m2`m3`m4`r1`r2`r3`r4!0 0 0 0 1 0 0 0;
	skipBuild::{[blueprint;states]
		states:delete from states where r1=blueprint`m1r4,r3=blueprint`m3r4,m1>=blueprint`m1r4,m3>=blueprint`m3r4;
		update m1:m1+r1,m2:m2+r2,m3:m3+r3,m4:m4+r4 from states};
	buildOreRobot::{[blueprint;states]
		states:delete from states where r1=blueprint`m1r4,r3=blueprint`m3r4;
		oreCost:blueprint`m1r1;
		t:select from states where m1>=oreCost,r1<max blueprint`m1r1`m1r2`m1r3`m1r4;
		update r1:r1+1,m1:m1+r1-oreCost,m2:m2+r2,m3:m3+r3,m4:m4+r4 from t};
	buildClayRobot::{[blueprint;states]
		states:delete from states where r1=blueprint`m1r4,r3=blueprint`m3r4;
		oreCost:blueprint`m1r2;
		t:select from states where m1>=oreCost,r2<blueprint`m2r3;
		update r2:r2+1,m1:m1+r1-oreCost,m2:m2+r2,m3:m3+r3,m4:m4+r4 from t};
	buildObsidianRobot::{[blueprint;states]
		states:delete from states where r1=blueprint`m1r4,r3=blueprint`m3r4;
		oreCost:blueprint`m1r3;
		clayCost:blueprint`m2r3;
		t:select from states where m2>=clayCost,m1>=oreCost,r3<blueprint`m3r4;
		update r3:r3+1,m1:m1+r1-oreCost,m2+r2-clayCost,m3:m3+r3,m4:m4+r4 from t};
	buildGeodeRobot::{[blueprint;states]
		oreCost:blueprint`m1r4;
		obsidianCost:blueprint`m3r4;
		t:select from states where m3>=obsidianCost,m1>=oreCost;
		update r4:r4+1,m1:m1+r1-oreCost,m2:m2+r2,m3:m3+r3-obsidianCost,m4:m4+r4 from t};
	step::{[blueprint;minutes;states;minute]
		newstates:skipBuild[blueprint;states],/(buildOreRobot;buildClayRobot;buildObsidianRobot;buildGeodeRobot).\:(blueprint;states);
		pruneStates[blueprint;newstates;minutes;minute]};
	maxGeodes::{[blueprint;minutes]
		states:step[blueprint;minutes]/[initialStates;1+til minutes];
		exec max m4 from states};
	pruneStates::{[blueprint;states;minutes;minute]
		if[not remainingSteps:minutes-minute;:states];
		t:distinct states;
		minGeodes:exec max m4+remainingSteps*r4 from t;
		if[count f:select from states where minGeodes=m4+remainingSteps*r4,r1=blueprint`m1r4,r3=blueprint`m3r4;:1#f];
		if[minGeodes>maxExtraGeodes:(remainingSteps*remainingSteps-1)div 2;
			t:select from t where minGeodes<=maxExtraGeodes+m4+remainingSteps*r4];
		g:t{update c:count each j from 0!?[x;();(1#`b)!1#y;(1#`j)!enlist(asc;`i)]}/:cols t;
		b:g{[g;d]
			i:iasc n:g{[g;v]exec sum c from g where b>=v}'v:value d;
			first a:{[x;g;v]
				if[x 0;:x];
				e:$[(::)~x 1;;inter[;x 1]]exec raze j from g where b>=v;
				(1=count e;e)}/[(0b;::);g i;v i]}/:t;
		t where b};
	(sum(blueprints@'`id)*maxGeodes[;24]each blueprints;prd maxGeodes[;32]each 3#blueprints)}

f20:{
	g:{[a;c]
		t:flip`val`idx!(a;til n:count a);
		f:{[t;j]
			o:t[j;`idx];v:t[j;`val];
			n:$[v=0;o+v;v>0;(o+v)mod -1+count t;1+(-1+o+v)mod -1+count t];
			t:$[n=o;t;
				n>o;update idx:idx-1 from t where idx within(o;n);
				update idx:idx+1 from t where idx within(n;o)];
			t[j;`idx]:n;t};
		b:exec val from r:`idx xasc f/[t;(c*n)#til n];
		sum r[`val]r[`idx]?((1!r)[0;`idx]+1000 2000 3000)mod n};
	g'[1 811589153*\:first(enlist"J";",")0:x;1 10]}

f21:{
	b::{if[2=count v:" "vs x;:".p1.",x];if[(v:@[v;0 1 3;".p1.",])[2]~1#"/";v[2]:"div"];" "sv v}each a:read0 x;
	(::){any 10=type each@[value;;::]each b}/1b;
	f:{
		if[2=count v:" "vs x;
			:enlist$["humn:"~v 0;"::";".p2.",x]];
		u:@[v;0 1 3;".p2.",];u[0]:-1_u 0;
		if["root:"~v 0;
			:(u[1],":",u 3;u[3],":",u 1)];
		o:u 2;
		$[o~1#"*";(u[0],":",u[1],"*",u 3;u[1],":",u[0]," div ",u 3;u[3],":",u[0]," div ",u 1);
			o~1#"+";(u[0],":",u[1],"+",u 3;u[1],":",u[0],"-",u 3;u[3],":",u[0],"-",u 1);
			o~1#"-";(u[0],":",u[1],"-",u 3;u[1],":",u[0],"+",u 3;u[3],":",u[1],"-",u 0);
			o~1#"/";(u[0],":",u[1]," div ",u 3;u[1],":",u[0],"*",u 3;u[3],":",u[1]," div ",u 0);
			'"op"]};
	c::raze f each a;
	(::){@[value;;::]each c;()~key`.p2.humn}/1b;
	r:(.p1.root;.p2.humn);
	.p1:.p2:1#.q;
	r}

f23:{
	xy2p::{[N;x;y]y+N+2*N*x+N}N:1000000000;
	p2xy::{[N;p](p div 2*N;p mod 2*N)-N}N;
	p0:xy2p . b:flip raze til[count a],/:'where each"#"=a:read0 x;
	chk::{[p;dir]
		x:first xy:p2xy p;y:last xy;
		testp:xy2p .'$[
			dir=`N;(x-1;)each y+/:-1 0 1;
			dir=`S;(x+1;)each y+/:-1 0 1;
			dir=`W;(;y-1)each x+/:-1 0 1;
			dir=`E;(;y+1)each x+/:-1 0 1;
			'"lost"];
		(not any testp in p;testp 1)};
	round:{[r]
		b:chk[p:r 0]each dirs:r 1;
		pr:{[p;b] @[p;w;:;b[1]w:where b 0]}/[p;reverse b];
		pr[w]:p w:where all b[;0];
		w:raze g where 1<count each g:group pr;
		(@[pr;w;:;p w];1_dirs,1#dirs;1+r 2;any pr<>p)};
	s:(p0;`N`S`W`E;0;1b);
	({(prd{1+max[x]-min x}'[x])-count x 0}p2xy round/[10;s]0;round/[last;s]2)}

f24:{
	d:-2+(xs::count a 0)*ys::count a:read0 x;
	s:`w`r`d`l`u!value"#>v<^"#g:group b:raze a;
	m:(!/)flip(
		(`r;{@[p;where(xs-1)=(p:x+1)mod xs;-[;xs-2]]});
		(`d;{@[p;where(ys-1)=(p:x+xs)div xs;-[;xs*ys-2]]});
		(`l;{@[p;where 0=(p:x-1)mod xs;+[;xs-2]]});
		(`u;{@[p;where 0=(p:x-xs)div xs;+[;xs*ys-2]]}));
	e::@[;;]/[;k;m k:1_key s];
	f::{p where 0<p:distinct x,(x+1),(x-1),(x+xs),x-xs};
	h::{(not x in r;p;r@:where not any(r:f y 2)in/:p:e y 1;1+y 3)};
	o:{@[;1 3]h[z]/[first;(1b;x 0;y;x 1)]};
	(o\[(s;0);(1;d;1);(d;1;d)])[0 2;1]}

f25:{
	decode:{w:where'[x=/:"-="];(-/)5 sv'("J"$'@[x;w;:;"0"];@[count[x]#0;w;:;1 2])};
	encode:{
		b:{$[count w:where 2<p:reverse 5 vs x 1;x+prd[f#5]*5-p f:first w;x]}/[(0;x)];
		{reverse{@[y;where'[x=/:"12"];:;"-="]}.{raze string reverse x}'[5 vs'x]}b};
	encode sum decode each read0 x}