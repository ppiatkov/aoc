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
	t:raze".",/:(r,i,r:enlist n#"."),\:".";
	b:0N 2#1_where differ t within"09";
	p:"J"$t c:{x+til y-x}.'b;
	f:m{a:(-1+first y),1+last y;raze a,(a,y)+/:x*1 -1}/:c;
	a1:sum p where any each not"."=t f;
	e:where 2=count each group raze f@'where each"*"=t f;
	a2:sum prd each p where each flip e in/:f;
	(a1;a2)}
