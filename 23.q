f01:{
	a:raze b:string til 10;
	v:((a;b);(a,1_a;b,string`one`two`three`four`five`six`seven`eight`nine));
	f:{{"J"$raze where'[x=/:(min;max)@\:x]}(x[0]where count'[a])!raze a:y ss/:x 1};
	sum v f\:/:read0 x}
