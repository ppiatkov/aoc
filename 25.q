f01:{
	(s;n):50 100; / Starting position and total number of distinct positions
	v:"J"$1_'t:read0 x; / Absolute values of rotation "distances"
	d:1 -1"RL"?first each t; / Signs corresponding to rotation directions
	p:-1 1_\:sums s,v*d; / Positions before and after each rotation
	(sum 0=p[1]mod n;sum abs(-).(p*\:d)div n) / Rest of the owl
	}
