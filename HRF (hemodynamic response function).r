glover=function(HZ){
	a1=6
	a2=12
	b1=0.9
	b2=0.9
	d1=5.4
	d2=10.8
	c=0.35
	x=seq(0, 30, 1/HZ) # HZ is the Sampling Rate (Heartz)
	glover1=((x/d1)^a1)*exp((-x+d1)/b1)
	glover2=((x/d2)^a2)*exp((-x+d2)/b2)
	G=glover1-c*glover2
	return(G)
}