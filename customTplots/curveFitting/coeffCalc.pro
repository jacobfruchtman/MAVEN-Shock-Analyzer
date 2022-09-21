function coeffCalc, xx,yy, ishock,x0
	;;		'dim' is xp[0]-xs[0]
	;;		then xx0=xx[0]-x0		
	
	xx0=xx[0]-x0
	print,ishock-xx0,numel(yy)
	yys=smooth(yy,10,/edge_mirror)
	ybeg=yys[0:ishock-xx0]
	yend=yys[ishock-xx0:*]

	xbeg=xx[0:ishock-xx0]
	xend=xx[ishock-xx0:*]
	
	xpa=xx-xx[0]


	N=numel(xx)
	nend=numel(yend)
	nbeg=numel(ybeg)
	i0=xx[0]-x0
	

	mx=max(yend[nend/5:*])

	mn=min(ybeg[0:min([2*nbeg/5,60])])
	print,mn,mx
	m0=(mx-mn)/2
	m3=(mx+mn)/2
			
			;m2  is the x value in approximately at the top of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			;print,'m2  is the x value in the middle of the curve. to zeroth order, lets use m2=xpa[hj-gim]
;			m2=xpa[hj-gim]
	m2=xpa[ishock-xx0]

	m1=.1
	
	MM=[m0,m1,m2,m3]

	RETURN,MM

end

