function dist2point,pnt,xvars,yvars,bins=bins,xnvars=xnvars
	
	if keyword_set(xnvars) then begin
		nd=numel(pnt)

		if not keyword_set(bins) then bbins=make_array(nd,value=1) else bbins=bins;[1,1]
		if keyword_set(bins) and numel(bins) eq 0 and nd gt 1 then bbins=make_array(nd,value=bins) 
		
		
		len=numel(xnvars)/nd
		xxvars=xvars
		if (size(xvars,/dim))[0] eq nd then xxvars=transpose(xvars)
		z=fltarr(len) 
		for i=0,nd-1 do begin
			pn=pnt[i]
			xn=xxvars[*,i]
			bn=bbins[i]
			z+=(xn-pn)^2
		endfor
		return,sqrt(z)
	endif

	if not keyword_set(bins) then bins=[1,1]
	px=pnt[0]
	py=pnt[1]
	
	xdist=((px-xvars)/bins[0])^2
	ydist=((py-yvars)/bins[1])^2
	return,sqrt(xdist+ydist)


end
