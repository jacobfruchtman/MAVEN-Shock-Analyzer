function geometricmedian,xvals,yvals,bins,grid=grid
	xcoord=xvals
	ycoord=yvals
	if keyword_set(grid) then begin
		xminval=min(xvals)
		yminval=min(yvals)
		xmaxval=max(xvals)
		ymaxval=max(yvals)
		xgrid=findgen((xmaxval-xminval)/bins[0]+2)*bins[0]+xminval-.5*bins[0]
		ygrid=findgen((ymaxval-yminval)/bins[1]+2)*bins[1]+yminval-.5*bins[1]
		makegridpoints,xgrid,ygrid,xcoord=xcoord,ycoord=ycoord,vec=vec
	endif

	
	len=numel(xcoord)
	zz=fltarr(len)
	nn=numel(xvals)
	;print,'entering for i=0,nn-1 do begin loop'
	for i=0,nn-1 do begin
		tx=xvals[i]
		ty=yvals[i]
		zz+=dist2point([tx,ty],xcoord,ycoord,bins=bins)
		
	endfor
	wz=where(zz eq min(zz),nz)
	xm=xcoord[wz[0]]
	ym=ycoord[wz[0]]
	return,[xm,ym]
end
