pro makegridpoints,xvals,yvals,xcoord=xcoord,ycoord=ycoord,vec=vec

	nx=numel(xvals)
	ny=numel(yvals)
	xx=xvals
	for i=1,ny-1 do xx=[xx,xvals]
	yy=make_array(nx,value=yvals[0])
	for i=1,ny-1 do yy=[yy,make_array(nx,value=yvals[i])]
	
	xcoord=xx
	ycoord=yy
	vec=[[xcoord],[ycoord]]
end	
