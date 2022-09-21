pro finiteswitch,plt,newname=newname
	get_data,plt,data=dat
	if not keyword_set(newname) then newname=plt+'_finiteswitch'
	ys=dat.y
	N=N_elements(dat.x)
	yl=shift(ys,-1)
	yr=shift(ys,1)
	wl=where(finite(ys) eq 1 and finite(yl) ne 1,lcount)
	wr=where(finite(ys) eq 1 and finite(yr) ne 1,rcount)
	flshift=fltarr(N)
	frshift=fltarr(N)
	flshift[wl]=1
	frshift[wr]=1
	fshift=flshift or frshift
	store_data,newname,data={x:dat.x,y:fshift,ytitle:plt+'!C finite shift flag'}
	store_data,newname+'_left',data={x:dat.x,y:flshift,ytitle:plt+'!C finite lshift flag'}
	store_data,newname+'_right',data={x:dat.x,y:frshift,ytitle:plt+'!C finite lshift flag'}
end
