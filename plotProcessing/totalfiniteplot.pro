pro totalfiniteplot,plt,width,newName=newName

	get_data,plt,data=dat
	if not keyword_set(newName) then newName=plt+"_totalfinite"

	ys=dat.y
	N=numel(ys)
	z=fltarr(N)
	fns=1*(finite(ys) eq 1)
	for i=0,N-1 do begin
		lrad=max([i-width/2,0])
		rrad=min([i+width/2,N-1])
		z[i]=total(fns[lrad:rrad])
	endfor

	ytitle="num finite in radius"

	store_data,newName,data={x:dat.x,y:z,ytitle:ytitle}
end
	
