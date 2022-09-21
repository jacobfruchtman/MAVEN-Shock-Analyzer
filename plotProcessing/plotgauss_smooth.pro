pro plotgauss_smooth,plt,newname=newname,sigma=sigma,width=width,edge_mirror=edge_mirror,edge_trucate=edge_trucate,edge_zero=edge_zero,kernel=kernel,missing=missing,nan=nan
	if not keyword_set(sigma) then sigma=1.
	get_data,plt,data=dat
	if not keyword_set(newname) then begin
		newname=plt+'_gsmooth'
		if keyword_set(width) then newname+='_'+strtrim(width,2)
	endif

	y=dat.y
	z=gauss_smooth(y,sigma,width=width,edge_mirror=edge_mirror,edge_trucate=edge_trucate,edge_zero=edge_zero,kernel=kernel,missing=missing,nan=nan)

	dat.y=z
	store_data,newname,data=dat
end
	
