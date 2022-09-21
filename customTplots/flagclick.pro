pro flagclick,plt=plt,name=name,ytitle=ytitle

	ctime,t,y,vname=vname
	get_data,vname[0],data=datv
	xs=datv.x
	x0=xs[0]
	N=n_elements(xs)
	flags=fltarr(N)
	is=round(t-x0-.5)
	flags[is]=1.

	if keyword_set(plt) then begin
		get_data,plt,data=dat
		dat.y+=flags
		store_data,plt,data=dat
		return
	endif
	if not keyword_set(name) then name='flagset'

	if ~keyword_set(ytitle) then ytitle=name
	dat={x:xs,y:flags,ytitle:ytitle}
	store_data,name,data=dat
end
