pro normalizevector, plt,newname=newname
	get_data,plt,data=dat,alim=lim
	if not keyword_set(newname) then newname=plt+"_normal"

	y=dat.y
	x=dat.x
	tvectot,plt,tot=mag
	help,mag
	N=n_elements(x)
	for i=0, N-1 do begin
		y[i,0]=y[i,0]/mag[i]
		y[i,1]=y[i,1]/mag[i]
		y[i,2]=y[i,2]/mag[i]
	endfor
	dat.y=y
	str_element,dat,'ytitle',ytitle,success=s
	if s then begin
		unt=ytitle.LastIndexOf('[')
		if unt ne -1 then begin
			len=strlen(ytitle)
			ytitle=ytitle.remove(unt-len) 
			
		endif 
		ytitle+='!C unit vector'
		str_element,dat,'ytitle',ytitle,/add	
	endif else str_element,dat,'ytitle',plt+'!C unit vector',/add

	store_data,newname,data=dat,dlim=lim
	
end
