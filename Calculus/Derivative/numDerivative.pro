pro numDerivative, plt,step,type,yt=yt,timederivative=timederivative,name=name
	

	get_data,plt,data=dat
	;function isArray,x
	xs=dat.x
	y=dat.y
	dim1=n_elements(xs)
	N=n_elements(y)
	dim2=N/dim1
	print,N,dim1,dim2
	;nd=isVec(dat.Y)
	;dx=xs[1]-xs[0]
	if keyword_set(timederivative) then x=xs else x=0
	;IF(nd) THEN BEGIN
	IF dim2 ne 1 then begin
		for j=0,dim2-1 do dat.Y[*,j]=derivator(y[*,j],step,type,x=x)
		;dat.Y[*,0]=derivator(dat.Y[*,0],step,type)
		;dat.Y[*,1]=derivator(dat.Y[*,1],step,type)
		;dat.Y[*,2]=derivator(dat.Y[*,2],step,type)
	ENDIF ELSE BEGIN
		dat.Y=derivator(y,step,type,x=x)
	ENDELSE
	
	if not keyword_set(name) then name=plt+"deriv"
	isdt=where(tag_names(dat) eq 'ytitle',dytexist)
	;islt=where(tag_names(limits) eq 'ytitle',lytexist)
	if (dytexist gt 0) then begin 
		if not keyword_set(yt) then yt="d/dt("+ dat.YTITLE+")"
		dat.YTITLE=yt
	endif
	
	store_data,name,data=dat

	if (dytexist le 0) then begin
		if not keyword_set(yt) then yt="d/dt("+ plt+")"
		tplot_element,name,'ytitle',yt,/add
	endif	
	
END
