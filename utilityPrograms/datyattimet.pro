function datyattimet,t,x,y,plt=plt,data=data,stringtime=stringtime

	if keyword_set(stringtime) then begin
		t=Cal2Unix(1,1,1,stringTime=stringtime)
		print,t,Format='(I20)'
	endif

	if keyword_set(plt) and not keyword_set(data) then begin
		get_data,plt,data=data
			
		
	endif

	if keyword_set(data) or isa(data) then begin
		print,"data"
		dx=(data.x[1]-data.x[0])/2.0
		xt=abs(data.x-t)
 		i=where(xt eq min(abs(data.x-t)))
		print,i
		mn=min(xt,i)
		print,dx
		print,mn
		print,data.x[i],Format='(I20)'
		print,x2Greg(data.x[i],/str)
		if mn le dx then return, data.y[i] else return,!VALUES.F_NAN
	endif
	dx=(x[1]-x[0])/2.0
	xt=abs(x-t)
 		;i=where(xt eq min(abs(data.x-t)))
	mn=min(xt,i)
	;i=where(abs(x-t) eq min(abs(x-t)))
	if mn le dx then return, y[i] else return,!VALUES.F_NAN
end
