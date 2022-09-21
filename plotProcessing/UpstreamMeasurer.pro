pro UpstreamMeasurer,plt,newName=newName
		;get_data,"shocks_inbound",data=datin
		;get_data,"shocks_outbound",data=datout


		;if total(datin.y + datout.y) eq 0 then return

	if not keyword_set(newName) then newName=plt+"_upstream"

	get_data,plt,data=dat

	;get_data,"upstream_indices_inbound",data={x:xs,y:ui}
	;get_data,"upstream_indices_outbound",data={x:xs,y:uo}
	get_data,"upstream_indices",data=datu
	if size(datu,/typ) eq 2 or size(dat,/typ) eq 2 then return

	y=dat.y
	u=datu.y
	st=u[*,0]
	fn=u[*,1]

	if (size(y))[0] eq 1 then dim =1 else dim=(size(y))[2]
	zu=y*0
	
	GG=where(st gt 0,gcount)
	if dim eq 1 then begin
		;print,"numel(y)=",numel(y)
		foreach el, GG do begin
			i=el
			;print,"u[i,*]=u[",i,",*]=",u[i,*]
			bgn=min(u[i,*]);st[i]
			en=max(u[i,*]);fn[i]
			
			zu[i]=mean(y[bgn:en],/nan)		
		endforeach
	endif else begin
		;print,"numel(y)=",numel(y[*,0])
		foreach el, GG do begin
			i=el
			;print,"u[i,*]=u[",i,",*]=",u[i,*]
			bgn=min(u[i,*]);st[i]
			en=max(u[i,*]);fn[i]

			for j=0,dim-1 do zu[i,j]=mean(y[bgn:en,j],/nan)		
		endforeach
	endelse
	dat.y=zu
	store_data,newName,data=dat

end 
