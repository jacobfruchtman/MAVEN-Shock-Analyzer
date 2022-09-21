pro DownstreamMeasurer,plt,newName=newName

	if not keyword_set(newName) then newName=plt+"_downstream"

	get_data,plt,data=dat


	get_data,"downstream_indices",data=datd


	y=dat.y
	d=datd.y
	st=d[*,0]
	fn=d[*,1]
	;xs=dat.x
	;nxs=reverse(xs)
	if (size(y))[0] eq 1 then dim=1 else dim=(size(y))[2]
	zd=y*0
	
	GG=where(st ne 0,gcount)
	print,gcount

	
	N=numel(xs)
	if dim eq 1 then begin
		for i=0,size(dat.x,/n_el)-1 do begin

			if st[i] ne 0 then begin
				bgn=min(d[i,*])
				en=max(d[i,*]);fn[i]	
				zd[i]=mean(y[bgn:en])
			endif
		endfor
	endif else begin
		print,"dim=",dim
		for i=0,size(dat.x,/n_el)-1 do begin

			if st[i] ne 0 then begin
				;bgn=st[i]
				;en=fn[i]
				
				bgn=min(d[i,*])
				en=max(d[i,*]);fn[i]		

				;return
				for j=0,dim-1 do zd[i,j]=mean(y[bgn:en,j])
			endif
		endfor
	;	foreach el, GG do begin
	;		i=GG[el]
	;		bgn=st[i]
	;		en=fn[i]
;
	;		for j=0,dim-1 do zd[i,j]=mean(y[bgn:en,j])		
	;	endforeach
	endelse

	print,"size(y)=",size(y)
	print,"size(zd)=",size(zd)
	dat.y=zd
	

	store_data,newName,data=dat


end 
