pro aggrafilter,plt,thresh,teq=teq,maxallowed=maxallowed,minallowed=minallowed

	sn=sign(thresh) ;;if thresh < 0 then throw out all data points less than abs(thresh). if >0 then throw out everything greater 
	thresh=abs(thresh)
	get_data,plt,data=dat
	;get_data,plt+'0',data=dat0
	yy=dat.y
	yy2=yy
	;yy0=dat0.y
	xx=dat.x
	oldN=numel(xx)
	PRINT,numel(yy)
	cond=-1*sn*(yy-thresh) ge 0;;if this is >=0, then the data point is okay. Otherwise, throw it out
	if keyword_set(maxallowed) then cond=1*(yy le maxallowed)
	if keyword_set(minallowed) then cond=1*(yy ge minallowed)
	if keyword_set(teq) then begin
		cond =1*( abs(xx - teq) gt 1)
	
	endif
	
	b=where(cond,nb,complement=nob,ncomplement=nnob)  	
	xx=xx[b]
	print,yy[b[0]]
	print,min(yy[b])
	print,max(yy[b])
	print,numel(b)

	;nowwork=where(-1*sn*(yy0[b]-thresh) lt 0,numnowwork)
	;if nnob gt 0 then nowbroken=where(-1*sn*(yy0[nob]-thresh) ge 0,numnowbroken) else numnowbroken=0

	get_data,10,data=datTH
	TH=datTH.y
	;if nnob gt 0 then badQpar=where(TH[nob] lt 45,bparcount) else bparcount=0
	;if nnob gt 0 then badQperp=where(TH[nob] ge 45,bperpcount) else bperpcount=0
	;gQpar=where(TH[b] lt 45,gparcount)
	;gQperp=where(TH[b] ge 45,gperpcount)
	;openU,1,'Documents/aggragatestats.txt',/append
	;printf,1,'threshold '+plt+'='+strtrim(thresh,2)+' kills:'
	;printf,1,'[badQpar,badQperp,totalbad]=',[bparcount,bperpcount,nnob]
	;printf,1,'leaving behind: [goodQpar,goodQperp,total]=',[gparcount,gperpcount,nb]
	
	;printf,1,strtrim(numnowwork,2)+" that were rejected under old definition now work"
	;printf,1,strtrim(numnowbroken,2)+" that were accepted under old definition now bad"
	;close,1
	;return
	
	if nnob eq 0 then return
	for j=1,500 do begin
		jj=j
		get_data,jj,data=dat2
		print,jj
		if size(dat2,/typ) eq 2 then break
		if keyword_set(teq) then if total(abs(dat2.x-teq) le 1) eq 0 then continue
		;dat2.x=xx
		ys=dat2.y
		if numel(dat2.x) ne oldN then continue
		if size(ys,/N_dim) eq 1 then yss=ys[b] else yss=ys[b,*] 
		
		;dat2.y=ys[b]
		str_element,dat2,'y',yss,/add
		str_element,dat2,'x',xx,/add
		
		ndat={x:xx,y:yss}
		tns=tag_names(dat2)
		
		foreach el,tns do begin
			str_element,dat2,el,val
			str_element,ndat,el,val,/add
		endforeach
		
		
		print,numel(dat2.x)
		if (jj ne plt and ys[0] eq yy[b[0]]) and jj ne 'Mfms2' then break
		store_data,jj,data=ndat

	endfor		
	get_data,plt,data=dat
	yy=dat.y
	xx=dat.x
	print,min(yy)
	print,max(yy)
	print,numel(yy)
	print,numel(yy2)
end
