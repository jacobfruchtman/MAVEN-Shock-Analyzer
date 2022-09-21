pro aggradateremover,ts
help,ts
foreach el,ts do begin
	bady=-1

	if size(el,/typ) eq 7 then tt=time_double(el) else tt=el
	print,time_string(el)
	for i=1,500 do begin
		ii=i
		
		get_data,ii,data=dat
		
		if size(dat,/typ) eq 2 then break
		str_element,dat,'x',success=s
		if ~s then begin
			;print,ii
			;return
			del_data,ii
			return
			continue
		endif; else continue

		;print,numel(dat.x)
		x=dat.x
		y=dat.y
		
		N=numel(x)
		b=where(x ne tt,NN,ncomplement=nkill,complement=wbad)
		
		;print,'nkill=',nkill
		if nkill eq 0 then continue
		if NN ne numel(x)-1 then continue
		bady=y[wbad]
		;print,bady
		xx=x[b]
		ys=dat.y
		if size(ys,/N_dim) eq 1 then yss=ys[b] else yss=ys[b,*] 
		
		;dat2.y=ys[b]
		;str_element,dat,'y',yss,/add
		;str_element,dat,'x',xx,/add
		
		ndat={x:xx,y:yss}
		tns=tag_names(dat)
		
		foreach el,tns do begin
			if total(el eq ['y','x']) eq 1 then continue
			str_element,dat,el,val
			str_element,ndat,el,val,/add
		endforeach
		unqs = yss[UNIQ(yss, SORT(yss))]
		if array_equal(unqs,[0,1]) or array_equal(unqs,[0,1,2,3]) then sub=1 else sub=0
		
		str_element,dat,'ytitle',success=subs
		bb=where(ndat.y eq bady,Nreo,ncomplement=nk)
		if Nreo ne 0 and ~sub and subs and ii ne 'pointsperday' and ii ne 'invMM1s' and ii ne 'fitWidth' then begin
			print,ii
			print,time_string(ndat.x[bb])
			print,bady
		
			return
		endif; else print,"not repeated"
		;print,numel(dat.x)
		;return
		store_data,ii,data=ndat
	endfor

endforeach
end
