pro plotbutterlowpass,plt,cutoff=cutoff,order=order,newname=newname,local=local,ytitle=ytitle,btitle=btitle

	get_data,plt,data=dat
	if not keyword_set(newname) then begin
		newname=plt+'_butterpassed'
		;if keyword_set(width) then newname+='_'+strtrim(width,2)
	endif
	xs=dat.x
	N=n_elements(xs)
	ys=dat.y
	filter=butterworth(N,2,cutoff=cutoff,order=order)
	y_filtered = FFT( FFT(ys, -1) * filter, 1 )
	if keyword_set(local) then begin
		get_data,"proton_cyclotron_period",data=datPer
		if ~(size(datPer,/typ) eq 2) then begin
			per=datPer.y
			b=per[UNIQ(per, SORT(array))]
			b=b[where(b gt 0 and finite(b) eq 1)]
			foreach el, b do begin
				ww=where(per eq el)
				imin=ww[0]
				imax=ww[-1]
				N2=imax-min
				
				filter2=butterworth(N2,2,cutoff=el,order=order)
				if local eq 1 then ylocal=ys[imin:imax] else ylocal=y_filtered[imin:imax]
				y_filtered[imin:imax] = FFT( FFT(ylocal, -1) * filter2, 1 )
			endforeach

		endif


	endif
	dat.y=y_filtered
	if keyword_set(ytitle) then str_element,dat,'ytitle',ytitle,/add
	if keyword_set(btitle) then begin
		str_element,dat,'ytitle',yt,success=s
		if s then str_element,dat,'ytitle','butterpassed !C '+yt,/add else str_element,dat,'ytitle','butterpassed !C '+plt,/add
	endif
	store_data,newname,data=dat
	numDerivative,newname,1,'mid',name='BbutterDeriv'
	plotsmoother,'BbutterDeriv',width=20,s=6,new='BbutterDSmoothed20'
end
