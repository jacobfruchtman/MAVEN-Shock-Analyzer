pro multiplotaverager,plotlist,newname,arevecs=arevevs

	Numplots=N_elements(plotlist)

	get_data,plotlist[0],data=dat

	xs=dat.x
	N=N_elements(xs)
	notscalar=0
	dim2=N_elements(dat.y)/N
	if keyword_set(arevecs) then notscalar=1

	allys=nanarr(N,dim2*Numplots)
	avg=fltarr(N,dim2)
	std=fltarr(N,dim2)
	numcomp=dim2*Numplots
	ylist=list()
	if ~notscalar then begin
		for j=0,N-1 do allys[j,0]=dat.y[j]
		for i=1,Numplots-1 do begin
			get_data,plotlist[i],data=dat
			for j=0,N-1 do allys[j,i]=(dat.y)[j]
		endfor
		;print,allys[0,*],mean(allys[0,*],/nan)
		for i=0,N-1 do begin

			avg[i]=mean(allys[i,*],/nan)
			std[i]=stddev(allys[i,*],/nan)

		endfor


	endif else begin
		for j=0,N-1 do begin
			for k=0,dim2-1 do	 allys[j,k*(numplots-1)]=dat.y[j,k]
		endfor	
		for i=1,Numplots-1 do begin
			get_data,plotlist[i],data=dat
			;for j=0,N-1 do allys[j,i]=dat.y[j]
			for j=0,N-1 do begin
				for k=0,dim2-1 do	 allys[j,k*(numplots-1)+i]=dat.y[j,k]
			endfor
		endfor

		for i=0,N-1 do begin

			for k=0,dim2-1 do begin
			st=k*(numplots-1)+k
			ed=(k+1)*(numplots-1)+k
			avg[i,k]=mean(allys[i,st:ed],/nan)
			std[i,k]=stddev(allys[i,st:ed],/nan)
			endfor
		endfor

	endelse

	dat.y=avg
	store_data,newname+'avg',data=dat
	dat.y=std
	store_data,newname+'std',data=dat
	
end
	
