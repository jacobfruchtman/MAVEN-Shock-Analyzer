

pro updownFlag, plt, filter=filter

	get_data,plt,data=dat
	if NOT KEYWORD_SET(filter) THEN filter=0


	xs=dat.x
	ys=dat.y
	N=size(xs,/n_el)
	z=ys*0
	maxflag=0
	minflag=0
	lastmax=0

	passthresh=1*(ys gt filter)
	for i=1,N-2 do begin
		
		if (ys[i] gt ys[i-1]) and (ys[i] gt ys[i+1]) then begin

			;print,"up at i=",i
			maxflag=1
			lastmax=i
			continue
		endif
		if (ys[i] lt ys[i-1]) and (ys[i] le ys[i+1]) then begin
			;print,"down at i=",i
			for j=lastmax,i do z[j]=1

			;print, "z[",i,"-1]=",z[i-1]
			;print, "z[",i,"+1]=",z[i+1]
			maxflag=0
			continue
		endif

	endfor

	if maxflag eq 1 then for j=lastmax, N-1 do z[j]=1
	z=z*passthresh
	dat.y=z

	store_data,plt+'_updown',data=dat
	
end
