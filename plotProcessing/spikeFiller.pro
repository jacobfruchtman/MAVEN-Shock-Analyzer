pro spikeFiller, flg,side=side,toloc=toloc
	
	if not keyword_set(side) then side="end"

	;print,side

	if total(side eq list("both","full","NaN",1,"whole")) ne 0 then side=""	
	if side ne "" then sd="_"+side else sd=side

	get_data,flg,data=datFlag
	;help,datFlag
	if size(datFlag,/typ) eq 0 or size(datFlag,/typ) eq 2 then return

	get_data,'sublengths_inbound'+sd,data=datin
	get_data,'sublengths_outbound'+sd,data=datout
	
	yf=datFlag.y

	N=numel(yf)

	yin=datin.y
	yout=datout.y

	ylen=yin+yout
	if total(ylen) eq 0 then return
	GG=where(yf ne 0,gcount)
	z=fltarr(N);yin
	;for i=1,N-1 do z[i]=0
	for el=0, gcount-1 do begin

		i=GG[el]
		if not keyword_set(toloc) then yi=yf[i] else yi=i
		iStart=i-1; where the sublength starts
		iEnd=i  ;where the sublength ends

		while ylen[iStart] ne 0 and iStart gt 0 do iStart--
		while ylen[iEnd] ne 0 and iEnd lt N-1 do iEnd++
		;print,"[istart,iEnd]=",[istart,iEnd]
		z[max([iStart,0]):min([N,iEnd])-1]=yi
	endfor

	datFlag.y=z

	name=flg+"_flattened"
	if keyword_set(toloc) then name+="_loc"
	store_data,name,data=datFlag

end
