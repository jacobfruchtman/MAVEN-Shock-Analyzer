pro spikeFiller3, flg,side=side,toloc=toloc
	
	get_data,'shocks',data=dats
	if total(dats.y ) eq 0 then return
	shocks=dats.y
	HH=where(shocks eq 1)
	get_data,"Franken_fitted",data=datFF
	imins=datFF.imins
	imaxs=datFF.imaxs

	if not keyword_set(side) then side="end"

	;print,side

	if total(side eq list("both","full","NaN",1,"whole")) ne 0 then side=""	
	if side ne "" then sd="_"+side else sd=side

	get_data,flg,data=datFlag
	;help,datFlag
	if size(datFlag,/typ) eq 0 or size(datFlag,/typ) eq 2 then return

	
	yf=datFlag.y

	N=numel(yf)

	;yin=datin.y
	;yout=datout.y

	;ylen=yin+yout
	;if total(ylen) eq 0 then return
	GG=where(shocks ne 0,gcount);where(yf ne 0,gcount)
	z=fltarr(N);yin
	;for i=1,N-1 do z[i]=0
	for el=0, gcount-1 do begin

		i=GG[el]
		mn=(where(imins eq imins[i]))[0]
		mx=(where(imaxs eq imaxs[i]))[-1]
		for k=mn,mx do z[k]=yf[i]
	endfor

	datFlag.y=z

	name=flg+"_flattened"
	if keyword_set(toloc) then name+="_loc"
	store_data,name,data=datFlag

end
