pro hallwindfinder
	get_data,'wind_shift_left',data=datWSL
	get_data,'wind_shift_right',data=datWSR
	get_data,'wind_interpolated',data=datWIND
	get_data,'B_median',data=datB
	wind=datWIND.y
	t=datB.x
	B=datB.y
	N=numel(B)
	fins=wind*0.0+1
	wsl=datWSL.y
	wsr=datWSR.y
	wl=where(datWSL.y eq 1,lcount)
	wr=where(datWSR.y eq 1,rcount)
	medl=fins*B
	foreach el,wl do print,time_string(t[el])
	if finite(wind[0]) eq 1 then begin
		fnl=wl[0]
		medl[0:fnl]=median(medl[0:fnl])
	endif
	foreach el,wr do begin
		;waj=where(WSL[el,*] eq 1,acount)
		wcs=where(wl gt el,cnt)
		print,cnt
		if cnt eq 0 then aj=N-1 else aj=(wl[wcs])[0]
		;if acount eq 0 then aj=N-1 else aj=waj[0]
		print,el,aj,time_string(t[el]),' , ',time_string(t[aj])
		medl[el:aj]=median(medl[el:aj])
	endforeach
	STORE_DATA,'hallwind',data={x:datB.x,y:medl,ytitle:'Hall etal !C Magnetic Field [nT]'}
	options,'hallwind','colors','b'
	options,'B_median','colors','g'
	store_data,'bhallover',data='mvn_B_1sec_MAVEN_MSO_Mag B_median hallwind'
	
	;get_data,'mvn_B_1sec_MAVEN_MSO_Mag',data=datB
	;B=datB.y
	rat=medl/B
	lrat=alog10(rat)
	bl= 1*(-.3 le lrat and lrat lt .1)
	store_data,'hall_1st_cond',data={x:datB.x,y:bl,ytitle:"Hall etal's 1st condition"}
	
	cond3=fltarr(N,2)
	for i=0,N-1-5*30.-1 do cond3[i,0]= (total(B[i+1:i+5*30.] gt B[i]))/(total(B[i+1:i+5*30.] lt B[i])) ge 1
	bcond=fltarr(N,1)
	nB=reverse(B)
	for i=0,N-1-5*30.-1 do bcond[i]= (total(nB[i+1:i+5*30.] gt nB[i]))/(total(nB[i+1:i+5*30.] lt nB[i])) ge 1
	bcond=reverse(bcond)
	for i=0,N-1 do cond3[i,1]= bcond[i]
	
	store_data,'hall_3_cond',data={x:datB.x,y:cond3,ytitle:"Hall etal's 3rd condition"}
	
end
