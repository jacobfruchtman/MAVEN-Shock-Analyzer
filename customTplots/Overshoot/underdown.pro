pro underdown
		get_data,"shocks_inbound",data=datin
		get_data,"shocks_outbound",data=datout


		if total(datin.y + datout.y) eq 0 then return
	get_data,"B_1stModalAverage",data=datB1
	get_data,"B_2ndModalAverage",data=datB2
	get_data,"B_3rdModalAverage",data=datB3
	get_data,"mvn_B_1sec_MAVEN_MSO_Mag_modal_mean",data=datmm
	get_data,"Franken_fitted",data=datFF

	FF=datFF.y
	
	down=datFF.downs

	;store_data,"downstream_fit",data={x:datFF.x,y:down}

	get_data,"B_above-up_regions_cleaned",data=datBau

	Bau=datBau.y

	BMM=datmm.y


	N=size(Bau,/n_el)

	ud=fltarr(N)
	OD=fltarr(N)
	ud2=fltarr(N)
	OD2=fltarr(N)

	modes=fltarr(N,3);2);3)

	modes[*,0]=datB1.y
	modes[*,1]=datB2.y
	modes[*,2]=datB3.y
	for i=0,N-1 do begin 
		if Bau[i] and BMM[i] le down[i] then ud2[i]=1
		if Bau[i] and (BMM[i] ge down[i]) then OD2[i]=1
		if Bau[i] and (Total(modes[i,*] le down[i]) ge 1) then ud[i]=Total(modes[i,*] le down[i])
		if Bau[i] and (Total(modes[i,*] ge down[i]) gt 2) then OD[i]=1
		if ~Bau[i] then begin
			OD[i]=-1
			ud[i]=-1
			OD2[i]=-1
			ud2[i]=-1
		endif		
	endfor
	store_data,"underdownstream",data={x:datBau.x,y:ud}
	store_data,"overdownstream",data={x:datBau.x,y:OD}
	store_data,"underdownstream2",data={x:datBau.x,y:ud2}
	store_data,"overdownstream2",data={x:datBau.x,y:OD2}
end
