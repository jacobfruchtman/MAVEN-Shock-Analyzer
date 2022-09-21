pro rhcalcs
	get_data,'Mfms',data=datMfms
	get_data,10,data=datTH
	get_data,'beta',data=datBeta
	Mfms=datMfms.y
	ThNB=datTH.y*!pi/180
	bta=datBeta.y
	N=numel(bta)
	B2B1_RH=fltarr(N);B2B1_Fit*0.0
	densityjump=fltarr(N)
	beta2=fltarr(N)
	t=datTH.x
	for i=0,N-1 do begin


			;th=ThetaNB[i]
			;ourBta=betas[i]

			RH_parameters,Mfms[i],ThNB[i],bta[i],a,b,c,d,yy,delta
			densityjump[i]=1/yy
			B2B1_RH[i]=SQRT((b+c*delta)/(b+c))
			beta2[i]=2*(a+0.5*c*(1-delta)+1-yy)/(b+c*delta)

			
			print,time_string(t[i])
			print,B2B1_RH[i]
	endfor
	help,bta
	betajump=beta2/bta
	get_data,'B2B1_RH0',data=datB2B1_RH
	
	datB2B1_RH.y=B2B1_RH
	store_data,'B2B1_RH',data=datB2B1_RH
end
