pro rh_contourplots
	dire="Documents/"
	pltpos=[0.15,0.20,0.95,0.8]
	cbpos=[0.30,0.05,0.70,0.10]
	mtest=findgen(401,start=1.,increment=.01)
	betatest=findgen(51,start=0.0,increment=.1)
	thetatest=findgen(91,start=0,increment=1)*!pi/180.
	;if 0 then begin
	crits=fltarr(91,51)
	for i=0,90 do begin
		print,i
		for j=0,50 do crits[i,j]=calccritmachnumber(thetatest[i],betatest[j])
	endfor

	c1=contour(crits,180*thetatest/!pi,betatest,title='$M_{crit}$',XTITLE="theta", YTITLE='beta',rgb_table=33,POSITION=pltpos,C_VALUE=[1.0,1.1,1.2,1.3,1.4,1.5,1.7,1.8,2.0,2.2,2.4,2.6])
	c1.save,dire+'crit_vs_beta_vs_theta_contour.png'
	c1.close
	;endif
	print,'end crits'
	B2B1_RH=fltarr(401,91);B2B1_Fit*0.0
	densityjump_RH=fltarr(401,91)
	beta2=fltarr(401,91)
	btalst=[0,1,10,20]
	btacont0=[0.1,.5,1.,2.,5.,10.,20.,30.]
	betacont1=[0.95,1.,2.,5.,9.,10.,20.,30.]
	betacont10=[9.,10.,20.,30.,50.,100.,200.]
	betacont20=[9.,10.,20.,30.,50.,100.,200.]
	btaconts=list(btacont0,btacont1,btacont10,btacont20)
	plotlist=list()
	for k=0,2 do begin
		beta1=btalst[k]
		print,beta1
		betacont=btaconts[k]
		sfx='at $\beta_1$='+strtrim(beta1,2);'at_beta1='+strtrim(beta1,2)
		for j=0,90 do begin
			print,'theta=',j
			for i=0,400 do begin

			;th=ThetaNB[i]
			;ourBta=betas[i]

			RH_parameters,mtest[i],thetatest[j],beta1,a,b,c,d,yy,delta
			densityjump_RH[i,j]=1/yy
			B2B1_RH[i,j]=SQRT((b+c*delta)/(b+c))
			beta2[i,j]=2*(a+0.5*c*(1-delta)+1-yy)/(b+c*delta)
			endfor
		endfor
		p=contour(/current,B2B1_RH,mtest,180*thetatest/!pi,XTITLE='$M_{fms1}$',YTITLE='$\theta_{BN1}$ [deg]',title='Magnetic Jump (B2/B1) '+sfx,rgb_table=33,C_VALUE=[1.,1.25,1.5,1.75,2.,2.25,2.5,2.75,3.,3.5],layout=[2,3,2*k+1])
		
		p=contour(/current,densityjump_RH,mtest,180*thetatest/!pi,XTITLE='$M_{fms1}$',YTITLE='$\theta_{BN1}$ [deg]',title='Density Jump ($n_2/n_1$) '+sfx,rgb_table=33,C_VALUE=[1.,1.25,1.5,1.75,2.,2.25,2.5,2.75,3.,3.5],layout=[2,3,2*k+2])
		
	endfor

p.save,Dire+'RH_jumps.eps'

return

for k=0,3 do begin
		beta1=btalst[k]
		print,beta1
		betacont=btaconts[k]
		sfx='at_beta1='+strtrim(beta1,2)
		for j=0,90 do begin
			print,'theta=',j
			for i=0,400 do begin

			;th=ThetaNB[i]
			;ourBta=betas[i]

			RH_parameters,mtest[i],thetatest[j],beta1,a,b,c,d,yy,delta
			densityjump_RH[i,j]=1/yy
			B2B1_RH[i,j]=SQRT((b+c*delta)/(b+c))
			beta2[i,j]=2*(a+0.5*c*(1-delta)+1-yy)/(b+c*delta)
			endfor
		endfor
		cb2b1=contour(B2B1_RH,mtest,180*thetatest/!pi,XTITLE='$M_{fms1}$',YTITLE='$\theta_{BN1}$ [deg]',title='Magnetic Jump (B2/B1) '+sfx,rgb_table=33,POSITION=pltpos,C_VALUE=[1.,1.25,1.5,1.75,2.,2.25,2.5,2.75,3.,3.5])
		cb2b1.save,dire+'MagJump_vs_M_vs_theta_contour_'+sfx+'.png'
		cb2b1.close
		cden=contour(densityjump_RH,mtest,180*thetatest/!pi,XTITLE='$M_{fms1}$',YTITLE='$\theta_{BN1}$ [deg]',title='Density Jump ($n_2/n_1$) '+sfx,rgb_table=33,POSITION=pltpos,C_VALUE=[1.,1.25,1.5,1.75,2.,2.25,2.5,2.75,3.,3.5])
		cden.save,dire+'DensJump_vs_M_vs_theta_contour_'+sfx+'.png'
		cden.close
		cbta2=contour(beta2,mtest,180*thetatest/!pi,XTITLE='$M_{fms1}$',YTITLE='$\theta_{BN1}$ [deg]',title='downstream $\beta$ '+sfx,rgb_table=13,POSITION=pltpos,C_VALUE=betacont)
		cbta2.save,dire+'Beta2_vs_M_vs_theta_contour_'+sfx+'.png'
		cbta2.close
	endfor


end
