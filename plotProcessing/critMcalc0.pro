Pro critMcalc,angle=angle,startdate=startdate,currtime=currtime
	get_data,'mvn_B_1sec',data=datB

	xs=datB.x
	x0=xs[0]
	if not keyword_set(startdate) then startdate=""

	if not keyword_set(currtime) then currtime=systime()

	;if startdate eq "" then begin
		;date=""
		;dire=plotDirector(Mdate=date,currtime="")
	;endif else begin
		xShock=xs[0]
		xsJ=x2Greg(xShock,/strformat)
		shockDate=(xsJ.split('T'))[0]
		print,shockDate
		dire='Documents/Plots/'+shockDate+'/'
		;str=x2Greg() ;string(startdate)
		;print,str
		;spltstime=strsplit(str,'/',/extract)
		;print,"spltstime=",spltstime
		;print,spltstime[0]
		;dire=plotDirector(Mdate=spltstime[0],currtime=currtime)
		;print,dire
;		date=spltstime[0]
		;date="_"+spltstime[0]
	;endelse


	get_data, "upstream_indices",data=datu
	ui=datu.y
;	get_data,'v_Sound_Fine',data=datCs

;	get_data,'alfven_velocity_Fine',data=datVA



	get_data,'Plasma_Beta',data=datbeta

	if not keyword_set(angle) then angle="Shock_Angle_AVG"

	get_data,angle,dat=datAngle

	theta2=datAngle.y
	theta=!const.pi/2-abs(theta2-!const.pi/2)

	

	betas=datbeta.y
	betas2=betas
	N=numel(theta)
	get_data,"shocks_inbound",data=datins
	get_data,"shocks_outbound",data=datouts

;	Cs=datCs.y

	;VA=datVA.y

	;QQ=Sqrt(1+(VA/Cs)^2)

	ins=datins.y
	outs=datouts.y
;
	GG=where(ins ne 0, gcount)
	;print,"gcount=",gcount
	crits=fltarr(N)
	;for i=0, numel(crits)-1 do crits[i]=0 
	;print,"finished zeroing crits"
	;for debugging
	trueCrits=crits
	critError=crits
	for el=0,gcount-1   do begin
		i=GG[el]

		ubeg=ui[i,0]
		uend=ui[i,1]
		;print,"[ubeg,uend]=",[ubeg,uend]

		th1=theta[i]
	
		;iup=max([i-20*60,0])
		;print,"iup=",iup
		betas2[i]=mean(betas[ubeg:uend]);betas[iup]

		MfmsTest=(findgen(10000))/1000.0+1.0
 		help,MfmsTest
		m2test=fltarr(10000)

		for j=0,10000-1 do begin
		;print,"i=",j
		m2test[j]=m2find(MfmsTest[j],betas2[i],th1)
		endfor

		help,m2test

		;p1=plot(MfmsTest,m2test)

		m2loc=-1
		m2closest=min(ABS(m2test),m2loc)
		help,m2loc
		crit=Mfmstest[m2loc]
		trueCrits[i]=KcalcN(betas2[i],th1)
		critError[i]=Abs(crit-trueCrits[i])/trueCrits[i]
		;print,"[i,m2test[m2loc],Mfmstest[m2loc],betas[iup],th1]=",[i,m2test[m2loc],Mfmstest[m2loc],betas2[i],th1*180/!const.pi]

		crits[i]=crit

	endfor
p1=plot(MfmsTest,m2test)
p1.close
print,"======================================"

	HH=where(outs ne 0, hcount)

	;N=numel(Cs)

		MfmsTest=(findgen(10000))/1000+1.0
 		help,MfmsTest
		m2test=fltarr(10000)

	for el=0,hcount-1   do begin
		i=HH[el]
		ubeg=ui[i,0]
		uend=ui[i,1]
		;print,"[ubeg,uend]=",[ubeg,uend]
		th1=theta[i]
	
		;iup=min([i+20*60,N-1])
		;print,"iup=",iup
		;betas2[i]=betas[iup]
		betas2[i]=mean(betas[ubeg:uend]);betas[iup]


		for j=0,10000-1 do begin
		;print,"i=",j
			m2test[j]=m2find(MfmsTest[j],betas2[i],th1)
		endfor
		trueCrits[i]=KcalcN(betas2[i],th1)
		help,m2test

		;

		m2loc=-1
		m2closest=min(ABS(m2test),m2loc)
		crit=Mfmstest[m2loc]
		critError[i]=Abs(crit-trueCrits[i])/trueCrits[i]
		;print,"[i,m2test[m2loc],Mfmstest[m2loc],betas[iup],th1]=",[i,m2test[m2loc],Mfmstest[m2loc],betas2[i],th1*180/!const.pi]
;
		crits[i]=crit

	endfor
	
KK=where(crits ne 0,kcount)
;print, "[KK,th1[KK],betas[KK+20*60],betas[KK-20*60],crits[KK]]"
foreach el,KK do begin
	print, [el,theta[el],betas[min([el+20*60,N-1])],betas[max([el-20*60,0])],betas2[el],crits[el]]
endforeach 
;print,"[th1[KK],betas[KK+20*60],betas[KK-20*60],crits[KK]]=",[th1[KK],betas[KK+20*60],betas[KK-20*60],crits[KK]]

p2=plot(MfmsTest,m2test)

	mxBta=max(betas2[KK])
	mnBta=min(betas2[KK])
	
;	betatest=findgen(ceil(mxBta),start=0.0,increment=.01)
;	thetatest=findgen(90,start=1,increment=1);*!const.pi/180



	;ZZZ=m2Ffind(betatest,thetatest)
;	ZZZ=FFcalcN(thetatest,betatest)
	;print,"[thetatest[30],betatest[]]"
;	p4=CONTOUR(ZZZ,thetatest,betatest, $
;	XTITLE="theta", YTITLE='beta',rgb_table=33)
	pltpos=[0.15,0.20,0.95,0.8]
	cbpos=[0.30,0.05,0.70,0.10]

	p3=scatterplot(theta[KK],betas2[KK],SYMBOL='star', /SYM_FILLED, $
	XTITLE="theta", YTITLE='beta',rgb_table=33,$
	$

  	 MAGNITUDE=crits[KK],title="MCrit locations", position=	pltpos)
	c3 = COLORBAR(TARGET=p3, $
	TITLE='Mcrit',position=cbpos)

	c3.save,dire+"Mcrit_vs_theta_vs_beta.png",Resolution=300,border=10
	

	store_data,"critical_Mfms",data={x:xs,y:crits,ytitle:"critical points"}

	;print,KcalcN(1,!const.pi/6)

	p4=scatterplot(theta[KK],betas2[KK],SYMBOL='star', /SYM_FILLED, $
	XTITLE="theta", YTITLE='beta',rgb_table=33,$
	$

  	 MAGNITUDE=critError[KK],title="MCrit % errors", position=	pltpos)
	c4 = COLORBAR(TARGET=p4, $
	TITLE='abs(Mcrit_calc-Mcrit_measured)/Mcrit_calc',position=cbpos)

	c4.save,dire+"McritError_vs_theta_vs_beta.png",Resolution=300,border=10
	

	store_data,"critical_Mfms_Percent_Error",data={x:xs,y:critError,ytitle:"percent error"}

	print,"trueCrit[KK]=",trueCrits[KK]
	print,"()()()()()"
	print,"critError[KK]=",critError[KK]
p2.close
p3.close
p4.close
end

	
	




	
