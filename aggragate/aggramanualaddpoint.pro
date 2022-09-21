pro aggraorderpush,plt,xt,term
	get_data,plt,data=dat
	x=dat.x
	y=dat.y
	if total(abs(x - xt) lt 2) eq 1 then begin
		w=where(abs(x - xt) lt 2,nw)
		print,nw
		print,time_string(xt)
		print,time_string(x[w])
		print,plt+'=',y[w]
		return 
	endif
	xx=[x,xt]
	yy=[y,term]
	xsort=sort(xx)
	x=xx[xsort]
	if numel(term) eq 1 then y=yy[xsort] else begin
		for k=0,numel(term)-1 do yy[*,k]=yy[xsort,k]
		y=yy
	endelse
	tplot_element,plt,'x',x
	tplot_element,plt,'y',y
end
function crossingSeasonFinder,Ls,lat
	;print,"dls in function"
	;help,Ls
	season=Ls*0
	;N=numel(Ls)
	;if (Ls ge 0) doesn't work because seasons are assymetric. Using southern seasons only
	return, (floor(Ls/90) +2) mod 4

	;YOU FUCKED UP HERE. YOU IDIOT!
;	for i=0,N-1 do if 1 then season[i]=floor(Ls[i]/90) mod 4 else season[i]= (floor(Ls[i]/90) +2) mod 4

	;return,floor(Ls[i]/90) mod 4 else season[i]= (floor(Ls[i]/90) +2) mod 4
end 

pro aggramanualaddpoint,tstring


	killBigFM=0
	killNegOverDiff=0
	killNegOverDiff=1
	killBigFM=1
	maxAllowedFM=20 
	;if not keyword_set(maxUpDev) then maxUpDev=5
	maxUpDiff=1.3
	 maxB2B1fracdiff=.6
	maxMagJump=7.
	mindop=.5;.4
	minMfms=1.;0;1;0;1.;.5;1.;0.;1.0
	betamaxallowed=20;30;20;30;20.;9.;20.
	maxover=5.
	R_mars=3389.5
	semimajor=227.956 *10.^6.
	ecc=0.0935
	semilat=semimajor*(1-ecc^2)
	dstring= (strsplit(tstring,"/" , /extract))[0]

	print,dstring

	name='OverVsMach_'+dstring
	
	tplot_restore,filename="Documents/overVsMachData/"+name+'.tplot'
	get_data,name,data=dat
	dat=paramrecalc(dat)
	tt=-1
	j=-1
	for jjj=0,numel(dat.t)-1 do begin
		tt=dat.t[jjj]
		print,time_String(tt)
		print,tstring
		;print,tt
		;print,time_double(tstring)
		print,abs(tt- time_double(tstring))
		if abs(tt- time_double(tstring)) gt 1  then continue else begin

			j=jjj
			break
		endelse
		
	endfor
	print,'j=',j
	if j eq -1 then return

		badNAN=0
			bdmanpoint=0
			badPoint=0

			isqperp=1
			th=90-abs(dat.ANGLE[j]*180/!pi-90)
			if th lt 45 then isqperp=0

			t=dat.t[j];shockUnix[j];dat.t[j]
			MM=transpose(dat.MMs[j,*])
			down=dat.downs[j]
			up=dat.ups[j]


			;print,xt
			Ls=dat.Ls[j]
			;print,j
			;print,numel(dat.t)
			;print,size(dat.Ls,/n_el)
			;print,size(Ls,/typ)
			;print,Ls

			;lll=dat.Ls
			;seasons,tt,marsSeasonFinder(lll,dat.lat)
			;sss=marsSeasonFinder(lll,dat.lat)
			isBadMM=0
			if abs(fracdiff(down ,(MM[3]+MM[0]) )) gt .0001 or abs(fracdiff(up , (MM[3]-MM[0]) )) gt .0001 then begin
				badMMs++
				isBadMM=1
				badPoint=1
				;wait,10
				txet='[down,up, MM[3]+MM[0],MM[3]-MM[0]] = ['+strtrim(down,2)+","+strtrim(up,2)+","+strtrim(MM[3]+MM[0],2)+','+strtrim(MM[3]-MM[0],2)+']'
				print,txet
				print,'fracdiff(down,MMdown)=',abs(fracdiff(down ,(MM[3]+MM[0]) ))
				print,'fracdiff(up,MMup)=',abs(fracdiff(up ,(MM[3]-MM[0]) ))
				return
			endif
		

			if 0 and (dat.N_p[j] lt 0 or dat.N_p[j] gt 1000) and finite(dat.N_e[j]) eq 1 then begin
				print,xt
				return
			endif



			N_SN=dat.N_SN[j,*]

			nurm=sqrt(N_SN[0]^2+N_SN[1]^2+N_SN[2]^2)
			if nurm ne 1. then begin
				print,'nurm<1'
				if  abs(1.-nurm) lt .00001 then N_SN=N_SN/nurm else begin
					print,"abs(1-nurm(N_SN))=",abs(1.-nurm)
					print,"abs(1-sqrt(total(N_SN^2)))=",abs(1-sqrt(total(N_SN^2)))
					
					print,"N_SN=",N_SN

					print,xt
					

				
				;badN_SN++
				;badN_SNlst=N_SN
				;whereBadSN,tt,xt
				if nurm lt .7 then begin
					print,numel(dat.t)
					print,j
					print,xt
					return

				endif

				;return
				endelse
			endif

		

			


			totquasipar=0
			totquasiperp=0
	

			mfms=dat.mfms[j]
			theta=dat.ANGLE[j]


			Bmax=dat.Bmaxs[j]
			
			upM=dat.downMeasured[j]
			downM=dat.upMeasured[j] ; yes. I know. Need to fix the mix up later
			dop=dat.shock0Acc[j]
			dist0=dat.shock0dist[j]
			dum=downM/upM
			downup=dat.downups[j]
			duRatio=abs(dum/downup)
			bta=dat.betas[j]
			
			if downM eq 0 then begin
				print,"downM=0"

			endif

			if upM eq 0 then begin
				print,"upM=0"

			endif

			dstd=dat.downSTD[j]
			ustd=dat.upSTD[j]
			;print,"downM=",downM
			;print,"upM=",upM
			;mxFM=max(dat.mfms[j])	
			;help,dat
			;help,dat.crits
			crit=dat.crits[j];calccritmachnumber(theta,bta);dat.crits[j]
			FM=mfms/crit
			N_e=dat.N_e[j]
			N_p=dat.N_p[j]
			Tion=dat.T_ion[j]
			Tproton=Tion
			Beta_ion=dat.beta_ion[j]
			Telec=Tion*(bta/Beta_ion -1.)

			if Telec eq 0 or finite(Telec) ne 1 then begin
				print,"Telec=",Telec

			endif
			if fracdiff(downup, down/up) gt 0.001 then begin
				print,xt
				print,"downup=",downup
				print,"down/up=",down/up
				print,"Mfms=",mfms
				print,"beta=",bta
				badPoint=1;continue
				return
			endif
			;print,MM
			;print,MM[0]+MM[3],down


			if mfms lt minMfms then begin
					print,'mfms<1'
					return
			endif
			T_rat=1.0 * Telec/Tion
			if finite(N_e) ne 1 or N_e le 0 or N_p le 0 or N_e /N_p lt 1/15. or N_e /N_p gt 15. or T_rat lt 0  then begin
					print,'NAN N_E'
					return
			endif

			if ~isBadMM and 1/MM[1] gt 910 then begin
					print,'isbadMM'
					return
			endif



			if dist0 gt 2500 then begin
				return
			endif

			if dop lt mindop then begin
				print,'dop GT mindop'
				return
			endif
			if bta gt betamaxallowed then begin;or Beta_ion gt betamaxallowed/2.  then begin
				print,'too big beta'
				return

			endif
			if finite(bta) ne 1 or bta le 0. then begin
				print,"bta<0"
				return

			endif
			if size(Ls,/typ) eq 0 then begin
				print,"LS"
				return
	
			endif
			if finite(dat.ANGLE[j]) ne 1 then badPoint=1
			if FM le 0.0 then begin
				return
			endif
			if (FM ge maxAllowedFM) then begin
				return
			endif


			downup=down/up
			if ((downup ge maxMagJump or downM/upM ge maxMagJump or (downup lt 1.5 and down-up lt 1.3)) ) OR downup eq 0  then begin
				badPoint=1
				badB2B1++
				if keyword_set(keepPathologies) then return

			endif

	

			;if upM ge downM or fracdiff(dum,downup) gt maxB2B1fracdiff or fracdiff(downM,down)  gt .3 or duRatio gt 3 or duRatio lt .33 or abs(down-downM) gt 3*dstd then begin;
			if ((abs(upM-up) gt max([3*ustd,maxUpDiff]) or fracdiff(up,upM)  gt .25 )  or upM ge downM or fracdiff(dum,downup) gt maxB2B1fracdiff or fracdiff(downM,down)  gt .3 or duRatio gt 3 or duRatio lt .33 or abs(down-downM) gt 3*dstd)  then begin;or abs(downM-down)/mean([downM,down])  gt .3 or duRatio gt 3 or duRatio lt .33 or fracdiff(dum,downup) gt maxB2B1fracdiff then begin
				badMeasure++
				;print,"bad measure"
				;print,H[i]
				badPoint=1
			endif 

			

			if ( ((Bmax lt down ) or (Bmax lt up) or (Bmax lt downM) ) or (Bmax-down)/down gt maxover ) then begin
				return
			endif

			Ls=dat.Ls[j]
			;lll=Ls*0
			;catch,error_status
			;if error_status ne 0 then begin
			;badPoint=1
			;print,xt
			;print,H[i]
			;help,dat
			;help,dat.Ls
			;help,dLs
				;catch,/cancel
			;return
			;endif
			;lll=Ls*0
	





			tj=x2jul(t)
			if tj le 0 then begin
				print,t
				print,tj,"<=0"
				print,H[i]
				print,xt

				return
				badPoint=1
			endif

			if badPoint   then return


			MSO=transpose(dat.pos[j,*])
			;usMSO=transpose(dat.upstartpos[j,*])
			;umMSO=transpose(dat.upmidpos[j,*])
			;ueMSO=transpose(dat.upendpos[j,*])
			;dsMSO=transpose(dat.downstartpos[j,*])
			;dmMSO=transpose(dat.downmidpos[j,*])
			;deMSO=transpose(dat.downendpos[j,*])

			;print,size(umMSO,/n_dim)
			;if size(umMSO,/n_dim) eq 1 then return else print,umMSO[4]
			;return
			alt=SQRT(MSO[0]^2+MSO[1]^2+MSO[2]^2)
			;usALT=norm(usMSO)
			;umALT=norm(umMSO)
			;ueALT=norm(ueMSO)
			;dsALT=norm(dsMSO)
			;dmALT=norm(dmMSO)
			;deALT=norm(deMSO)


			;upsPOSlst=usMSO
			;upmPOSlst=umMSO
			;upePOSlst=ueMSO
			;downsPOSlst=dsMSO
			;downmPOSlst=dmMSO
			;downePOSlst=deMSO
			;usDX=norm(MSO-usMSO)
			;umDX=norm(MSO-umMSO)
			;ueDX=norm(MSO-ueMSO)
			;dsDX=norm(MSO-dsMSO)
			;dmDX=norm(MSO-dmMSO)
			;deDX=norm(MSO-deMSO)
			MSO=dat.pos[j,*]
			XMSO=MSO[0]
			YMSO=MSO[1]
			ZMSO=MSO[2]
			RHO_MSO=SQRT(YMSO^2+ZMSO^2)
			RHO_0_MSO=SQRT(XMSO^2+YMSO^2)
			Azimuth=atan(YMSO,ZMSO)
			TH_MSO=atan(SQRT(XMSO^2+YMSO^2),ZMSO)


			;if 0 and total([dsDX,dmDX,deDX] gt R_mars) gt 0 then begin


				;th=90-abs(dat.ANGLE[j]*180/!pi-90)
				
				;badthetaVals,tt,th
				;if bdmanpoint then badthetaManVals,tt, th else badthetaAutoVals,tt,th
				;continue


			;endif

			

			N_conic=dat.N_conic[j,*]
			POSconic=dat.pos_conic[j,*]

			Vdvec=dat.Vdvec[j,*]
			Vuvec_fine=dat.Vuvec_fine[j,*]
			Vuvec_coarse=dat.Vuvec_coarse[j,*]

			Buvec=dat.Buvec[j,*]
			Bdvec=dat.Bdvec[j,*]
			NSNexists=1
			if NSNexists then N_SN=dat.N_SN[j,*] else begin

				N_SN=normalrefinder( transpose(Buvec),theta,transpose(Vuvec_coarse),dat.flowangle[j],transpose(MSO),transpose(N_conic,dop))
				if (size(N_SN,/n_dim))[0] ne 2 then N_SN=transpose(N_SN)
			endelse

			;print,"good point"


	
			N_conic_cyl=N_conic
			N_conic[0]=N_conic_cyl[2]
			N_conic[1]=N_conic_cyl[0]*cos(azimuth)
			N_conic[2]=N_conic_cyl[0]*sin(azimuth)

			;goodPoints++
			;dayCrosses++
			;if th ge 45 then goodquasiperp++ else goodquasipar++

			aggraorderpush,12,tt,dat.betas[j]
			aggraorderpush,13,tt,Beta_ion
			aggraorderpush,'Tproton',tt,Tion
			aggraorderpush,'Telectron',tt,Telec
			
			aggraorderpush,'Velocity_fine',tt,Vuvec_fine
			

			aggraorderpush,'Velocity_coarse',tt,Vuvec_coarse
			aggraorderpush,'Vdvec',tt,Vdvec
			if numel(Buvec) ne 3 then return
			aggraorderpush,'Buvec',tt,Buvec
			aggraorderpush,'Bdvec',tt,Bdvec
			aggraorderpush,'n_conic',tt,N_conic
			aggraorderpush,'POSCONIC',tt,POSconic
			aggraorderpush,'N_SN',tt,N_SN

			;print,"M,FM,down/up,(Bmax-down)/down,Ls,date=",mfms,FM,down/up,(Bmax-down)/down,Ls,H[i]
			locsLst=dat.maxlocs[j]
			tLst=t
			aggraorderpush,'tjul',tt,tj

			aggraorderpush,'Mfms',tt,mfms
			aggraorderpush,'Mcrit',tt,crit
			aggraorderpush,'Bmax',tt,Bmax
			;overDiffsLst=dat.overDiffs[j]
			;fracOverDiffsLst=dat.fracOverDiffs[j]
			aggraorderpush,'B2B1fit',tt,downup;downup
			angleLst=theta
			aggraorderpush,'down',tt,down
			aggraorderpush,'up',tt,up
			downlst=down
			uplst=up
			;overLst=dat.overshoot[j]
			lat=dat.lat[j]
			latLst=lat
			LsLst=Ls
			Nelst=N_e
			Nplst=N_p
			betas=dat.betas[j]
			aggraorderpush,'shock0dist',tt,dist0
			aggraorderpush,'shock0Acc',tt,dop
			perdaylst=0
			downMlst=downM
			upMlst=upM
			downstdlst=dstd
			upstdlst=ustd
			thetaNVlst=dat.flowangle[j];thetaVN[j];flowangle[j]
			alt=SQRT(MSO[0]^2+MSO[1]^2+MSO[2]^2)
		
			POSlst=MSO
			altlst=alt
			alfvenlst=dat.Alfven[j]


			Soundlst=dat.Cs[j]
	tplot_element,12,'y',allbetas
	N=numel(allbetas)
			

	ThetaNV=thetaNVlst
	ThetaNVn=!pi/2-abs(ThetaNV-!pi/2)	
	ThetaNVnd=ThetaNVn*180/!pi




	ThetaNB=angleLst
	ThetaNBn=!pi/2-abs(ThetaNB-!pi/2)	
	ThetaNBnd=ThetaNBn*180/!pi

	;upDT=upintervaloffsetlst.toarray()
	;downDT=downintervaloffsetlst.toarray()


	costh=cos(ThetaNBn)

	;N_p=Nplst
	;N_e=Nelst

	downs=down
	ups=up
	;overshoots=overLst
	crits=crit

	downMs=downMlst
	upMs=upMlst
	B2B1_Measure=downMs/upMs
	downstds=downstdlst
	upstds=upstdlst
	downfluctuation=downstds/downMs
	upfluctuation=upstds/upMs
	B2B1_Fit=downup
	
	;return
	normalOverheight=(Bmax-downs)/downs
	normalOverheightM=(Bmax-downMs)/downMs



	N=size(lat,/n_el)
	N=numel(betas)
	tjul=tj
	print,min(tjul,mnlc),time_string(t[mnlc])

	;usPOS=upsPOSlst.toarray()
	;umPOS=upmPOSlst.toarray()
	;uePOS=upePOSlst.toarray()
	
	;dsPOS=downsPOSlst.toarray()
	;dmPOS=downmPOSlst.toarray()
	;dePOS=downePOSlst.toarray()
	POS=POSlst
	MMs=MM

	invMM1s=(1./MMs[1])[0]
	fitwidths=2*!pi*invMM1s
	help,invMM1s
	;return
	;usDX=usDXlst.toarray()
	;umDX=umDXlst.toarray()
	;ueDX=ueDXlst.toarray()
	;usALT=usALTlst.toarray()
	;umALT=umALTlst.toarray()
	;ueALT=ueALTlst.toarray()
	;dsDX=dsDXlst.toarray()
	;dmDX=dmDXlst.toarray()
	;deDX=deDXlst.toarray()
	;dsALT=dsALTlst.toarray()
	;dmALT=dmALTlst.toarray()
	;deALT=deALTlst.toarray()

	;return

	
	T_ion=Tion
	T_el=Telec
	
	vumag=SQRT(TOTAL(vuvec_fine^2))
	bumag=SQRT(TOTAL(Buvec^2))
	Euvec=-crossprod(Vuvec_coarse,Buvec)
	Edvec=-crossprod(vdvec,Buvec)

	clockangle=calcclockangle(Buvec)
	clockanglen=!pi/2-abs(!Pi/2-clockangle)
	clockanglend=clockanglen*180/!pi
	coneangle=calcconeangle(Buvec)
	coneanglen=!pi/2-abs(!Pi/2-coneangle)
	coneanglend=coneanglen*180/!pi
	help,POS
	;help,usDX
	;help,usPOS
	;POS=POS[*,0,*];TRANSPOSE(POS)
	help,POS
	;return

	Alfven=alfvenlst
	Cs=Soundlst

	Mflow=vumag/sqrt(Cs^2+Alfven^2)

	AlfCalc=upM/Sqrt(!const.mp* 10.0^6 *N_p) /10.0^9
	AlfCalcF=up/Sqrt(!const.mp* 10.0^6 *N_p) /10.0^9
	VA=Alfcalc
	print,total(( Cs^2+Alfven^2)^2 -4 *Cs^2 * Alfven^2*(cos(ThetaNBn))^2 le 0)

	fms=SQRT(1/2. *(( Cs^2+Alfven^2)+ SQRT( ( Cs^2+Alfven^2)^2 -4 *Cs^2 * Alfven^2*(cos(ThetaNBn))^2 )))
	
	fms_corr=SQRT(1/2. *(( Cs^2+Alfven^2)+ SQRT( ( Cs^2+Alfven^2)^2 -4 *Cs^2 * Alfven^2*(cos(ThetaNVn))^2 )))
	v_normal=total(N_SN*Vuvec_fine);Mfms*fms
	
	ThetaNV2=acos(v_normal/vumag)
	ThetaNVn2=!pi/2-abs(ThetaNV2-!pi/2)	
	ThetaNVnd2=ThetaNVn2*180/!pi
	
	;Malfven=fltarr(N);Mfms*fms/Alfven


		Msound=vumag/Cs;v_normal/Cs
		;Malfven=vumag/Alfven;v_normal[i]/Alfven[i]
		Malfven=abs(v_normal/Alfven)
		Mfms_corr=abs(v_normal)/fms_corr


	BuN=total(N_SN*Buvec)
	ThetaNB2=acos(BuN/bumag)
	ThetaNBn2=!pi/2-abs(ThetaNB2-!pi/2)	
	ThetaNBnd2=ThetaNBn2*180/!pi
	;p1=scatterplot(Mfms,vumag*Mfms/v_normal,xtitle='$M_{fms}$',ytitle='$M_{fms} (^{|{\bf v_U}|}/_{-\bf v_U \cdot N_{sw}} )$',$
	;					SYMBOL='.', /SYM_FILLED,$;POSITION=pltpos, $
				;		magnitude=bytscl(ThetaNVnd,max=90.,min=0.),RGB_TABLE=13.,aspect_ratio=1)
	;cbb2b2=colorbar(ORIENTATION=0,POSITION=cpos1, $; POSITION=cpos1, title="$\theta_{VN}$",RGB_TABLE=62,RANGE=[0.,90.])
	;return
	;alt=altlst.toarray()


	orbitTHETAd=(Ls-70-180) MOD 360.

	orbitTHETAdnn=180-abs(orbitTHETAd-180.)

	orbitTHETA=orbitTHETAd *!pi/180.

	 SolDist=semilat /(1+ecc*cos(orbitTHETA))
	;for i=0,N-1 do tjul[i]=x2Jul(t[i])
	;tgreg=t
	;for i=0,N-1 do tgreg[i]=x2greg(t[i],/str)
	whereapo=where(orbitTHETAdnn gt 135)
	wherefarther=where(orbitTHETAdnn gt 90 and orbitTHETAdnn le 135)
	wherecloser=where(orbitTHETAdnn gt 45 and orbitTHETAdnn le 90)
	whereperi=where(orbitTHETAdnn le 45)

	distIndices=list(whereperi,wherecloser,wherefarther,whereapo)

	TempRatio=T_el/T_ion

	FM=Mfms/crits
	;tjday=floor(tjul)

	;print,Ls[where(t eq min(t))]
	;return
	

	

	;SEASONS2=marsSeasonFinder(Ls,lat)
	;seasonNums=[0,1,2,3]
	season=crossingseasonfinder(Ls,lat)


	
	XMSOAdj=XMSO-.600*R_mars
	RHO=SQRT(YMSO^2+ZMSO^2)
	R_conic=SQRT(XMSOAdj^2+RHO^2)

	ecc_MAVEN=1.026

	SZA=atan(RHO,XMSO)

	l_MAVEN=R_conic+ecc_MAVEN*XMSOAdj

	help,XMSO
	help,RHO
	


;	betas=betas.toarray()
	beta_el=betas-beta_ion


	betapart=betas[sort(allbetas)]

	betacalc=2*Cs^2/Alfven^2

	betafrac=betacalc/betas
	m_p=938.0*10.0^6
	
	CspeedI=SQRT(( 5./3 *Tproton )/m_p   )*!const.c/1000
	thNB=thetaNBn
fmsIon=Sqrt( .5*  (VA^2+CspeedI^2+sqrt((VA^2+CspeedI^2)^2-4*VA^2*CSpeedI^2*(cos(thNB)^2) ) ))
	MfmsIon=abs(v_normal)/fmsIon
	betafdiff=fracdiff(betacalc,betas)
	
			RH_parameters,Mfms,ThetaNBn,betas,a,b,c,d,yy,delta
			densityjump_RH=1/yy
			B2B1_RH=SQRT((b+c*delta)/(b+c))
			beta2=2*(a+0.5*c*(1-delta)+1-yy)/(b+c*delta)

			RH_parameters,MfmsIon,ThetaNBn,beta_ion,a,b,c,d,yy,delta
			densityjump_RH_i=1/yy
			B2B1_RH_i=SQRT((b+c*delta)/(b+c))
			beta2_i=2*(a+0.5*c*(1-delta)+1-yy)/(b+c*delta)




	;return


	betajump=beta2/betas
	betajump_i=beta2_i/beta_ion
	B2B1fdiff=200*abs(B2B1_Fit-B2B1_RH)/abs(B2B1_Fit+B2B1_RH)
	B2B1Mdiff=200*abs(B2B1_Measure-B2B1_RH)/abs(B2B1_Measure+B2B1_RH)
	B2B1FMdiff=200*abs(B2B1_Measure-B2B1_Fit)/abs(B2B1_Measure+B2B1_Fit)

	RH_error=abs(B2B1_Fit-B2B1_RH)/abs(B2B1_RH)
	Fit_error=abs(B2B1_Fit-B2B1_RH)/abs(B2B1_Fit)

	B2B1fdiff_i=200*abs(B2B1_Fit-B2B1_RH_i)/abs(B2B1_Fit+B2B1_RH_i)
	B2B1Mdiff_i=200*abs(B2B1_Measure-B2B1_RH_i)/abs(B2B1_Measure+B2B1_RH_i)

	RH_error=abs(B2B1_Fit-B2B1_RH)/abs(B2B1_RH)
	Fit_error=abs(B2B1_Fit-B2B1_RH)/abs(B2B1_Fit)

	RH_error_i=abs(B2B1_Fit-B2B1_RH_i)/abs(B2B1_RH_i)
	Fit_error_i=abs(B2B1_Fit-B2B1_RH_i)/abs(B2B1_Fit)



;	return





	;return
	allindices=findgen(N)



	lowerquartpart=betapart[1.*N/4.-1]
	upperquartpart=betapart[3.*N/4.-1]
	wherebetalowerq=where(betas le lowerquartpart,numlowerq)
	wherebetaupperq=where(betas ge upperquartpart,numupperq)
	print,numupperq,numlowerq
	betaupperq=fltarr(N)
	foreach el, wherebetaupperq do betaupperq[el]=1
	betalowerq=fltarr(N)
	foreach el, wherebetalowerq do betalowerq[el]=1
	
	print,"N,N/4,numel(where(betas le lowerquartpart)), numel(where(betas ge lowerquartpart))"
	print,N,N/4,numel(where(betas le lowerquartpart)), numel(where(betas ge upperquartpart))
	betaparts=[[1.,1.],[2.,2.],[lowerquartpart,upperquartpart]]
	;foreach el,betaparts do print,el


	;foreach el,betaparts do for j=0,1 do print,el[j]
betaparttype=2
	betabounds=betaparts[*,betaparttype-1]
	;print,"betabounds:",betabounds
	lowbetabound=betabounds[0]
	highbetabound=betabounds[1]
	;print,lowbetabound,highbetabound
	wheresmallbeta=where(betas le lowbetabound,nsbeta)
	wherebigbeta=where(betas gt highbetabound,nsbeta)


	HH0=ThetaNBnd ge 45
	nHH= NOT HH0 
	;AA0=where(ThetaNB eq ThetaNB) ;All Locations

	sprng=SEASON eq 0
	smmr=season eq 1
	fall=SEASON eq 2
	wntr=SEASON eq 3
  	


	;sprng=fltarr(N)
	;foreach el,springIndices do sprng[el]=1
	;smmr=fltarr(N)
	;foreach el,summerIndices do smmr[el]=1
	;fall=fltarr(N)
	;foreach el,autumnIndices do fall[el]=1	
	;wntr=fltarr(N)
	;foreach el,winterIndices do wntr[el]=1	

	
	angleNames=["","Quasiperpendicular "]
	angleShort=["","_quasiperp_"]

	;wheresmallbeta=where(betas le 1,nsbeta,complement=wherebigbeta)
	wheresmallbeta=betas le lowbetabound
	wherebigbeta=betas gt highbetabound
	

	wherecritical=FM ge 1
	wheresubcritical=FM lt 1
	

	
	aggraorderpush,'season',tt,SEASON

	aggraorderpush,'spring',tt,1*sprng
	aggraorderpush,'summer',tt,1*smmr
	aggraorderpush,'autumn',tt,1*fall
	aggraorderpush,'winter',tt,1*wntr

	aggraorderpush,"FM",tt,FM
	aggraorderpush,"FM_NV",tt,Mfms_corr/crits
	aggraorderpush,"Mfms",tt,Mfms
	aggraorderpush,"Mfms_NV",tt,Mfms_corr
	aggraorderpush,"ThetaNBn",tt,ThetaNBnd
aggraorderpush,"FM",tt,FM
	aggraorderpush,"FM_NV",tt,Mfms_corr/crits
	aggraorderpush,"Mfms",tt,Mfms
	aggraorderpush,"Mfms_NV",tt,Mfms_corr
	aggraorderpush,"ThetaNBn",tt,ThetaNBnd
	aggraorderpush,"ThetaNB",tt,ThetaNB 
	aggraorderpush,"beta",tt,betas 
	aggraorderpush,"beta_proton",tt,beta_ion 
	aggraorderpush,"beta_electron",tt,beta_el 
	aggraorderpush,"Mcrit",tt,crits 
	aggraorderpush,"ThetaNVn",tt,ThetaNVnd 
	aggraorderpush,"ThetaNV",tt,ThetaNV 

	aggraorderpush,"N_ion",tt,N_p  
	aggraorderpush,"N_e",tt,N_e  

	aggraorderpush,'Tproton',tt,T_ion  
	aggraorderpush,'Telectron',tt,T_el  
	aggraorderpush,'Temp_ratio',tt,TempRatio  

	aggraorderpush,'Pproton',tt,5. *N_p*T_ion/3  
	aggraorderpush,'Pelectron',tt,N_e*T_el  

	aggraorderpush,"Electron_Ion_fraction",tt,N_e/N_p  
	aggraorderpush,"Electron_Ion_fracdiff",tt,fracdiff(N_e,N_p)  


	aggraorderpush,"OrbitTheta",tt,orbitTheta 

	aggraorderpush,'alt',tt,alt 
	aggraorderpush,"altNorm",tt,alt/R_mars 
	aggraorderpush,'Ls',tt, Ls  
	;aggraorderpush,'lmaven',tt, l_MAVEN  
aggraorderpush,'lmaven',tt, l_MAVEN  
aggraorderpush,'lmavenNorm',tt, l_MAVEN/R_mars  
	;aggraorderpush,'lmavenNorm',tt, l_MAVEN/R_mars  
	aggraorderpush,'tjul',tt, tjul  
	aggraorderpush,'SolDist',tt, SolDist  
	aggraorderpush,'SolDistNorm',tt, SolDist/semimajor  


	aggraorderpush,'down',tt, downs  
	aggraorderpush,'downM',tt, downMs  
	aggraorderpush,'up',tt, ups  
	aggraorderpush,'upM',tt, upMs  
	aggraorderpush,'Bmax',tt, Bmax  
	aggraorderpush,'downfluc',tt, downfluctuation  
	aggraorderpush,'upfluc',tt, upfluctuation  

	aggraorderpush,'B2B1fit',tt, B2B1_Fit  
	aggraorderpush,'B2B1M',tt, B2B1_Measure  
	
aggraorderpush,'overshootAmplitude',tt, normalOverheight  

	aggraorderpush,'overshootAmplitudeM',tt, normalOverheightM  


aggraorderpush,'overshootAmplitudeLowRes',tt, normalOverheight-downstds/downs  

	aggraorderpush,'overshootAmplitudeLowResM',tt, normalOverheightM-downfluctuation  



	aggraorderpush,'normOverfitup',tt, (Bmax-ups)/ups  
	aggraorderpush,'normOverupM',tt, (Bmax-upMs)/upMs  

	aggraorderpush,'B2B1_RH',tt, B2B1_RH  
	aggraorderpush,'densityJump',tt, densityjump_RH  
	aggraorderpush,'betajump',tt, betajump  
	aggraorderpush,'beta2',tt, beta2  


aggraorderpush,'B2B1_RH_ion',tt, B2B1_RH_i  
	aggraorderpush,'densityJump_ion',tt, densityjump_RH_i  
	aggraorderpush,'betajump_ion',tt, betajump_i  
	aggraorderpush,'beta2_ion',tt, beta2_i  


	aggraorderpush,'B2B1fdiff',tt, B2B1fdiff  
	aggraorderpush,'B2B1Mdiff',tt, B2B1Mdiff  
	aggraorderpush,'RH_error',tt, RH_error  
	aggraorderpush,'Fit_error',tt, Fit_error  

	aggraorderpush,'B2B1fdiff_i',tt, B2B1fdiff_i  
	aggraorderpush,'B2B1Mdiff_i',tt, B2B1Mdiff_i  
	aggraorderpush,'RH_error_i',tt, RH_error_i  
	aggraorderpush,'Fit_error_i',tt, Fit_error_i  

	;aggraorderpush,'shock0Acc',tt, shock0Acc  
	;aggraorderpush,'shock0dist',tt, shock0dist  

	aggraorderpush,'SoundSpeed',tt, Cs  
	aggraorderpush,'Alfven',tt, Alfven  

	aggraorderpush,"AlfCalc",tt, AlfCalc  

	aggraorderpush,'fms',tt, fms  
	aggraorderpush,'fms_corr',tt, fms_corr  
	aggraorderpush,'MachSound',tt, Msound  
	aggraorderpush,'MachAlfven',tt, Malfven  

	aggraorderpush,'Vumag',tt, vumag  
	aggraorderpush,'Velocity_coarse',tt, Vuvec_coarse  
aggraorderpush,'Velocity_fine',tt, Vuvec_fine  
	aggraorderpush,'Velocity_N',tt, v_normal  
aggraorderpush,"ThetaNVn2",tt,ThetaNVnd2 
	aggraorderpush,"ThetaNV2",tt,ThetaNV2 


	aggraorderpush,'Buvec',tt,Buvec 
	Pdyn=N_p*938.272 *10.^6 *vumag^2
	aggraorderpush,'P_ion_dyn1',tt,Pdyn 
	;aggraorderpush,'upDT',tt,upDT/60. 
	;aggraorderpush,'downDT',tt,downDT/60. 
	aggraorderpush,"Mflow",tt,Mflow 
	;Bux=fltarr(N)
	;for i=0,N-1 do Bux[i]=Buvec[i,0]
	;theta_BX=acos(Bux/upM)
	;theta_BXd=theta_BX*180./!pi
	;theta_BXnd=theta_BXd
	aggraorderpush,"coneangle_nd",tt,coneanglend 
	aggraorderpush,"coneangle_rad",tt,coneangle 

	aggraorderpush,"clockangle_nd",tt,clockanglend 
	aggraorderpush,"clockangle_rad",tt,clockangle 


	aggraorderpush,"ThetaNBn2",tt,ThetaNBnd2 
	;aggraorderpush,"ThetaBXn",tt,Theta_BXnd 
	;aggraorderpush,"ThetaBX",tt,Theta_BX 

;	aggraorderpush,"ThetaNBn2",tt,ThetaNBnd2 

	aggraorderpush,'N_SN',tt,N_SN 

	aggraorderpush,'POS',tt,POS 

	aggraorderpush,'X_MSO',tt,XMSO 

aggraorderpush,'Y_MSO',tt,YMSO 

aggraorderpush,'Z_MSO',tt,ZMSO 


	aggraorderpush,'RHO_MSO',tt,RHO 

aggraorderpush,'RHO-0_MSO',tt,sqrt(XMSO^2+YMSO^2) 

	aggraorderpush,'SZA',tt,SZA 
aggraorderpush,'SZAd',tt,SZA*180./!pi 
	aggraorderpush,'lat',tt,lat 
	aggraorderpush,'pointsperday',tt,perdaylst 
	aggraorderpush,'invMM1s',tt,invMM1s 
	aggraorderpush,'fitWidth',tt,fitwidths 




	Vux=Vuvec_coarse[0]
	theta_VX=acos(Vux/vumag)
	theta_VXd=theta_VX*180./!pi
	theta_VXnd=90-abs(90-theta_VXd)
	aggraorderpush,"ThetaVXn",tt,Theta_VXnd 
	aggraorderpush,"ThetaVX",tt,Theta_VX 


	aggraorderpush,'Quasiperp',tt,HH0
	aggraorderpush,'Quasipar',tt,nHH
	aggraorderpush,'thermal',tt,wherebigbeta
	
	aggraorderpush,'magneto',tt,wheresmallbeta
	

	aggraorderpush,'supercritical',tt,wherecritical
	aggraorderpush,'subcritical',tt,wheresubcritical

	tplot_element,'where_beta_upperquartile','y',wheretoflag(wherebetalowerq,N),/add

	tplot_element,'where_beta_lowerquartile','y',wheretoflag(wherebetalowerq,N),/add





	del_data,name
	;;;;;;hhhhhh

thNBn=ThetaNBn
thNB=ThetaNB
thNBnd=ThetaNBnd

help,Buvec
help,n_conic



Buconic=total(buvec*n_conic)


Bun=sqrt(total(Buvec^2))
Bunorm=Buconic/Bun
thCon=acos(Buconic/Bun)

aggraorderpush,'thetaBconicRad',tt, thCon 
thConD=thCon*180/!pi
thConDn=90-abs(90-thConD)
aggraorderpush,'thetaBconic',tt, thConDn 

vuvec=vuvec_fine
Vuconic=total(Vuvec*n_conic)


Vun=sqrt(total(Vuvec^2))
thVCon=acos(Vuconic/Vun)
help,thVCon
aggraorderpush,'thetaVconicRad',tt, thVCon 
thVConD=thVCon*180/!pi
thVConDn=90-abs(90-thVConD)
aggraorderpush,'thetaVconic',tt, thVConDn 

aggraorderpush,'V_conic',tt, Vuconic 

thConN=thConDn*!pi/180
fmsConic=Sqrt( .5*  (VA^2+Cs^2+sqrt((VA^2+Cs^2)^2-4*VA^2*CS^2*(cos(thConN)^2) ) ))
MfmsConic=abs(Vuconic)/fmsConic
aggraorderpush,'Mfms_conic',tt, MfmsConic 
MAconic=abs(Vuconic)/VA
aggraorderpush,'MachAlfvenConic',tt,  MAconic  




McritConic=calccritmachnumber(thConN,betas)
aggraorderpush,"McritConic",tt, McritConic 

aggraorderpush,"FMconic",tt, MfmsConic/McritConic 


B2B1_RH_conic=rh_magnetic_jump(M=MfmsConic,bta=betas,theta=thConN)
aggraorderpush,'B2B1_RHconic',tt,  B2B1_RH_conic  



RHRHCfdiff=fracdiff(B2B1_RH,B2B1_RH_conic)
aggraorderpush,'RHRHCfdiff',tt,  RHRHCfdiff  



B2B1RHCfdiff=fracdiff(B2B1_Fit,B2B1_RH_conic)
aggraorderpush,'B2B1Cfdiff',tt,  B2B1RHCfdiff  
B2B1Cfdiff=B2B1RHCfdiff
B2B1fdiff=fracdiff(B2B1_Fit,B2B1_RH)
aggraorderpush,'B2B1fdiff',tt,  B2B1fdiff  

Vd_N=total(vdvec*N_SN)
aggraorderpush,'Vd_N',tt, Vd_N 


Vdconic=total(vdvec*N_conic)
aggraorderpush,'Vdconic',tt, Vdconic 

VuN=v_normal
VdN=Vd_N

MA=Malfven
bb=2 *Cos(thNB)^2 /MA^2
yy=VdN/VuN
dl=(bb -1)^2/(bb-yy)^2

RH2=SQRT(Cos(thNBn)^2+dl*Sin(thNB)^2)

aggraorderpush,'B2B1_RHv',tt, RH2 



VuC=Vuconic

VdC=Vdconic


bb=2 *Cos(thCon)^2 /MAconic^2
yy=VdC/VuC
dl=(bb -1)^2/(bb-yy)^2

RH2con=SQRT(Cos(thCon)^2+dl*Sin(thCon)^2)

aggraorderpush,'B2B1_RHvconic',tt, RH2con 




wQuasiperpsC=thConDn ge 45
wQuasiparsC=~wQuasiperpsC

aggraorderpush,'QuasiperpConic',tt, wQuasiperpsC
aggraorderpush,'QuasiparConic',tt, wQuasiparsC


TH=ThetaNBnd


THdiff=thConDn-TH
aggraorderpush,'THdiff',tt, THdiff 
aggraorderpush,'THfdiff',tt, fracdiff(TH,thCon) 
aggraorderpush,'THdiffAbs',tt, abs(THdiff) 


RHfdiffFrac=B2B1fdiff/B2B1Cfdiff
aggraorderpush,'RHfdiffFrac',tt, RHfdiffFrac 

Navg=N_SN

Butvec=crossprod(Navg,crossprod(Navg,Buvec))
Bdtvec=crossprod(Navg,crossprod(Navg,Bdvec))
ButMag=sqrt(total(Butvec^2,2))
BdtMag=sqrt(total(Bdtvec^2,2))

BtanJump=BdtMag/ButMag
aggraorderpush,'BupTan',tt, ButMag 
aggraorderpush,'BdownTan',tt, BdtMag 
aggraorderpush,'BTanJump',tt, BtanJump 



N=numel(TH)

TH=ThetaNBn
rh_recalculate,Mfms,th,betas,m1,th1,beta1

RH_parameters,Mfms,th,betas,a,b,c,d,y,delta
BtanJumpRH=sqrt(delta)
aggraorderpush,'BtanJumpRH',tt, BtanJumpRH 

BTANJUMPFDIFF=fracdiff(BtanJumpRH,BtanJump)
aggraorderpush,'BtanJumpFdiff',tt, BTANJUMPFDIFF 

m_p=938.0*10.0^6

CspeedI=SQRT(( 5./3 *Tproton )/m_p   )*!const.c/1000


aggraorderpush,'Cspeed_ion',tt, CspeedI 
fmsIon=Sqrt( .5*  (VA^2+CspeedI^2+sqrt((VA^2+CspeedI^2)^2-4*VA^2*CSpeedI^2*(cos(thNB)^2) ) ))




thVB=vecanglecalc(Vuvec,Buvec,/deg,/q1)
aggraorderpush,'thVBnd',tt, thVB 


thB2N=vecanglecalc(N_SN,Bdvec)
aggraorderpush,'thB2N_rad',tt, thB2N 
thB2N=vecanglecalc(N_SN,Bdvec,/deg,/q1)
aggraorderpush,'thB2N',tt, thB2N 



TH=ThetaNBnd

TH*=!pi/180
rh_recalculate,Mfms,th,betas,m1,th1,beta1

RH_parameters,Mfms,th,betas,a,b,c,d,y,delta
ThB2N_RH=acos(sqrt(b/(b+delta*c)))
aggraorderpush,'thB2N_RHrad',tt, thB2N_RH 

THB2N_RH=90-abs(90-180*THB2N_RH/!pi)
aggraorderpush,'thB2N_RH',tt, thB2N_RH 


thV2B2=vecanglecalc(Vdvec,Bdvec,/deg,/q1)
aggraorderpush,'thV2B2nd',tt, thV2B2 

lat=90-lat
lat*=!pi/180
Z_GEO=alt*cos(lat)
RHO0_GEO=alt*sin(lat)
aggraorderpush,'Z_GEO',tt, Z_GEO 
aggraorderpush,'RHO-0_GEO',tt, RHO0_GEO 


get_data,'Mfms',data=datMfms
store_data,'Mfms2',data=datMfms



;;;Large scale stuff


get_data,'Ls',data=datLS
get_data,'OrbitTheta',data=datOTH
LS=datLS.y
N=numel(LS)
R_mars = 3389.5
semimajor = 227.956*10.^6.
ecc = 0.0935
semilat = semimajor*(1 - ecc^2)
OTHd=(LS-70.-180.) MOD 360
OTHr=OTHd*!pi/180.
datOTH.y=OTHr
store_data,'OrbitTheta',data=datOTH
SOLDIST=semilat/(1+ecc*cos(OTHr))
get_data,'SolDist',data=DATSOLDIST
DATSOLDIST.y=SolDist
store_data,'SolDist',data=DATSOLDIST
soldistnorm=soldist/semimajor
tplot_element,'SolDistNorm','y',soldistnorm,/add

get_data,'Ls',data=datLS
LS=datLS.y
x=datLS.x
solsticedist=180-Abs(180-abs(270-LS))
store_data,"solsticedist",data={x:datLS.x,y:solsticedist,ytitle:['degrees from $L_S=270^\circ$ southern summer solstice'],YN:['Summer Solstice distance'], fn:["solsticedist"],  binsize:[10.],radian:[0],degree:[1]}
standdegdist=180-Abs(180-abs(273-LS))
store_data,"standdegdist",data={x:datLS.x,y:standdegdist,ytitle:['degrees from $L_S=273^\circ$ standoff maximum at $L_S=270^\circ$)'],YN:['max standoff angular distance'], fn:["standdegdist"],  binsize:[10.],radian:[0],degree:[1]}


periheliondist=180-Abs(180-abs(250-LS))
store_data,"periheliondist",data={x:datLS.x,y:periheliondist,ytitle:['degrees from $L_S=273^\circ$ perihelion ($L_S=250^\circ$)'],YN:['perihelion angular distance'], fn:["periheliondist"],  binsize:[10.],radian:[0],degree:[1]}


get_data,"solsticedist",data=dat
solsticedist=dat.y
N=numel(solsticedist)
solst=fltarr(N)
solst+=1*(solsticedist lt 45)
solst+=2*(solsticedist ge 45 and solsticedist lt 90)
solst+=3*(solsticedist ge 90 and solsticedist lt 135)
solst+=4*(solsticedist ge 135 and solsticedist lt 180)
solst-=1
store_data,'solsbins',data={x:dat.x,y:solst}

get_data,"dustdist",data=dat
dustdist=dat.y
N=numel(dustdist)
solst=fltarr(N)
solst+=1*(dustdist lt 45)
solst+=2*(dustdist ge 45 and dustdist lt 90)
solst+=3*(dustdist ge 90 and dustdist lt 135)
solst+=4*(dustdist ge 135 and dustdist lt 180)
solst-=1
store_data,'dustbins',data={x:dat.x,y:solst}

get_data,"periheliondist",data=dat
dustdist=dat.y
N=numel(dustdist)
solst=fltarr(N)
solst+=1*(dustdist lt 45)
solst+=2*(dustdist ge 45 and dustdist lt 90)
solst+=3*(dustdist ge 90 and dustdist lt 135)
solst+=4*(dustdist ge 135 and dustdist lt 180)
solst-=1
store_data,'peribins',data={x:dat.x,y:solst}


get_data,'shock0Acc',data=dat;

minAcc=1-dat.y
expAcc=10^(dat.y-1)
store_data,'1minShock0Acc',data={x: dat.x ,y: minAcc ,ytitle:[ '1-$N_{SN}^{AVG}\cdotN_{Conic}$' ], YN:[ '1-$N_{SN}^{AVG}\cdotN_{Conic}$' ], fn:[ "minshock0acc" ], binsize:[ .1 ], radian:[0],degree:[0]}
store_data,'exp10Shock0Acc',data={x: dat.x ,y: expAcc ,ytitle:[ '10^($N_{SN}^{AVG}\cdotN_{Conic}$-1)' ], YN:[ '10^($N_{SN}^{AVG}\cdotN_{Conic}$-1)' ], fn:[ "exp10shock0acc" ], binsize:[ .1 ], radian:[0],degree:[0]}
get_data,'shock0Acc',data=dat;
Acc=dat.y
minAcc=1-dat.y
dop70=1*(Acc gt .7)
store_data,'dop70',data={x:dat.x,y:dop70,YN:'dop>.7',fn:'dop70'}
dop75=1*(Acc gt .75)
store_data,'dop75',data={x:dat.x,y:dop75,YN:'dop>.75',fn:'dop75'}
dop80=1*(Acc gt .8)
store_data,'dop80',data={x:dat.x,y:dop80,YN:'dop>.8',fn:'dop80'}
dop85=1*(Acc gt .85)
store_data,'dop85',data={x:dat.x,y:dop85,YN:'dop>.85',fn:'dop85'}
dop90=1*(Acc gt .9)
store_data,'dop90',data={x:dat.x,y:dop90,YN:'dop>.9',fn:'dop90'}
dop92=1*(Acc gt .92)
store_data,'dop92',data={x:dat.x,y:dop92,YN:'dop>.92',fn:'dop92'}
dop94=1*(Acc gt .94)
store_data,'dop94',data={x:dat.x,y:dop94,YN:'dop>.94',fn:'dop94'}
dop95=1*(Acc gt .95)
store_data,'dop95',data={x:dat.x,y:dop95,YN:'dop>.95',fn:'dop95'}

end
