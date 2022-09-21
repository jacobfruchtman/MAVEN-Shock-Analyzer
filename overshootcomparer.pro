pro overshootcomparer, source=source,newName=newName,currtime=currtime,verifying=verifying,posplot=posplot
	;close,1

	get_data,"shocks_inbound",data=datin
	get_data,"shocks_outbound",data=datout
	print,"============================================="
	print,"overshootComparerovershootComparer"
	ins=datin.y
	outs=datout.y
	xs=datout.x
	
	x0=xs[0]

	tim=	x2Greg(x0,/strformat);
	date=(tim.split('T'))[0]
	if total(datin.y + datout.y) eq 0 then begin
		print,"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
		print,"Nothing here"
		tplot_element,"dateDataCurrent","finished",1,/add
		get_data,"dateDataCurrent",data=datcc
		store_data,"dateDataPrev",data=datcc
		;return
		read_strDATA2,H,"Documents/overmachDays.txt",numLines=numLines
			print,numLines
			H2=strarr(numLines)
			for i=0,numLines-1 do H2[i]=(strsplit(H[i],";",/extract))[0] 
			
			;x0=datin.x[0]
			;timeString=(string(x0)).trim()
			H=H[UNIQ(H2, SORT(H2))]
			fname="OverVsMach_"+date+".tplot"

			existlocs=where(H2 eq fname,ecount)
			if ecount gt 0 then H[existlocs]=";"+fname

			openW,1,"Documents/overmachDays.txt";,/append
			foreach el,H do printf,1,el
			close,1
			print, "finished"

		return
		print,"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	end
	error_status=0
	shocks=where(datin.y + datout.y ne 0,numshocks)
	;catch,error_status
	if error_status ne 0 then begin
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG

			;PRINT,'ERROR WHEN PERFORMING '+errorloc
			tplot_element,"dateDataCurrent","finished",1,/add
			get_data,"dateDataCurrent",data=datcc
			store_data,"dateDataPrev",data=datcc

			xaaa=xs[0]
				
				;errorloc=""
			
				errmsg=["error in overshootcomparer",'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG]
				errorsaver,xaaa,errmsg,/spec
			catch,/cancel
			return
			read_strDATA2,H,"Documents/overmachDays.txt",numLines=numLines
			print,numLines
			H2=strarr(numLines)
			for i=0,numLines-1 do H2[i]=(strsplit(H[i],";",/extract))[0] 
			
			;x0=datin.x[0]
			;timeString=(string(x0)).trim()
			H=H[UNIQ(H2, SORT(H2))]
			fname="OverVsMach_"+date+".tplot"

			existlocs=where(H2 eq fname,ecount)
			if ecount gt 0 then H[existlocs]=";ERROR;"+fname

			openW,1,"Documents/overmachDays.txt";,/append
			foreach el,H2 do printf,1,el
			close,1
			print, "finished"
			return
endif

	TIC
  TIC
	if not keyword_set(newName) then newName=""
	if not keyword_set(source) then source='mach_fms_Fine_AVG'
	if not keyword_set(currtime) then currtime=systime()
	interpolator,source
	;get_data,'mvn_B_1sec_MAVEN_MSO_Mag',data=datB

	spikeFiller,source+"_interpolated"

	get_data,"closestConic",data=datClosestConic


	closestConic=datClosestConic.y
	get_data,"closestConicNormal",data=datNormConic
	N_conic=datNormConic.y
	;get_data,'shocks_inbound',data=datin
	;get_data,'shocks_outbound',data=datout

	;sin=datin.y
	;sout=datout.y
	get_data,'mvn_B_1sec_MAVEN_MSO_Mag',data=datBB
	Bmag=datBB.y
	get_data,'overshoot',data=datO

	get_data,'Franken_fitted',data=datff
	get_data,"dateDataCurrent",data=datDC
	get_data,'B_maxs',data=datmax

	get_data,'POS_NIF',data=datNIF

	NIF=datNIF.y
	get_data,'V_NIF',data=datVNIF

	V_NIF=datVNIF.y

	V_NORMAL=TRANSPOSE(V_NIF[*,0])
	POS_NORMAL=TRANSPOSE(NIF[*,0])

	spikeFiller,"Shock_Angle_AVG"

	get_data,"Shock_Normal_AVG",data=datNAVG

	NAVG=datNAVG.y

	get_data,"Shock_Angle_AVG_flattened",data=datAVG

	spikeFiller,"Shock_Accuracy_AVG"
	get_data,"Shock_Accuracy_AVG_flattened",data=datAVGA
;	spikeFiller,"Shock_Angle_best"
;	get_data,"Shock_Angle_best_flattened",data=datAVG
	get_data,"v_Sound_Fine_upstream_flattened",data=datCs
	Cs=datCs.y

	get_data,"Vion_down",data=datVdown

	Vdown=datVdown.y
	get_data,"Vion_up",data=datVup

	Vup_coarse=datVup.y
	get_data,"Vion_up_fine",data=datVupf

	Vup_fine=datVupf.y

	get_data,'B_upstream',data=datBup
	get_data,'Bd_vector',data=datBdown

	Bvu=datBup.y
	Bvd=datBdown.y

	get_data,"Flow_Angle_upstream",data=datThetaVB	
	ThetaVB=datThetaVB.y
	get_data,"alfven_speed_Fine_upstream_flattened",data=datAlfven
	get_data,"Plasma_Beta_upstream_flattened",data=datBeta
	get_data,"Plasma_Beta_proton_upstream_flattened",data=datBetaI
	get_data,"tproton_upstream_flattened",data=datTp
	bta=datBeta.y
	btaI=datBetaI.y
	T_I=datTp.y
	spikeFiller,"critical_Mfms"
	get_data,"critical_Mfms_flattened",data=datCrit

	get_data,'regid_cleaned_plot',data=datReg

	get_data,'upstream_indices',data=datUI

	uis=datUI.y

	get_data,"downstream_indices",data=datDI
	get_data,"proton_cyclotron_period",data=datpcp
	dis=datDI.y

	pcp=datpcp.y
	gyrodistance=POS_NORMAL;2*!pi*POS_NORMAL/(pcp*V_NORMAL)
	reg=datReg.y

	N=numel(datO.x)
	overshoot=datO.y
	for i=0,N-1 do if overshoot[i] eq 0 then  overshoot[i]=!VALUES.F_NAN

	xs=datO.x

	x0=xs[0]

	get_data,'mvn_swifs_density_interpolated',data=datni
	get_data,'mvn_swe_spec_dens_interpolated',data=datne

	get_data,'mvn_B_1sec_MAVEN_MSO_Mag_modal_mean',data=datmm

	Bmm=datmm.y
	get_data,'foot_start_inbound',data=datFootI
	get_data,'foot_start_outbound',data=datFootO
	footI=datFootI.y
	footO=datFootO.y


	spikeFiller,'shock0dist'
	get_data,'shock0dist_flattened',data=dat0dist
	n_p=datni.y
	n_e=datne.y
	get_data,"POS_interpolated_(MARS_MSO)",data=datpos
	POS=datpos.y
	;print,stime
	;str=string(stime)
	;print,string(stime)
	;spltstime=strsplit(str,'/',/extract)
	;print,"spltstime=",spltstime
	;print,spltstime[0]
	tim=	x2Greg(x0,/strformat);
	date=(tim.split('T'))[0]
	;date=spltstime[0]
	YEAR=(date.split('-'))[0]
	dir='Documents/Plots/'+YEAR+'/'+date+'/'
	FILE_MKDIR,dir
	;if currtime ne '' then begin
	;	currtime=systime()
	;	secs=currtime.extract(':[0-6]{2}') 
	;	currtime=currtime.REMOVE(0,3)
	;	currtime=currtime.replace(secs,'')
	;;	currtime=currtime.replace('  ',' ')
	;	var=currtime.split(' ')  
	;	var2=var[3]
	;	var3=var[2]
	;	var[2]=var2
	;	var[3]=var3 
	;	currtime=var.join('_')+"/"
	;endif
	;dire="Documents/Plots/"+date+"/"+currtime
	;FILE_MKDIR,dir
	dire=plotDirector(Mdate=date,currtime=currtime)

	get_data,source+'_interpolated_flattened',data=dats

	downups=datff.downups


	AVG=datAVG.y

	shA=datAVGA.y
	crits=datCrit.y

	AVGn=!pi/2-abs(AVG-!pi/2)	

	;timeString=(string(x0)).trim()
	;print,timeString

	ymaxs=datmax.y
	adam=datff.y
	s=dats.y

	chisqs=datff.chis
	alg0s=datff.algshocks0
	alg1s=datff.algshocks1
	MMs=datff.MMs
	direction=datff.direction

	GG=shocks;where(ymaxs ne 0, gcount)
	GG2=where(ymaxs ne 0, g2count)
	gcount=numel(shocks)
	if g2count ne numshocks then begin
	GG2=GG
	for el=0, gcount-1 do begin

		
		i=GG[el]
		;i=GG[el]
		imin=datff.imins[i]
		imax=datff.imaxs[i]
		thesemax=where(ymaxs[imin:imax] ne 0,nummaxs)
		if nummaxs lt 1 then begin
			thesemax=where(overshoot[imin:imax] eq max(overshoot[imin:imax]))
		endif
		if nummaxs gt 1 then begin
			thesemax=where(Bmm[thesemax+imin] eq max(Bmm[thesemax+imin]))+thesemax[0]
		endif	
		GG2[el]=thesemax[0]+imin
		if total(ymaxs[imin:imax]) eq 0 then ymaxs[GG2[el]]=overshoot[GG2[el]]
	endfor
	endif
	;if gcount eq 0 then return

	;print,"AVG[GG]=",AVG[GG]
	;print,"AVG[GG]=",AVGn[GG]
	;Return
	HH=where(AVGn[GG] gt !PI/4 , hxcount,Complement=nHH)
	;HH=where((Abs(AVG[GG] - !pi/2) lt !PI/4) , hxcount,Complement=nHH)

	
	perps=GG[HH]
	;print,"s[GG],crits[GG]"
	;print,transpose([[s[GG]],[crits[GG]]])

	;print,"GG=",GG
	;print,"HH=",HH," GG[HH]=",GG[HH]
	;print,"nHH=",nHH," GG[nHH]=",GG[nHH]
	overs=ymaxs[GG2]-adam[GG]
	downs=datff.downs

	downMeasured=fltarr(gcount)
	upMeasured=fltarr(gcount)
	downSTD=fltarr(gcount)
	upSTD=fltarr(gcount)
	upNp=fltarr(gcount)
	upNe=fltarr(gcount)

	ups=datff.ups
	fracOvers=overs/adam[GG]

	downfitOver=ymaxs[GG2]-downs[GG]
	fracDownfitOver=(ymaxs[GG2]-downs[GG])/downs[GG]

	upfitOver=ymaxs[GG2]-datff.ups[GG]
	fracUpfitOver=(ymaxs[GG2]-datff.ups[GG])/datff.ups[GG]
	fracM=s[GG]/crits[GG]

	betas=bta[GG]

	
	get_data,"shock_locs",data=datSL
	get_data,'mvn_eph_ls',data=datLS
	get_data,'lat',data=datLat

	Ls=datLS.y
	lat=datLat.y

	SL=datSL.y


	B2B1_RH=GG*0.0
error_status=0
	B2B1_fit=downs[GG]/datff.ups[GG]
	shock0dist=dat0dist.y[GG];datDC.shock0Dist
	;help,shock0dist
	;help,B2B1_fit




	shock0Acc=datDC.shock0Acc

	;str_element,datcc,'numCrossings',gcount,/add
	;str_element,datcc,'numPerp',hxcount,/add
	;str_element,datcc,'numPar',gcount-hxcount,/add
	
	shocksUnix=xs[SL[GG]]
	shocksPerpUnix=xs[SL[GG[HH]]]
	;str_element,datcc,'shocksUnix',shocksUnix,/add

	shocksGreg=list()
	physicalOvers=fltarr(gcount)+1
	foreach el, shocksUnix do shocksGreg.add,x2Greg(el,/strformat)
	shocksGreg=shocksGreg.toArray()
	shocksGregPerp=shocksGreg[HH]
	shocksGregPar=shocksGreg[nHH]
	pltposi=[0.30,0.10,0.95,0.90]
	pltposo=[0.15,0.10,0.80,0.90]
	cbpos=[0.30,0.05,0.70,0.10]

	legposi=[.20,.95]
	;legposo=[1.2,.95]
	legposo=[.7,.95]
	anamolous=WHERE((fracM gt 9 ) or (fracM le 0) or (downfitOver gt 30) or (downfitOver lt 0) or (fracDownfitOver gt 3.0) or (fracDownfitOver le 0) or (chisqs[GG] gt 1.15) or (fracUpfitOver lt 0) );or (upfitOver gt 20) or (fracUpfitOver gt 2.0) 


	;foreach el, anamolous do begin
	;foreach el, GG do begin
	for el=0, gcount-1 do begin

		
		i=GG[el]
		ymaxloc=GG2[el]
		;i=GG[el]
		imin=datff.imins[i]
		imax=datff.imaxs[i]

		ourMM=MMs[i,*]
		dop=shA[i];shock0Acc[el]
		uib=min(uis[i,*])

		uie=max(uis[i,*])

		dib=min(dis[i,*])

		die=max(dis[i,*])

		ourChisq=chisqs[i]

		anomChi=1*(ourChisq gt 1.15)
		ourBta=betas[el]
		ishock=SL[i]
		ourFracM=fracM[el]

		if ishock+x0 gt time_double('2015-08-03/05:57') and ishock+x0 lt time_double('2015-08-03/05:59') then begin
		ymaxs[ishock:ishock+9.*60]=max(Bmag[ishock:time_double('2015-08-03/05:58')-x0])

		endif

		anomFrac=1*((ourFracM gt 8 ) or (ourFracM le 0))
		
		ourDFO=downfitOver[el]
		
		anomDFO=1*( (ourDFO lt 0) );or(ourDFO gt 10))

		ourFDFO=fracDownfitOver[el]
		
		anomFDFO=1*( (ourFDFO lt 0)); or (ourFDFO gt 2.0)  )
		
		ourUFO=upfitOver[el]
		
		anomUFO=1*( (ourUFO lt 0)); or (ourUFO gt 20) )
	
		ourFUFO=fracUpfitOver[el]

		anomFUFO=1*(ourFUFO lt 0)
		
		anomBeta=1*(ourBta gt 20. or finite(ourBta) ne 1)

		anomtxt=""
		anomDop=1*(dop lt .3)
		
		if (fracM[el] gt 15 ) or (fracM[el] lt 0) or (downfitOver[el] lt 0) then physicalOvers[el]=0
		maxishift=i-ishock
		if keyword_set(verifying) then begin
			M=s[i]

			th=AVGn[i]
			ourBta=betas[el]

			RH_parameters,M,th,ourBta,a,b,c,d,yy,delta

			B2B1_RH[el]=SQRT((b+c*delta)/(b+c))





			continue
		endif
		drctn=1*(adam[imin+2] lt adam[imax-2])
		if drctn then pltpos=pltposi else pltpos=pltposo
		if drctn then legpos=legposi else legpos=legposo

		starti=imin
		endi=imax
		print,"drctn=",drctn


		if drctn then begin
			pltpos=pltposi
			legpos=legposi
			wherefoot=where(FootI[imin:imax] ne 0)
			if wherefoot[0] ne -1 then foot=wherefoot[-1]+imin else foot=ishock
		endif else begin
			pltpos=pltposo
			legpos=legposo

			wherefoot=where(FootO[imin:imax] ne 0)
			if wherefoot[0] ne -1 then foot=wherefoot[0]+imin else foot=ishock

		endelse
;		if drctn then starti=mean([imin,ishock]) else endi=mean([imax,ishock])
		print,"[imin,uib,uie,ishock,dib,die,max]=",[imin,uib,uie,ishock,dib,die,imax]
		instart=max([min([uib-120,ishock-15*60.]),0])
		inend=max([min([die+120,N-1]),min([imax+120,N-1])])
		outend=min([max([uie+120,ishock+15*60.]),N-1])
		outstart=min([max([dib-120.,0]),max([imin-120.,0])])
		if uib gt 0 then begin

			if drctn then begin
				starti=instart;max([min([uib-120,foot-120,ishock-15*60.]),0])
						endi=inend;min([die+6*60.,imax]);max([min([die+120,N-1]),imax])
			endif else begin
				endi=outend;mean([imax,ishock])
				starti=outstart;min([max([dib-120.,0]),imin])
			endelse
			if starti gt endi and endi eq outend then endi=mean([imax,ishock])
			if starti gt endi and starti eq instart then starti=mean([imin,ishock])
		endif else begin
			if drctn then starti=0 else endi=mean([imax,foot])

		endelse

		numsec=endi-starti

		if numsec le 40*60. then res=min([numsec*.75,1000]) else res=1000;min([numsec*.75,1000]);5000

		print,"[starti,endi]=",[starti,endi]
		ajshock=ishock-starti
		bfit=adam[starti:endi]
		bb=Bmag[starti:endi]

		;ymm=Bmm[starti:endi]

		adowns=downs[starti:endi]
		ov=overshoot[starti:endi]

		aups=ups[starti:endi]
		sovers=fltarr(numsec)+ymaxs[ymaxloc]

		oox=ishock-maxishift
		ddx=ishock+maxishift

		oxj=x2Jul(oox)
		dxj=x2Jul(ddx)

		;xBmax=xs[el]
		;xjBmax=x2Jul(xBmax)
		uibj=x2Jul(xs[uib])
		uiej=x2Jul(xs[uie])
		dibj=x2Jul(xs[dib])
		diej=x2Jul(xs[die])
		footj=x2Jul(xs[foot])
		print,foot,footj,",foottime:",time_string(xs[foot])
		xG=xs[starti:endi]
		xjul=x2Jul(xG)
		shockTime=shocksGreg[el];x2Greg(xs[ishock],/strformat)

		xShock=xs[ishock]
		xsJ=x2Jul(xShock)
		jmax=max(xjul)
		jmin=min(xjul)
		begu=starti
		endu=endi
		if 0 then begin

		if (uie lt starti) then begin
			;endur=min([starti-uie,4000])
			begu=instart;max([min([uib-120,foot-120,ishock-15*60.]),0])
			;endu=endur+uib
			print,endu
			begur=uib-begu
			ut=findgen(endu-begu)+begu


		endif

		if 	(uib gt endi) then begin
			endu=outend;min([max([uie+120,foot+120,ishock+15.*60]),N-1])
			print,endu
			;begur=min([uib-endi,4000])
			endur=endu-uie
			;begu=uib-begur
			ut=findgen(begur+endur+(uie-uib))+begu
	 		;xut=xs[begu:endu]

		endif
	 		xut=xs[begu:endu]
			xuj=x2Jul(xut)
			bb2=Bmag[begu:endu]
		endif

			bbmean=mean(Bmag[uib:uie])
			bbdmean=mean(Bmag[dib:die])
			bustddev=stddev(Bmag[uib:uie])
			bdstddev=stddev(Bmag[dib:die])
			nelec=mean(n_e[uib:uie],/nan)
			npro=mean(n_p[uib:uie])

			gyrorad=pcp[ishock]*mean(V_NORMAL[uib:uie])/(2*!pi)
			gyrodistance[imin:imax]/=gyrorad

			upNp[el]=npro
			upNe[el]=nelec
		downMeasured[el]=bbmean
		upMeasured[el]=bbdmean
		downSTD[el]=bdstddev
		upSTD[el]=bustddev
		th=AVGn[ishock]*180/!pi



		qtext=""
		;if th gt 45 then qtext="qperp" else qtext="qpar"
		anomUp=1*(abs(ups[i] - bbmean ) gt 2* bustddev and fracdiff(ups[i],bbmean) ge .25) or (fracdiff(ups[i],bbmean) gt .5)
		anomDown=1*((abs(downs[i] - bbdmean ) gt 2*bdstddev  and fracdiff(downs[i],bbdmean) ge .25)  or abs(downs[i] - bbdmean )/downs[i] gt abs(ymaxs[i] - bbdmean )/ymaxs[i] )

		if anomFrac or anomDFO or anomFDFO or anomUFO or anomFUFO or anomUp or anomDown or anomDop or anomBeta then begin
			anomtxt="Anomoly"
			res=300
			if (finite(nelec,/nan) eq 1) or (finite(ourBta,/nan) eq 1) or (ourBta eq 0) or nelec le 0 or npro le 0 then begin
					anomtxt+="NAN"
					res=200
			endif
		endif
		print,shockTime
		shockDate=(shockTime.split('T'))[0]
		print,shockDate
		if shockDate ne date then begin
			dir='Documents/Plots/'+YEAR+'/'+shockDate+'/'
			FILE_MKDIR,dir
		endif
		plotlist=list()
		plota1=plot(xjul,bb,xtickunits='minutes','-',name="|B|",xtitle='time',ytitle='Magnetic Field (nT)',position=pltpos,title="Shock" +anomtxt+" at "+shockTime);,/buffer)
		plotlist.add,plota1;,plot(xjul,bb,xtickunits='minutes','-',name="|B|",xtitle='time',ytitle='Magnetic Field (nT)',position=pltpos,title="Anamolous Shock at "+shockTime,/buffer)
		;plotlist.add,plot(fltarr(2) + xjBmax, plota1.yrange,xtickunits='minutes','g:',name="Overshoot Maximum location",/overplot)
		;plota3=plot(xjul,ov,xtickunits='minutes','-',color=!color.firebrick,name="Overshoot Region",position=pltpos,/overplot)
		plotlist.add,plot(xjul,ov,xtickunits='minutes','-',color=!color.firebrick,name="Overshoot Region",position=pltpos,/overplot)
		;plota4=
		plotlist.add,plot(xjul,sovers,xtickunits='minutes','g-',name="Overshoot Maxima="+strtrim(ymaxs[ymaxloc],2)+"nT",position=pltpos,/overplot)
;		plota5=
		plotlist.add,plot(xjul,adowns,xtickunits='minutes','-',color=!color.purple,name="fit's average downstream="+strtrim(downs[i],2)+"nT",position=pltpos,/overplot)
;		plota6=
		plotlist.add,plot(xjul,aups,xtickunits='minutes','-',color=!color.cyan,name="fit's average upstream="+strtrim(ups[i],2)+"nT",position=pltpos,/overplot)
		;plota2=
		;plotlist.add,plot(xjul,ymm,xtickunits='minutes','-',color=!color.peru,name="mean($<|B|>_{2\tau},$<|B|>_{3\tau},$<|B|>_{4\tau}$)",position=pltpos,/overplot)
		plotlist.add,plot(xjul,bfit,xtickunits='minutes','b-',name="Fitted B",position=pltpos,/overplot)

		;plota7=
		plotlist.add,plot(fltarr(2) + xsJ, plota1.yrange, name="Calculated Shock Crossing",color='orange',xtickunits='minutes', /overplot)

		if (uib lt endi) and (uib gt starti) then plotlist.add,plot(fltarr(2) + uibj, plota1.yrange, name="upstream Flag st",color='slate blue',xtickunits='minutes', /overplot)
		if (uie lt endi) and (uie gt starti) then plotlist.add, plot(fltarr(2) + uiej, plota1.yrange, color='cornflower',xtickunits='minutes', /overplot)
		;plota8=plot(fltarr(2) + uibj, plota1.yrange, name="upstreamFlags",color='cornflower',xtickunits='minutes', /overplot)
		;plota9=plot(fltarr(2) + uiej, plota1.yrange,color='cornflower',xtickunits='minutes', /overplot)

;		plota10=
		if (dib lt endi) and (dib gt starti) then plotlist.add,plot(fltarr(2) + dibj, plota1.yrange, name="downsrtreamFlags",color='plum',xtickunits='minutes', /overplot)
		;plota11=
		if (die lt endi) and (die gt starti) then plotlist.add,plot(fltarr(2) + diej, plota1.yrange,color='plum',xtickunits='minutes', /overplot)
		plotlist.add,plot(fltarr(2) + footj, plota1.yrange,xtickunits='minutes',color='chocolate', /overplot,name="Foot Bottom")
		plotlist=plotlist.toarray()
		;leg = LEGEND(TARGET=[plota1,plota2,plota3,plota4,plota5,plota6,plota7,plota10], POSITION=legpos, $
		leg = LEGEND(TARGET=plotlist, POSITION="best",$;;legpos, $
		 /RELATIVE, /AUTO_TEXT_COLOR ,FONT_SIZE=6,TRANSPARENCY=30)
		;print,(xsJ-2*(2*drctn-1))/(jmax-jmin-1)

		fshock=1.0*(xsJ-jmin)/(jmax-jmin+1)
		t1=text(fshock+pltpos[0] , 0.8, shockTime ,color=!color.orange,/relative)
		;plotatext=text(legpos[0],.5,'$N^{AVG}\cdot N_0$='+strtrim(dop,2),color=anomclr(anomDop),/current)
		plotatext5=text(fshock-.1*(2*drctn-1)+pltpos[0],.45,'$N^{AVG}\cdot N_0$='+strtrim(dop,2),color=anomclr(anomDop),/current)
		plotatext5=text(fshock-.1*(2*drctn-1)+pltpos[0],.41,'mean($B_d$) within flagged interval='+strtrim(bbdmean,2),color=anomclr(anomDown),/current)
		plotatext4=text(fshock-.1*(2*drctn-1)+pltpos[0],.37,'mean($B_u$) within flagged interval='+strtrim(bbmean,2),color=anomclr(anomUp),/current)
		plotatext2=text(dxj-jmin,downs[i]+2,'fitted downstream average='+strtrim(downs[ishock],2),color='magenta',/DATA)
		plotatext3=text(fshock-.1*(2*drctn-1)+pltpos[0],.33,'fitted upstream average='+strtrim(ups[ishock],2),color=anomclr(anomUp),/current)
		;plotatext3=text(oxj,ymaxs[i]+2,'Bmax='+strtrim(ymaxs[i],2),color="green",/DATA)
		t2=text(fshock-.1*(2*drctn-1)+pltpos[0],.29,'M/Mcrit='+strtrim(ourFracM,2),/current,color=anomclr(anomFrac))
		t3=text(fshock-.1*(2*drctn-1)+pltpos[0],.25,'Bmax-Bd='+strtrim(ourDFO,2),/current,color=anomclr(anomDFO))
		t4=text(fshock-.1*(2*drctn-1)+pltpos[0],.21,'(Bmax-Bd)/Bd='+strtrim(ourFDFO,2),/current,color=anomclr(anomFDFO))
		;t5=text(fshock-.1*(2*drctn-1)+pltpos[0],.15,'CHISQ='+strtrim(ourChisq,2),/current,color=anomclr(anomChi))
		plotatext6=text(fshock-.1*(2*drctn-1)+pltpos[0],.17,'$\tau_{cyclotron}=$'+strtrim(pcp[ishock],2)+"sec, $MM_1^{-1}=$"+strtrim(1./ourMM[1],2),color="dodger blue",/current)
		plotatext7=text(fshock-.1*(2*drctn-1)+pltpos[0],.13,'$\theta_{BN}=$'+strtrim(th,2)+"$\deg$, $n^{up interval}_{SWIFA}$="+strtrim(npro,2)+", $n^{up interval}_{SWE}$="+strtrim(nelec,2),color="dodger_blue",/current)
		plotatext8=text(fshock-.1*(2*drctn-1)+pltpos[0],.09,'$\beta=$'+strtrim(ourBta),color=anomclr(anomBeta),/current)
		plotatext9=text(fshock-.1*(2*drctn-1)+pltpos[0],.05,'$M=$'+strtrim(s[i]),color="dodger_blue",/current)
		fname=dir+shockTime+"_shock"+qtext+anomtxt+".png"
		;fname=dir+shockTime+"_shockAnomoly.png"
		print,"saving: ",fname
		;t4.save,fname,RESOLUTION=res
		print,"saved: ",fname
		;plota1.close
		
		
		;;;;;;;
		if keyword_set(posplot) then begin
			
			wf=where(gyrodistance[imin:imax] eq -2,wc)+imin
			if wc ne 0 then begin
				if drctn and wf[0] lt starti then starti=wf[0]
				if ~drctn and wf[0] gt endi then endi=wf[0]
			endif
			bfit=adam[starti:endi]
			bb=Bmag[starti:endi]

		;ymm=Bmm[starti:endi]

			adowns=downs[starti:endi]
			ov=overshoot[starti:endi]
			aups=ups[starti:endi]

			pfoot=gyrodistance[foot]
			pshock=gyrodistance[ishock]
			uibp=gyrodistance[uib]
			uiep=gyrodistance[uie]
			dibp=gyrodistance[dib]
			diep=gyrodistance[die]
			gyrodist=gyrodistance[starti:endi]
plotlist=list()
		plota1=plot(gyrodist,bb,'-',name="|B|",xtitle='$x_N/ (V^{ion}_N/\omega_p)$',ytitle='Magnetic Field (nT)',position=pltpos,title="Shock" +anomtxt+" at "+shockTime,/buffer)
		plotlist.add,plota1;,plot(xjul,bb,xtickunits='minutes','-',name="|B|",xtitle='time',ytitle='Magnetic Field (nT)',position=pltpos,title="Anamolous Shock at "+shockTime,/buffer)
		;plotlist.add,plot(fltarr(2) + xjBmax, plota1.yrange,xtickunits='minutes','g:',name="Overshoot Maximum location",/overplot)
		;plota3=plot(xjul,ov,xtickunits='minutes','-',color=!color.firebrick,name="Overshoot Region",position=pltpos,/overplot)
		plotlist.add,plot(gyrodist,ov,'-',color=!color.firebrick,name="Overshoot Region",position=pltpos,/overplot)
		;plota4=
		plotlist.add,plot(gyrodist,sovers,'g-',name="Overshoot Maxima="+strtrim(ymaxs[ymaxloc],2)+"nT",position=pltpos,/overplot)
;		plota5=
		plotlist.add,plot(gyrodist,adowns,'-',color=!color.purple,name="fit's average downstream="+strtrim(downs[i],2)+"nT",position=pltpos,/overplot)
;		plota6=
		plotlist.add,plot(gyrodist,aups,'-',color=!color.cyan,name="fit's average upstream="+strtrim(ups[i],2)+"nT",position=pltpos,/overplot)
		;plota2=
		;plotlist.add,plot(xjul,ymm,xtickunits='minutes','-',color=!color.peru,name="mean($<|B|>_{2\tau},$<|B|>_{3\tau},$<|B|>_{4\tau}$)",position=pltpos,/overplot)
		plotlist.add,plot(gyrodist,bfit,'b-',name="Fitted B",position=pltpos,/overplot)

		;plota7=
		plotlist.add,plot(fltarr(2) + pshock, plota1.yrange, name="Calculated Shock Crossing",color='orange', /overplot)

		if (uib lt endi) and (uib gt starti) then plotlist.add,plot(fltarr(2) + uibp, plota1.yrange, name="upstream Flag st",color='slate blue', /overplot)
		if (uie lt endi) and (uie gt starti) then plotlist.add, plot(fltarr(2) + uiep, plota1.yrange, color='cornflower', /overplot)
		;plota8=plot(fltarr(2) + uibj, plota1.yrange, name="upstreamFlags",color='cornflower',xtickunits='minutes', /overplot)
		;plota9=plot(fltarr(2) + uiej, plota1.yrange,color='cornflower',xtickunits='minutes', /overplot)

;		plota10=
		if (dib lt endi) and (dib gt starti) then plotlist.add,plot(fltarr(2) + dibp, plota1.yrange, name="downsrtreamFlags",color='plum', /overplot)
		;plota11=
		if (die lt endi) and (die gt starti) then plotlist.add,plot(fltarr(2) + diep, plota1.yrange,color='plum', /overplot)
		plotlist.add,plot(fltarr(2) + pfoot, plota1.yrange,color='chocolate', /overplot,name="0th order Foot Bottom")
		plotlist=plotlist.toarray()
		;leg = LEGEND(TARGET=[plota1,plota2,plota3,plota4,plota5,plota6,plota7,plota10], POSITION=legpos, $
		leg = LEGEND(TARGET=plotlist, POSITION="best",$;;legpos, $
		 /RELATIVE, /AUTO_TEXT_COLOR ,FONT_SIZE=6,TRANSPARENCY=30)
		;print,(xsJ-2*(2*drctn-1))/(jmax-jmin-1)

		
		fname=dir+shockTime+"_shock"+qtext+anomtxt+"_NIF.png"
		;fname=dir+shockTime+"_shockAnomoly.png"
		print,"saving: ",fname
		;leg.save,fname,RESOLUTION=res
		print,"saved: ",fname
		;plota1.close





		end


		if (uie lt starti) and 0 then begin
			endur=min([starti-uie,4000])
			begu=max([uib-120,0])
			endu=endur+uib
			print,endu
			begur=uib-begu
			ut=findgen(endu-begu)+begu
	 		xut=xs[begu:endu]
			xuj=x2Jul(xut)
			bb2=Bmag[begu:endu]
			print,min(xuj),uibj,uiej,max(xuj)
			print,min(xuj)-min(xuj),uibj-min(xuj),uiej-min(xuj),max(xuj)-min(xuj)
			ups2=fltarr(endu-begu)+ups[i]
			plotlist=list()
			plotb1=plot(xuj,bb2,xtickunits='minutes','-',name="|B|",xtitle='time',ytitle='Magnetic Field (nT)',position=pltpos,title="upstream interval of Shock" +anomtxt+" at "+shockTime,/buffer)
			plotlist.add,plotb1;,plot(xjul,bb,xtickunits='minutes','-',name="|B|",xtitle='time',ytitle='Magnetic Field (nT)',position=pltpos,title="Anamolous Shock at "+shockTime,/buffer)
			plotlist.add,plot(xuj,ups2,xtickunits='minutes','-',color=!color.cyan,name="fit's average upstream="+strtrim(ups[i],2)+"nT",position=pltpos,/overplot)

			plotlist.add,plot(fltarr(2) + uibj, plotb1.yrange, name="upstream Flags",color='cornflower',xtickunits='minutes', /overplot)
			plotlist=plotlist.toarray()
		;leg = LEGEND(TARGET=[plota1,plota2,plota3,plota4,plota5,plota6,plota7,plota10], POSITION=legpos, $
		leg = LEGEND(TARGET=plotlist, POSITION=legpos, $
		 /RELATIVE, /AUTO_TEXT_COLOR ,FONT_SIZE=6,TRANSPARENCY=30)
			pltb2= plot(fltarr(2) + uiej, plotb1.yrange, color='cornflower',xtickunits='minutes', /overplot)
			t1=text(fshock+pltpos[0] , 0.8, shockTime ,color=!color.orange,/relative)
			plotatext2=text(.5,(ups[i]+2)/(max(ups[begu:endu])),'fitted upstream average='+strtrim(ups[i],2),color=anomclr(anomUp),/current)
			plotatext3=text(.5,(ups[i]+3)/(max(ups[begu:endu])),'mean(B) within marked interval='+strtrim(bbmean,2),color=anomclr(anomUp),/current)
			fname=dir+shockTime+"_upstream_of_shock"+anomtxt+".png"
		;fname=dir+shockTime+"_shockAnomoly.png"
			print,"saving: ",fname
			;plotatext3.save,fname,RESOLUTION=7200
			;print,"saved: ",fname
			;plotb1.close
		endif

		if (uib gt endi) and 0 then begin
			endu=min([max([uie+120,foot+120,ishock+15*60.]),N-1])
			print,endu
			begur=min([uib-endi,4000])
			endur=endu-uie
			begu=uib-begur
			ut=findgen(begur+endur+(uie-uib))+begu
	 		xut=xs[begu:endu]
			xuj=x2Jul(xut)
			bb2=Bmag[begu:endu]
			print,min(xuj),uibj,uiej,max(xuj)
			print,min(xuj)-min(xuj),uibj-min(xuj),uiej-min(xuj),max(xuj)-min(xuj)
			ups2=fltarr(begur+endur+(uie-uib))+ups[i]
			plotlist=list()
			plotb1=plot(xuj,bb2,xtickunits='minutes','-',name="|B|",xtitle='time',ytitle='Magnetic Field (nT)',position=pltpos,title="upstream interval of Shock" +anomtxt+" at "+shockTime,/buffer)
			plotlist.add,plotb1;,plot(xjul,bb,xtickunits='minutes','-',name="|B|",xtitle='time',ytitle='Magnetic Field (nT)',position=pltpos,title="Anamolous Shock at "+shockTime,/buffer)
			plotlist.add,plot(xuj,ups2,xtickunits='minutes','-',color=!color.cyan,name="fit's average upstream="+strtrim(ups[i],2)+"nT",position=pltpos,/overplot)

			plotlist.add,plot(fltarr(2) + uibj, plotb1.yrange, name="upstream Flags",color='cornflower',xtickunits='minutes', /overplot)
			plotlist=plotlist.toarray()
		;leg = LEGEND(TARGET=[plota1,plota2,plota3,plota4,plota5,plota6,plota7,plota10], POSITION=legpos, $
		leg = LEGEND(TARGET=plotlist, POSITION=legpos, $
		 /RELATIVE, /AUTO_TEXT_COLOR ,FONT_SIZE=6,TRANSPARENCY=30)
			pltb2= plot(fltarr(2) + uiej, plotb1.yrange, color='cornflower',xtickunits='minutes', /overplot)


			t1=text(fshock+pltpos[0] , 0.8, shockTime ,color=!color.orange,/relative)
			plotatext2=text(.4,(ups[i]+2)/(max(ups[begu:endu])),'fitted upstream average='+strtrim(ups[i],2),color=anomclr(anomUp),/current)
			plotatext3=text(.4,(ups[i]+3)/(max(ups[begu:endu])),'mean(B) within marked interval='+strtrim(bbmean,2),color=anomclr(anomUp),/current)
			fname=dir+shockTime+"_upstream_of_shock"+anomtxt+".png"
		;fname=dir+shockTime+"_shockAnomoly.png"
			print,"saving: ",fname
			;plotatext3.save,fname,RESOLUTION=res
			print,"saved: ",fname
			;plotb1.close
		endif

	endfor;each
	
	
	return
	
	
	AVGnd=AVGn*180/!pi
	;print,"AVGnd[GG]=",AVGnd[GG]
	if keyword_set(verifying) then begin


		shock0dist=datDC.shock0dist
		shock0Acc=datDC.shock0Acc
		pos1=[0.10,0.22,0.9,0.9]
		cpos1=[.10,.09,.9,.17]
		b2b1Max=max([max(B2B1_fit),max(B2B1_RH)])
		b2b1Min=min([min(B2B1_fit),min(B2B1_RH)])
		B2B1_fit_byt=(255.9999)*(B2B1_fit-b2b1Min)/(b2b1Max-b2b1Min)
		B2B1_RH_byt=(255.9999)*(B2B1_RH-b2b1Min)/(b2b1Max-b2b1Min)
		b2b1diff=abs(B2B1_RH-B2B1_fit)
		b2b1fdiff=2*b2b1diff/(B2B1_RH+B2B1_fit)

		lin=findgen(20)*(b2b1Max-b2b1Min)/20.0+b2b1Min
		linearStatRegress,B2B1_fit,B2B1_RH,a,b,Rsqr

		lreg=B2B1_fit*a+b
		linearStatRegress,B2B1_RH,B2B1_fit,a,b,Rsqr2

		lreg2=B2B1_RH*a+b
		;cntr1=contour(B2B1_RH,s[GG],AVGn[GG],xtitle="Mach Number",ytitle="$\theta_{bn}$",title='|B2|/|B1| via RH',/DOWNHILL, 	LAYOUT=[2,1,1],C_VALUE=[1,1.5,1.75,2,2.25,2.5,2.75,3,3.5,4])
		;cntr2=contour(B2B1_fit,s[GG],AVGn[GG],xtitle="Mach Number",ytitle="$\theta_{bn}$",title='|B2|/|B1| via fit',/DOWNHILL,/CURRENT, LAYOUT=[2,1,2],C_VALUE=[1,1.5,1.75,2,2.25,2.5,2.75,3,3.5,4])
		;fname=dire+"B2B1contours.png"
		;cntr2.save,fname,RESOLUTION=800
		;cntr1.close
		
		;cntr3=plot(findgen(gcount),B2B1_RH-B2B1_fit,xtitle="shockNumber",ytitle='$(B2/B1)_{RH}-(B2/B1)_{fit}$',title='difference between calculations',LAYOUT=[1,2,1])
		;cntr6=plot(findgen(gcount),2.0*(B2B1_RH-B2B1_fit)/(B2B1_RH+B2B1_fit),xtitle="shockNumber",ytitle="$\frac{(B2/B1)_{RH}-(B2/B1)_{fit}}{(B2/B1)_{RH}+(B2/B1)_{fit}}$",$
			;title='relative difference between calculations',/current,LAYOUT=[1,2,2])
		;fname=dire+"B2B1diffs.png"
		;cntr6.save,fname,RESOLUTION=800
		;cntr3.close


		cntr4=scatterplot(/buffer,s[GG],AVGnd[GG],symbol='star',/sym_filled,RGB_TABLE=8,xtitle="Mach Number",ytitle="$\theta_{bn}$",title='|B2|/|B1| via RH',MAGNITUDE=B2B1_RH_byt,LAYOUT=[3,1,2])
		
		;cntr3.close
		cntr5=scatterplot(/buffer,s[GG],AVGnd[GG],symbol='star',/sym_filled,RGB_TABLE=8,xtitle="Mach Number",ytitle="$\theta_{bn}$",title='|B2|/|B1| via fit',$
			/CURRENT,MAGNITUDE=B2B1_fit_byt,LAYOUT=[3,1,3])

	
		cbb2b1=colorbar(ORIENTATION=1, $
			 POSITION=[0.22,0.05,0.29,0.9], $
			title='|B2|/|B1|',RGB_TABLE=8,RANGE=[b2b1Min,b2b1Max])

		;dir='Documents/Plots/'+shockDate+'/'
		fname=dire+"B2B1Scatters.png"
		cbb2b1.save,fname,RESOLUTION=800
		cntr5.close
		;cntr1.close


		cntr7=scatterplot(/buffer,B2B1_fit,B2B1_RH,symbol='star',/sym_filled,RGB_TABLE=13,xtitle="$(B2/B1)_{fit}$",ytitle="$(B2/B1)_{RH}$",title="|B2|/|B1| RH vs FIT",$
		MAGNITUDE=bytscl(chisqs[GG]),position=pos1)
		pl7=plot(lin,lin,/overplot)
		cbb2b17=colorbar(range=[min(chisqs[GG]),max(chisqs[GG])],$;target=cntr7, $
			title='CHISQ',RGB_TABLE=13,position=cpos1)
		fname=dire+"B2B1_RHvsFITvsCHI.png"
		cbb2b17.save,fname,RESOLUTION=800
		cntr7.close

		cntr8=scatterplot(/buffer,B2B1_fit,B2B1_RH,symbol='star',/sym_filled,RGB_TABLE=13,xtitle="$(B2/B1)_{fit}$",ytitle="$(B2/B1)_{RH}$",title="|B2|/|B1| RH vs FIT",$
		MAGNITUDE=BYTSCL(s[GG]),position=pos1)
		pl8=plot(lin,lin,/overplot)
		cbb2b18=colorbar(range=[min(s[GG]),max(s[GG])],$;target=cntr8, $
			title="M_{fms}",RGB_TABLE=13,position=cpos1)
		fname=dire+"B2B1_RHvsFITvsM.png"
		cbb2b18.save,fname,RESOLUTION=800
		cntr8.close

		cntr9=scatterplot(/buffer,B2B1_fit,B2B1_RH,symbol='star',/sym_filled,RGB_TABLE=13,xtitle="$(B2/B1)_{fit}$",ytitle="$(B2/B1)_{RH}$",title="|B2|/|B1| RH vs FIT",$
		MAGNITUDE=BYTSCL(AVGnd[GG]),position=pos1)
		pl9=plot(lin,lin,/overplot)
		cbb2b19=colorbar(range=[min(AVGnd[GG]),max(AVGnd[GG])],$;target=cntr9, $
			title="$\theta_{bn}$ [Deg]",RGB_TABLE=13,position=cpos1)
		fname=dire+"B2B1_RHvsFITvsTh.png"
		cbb2b19.save,fname,RESOLUTION=800
		cntr9.close

		cntr10=scatterplot(/buffer,B2B1_fit,B2B1_RH,symbol='star',/sym_filled,RGB_TABLE=13,xtitle="$(B2/B1)_{fit}$",ytitle="$(B2/B1)_{RH}$",title="|B2|/|B1| RH vs FIT",$
		MAGNITUDE=bytscl(betas),position=pos1)
		pl10=plot(lin,lin,/overplot)
		cbb2b110=colorbar(range=[min(betas),max(betas)],$;target=cntr10, $
			title='beta',RGB_TABLE=13,position=cpos1)
		fname=dire+"B2B1_RHvsFITvsBeta.png"
		cbb2b110.save,fname,RESOLUTION=800
		cntr10.close
		cntr0=scatterplot(/buffer,B2B1_fit,B2B1_RH,symbol='star',/sym_filled,RGB_TABLE=13,xtitle="$(B2/B1)_{fit}$",ytitle="$(B2/B1)_{RH}$",title="|B2|/|B1| RH vs FIT",$
		MAGNITUDE=bytscl(b2b1fdiff),position=pos1)
		pl0=plot(lin,lin,/overplot,name="expected slope")
		pl0r=plot(B2B1_fit,lreg,'-',color="brown",name="RH_{regress1}=$m_{regress1} Fit +b_{regress1}$, $R^2=$"+strtrim(Rsqr,2),/overplot)
		pl0r2=plot(lreg2,B2B1_RH,'-',color="gold",name="FIT_{regress2}=$m_{regress2} RH +b_{regress2}$, $R^2=$"+strtrim(Rsqr2,2),/overplot)
		lpl0= LEGEND(target=[pl0r,pl0r2])
		cbb2b10=colorbar(range=[min(b2b1fdiff),max(b2b1fdiff)],$;target=cntr10, $
			title='2*abs(RH-FIT)/(RH+FIT)',RGB_TABLE=13,position=cpos1)
		fname=dire+"B2B1_RHvsFIT.png"
		cbb2b10.save,fname,RESOLUTION=800
		cntr0.close




		pbb0=scatterplot(/buffer,AVGnd[GG],b2b1fdiff,xtitle='$\theta_{bn}$ [Deg]',ytitle='2*abs(RH-FIT)/(RH+FIT)',title="Bfdiff vs theta")

		pbb0.save,dire+"Bfdiff_vs_th.png"
		pbb0.close
		pbb1=scatterplot(s[GG],b2b1fdiff,xtitle="$\M_{fms}$",ytitle='2*abs(RH-FIT)/(RH+FIT)',title='Bfdiff vs Mfms')
		
		pbb1.save,dire+"Bfdiff_vs_Mfms.png"
		pbb1.close

		pbb2=scatterplot(betas,b2b1fdiff,xtitle="$\beta$",ytitle='2*abs(RH-FIT)/(RH+FIT)',title='Bfdiff vs beta')
		
		pbb2.save,dire+"Bfdiff_vs_beta.png"
		pbb2.close

		pbb3=scatterplot(chisqs[GG],b2b1fdiff,xtitle="$\chi^2$",ytitle='2*abs(RH-FIT)/(RH+FIT)',title="Bfdiff vs Chisq")
		
		pbb3.save,dire+"Bfdiff_vs_chisq.png"
		pbb3.close

		;fracDownfitOver
		pbb4=scatterplot(fracDownfitOver,b2b1fdiff,xtitle="$(B_{max}-B^{fit}_D)/B^{fit}_D$",ytitle='2*abs(RH-FIT)/(RH+FIT)',title="Bfdiff vs normalized overshoot")
		
		pbb4.save,dire+"Bfdiff_vs_fdover.png"
		pbb4.close

		pbb4=scatterplot(ymaxs[GG],b2b1fdiff,xtitle="$B_{max}$ [nT]",ytitle='2*abs(RH-FIT)/(RH+FIT)',title="Bfdiff vs Bmax")
		
		pbb4.save,dire+"Bfdiff_vs_Bmax.png"
		pbb4.close


		pbb5=scatterplot(shock0dist,b2b1fdiff,xtitle="distance between conic and measured shock",ytitle='2*abs(RH-FIT)/(RH+FIT)',title="Bfdiff vs shock0dist")
		
		pbb5.save,dire+"Bfdiff_vs_shock0dist.png"
		pbb5.close

		cooo=[[shock0Acc],[b2b1fdiff]]
		print,cooo
		pbb4=scatterplot(shock0Acc,b2b1fdiff,xtitle="dot product of conic and measured shock normals",ytitle='2*abs(RH-FIT)/(RH+FIT)',title="Bfdiff vs dp")
		
		pbb4.save,dire+"Bfdiff_vs_dp.png"
		pbb4.close

		cntr12=scatterplot(B2B1_fit,B2B1_RH,symbol='star',/sym_filled,RGB_TABLE=13,xtitle="$(B2/B1)_{fit}$",ytitle="$(B2/B1)_{RH}$",title="|B2|/|B1| RH vs FIT",$
		MAGNITUDE=bytscl(shock0Acc),position=pos1)
		pl12=plot(lin,lin,/overplot)
		cbb2b112=colorbar(range=[min(shock0Acc),max(shock0Acc)],$;target=cntr10, $
			title='$\bf{n}_{shock0Conic}\cdot\bf{n}_{AVG}$',RGB_TABLE=13,position=cpos1)
		fname=dire+"B2B1_RHvsFITvsShock0Acc.png"
		cbb2b112.save,fname,RESOLUTION=800
		cntr12.close

		cntr12=scatterplot(B2B1_fit,B2B1_RH,symbol='star',/sym_filled,RGB_TABLE=13,xtitle="$(B2/B1)_{fit}$",ytitle="$(B2/B1)_{RH}$",title="|B2|/|B1| RH vs FIT",$
		MAGNITUDE=bytscl(shock0dist),position=pos1)
		pl12=plot(lin,lin,/overplot)
		cbb2b112=colorbar(range=[min(shock0dist),max(shock0dist)],$;target=cntr10, $
			title='$| \bar{x}_{shock0Conic}-\bar{x}_{AVG}|$',RGB_TABLE=13,position=cpos1)
		fname=dire+"B2B1_RHvsFITvsShock0dist.png"
		cbb2b112.save,fname,RESOLUTION=800
		cntr12.close


		sctr1=scatterplot(shock0dist,shock0Acc,symbol='star',/sym_filled,RGB_TABLE=13,xtitle="distance between conic and measured shock (km)",ytitle="$\bf{n}^{shock}_{closestConic}\cdot\bf{n}^{shock}_{Measured}$",title="accuracy vs distance",$
		MAGNITUDE=bytscl(b2b1fdiff),position=pos1)
		cbsctr1=colorbar(range=[min(b2b1fdiff),max(b2b1fdiff)],$;target=cntr10, $
			title='2*abs(RH-FIT)/(RH+FIT)',RGB_TABLE=13,position=cpos1)
		cbsctr1.save,dire+"shock0AccVsDistVsBfdiff.png",RESOLUTION=800
		sctr1.close
		;return
	endif
read_strDATA2,H,"Documents/overmachDays.txt",numLines=numLines
H2=strarr(numLines)
for i=0,numLines-1 do H2[i]=(strsplit(H[i],";",/extract))[0] 
H2=H2[UNIQ(H2, SORT(H2))]

	;timeString=(string(x0)).trim()
	fname="OverVsMach_"+date


		store_data,"dateDataCurrent",data=datcc
	dat={maxlocs:GG2,t:shocksUnix,mfms:s[GG],crits:crits[GG],Bmaxs:ymaxs[GG2],overDiffs:overs,fracOverDiffs:fracOvers,$
		downups:downups[GG],ANGLE:AVG[GG],downs:datff.downs[GG],ups:datff.ups[GG],overshoot:datO.y,lat:lat[GG],Ls:Ls[GG],betas:betas,shock0dist:shock0dist,$
	shockUnix:shocksUnix,shock0Acc:shA[GG],downMeasured:downMeasured,upMeasured:upMeasured,upSTD:upSTD,downSTD:downSTD,pos:POS[GG,*],FF:datff.y,Alfven:datAlfven.y[GG],$
	uindices:uis[GG,*],dindices:dis[GG,*],Cs:Cs[GG],flowangle:ThetaVB[GG],imins:datff.imins[GG],imaxs:datff.imaxs[GG],N_p:upNp,N_e:upNe,$

	T_ion:T_I[GG],beta_ion:btaI[GG],N_SN:NAVG[GG,*],N_conic:N_conic[GG,*],$
	pos_conic:closestConic[GG,*],Vuvec_coarse:Vup_coarse[GG,*],Vuvec_fine:Vup_fine[GG,*],Vdvec:Vdown[GG,*],$
	Buvec:Bvu[GG,*],Bdvec:Bvd[shocks,*],MMs:MMs[GG,*],algshocks0:alg0s[GG],algshocks1:alg1s[GG],direction:direction[GG],period:pcp[GG]};shock0Acc}
	;print,"transpose([[GG],[x[GG]-x0],[s[GG]],[crits[GG]],[s[GG]/crits[GG]],[downups[GG]],[ANGLE]])"
	;print,transpose([[GG],[xs[GG]-x0],[s[GG]],[crits[GG]],[s[GG]/crits[GG]],[downups[GG]],[AVG[GG]]])
	;help,dat
	;alg0s=datff.algshocks0
	;alg1s=datff.algshocks1
	;if x0 lt time_double('2017-07-09/00:00:00') then str_element,dat
		numpoints=numel(GG)
		upstartpos=fltarr(numpoints,3)
		upendpos=fltarr(numpoints,3)
		downstartpos=fltarr(numpoints,3)
		downendpos=fltarr(numpoints,3)
		upmidpos=fltarr(numpoints,3)
		downmidpos=fltarr(numpoints,3)

		get_data,"POS_interpolated_(MARS_MSO)",data=datPos
		POS=datPos.y

		uindices=dat.uindices
		dindices=dat.dindices
		print,"numpoints=",numpoints
		for el=0,numpoints-1 do begin

			ustart=min(uindices[el,*])
			uend=max(uindices[el,*])
			dstart=min(dindices[el,*])
			dend=max(dindices[el,*])
			umid=mean(uindices[el,*])
			dmid=mean(dindices[el,*])

			thisusp=POS[ustart,*]
			thisump=POS[umid,*]
			thisuep=POS[uend,*]
			thisdsp=POS[dstart,*]
			thisdmp=POS[dmid,*]
			thisdep=POS[dend,*]

			for j=0, 2 do begin
				upstartpos[el,j]=thisusp[j]
				upmidpos[el,j]=thisump[j]
				upendpos[el,j]=thisuep[j]
				downstartpos[el,j]=thisdsp[j]
				downmidpos[el,j]=thisdmp[j]
				downendpos[el,j]=thisdep[j]
			endfor

		endfor
		str_element,dat,'upstartpos',upstartpos,/add
		str_element,dat,'upmidpos',upmidpos,/add
		str_element,dat,'upendpos',upendpos,/add

		str_element,dat,'downstartpos',downstartpos,/add
		str_element,dat,'downmidpos',downmidpos,/add
		str_element,dat,'downendpos',downendpos,/add


	store_data,fname,data=dat



	tplot_save,fname,filename="Documents/overVsMachData/"+fname
	if total(H2 eq fname+".tplot") lt 1 then begin
		print,fname
		openU,1,"Documents/overmachDays.txt",/append
		printf,1,fname+".tplot"
		close,1
	endif
	;print,"downMeasured=",downMeasured

;str_element,datcc,'MaxLocs',xs[GG],/add
;str_element,datcc,'betas',betas,/add
;str_element,datcc,'Mfms',s[GG],/add
;str_element,datcc,'crits',crits[GG],/add
;str_element,datcc,'Bmaxs',ymaxs[GG],/add
;str_element,datcc,'downs',datff.downs[GG],/add
;str_element,datcc,'ups',datff.ups[GG],/add
;;str_element,datcc,'overshoot',datO.y,/add
;str_element,datcc,'lat',lat[GG],/add
;str_element,datcc,'Ls',Ls[GG],/add

	GPO=where(physicalOvers ne 0,poc,Complement=nGPO)
	;get_data,"dateDataCurrent",data=datcc
	;shocksUnixReal=shocksUnix[GPO]
	;shocksUnixBad=shocksUnix[nGPO]
	;realPerp=Intersect(GPO,HH)
	;realPar=Intersect(GPO,nHH)
	;badPerp=Intersect(nGPO,HH)
	;badPar=Intersect(nGPO,nHH)
	;shocksPerpUnixReal=xs[SL[GG[realPerp]]]
	;;shocksParUnixReal=xs[SL[GG[realPar]]]
	;shocksPerpUnixBad=xs[SL[GG[badPerp]]]
	;shocksParUnixBad=xs[SL[GG[badPar]]]
	;shocksGregReal=shocksGreg[GPO]
	;shocksGregPerpReal=shocksGreg[realPerp]
	;shocksGregParReal=shocksGreg[realPar]
	;shocksGregBad=shocksGreg[nGPO]
	;shocksGregPerpBad=shocksGreg[badPerp]
	
	;str_element,datcc,'shocksUnix',shocksUnix,/add
	;str_element,datcc,'shocksGreg',shocksGreg,/add
	;str_element,datcc,'shocksGregReal',shocksGregReal,/add
	;str_element,datcc,'shocksGregBad',shocksGregBad,/add
	;str_element,datcc,'shocksGregPerp',shocksGregPerp,/add
	;str_element,datcc,'shocksUnixPerp',shocksUnixPerp,/add
	;str_element,datcc,'shocksGregPar',shocksGregPar,/add
	;str_element,datcc,'shocksUnixPar',shocksUnixPar,/add
	;str_element,datcc,'shocksGregPerpReal',shocksGregPerpReal,/add
	;str_element,datcc,'shocksUnixPerpReal',shocksUnixPerpReal,/add
	;str_element,datcc,'shocksGregParReal',shocksGregParReal,/add
	;str_element,datcc,'shocksUnixParReal',shocksUnixParReal,/add
	;str_element,datcc,'shocksGregPerpBad',shocksGregPerpBad,/add
	;str_element,datcc,'shocksUnixPerpBad',shocksUnixPerpBad,/add
	;str_element,datcc,'shocksGregParBad',shocksGregParBad,/add
	;str_element,datcc,'shocksUnixParBad',shocksUnixParBad,/add
	;str_element,datcc,'isPhysical',physicalOvers,/add

	;str_element,datcc,'shocksUnixReal',shocksUnixReal,/add

	;str_element,datcc,'shocksUnixBad',shocksUnixBad,/add
	;store_data,"dateDataCurrent",data=datcc
	TOC


if 0 then begin


pbb15=scatterplot(/buffer,AVGnd[GG],B2B1_fit,xtitle='$\theta_{bn}$ [Deg]',ytitle='Bd_fit/Bu_fit',title="Bd/Bu vs theta",xrange=[0.,100.],yrange=[0.,ceil(max(B2B1_fit))   ])

		pbb15.save,dire+"B2B1_vs_th.png"
		pbb15.close
pbb16=scatterplot(/buffer,betas,B2B1_fit,xtitle='$\beta$',ytitle='Bd_fit/Bu_fit',title="Bd/Bu vs Plasma Beta",yrange=[0.,ceil(max(B2B1_fit))   ])

		pbb16.save,dire+"B2B1_vs_beta.png"
		pbb16.close

	;p1=plot(/buffer,s[GG]/crits[GG],overs,'+g-',POSITION=[0.10,0.22,0.95,0.9], $


;	XTITLE=source+"/M_crit at "+date, YTITLE='overshootHeight-fit height')	
;p1.Save, dire+"overshoot_vs_MfmsC"+date+".png", BORDER=10, $

 ;  RESOLUTION=300
;p1.close
	p2=plot(/buffer,s[GG]/crits[GG],fracOvers,'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	XTITLE=source+"/M_crit at "+date, YTITLE='(overshootHeight-fit height)/fit height')	
p2.Save, dire+"frac_overshoot_vs_MfmsC"+date+".png", BORDER=10, $

   RESOLUTION=300
p2.close

	p30=plot(/buffer,s[GG],downups[GG],'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	XTITLE=source+" at "+date, YTITLE='downstream frankenfitted/upstream frankenfitted',xrange=[0.,ceil(max(s[GG]))] ,yrange= [0.,ceil(max(downups[GG]))]  )	
p30.Save, dire+"downups_vs_Mfms"+date+".png", BORDER=10, $

   RESOLUTION=300
p30.close

	p3=plot(/buffer,s[GG]/crits[GG],downups[GG],'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	XTITLE=source+"/M_crit at "+date, YTITLE='downstream frankenfitted/upstream frankenfitted')	
p3.Save, dire+"downups_vs_MfmsC"+date+".png", BORDER=10, $

   RESOLUTION=300
p3.close

	;p7=plot(/buffer,s[GG]/crits[GG],downFitOver,'+g-',POSITION=[0.10,0.22,0.9,0.9], $


;	XTITLE=source+"/M_crit at "+date, YTITLE='overshoot-average downstream fit')	
;p7.Save, dire+"overshoot-down_vs_MfmsC"+date+".png", BORDER=10, $

   ;RESOLUTION=300

;p7.close

	;p8=plot(/buffer,fracM,upFitOver,'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	;XTITLE=source+"/M_crit at "+date, YTITLE='overshoot-average upstream fit')
;p8.Save, dire+"overshoot-up_vs_MfmsC"+date+".png", BORDER=10, $

  ; RESOLUTION=300
;p8.close

	p9=plot(/buffer,s[GG]/crits[GG],fracDownFitOver,'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	XTITLE=source+"/M_crit at "+date, YTITLE='(overshoot-average downstream fit)/average downstream fit')	
p9.Save, dire+"frac_overshoot-down_vs_MfmsC"+date+".png", BORDER=10, $

   RESOLUTION=300
p9.close

	p10=plot(/buffer,s[GG]/crits[GG],fracUpFitOver,'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	XTITLE=source+"/M_crit at "+date, YTITLE='(overshoot-average upstream fit)/average upstream fit')
p10.Save, dire+"frac_overshoot-up_vs_MfmsC"+date+".png", BORDER=10, $

   RESOLUTION=300
p10.close

if (perps[0] ne -1) and (HH[0] ne -1) then begin



	;p4=plot(/buffer,s[perps]/crits[perps],overs[HH],'+g-',POSITION=[0.10,0.22,0.95,0.9], $


	;XTITLE=source+"/M_crit at "+date, YTITLE='overshootHeight-fit height, quasiperp')	
;p4.Save, dire+"quasiperp_overshoot_vs_MfmsC"+date+".png", BORDER=10, $

  ; RESOLUTION=300
;p4.close
	p5=plot(/buffer,s[perps]/crits[perps],fracOvers[HH],'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	XTITLE=source+"/M_crit at "+date, YTITLE='(overshootHeight-fit height)/fit height,quasiperp')	
p5.Save, dire+"quasiperp_frac_overshoot_vs_MfmsC"+date+".png", BORDER=10, $

   RESOLUTION=300
p5.close
	p6=plot(/buffer,s[perps],downups[perps],'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	XTITLE=source+"/M_crit at "+date, YTITLE='downstream frankenfitted/upstream frankenfitted,quasiperp')	
p6.Save, dire+"quasiperp_downups_vs_MfmsC"+date+".png", BORDER=10, $

   RESOLUTION=300
p6.close

	;p11=plot(/buffer,s[perps]/crits[perps],downFitOver[HH],'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	;XTITLE=source+"/M_crit at "+date, YTITLE='quasiperp overshoot-average downstream fit')	
;p11.Save, dire+"quasiperp_overshoot-down_vs_MfmsC"+date+".png", BORDER=10, $

   ;RESOLUTION=300
;p11.close
	;p12=plot(/buffer,s[perps]/crits[perps],upFitOver[HH],'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	;XTITLE=source+"/M_crit at "+date, YTITLE='quasiperp overshoot-average upstream fit')

;p12.Save, dire+"quasiperp_overshoot-up_vs_MfmsC"+date+".png", BORDER=10, $

  ; RESOLUTION=300
;p12.close

	p13=plot(/buffer,s[perps]/crits[perps],fracDownFitOver[HH],'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	XTITLE=source+"/M_crit at "+date, YTITLE='quasiperp (overshoot-average downstream fit)/average downstream fit')	

	p13.Save, dire+"quasiperp_frac_overshoot-down_vs_MfmsC"+date+".png", BORDER=10, $

   RESOLUTION=300
p13.close

	p14=plot(/buffer,s[perps]/crits[perps],fracUpFitOver[HH],'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	XTITLE=source+"/M_crit at "+date, YTITLE='(overshoot-average upstream fit)/average upstream fit')

	p14.Save, dire+"quasiperp_frac_overshoot-up_vs_MfmsC"+date+".png", BORDER=10, $

   RESOLUTION=300

	p14.close

endif
endif
	;AA=[[GG],[xs[GG]],[s[GG]],[overs],[fracOvers]]
	;print,AA[0,1],FORMAT='I10'
	;print,Transpose(AA)
	;fname="OverVsMach"+timeString

	;for i=0, gcount-1 do 
	;PRINTF,1,AA,FORMAT='(I7,5X,I10,5X,F2.7,5X,F2.7,5X,F2.7)'
	;CLOSE,1
;	name=

	;READ_DATA,A,fname
	;A=TRANSPOSE(A)
	;print,size(A)
	;PRINT,A[0,*]

	;print,A

	;print,Transpose(A[0:5,2:4])



;	for ii=0,gcount-1


	;XADJ=(A[*,1]-A[0,1])/(max(A[*,1])-min(A[*,1]))


	;p3=plot(A[*,2],A[*,3],'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	;XTITLE=source, YTITLE='overshootHeight-fit height')

	;p3.VERT_COLOR=XADJ

	;p4=plot(A[*,2],A[*,4],'+ ',POSITION=[0.10,0.22,0.9,0.9], $
	;XTITLE=source, YTITLE='(overshootHeight-fit height)/fit height')
	;c3 = COLORBAR(TARGET=p3,RGB_TABLE=28, $
	;TITLE='The fractional time of the shock')
	;p3.Save, "All_overshoot_vs_MfmsC"+newName+".png", BORDER=10, $

   ;RESOLUTION=300



	;p4.VERT_COLOR=XADJ

	

	;c4 = COLORBAR(TARGET=p4,RGB_TABLE=28, $
	;TITLE='The fractional time of the shock')
	;p4.Save, "All_frac_overshoot_vs_MfmsC"+newName+".png", BORDER=10, $

   ;RESOLUTION=300


	TOC
tplot_element,"dateDataCurrent","finished",1,/add
	store_data,"dateDataPrev",data=datDC

	if numel(GG) ne numel(shocks) then begin
		print,'======================='
		foreach el,shocks do print,time_string(el)
		print,'======================='
		foreach el,GG do print,time_string(el)


	endif

RETURN

	for i=0,gcount-1 do begin
		print,shocksUnix[i]
		print,"M=",s[GG[i]]
		print,"Mcrit=",crits[GG[i]]
		print,"Bmax=",ymaxs[GG[i]]
		print,"Theta=",AVG[GG[i]],"=",AVG[GG[i]]*180/!pi,"~=",90-abs(AVG[GG[i]]*180/!pi-90)
		print,"down=",downs[GG[i]]
		print,"up=",datff.ups[GG[i]]
	endfor


	mfms=dat.mfms
	ANGLE=dat.ANGLE
	betas=dat.betas
	ANGLEn=!pi/2-ABS(ANGLE-!pi/2)
	B2B1_RH=fltarr(numel(mfms))
	B2B1_RH2=fltarr(numel(mfms))
	B2B1_Measure=dat.upMeasured/dat.downMeasured

	Bmax=dat.Bmaxs
	Bd=dat.upMeasured
	for i=0,numel(mfms)-1 do begin

			M=mfms[i]
			th=ANGLEn[i]
			ourBta=betas[i]

			RH_parameters,M,th,ourBta,a,b,c,d,yy,delta

			B2B1_RH[i]=SQRT((b+c*delta)/(b+c))
			B2B1_RH2[i]=SQRT((Cos(th)) ^2 + delta * (Sin(th))^2 )
	endfor

	print,B2B1_RH-B2B1_RH2
	B2_RH=B2B1_RH*dat.downMeasured
	
	B2B1fdiff=200*abs(B2B1_Measure-B2B1_RH)/abs(B2B1_Measure+B2B1_RH)
	B2B1Mdiff=200*abs(B2B1_Measure-B2B1_RH)/abs(B2B1_Measure+B2B1_RH)
	B2B1FMdiff=200*abs(B2B1_Measure-B2B1_Measure)/abs(B2B1_Measure+B2B1_Measure)



	print,B2B1_Measure/B2B1_RH

	b2b1Max=max([max(B2B1_Measure),max(B2B1_RH)])
	b2b1Min=min([min(B2B1_Measure),min(B2B1_RH)])
	B2B1_Measure_byt=(255.9999)*(B2B1_Measure-b2b1Min)/(b2b1Max-b2b1Min)
	B2B1_RH_byt=(255.9999)*(B2B1_RH-b2b1Min)/(b2b1Max-b2b1Min)

	cpos2=[0.22,0.05,0.29,0.9]
	ANGLEnd=ANGLEn*180/!pi
	sctr1RH=scatterplot(mfms,ANGLEnd,symbol='.',/sym_filled,RGB_TABLE=13,xtitle="Mach Number",ytitle="$\theta_{bn}$ [deg]",title='|B2|/|B1| via RH',MAGNITUDE=B2B1_RH_byt,LAYOUT=[3,1,2])
	sctr1FIT=scatterplot(mfms,ANGLEnd,symbol='.',/sym_filled,RGB_TABLE=13,xtitle="Mach Number",ytitle="$\theta_{bn} [deg]$",title='|B2|/|B1| via measure',$
			/CURRENT,MAGNITUDE=B2B1_Measure_byt,LAYOUT=[3,1,3])

	
		cbb2b1=colorbar(ORIENTATION=1, $
			 POSITION=[0.22,0.05,0.29,0.9], $
			title='|B2|/|B1|',RGB_TABLE=13,RANGE=[b2b1Min,b2b1Max])

		;dir='Documents/Plots/'+shockDate+'/'
		fname=dire+"B2B1MeasureVsRHScatters.png"
		cbb2b1.save,fname,RESOLUTION=800
		sctr1RH.close
	;mxmx=max(Bmax)
	lx=findgen(11)
	p22=plot(B2_RH,Bmax,'r-',xtitle="$(B2/B1)_{RH}\times B_u$ ",ytitle="Magnetic Field",title="Bmax",name="Bmax")
	p23=plot(B2_RH,Bd,'g-',xtitle="$(B2/B1)_{RH}\times B_u$ ",ytitle="Magnetic Field",title="Bd measured",/over,name="$B_{DOWN}^{MEASURED}$")
	p24=plot(lx,lx,'b-',name="Line",/over)
	leg = LEGEND(TARGET=[p22,p23,p24], POSITION=legpos, $
		 /RELATIVE, /AUTO_TEXT_COLOR ,FONT_SIZE=6,TRANSPARENCY=30)

end
