pro overshootComparer, source=source,newName=newName,currtime=currtime,verifying=verifying
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
			fname="OverVsMach_"+date+".tplot"
			H2[where(H2 eq fname)]=";"+fname

			openW,1,"Documents/overmachDays.txt";,/append
			foreach el,H2 do printf,1,el
			close,1
			print, "finished"

		return
		print,"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
	end
	TIC
  
	if not keyword_set(newName) then newName=""
	if not keyword_set(source) then source='mach_fms_Fine_AVG'
	if not keyword_set(currtime) then currtime=systime()
	interpolator,source
	;get_data,'mvn_B_1sec_MAVEN_MSO_Mag',data=datB

	spikeFiller,source+"_interpolated"





	;get_data,'shocks_inbound',data=datin
	;get_data,'shocks_outbound',data=datout

	;sin=datin.y
	;sout=datout.y

	get_data,'overshoot',data=datO

	get_data,'Franken_fitted',data=datff
	get_data,"dateDataCurrent",data=datDC
	get_data,'B_maxs',data=datmax
	spikeFiller,"Shock_Angle_AVG"
	get_data,"Shock_Angle_AVG_flattened",data=datAVG

	spikeFiller,"Shock_Accuracy_AVG"
	get_data,"Shock_Accuracy_AVG_flattened",data=datAVGA
;	spikeFiller,"Shock_Angle_best"
;	get_data,"Shock_Angle_best_flattened",data=datAVG
	get_data,"Plasma_Beta_upstream_flattened",data=datBeta
	bta=datBeta.y
	spikeFiller,"critical_Mfms"
	get_data,"critical_Mfms_flattened",data=datCrit

	get_data,'regid_cleaned_plot',data=datReg

	get_data,'upstream_indices',data=datUI

	uis=datUI.y

	get_data,"downstream_indices",data=datDI
	get_data,"proton_cyclotron_period",data=datpcp
	dis=datDI.y

	pcp=datpcp.y

	reg=datReg.y

	N=numel(datO.x)
	overshoot=datO.y
	xs=datO.x

	x0=xs[0]


	;print,stime
	;str=string(stime)
	;print,string(stime)
	;spltstime=strsplit(str,'/',/extract)
	;print,"spltstime=",spltstime
	;print,spltstime[0]
	tim=	x2Greg(x0,/strformat);
	date=(tim.split('T'))[0]
	;date=spltstime[0]


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

	timeString=(string(x0)).trim()
	print,timeString

	ymaxs=datmax.y
	adam=datff.y
	s=dats.y

	chisqs=datff.chis

	

	GG=where(ymaxs ne 0, gcount)

	;if gcount eq 0 then return

	print,"AVG[GG]=",AVG[GG]
	print,"AVG[GG]=",AVGn[GG]
	;Return
	HH=where(AVGn[GG] gt !PI/4 , hxcount,Complement=nHH)
	;HH=where((Abs(AVG[GG] - !pi/2) lt !PI/4) , hxcount,Complement=nHH)

	
	perps=GG[HH]
	print,"s[GG],crits[GG]"
	print,transpose([[s[GG]],[crits[GG]]])

	print,"GG=",GG
	print,"HH=",HH," GG[HH]=",GG[HH]
	print,"nHH=",nHH," GG[nHH]=",GG[nHH]
	overs=ymaxs[GG]-adam[GG]
	downs=datff.downs

	downMeasured=fltarr(gcount)
	upMeasured=fltarr(gcount)
	downSTD=fltarr(gcount)
	upSTD=fltarr(gcount)

	ups=datff.ups
	fracOvers=overs/adam[GG]

	downfitOver=ymaxs[GG]-downs[GG]
	fracDownfitOver=(ymaxs[GG]-downs[GG])/downs[GG]

	upfitOver=ymaxs[GG]-datff.ups[GG]
	fracUpfitOver=(ymaxs[GG]-datff.ups[GG])/datff.ups[GG]
	fracM=s[GG]/crits[GG]

	betas=bta[GG]

	get_data,'mvn_B_1sec_MAVEN_MSO_Mag',data=datBB
	Bmag=datBB.y
	get_data,"shock_locs",data=datSL
	get_data,'mvn_eph_ls',data=datLS
	get_data,'lat',data=datLat

	Ls=datLS.y
	lat=datLat.y

	SL=datSL.y


	B2B1_RH=GG*0.0
error_status=0
	B2B1_fit=downs[GG]/datff.ups[GG]
	shock0dist=datDC.shock0Dist
	help,shock0dist
	help,B2B1_fit
	;catch,error_status
if error_status ne 0 then begin
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG

			;PRINT,'ERROR WHEN PERFORMING '+errorloc
			errorloc=""
			xaaa=x0
			errmsg=["error in overshootComparer", 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,errorloc]
			errorsaver,xaaa,errmsg,/spec
			read_strDATA2,H,"Documents/overmachDays.txt",numLines=numLines
			H2=strarr(numLines)
			for i=0,numLines-1 do H2[i]=(strsplit(H[i],";",/extract))[0] 

			x0=xs[0]
			timeString=(string(x0)).trim()
			fname="OverVsMach_"+date+".tplot"
			H2[where(H2 eq fname)]=";"+fname

			openW,1,"Documents/overmachDays.txt";,/append
			foreach el,H2 do printf,1,el
			close,1
			print, "finished"
			catch,/cancel
			return
endif



	shock0Acc=datDC.shock0Acc

	str_element,datcc,'numCrossings',gcount,/add
	str_element,datcc,'numPerp',hxcount,/add
	str_element,datcc,'numPar',gcount-hxcount,/add
	
	shocksUnix=xs[SL[GG]]
	shocksPerpUnix=xs[SL[GG[HH]]]
	str_element,datcc,'shocksUnix',shocksUnix,/add

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
	anamolous=WHERE((fracM gt 6 ) or (fracM le 0) or (downfitOver gt 30) or (downfitOver lt 0) or (fracDownfitOver gt 3.0) or (fracDownfitOver le 0) or (chisqs[GG] gt 1.15) or (fracUpfitOver lt 0) );or (upfitOver gt 20) or (fracUpfitOver gt 2.0) 


	;foreach el, anamolous do begin
	;foreach el, GG do begin
	for el=0, gcount-1 do begin

		
		i=GG[el]
		;i=GG[el]
		imin=datff.imins[i]
		imax=datff.imaxs[i]


		dop=shA[i];shock0Acc[el]
		uib=min(uis[i,*])

		uie=max(uis[i,*])

		dib=min(dis[i,*])

		die=max(dis[i,*])

		ourChisq=chisqs[i]

		anomChi=1*(ourChisq gt 1.15)

		ishock=SL[i]
		ourFracM=fracM[el]

		anomFrac=1*((ourFracM gt 6 ) or (ourFracM le 0))
		
		ourDFO=downfitOver[el]
		
		anomDFO=1*( (ourDFO lt 0) );or(ourDFO gt 10))

		ourFDFO=fracDownfitOver[el]
		
		anomFDFO=1*( (ourFDFO lt 0)); or (ourFDFO gt 2.0)  )
		
		ourUFO=upfitOver[el]
		
		anomUFO=1*( (ourUFO lt 0)); or (ourUFO gt 20) )
	
		ourFUFO=fracUpfitOver[el]

		anomFUFO=1*(ourFUFO lt 0)
		


		anomtxt=""
		anomDop=1*(dop lt .3)
		ourBta=betas[el]
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
;		if drctn then starti=mean([imin,ishock]) else endi=mean([imax,ishock])
		print,"[imin,uib,uie,ishock,dib,die,max]=",[imin,uib,uie,ishock,dib,die,imax]
		if uib gt 0 then begin
			if drctn then begin
				starti=max([uib-120,0])
				endi=max([min([die+120,N-1]),imax])
			endif else begin
				endi=min([uie+120,N-1]);mean([imax,ishock])
				starti=min([max([dib-120,0]),imin])
			endelse
			if starti gt endi and endi eq min([uie+120,N-1]) then endi=mean([imax,ishock])
			if starti gt endi and starti eq max([uib-120,0]) then starti=mean([imin,ishock])
		endif else begin
			if drctn then starti=mean([imin,ishock]) else endi=mean([imax,ishock])

		endelse

		

		print,"[starti,endi]=",[starti,endi]
		ajshock=ishock-starti
		bfit=adam[starti:endi]
		bb=Bmag[starti:endi]
		adowns=downs[starti:endi]
		ov=overshoot[starti:endi]
		aups=ups[starti:endi]
		sovers=ov*0.0+ymaxs[i]

		oox=ishock-maxishift
		ddx=ishock+maxishift

		oxj=x2Jul(oox)
		dxj=x2Jul(ddx)


		uibj=x2Jul(xs[uib])
		uiej=x2Jul(xs[uie])
		dibj=x2Jul(xs[dib])
		diej=x2Jul(xs[die])

		xG=xs[starti:endi]
		xjul=x2Jul(xG)
		shockTime=shocksGreg[el];x2Greg(xs[ishock],/strformat)

		xShock=xs[ishock]
		xsJ=x2Jul(xShock)
		jmax=max(xjul)
		jmin=min(xjul)
		begu=starti
		endu=endi
		if (uie lt starti) then begin
			;endur=min([starti-uie,4000])
			begu=max([uib-120,0])
			;endu=endur+uib
			print,endu
			begur=uib-begu
			ut=findgen(endu-begu)+begu


		endif

		if 	(uib gt endi) then begin
			endu=min([uie+120,N-1])
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
			bbmean=mean(Bmag[uib:uie])
			bbdmean=mean(Bmag[dib:die])
			bustddev=stddev(Bmag[uib:uie])
			bdstddev=stddev(Bmag[dib:die])

		downMeasured[el]=bbmean
		upMeasured[el]=bbdmean
		downSTD[el]=bdstddev
		upSTD[el]=bustddev
		th=AVGn[ishock]*180/!pi
		qtext=""
		;if th gt 45 then qtext="qperp" else qtext="qpar"
		anomUp=1*(abs(ups[i] - bbmean ) gt 2* bustddev and fracdiff(ups[i],bbmean) ge .25) or (fracdiff(ups[i],bbmean) gt .5)
		anomDown=1*((abs(downs[i] - bbdmean ) gt 2*bdstddev  and fracdiff(downs[i],bbdmean) ge .25)  or abs(downs[i] - bbdmean )/downs[i] gt abs(ymaxs[i] - bbdmean )/ymaxs[i] )

		if anomFrac or anomDFO or anomFDFO or anomUFO or anomFUFO or anomUp or anomDown or anomDop then anomtxt="Anomoly"

		print,shockTime
		shockDate=(shockTime.split('T'))[0]
		print,shockDate
		dir='Documents/Plots/'+shockDate+'/'
		FILE_MKDIR,dir

		plotlist=list()
		plota1=plot(xjul,bb,xtickunits='minutes','-',name="|B|",xtitle='time',ytitle='Magnetic Field (nT)',position=pltpos,title="Shock" +anomtxt+" at "+shockTime,/buffer)
		plotlist.add,plota1;,plot(xjul,bb,xtickunits='minutes','-',name="|B|",xtitle='time',ytitle='Magnetic Field (nT)',position=pltpos,title="Anamolous Shock at "+shockTime,/buffer)

		;plota3=plot(xjul,ov,xtickunits='minutes','-',color=!color.firebrick,name="Overshoot Region",position=pltpos,/overplot)
		plotlist.add,plot(xjul,ov,xtickunits='minutes','-',color=!color.firebrick,name="Overshoot Region",position=pltpos,/overplot)
		;plota4=
		plotlist.add,plot(xjul,sovers,xtickunits='minutes','g-',name="Overshoot Maxima="+strtrim(ymaxs[i],2)+"nT",position=pltpos,/overplot)
;		plota5=
		plotlist.add,plot(xjul,adowns,xtickunits='minutes','-',color=!color.purple,name="fit's average downstream="+strtrim(downs[i],2)+"nT",position=pltpos,/overplot)
;		plota6=
		plotlist.add,plot(xjul,aups,xtickunits='minutes','-',color=!color.cyan,name="fit's average upstream="+strtrim(ups[i],2)+"nT",position=pltpos,/overplot)
		;plota2=
		plotlist.add,plot(xjul,bfit,xtickunits='minutes','b-',name="Fitted B",position=pltpos,/overplot)
		;plota7=
		plotlist.add,plot(fltarr(2) + xsJ, plota1.yrange, name="Calculated Shock Crossing",color='orange',xtickunits='minutes', /overplot)

		if (uib lt endi) and (uib gt starti) then plotlist.add,plot(fltarr(2) + uibj, plota1.yrange, name="upstream Flags",color='cornflower',xtickunits='minutes', /overplot)
		if (uie lt endi) and (uie gt starti) then plotlist.add, plot(fltarr(2) + uiej, plota1.yrange, color='cornflower',xtickunits='minutes', /overplot)
		;plota8=plot(fltarr(2) + uibj, plota1.yrange, name="upstreamFlags",color='cornflower',xtickunits='minutes', /overplot)
		;plota9=plot(fltarr(2) + uiej, plota1.yrange,color='cornflower',xtickunits='minutes', /overplot)

;		plota10=
		if (dib lt endi) and (dib gt starti) then plotlist.add,plot(fltarr(2) + dibj, plota1.yrange, name="downsrtreamFlags",color='plum',xtickunits='minutes', /overplot)
		;plota11=
		if (die lt endi) and (die gt starti) then plotlist.add,plot(fltarr(2) + diej, plota1.yrange,color='plum',xtickunits='minutes', /overplot)
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
		plotatext6=text(fshock-.1*(2*drctn-1)+pltpos[0],.17,'$\tau_{cyclotron}=$'+strtrim(pcp[ishock],2)+"sec",color="dodger blue",/current)
		plotatext7=text(fshock-.1*(2*drctn-1)+pltpos[0],.13,'$\theta_{BN}=$'+strtrim(th,2)+"$\deg$",color="dodger_blue",/current)
		plotatext8=text(fshock-.1*(2*drctn-1)+pltpos[0],.09,'$\beta=$'+strtrim(ourBta),color="dodger_blue",/current)
		plotatext9=text(fshock-.1*(2*drctn-1)+pltpos[0],.05,'$M=$'+strtrim(s[i]),color="dodger_blue",/current)
		fname=dir+shockTime+"_shock"+qtext+anomtxt+".png"
		;fname=dir+shockTime+"_shockAnomoly.png"
		print,"saving: ",fname
		t4.save,fname,RESOLUTION=7200
		print,"saved: ",fname
		plota1.close
		
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
			plotatext3.save,fname,RESOLUTION=7200
			print,"saved: ",fname
			plotb1.close
		endif

		if (uib gt endi) and 0 then begin
			endu=min([uie+120,N-1])
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
			plotatext3.save,fname,RESOLUTION=7200
			print,"saved: ",fname
			plotb1.close
		endif

	endfor;each
	AVGnd=AVGn*180/!pi
	print,"AVGnd[GG]=",AVGnd[GG]
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


		cntr7=scatterplot(B2B1_fit,B2B1_RH,symbol='star',/sym_filled,RGB_TABLE=13,xtitle="$(B2/B1)_{fit}$",ytitle="$(B2/B1)_{RH}$",title="|B2|/|B1| RH vs FIT",$
		MAGNITUDE=bytscl(chisqs[GG]),position=pos1)
		pl7=plot(lin,lin,/overplot)
		cbb2b17=colorbar(range=[min(chisqs[GG]),max(chisqs[GG])],$;target=cntr7, $
			title='CHISQ',RGB_TABLE=13,position=cpos1)
		fname=dire+"B2B1_RHvsFITvsCHI.png"
		cbb2b17.save,fname,RESOLUTION=800
		cntr7.close

		cntr8=scatterplot(B2B1_fit,B2B1_RH,symbol='star',/sym_filled,RGB_TABLE=13,xtitle="$(B2/B1)_{fit}$",ytitle="$(B2/B1)_{RH}$",title="|B2|/|B1| RH vs FIT",$
		MAGNITUDE=BYTSCL(s[GG]),position=pos1)
		pl8=plot(lin,lin,/overplot)
		cbb2b18=colorbar(range=[min(s[GG]),max(s[GG])],$;target=cntr8, $
			title="M_{fms}",RGB_TABLE=13,position=cpos1)
		fname=dire+"B2B1_RHvsFITvsM.png"
		cbb2b18.save,fname,RESOLUTION=800
		cntr8.close

		cntr9=scatterplot(B2B1_fit,B2B1_RH,symbol='star',/sym_filled,RGB_TABLE=13,xtitle="$(B2/B1)_{fit}$",ytitle="$(B2/B1)_{RH}$",title="|B2|/|B1| RH vs FIT",$
		MAGNITUDE=BYTSCL(AVGnd[GG]),position=pos1)
		pl9=plot(lin,lin,/overplot)
		cbb2b19=colorbar(range=[min(AVGnd[GG]),max(AVGnd[GG])],$;target=cntr9, $
			title="$\theta_{bn}$ [Deg]",RGB_TABLE=13,position=cpos1)
		fname=dire+"B2B1_RHvsFITvsTh.png"
		cbb2b19.save,fname,RESOLUTION=800
		cntr9.close

		cntr10=scatterplot(B2B1_fit,B2B1_RH,symbol='star',/sym_filled,RGB_TABLE=13,xtitle="$(B2/B1)_{fit}$",ytitle="$(B2/B1)_{RH}$",title="|B2|/|B1| RH vs FIT",$
		MAGNITUDE=bytscl(betas),position=pos1)
		pl10=plot(lin,lin,/overplot)
		cbb2b110=colorbar(range=[min(betas),max(betas)],$;target=cntr10, $
			title='beta',RGB_TABLE=13,position=cpos1)
		fname=dire+"B2B1_RHvsFITvsBeta.png"
		cbb2b110.save,fname,RESOLUTION=800
		cntr10.close
		cntr0=scatterplot(B2B1_fit,B2B1_RH,symbol='star',/sym_filled,RGB_TABLE=13,xtitle="$(B2/B1)_{fit}$",ytitle="$(B2/B1)_{RH}$",title="|B2|/|B1| RH vs FIT",$
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




		pbb0=scatterplot(AVGnd[GG],b2b1fdiff,xtitle='$\theta_{bn}$ [Deg]',ytitle='2*abs(RH-FIT)/(RH+FIT)',title="Bfdiff vs theta")

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

	print,"downMeasured=",downMeasured
	GPO=where(physicalOvers ne 0,poc,Complement=nGPO)
	get_data,"dateDataCurrent",data=datcc
	shocksUnixReal=shocksUnix[GPO]
	shocksUnixBad=shocksUnix[nGPO]
	realPerp=Intersect(GPO,HH)
	realPar=Intersect(GPO,nHH)
	badPerp=Intersect(nGPO,HH)
	badPar=Intersect(nGPO,nHH)
	shocksPerpUnixReal=xs[SL[GG[realPerp]]]
	shocksParUnixReal=xs[SL[GG[realPar]]]
	shocksPerpUnixBad=xs[SL[GG[badPerp]]]
	shocksParUnixBad=xs[SL[GG[badPar]]]
	shocksGregReal=shocksGreg[GPO]
	shocksGregPerpReal=shocksGreg[realPerp]
	shocksGregParReal=shocksGreg[realPar]
	shocksGregBad=shocksGreg[nGPO]
	shocksGregPerpBad=shocksGreg[badPerp]
	shocksGregParBad=shocksGreg[badPar]
	TIC
	str_element,datcc,'shocksUnix',shocksUnix,/add
	str_element,datcc,'shocksGreg',shocksGreg,/add
	str_element,datcc,'shocksGregReal',shocksGregReal,/add
	str_element,datcc,'shocksGregBad',shocksGregBad,/add
	str_element,datcc,'shocksGregPerp',shocksGregPerp,/add
	str_element,datcc,'shocksUnixPerp',shocksUnixPerp,/add
	str_element,datcc,'shocksGregPar',shocksGregPar,/add
	str_element,datcc,'shocksUnixPar',shocksUnixPar,/add
	str_element,datcc,'shocksGregPerpReal',shocksGregPerpReal,/add
	str_element,datcc,'shocksUnixPerpReal',shocksUnixPerpReal,/add
	str_element,datcc,'shocksGregParReal',shocksGregParReal,/add
	str_element,datcc,'shocksUnixParReal',shocksUnixParReal,/add
	str_element,datcc,'shocksGregPerpBad',shocksGregPerpBad,/add
	str_element,datcc,'shocksUnixPerpBad',shocksUnixPerpBad,/add
	str_element,datcc,'shocksGregParBad',shocksGregParBad,/add
	str_element,datcc,'shocksUnixParBad',shocksUnixParBad,/add
	str_element,datcc,'isPhysical',physicalOvers,/add

	str_element,datcc,'shocksUnixReal',shocksUnixReal,/add

	str_element,datcc,'shocksUnixBad',shocksUnixBad,/add
	TOC


read_strDATA2,H,"Documents/overmachDays.txt",numLines=numLines
H2=strarr(numLines)
for i=0,numLines-1 do H2[i]=(strsplit(H[i],";",/extract))[0] 

	x0=xs[0]
	timeString=(string(x0)).trim()
	fname="OverVsMach_"+date

str_element,datcc,'MaxLocs',xs[GG],/add
str_element,datcc,'betas',betas,/add
str_element,datcc,'Mfms',s[GG],/add
str_element,datcc,'crits',crits[GG],/add
str_element,datcc,'Bmaxs',ymaxs[GG],/add
str_element,datcc,'downs',datff.downs[GG],/add
str_element,datcc,'ups',datff.ups[GG],/add
;str_element,datcc,'overshoot',datO.y,/add
str_element,datcc,'lat',lat[GG],/add
str_element,datcc,'Ls',Ls[GG],/add
		store_data,"dateDataCurrent",data=datcc
	dat={maxlocs:GG,t:xs[GG],mfms:s[GG],crits:crits[GG],Bmaxs:ymaxs[GG],overDiffs:overs,fracOverDiffs:fracOvers,$
		downups:downups[GG],ANGLE:AVG[GG],downs:datff.downs[GG],ups:datff.ups[GG],overshoot:datO.y,lat:lat[GG],Ls:Ls[GG],betas:betas,shock0dist:shock0dist,$
		shockUnix:shocksUnix,shock0Acc:shA[GG],downMeasured:downMeasured,upMeasured:upMeasured,upSTD:upSTD,downSTD:downSTD};shock0Acc}
	print,"transpose([[GG],[x[GG]-x0],[s[GG]],[crits[GG]],[s[GG]/crits[GG]],[downups[GG]],[ANGLE]])"
	print,transpose([[GG],[xs[GG]-x0],[s[GG]],[crits[GG]],[s[GG]/crits[GG]],[downups[GG]],[AVG[GG]]])
	;help,dat
	store_data,fname,data=dat
	tplot_save,fname,filename="Documents/overVsMachData/"+fname
	if total(H2 eq fname+".tplot") lt 1 then begin
		print,fname
		openU,1,"Documents/overmachDays.txt",/append
		printf,1,fname+".tplot"
		close,1
	endif


pbb15=scatterplot(AVGnd[GG],B2B1_fit,xtitle='$\theta_{bn}$ [Deg]',ytitle='Bd_fit/Bu_fit',title="Bd/Bu vs theta",xrange=[0.,100.],yrange=[0.,ceil(max(B2B1_fit))   ])

		pbb15.save,dire+"B2B1_vs_th.png"
		pbb15.close


	p1=plot(/buffer,s[GG]/crits[GG],overs,'+g-',POSITION=[0.10,0.22,0.95,0.9], $


	XTITLE=source+"/M_crit at "+date, YTITLE='overshootHeight-fit height')	
p1.Save, dire+"overshoot_vs_MfmsC"+date+".png", BORDER=10, $

   RESOLUTION=300
p1.close
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

	p7=plot(/buffer,s[GG]/crits[GG],downFitOver,'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	XTITLE=source+"/M_crit at "+date, YTITLE='overshoot-average downstream fit')	
p7.Save, dire+"overshoot-down_vs_MfmsC"+date+".png", BORDER=10, $

   RESOLUTION=300

p7.close

	p8=plot(fracM,upFitOver,'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	XTITLE=source+"/M_crit at "+date, YTITLE='overshoot-average upstream fit')
p8.Save, dire+"overshoot-up_vs_MfmsC"+date+".png", BORDER=10, $

   RESOLUTION=300
p8.close

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



	p4=plot(/buffer,s[perps]/crits[perps],overs[HH],'+g-',POSITION=[0.10,0.22,0.95,0.9], $


	XTITLE=source+"/M_crit at "+date, YTITLE='overshootHeight-fit height, quasiperp')	
p4.Save, dire+"quasiperp_overshoot_vs_MfmsC"+date+".png", BORDER=10, $

   RESOLUTION=300
p4.close
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

	p11=plot(/buffer,s[perps]/crits[perps],downFitOver[HH],'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	XTITLE=source+"/M_crit at "+date, YTITLE='quasiperp overshoot-average downstream fit')	
p11.Save, dire+"quasiperp_overshoot-down_vs_MfmsC"+date+".png", BORDER=10, $

   RESOLUTION=300
p11.close
	p12=plot(/buffer,s[perps]/crits[perps],upFitOver[HH],'+g-',POSITION=[0.10,0.22,0.9,0.9], $


	XTITLE=source+"/M_crit at "+date, YTITLE='quasiperp overshoot-average upstream fit')

p12.Save, dire+"quasiperp_overshoot-up_vs_MfmsC"+date+".png", BORDER=10, $

   RESOLUTION=300
p12.close

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

	for i=0,gcount-1 do begin
		print,shocksUnix[i]
		print,"M=",s[GG[i]]
		print,"Mcrit=",crits[GG[i]]
		print,"Bmax=",ymaxs[GG[i]]
		print,"Theta=",AVG[GG[i]],"=",AVG[GG[i]]*180/!pi,"~=",90-abs(AVG[GG[i]]*180/!pi-90)
		print,"down=",downs[GG[i]]
		print,"up=",datff.ups[GG[i]]
	endfor
tplot_element,"dateDataCurrent","finished",1,/add
	store_data,"dateDataPrev",data=datcc
end
