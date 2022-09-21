


pro shockPlotter,startdate=startdate,ndays=ndays,currtime=currtime
	;return
	TIC

	get_data,"shocks_inbound",data=datin
	get_data,"shocks_outbound",data=datout

	ins=datin.y
	outs=datout.y
	if total(datin.y + datout.y) eq 0 then return

	get_data,"dateDataCurrent",data=dateData
	startdate=dateData.startdate
	numDates=dateData.numDates
	dayLoaded=dateData.dayLoaded
	date=startdate.remove(-5)
	ndays=dateData.ndays

	;get_data,'MAVEN_POS_(MARS-MSO)',data=dpos
	get_data,'POS_interpolated_(MARS_MSO)',data=dpos


	get_data,'shock0rho_interpolated',data=datS0
	get_data,'shock0x_interpolated',data=datX0
	R_m = 3389.50D  ; +/- 0.2


	get_data,'closestConicNormal',data=datS0n

	S0n=datS0n.y

	get_data,"Shock_Normal",data=datN

	get_data,"Shock_Normal_MX1",data=datN1
	get_data,"Shock_Normal_MX2",data=datN2
	get_data,"Shock_Normal_MX3",data=datN3
	get_data,"Shock_Normal_AVG",data=datNAVG
	get_data,"Shock_Normal_best",data=datbest


get_data,'Shock_Angle',data=datAN
get_data,'Shock_Angle_MX1',data=datAMX1
get_data,'Shock_Angle_MX2',data=datAMX2
get_data,'Shock_Angle_MX3',data=datAMX3
get_data,'Shock_Angle_AVG',data=datAAVG

;help,datAMX2
MX1=datAMX1.y
MX2=datAMX2.y
MX3=datAMX3.y
AVG=datAAVG.y
AN=datAN.y 

	 x0  = 0.600*R_m
 	ecc = 1.026
  	L   = 2.081*R_m

	s0rho=datS0.y
	s0x=datX0.y
	;xs=datS0x.x
	N=size(s0x,/n_el)
	sx0=s0x-x0
	srr=SQRT(sx0^2+s0rho^2)
	srt=Sqrt(s0rho^2+(sx0+ecc*srr)^2)
	
	snxx=(ecc+sx0/srr)*srr/srt   ;<---THIS IS THE TANGENT VECTOR, NOT THE NORMAL VECTOR. NEED TO FIX IT
	snrr=s0rho/srt
	SN=[[snxx],[snrr],[fltarr(N)]]
	

	;s0rho=datS0.y;0=datS0.y
	;s0x=datX0.y;0=datX0.y
	
	Nsw=datN.y

	Nsw1=datN1.y
	Nsw2=datN2.y
	Nsw3=datN3.y

	NswA=datNAVG.y
	NswB=datbest.y
	xx=datN.x

	post=dpos.x
	oposx=dpos.y[*,0]
	oposy=dpos.y[*,1]
	oposz=dpos.y[*,2]

	pos=dpos.y

	if not keyword_set(startdate) then startdate=""
	if not keyword_set(ndays) then days="" else days=", for "+string(ndays)+" days"
	if not keyword_set(currtime) then currtime=systime()

	if startdate eq "" then begin
		date=""
		dire=plotDirector(Mdate=date,currtime="")
	endif else begin
		str=string(startdate)
		;print,str
		spltstime=strsplit(str,'/',/extract)
		;print,"spltstime=",spltstime
		;print,spltstime[0],currtime
		dire=plotDirector(Mdate=spltstime[0],currtime=currtime)
;		date=spltstime[0]
		date="_"+spltstime[0]
	endelse
	errorloop=0
error_status=0
	;catch,error_status
if error_status ne 0 then begin
		errorloop++
		if errorloop ge 10 then return
	errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON shockPlotter"]
	;print, 'Error index: ', error_status
			;print, 'Error message: ', !ERROR_STATE.MSG
			;;print,"FAILED ON "+nm
			errorsaver,xx[0],errmsg
			;print,"setting to null result"
			;wdelete
			return
endif

	;xp=interpol(oposx,post,xx)
	;yp=interpol(oposy,post,xx)
	;zp=interpol(oposz,post,xx)

	;pos=[[xp],[yp],[zp]]

	get_data,"Shock_Normal_CYL",data=datNCyl
	;Ncyl=datNCyl.y
	Ncyl=carttocyl(Nsw,pos)
	Ncyl1=carttocyl(Nsw1,pos)
	Ncyl2=carttocyl(Nsw2,pos)
	Ncyl3=carttocyl(Nsw3,pos)
	NcylA=carttocyl(NswA,pos)
	NcylB=carttocyl(NswB,pos)
	Pcyl=carttocyl(pos,pos)

	Nrho=Ncyl[*,0]
	Nphi=Ncyl[*,1]
	Nxx=Ncyl[*,2]
	;get_data,"Shock_Normal_MX1_CYL",data=datN1Cyl
	;Ncyl1=datN1Cyl.y

	;get_data,"Shock_Normal_MX2_CYL",data=datN2Cyl
	;Ncyl2=datN2Cyl.y

	;get_data,"Shock_Normal_MX3_CYL",data=datN3Cyl
	;Ncyl3=datN3Cyl.y
	Nrho1=Ncyl1[*,0]
	Nphi1=Ncyl1[*,1]
	Nxx1=Ncyl1[*,2]


	Nrho2=Ncyl2[*,0]
	Nphi2=Ncyl2[*,1]
	Nxx2=Ncyl2[*,2]

	Nrho3=Ncyl3[*,0]
	Nphi3=Ncyl3[*,1]
	Nxx3=Ncyl3[*,2]

	NrhoA=NcylA[*,0]
	NphiA=NcylA[*,1]
	NxxA=NcylA[*,2]


	NrhoB=NcylB[*,0]
	NphiB=NcylB[*,1]
	NxxB=NcylB[*,2]

	R_mars=3389.5
	Prho=SQRT(oposy^2+oposz^2)
	Pphi=atan(oposz,oposy)
	;Prho=Pcyl[*,0]
	;Pphi=Pcyl[*,1]
	Pxx=oposx;Pcyl[*,2]
	Pr=Sqrt(Pxx^2+Prho^2)
	
	GG=where(Nrho3^2+Nphi3^2+Nxx3^2 ne 0,gcount,Complement=nG)
	;print,gcount
	;;print,Nrho3[GG]^2+Nphi3[GG]^2+Nxx3[GG]^2
	;;FOR each point we pass the shock, find closest point in shock0 curve, determine distance, measure dot products
	minDists=fltarr(gcount)
	s0locs=fltarr(gcount)
	dotprods=fltarr(gcount)
	;for i=0,gcount-1 do begin
		;el=GG[i]
		;ox=Pxx[el]
		;ophi=Pphi[el]
		;orho=Prho[el]
		;oy=oposy[el]
		;oz=oposz[el]

		;D=Sqrt( (ox-s0x)^2+(orho-s0rho)^2);+ophi^2)
		;minDists[i]=min(D,minloc)
		;s0locs[i]=minloc
		;;print,minDists[i]
		;;print,"[s0x,s0rho][i]=",[s0x[i],s0rho[i]]
		;;print,"[Pxx[el],Prho[el]]=",[Pxx[el],Prho[el]]
		;dop=Nxx3[el]*snxx[minloc]+Nrho3[el]*snrr[minloc]
		;;print,"dop=",dop
		;dotprods[i]=dop
		;;print,"dotprods[i]=dotprods[",i,"]=",dotprods[i]
	;endfor

	vecsx=snxx
	vecsrr=snrr
	vecsx[nG]=0
	vecsrr[nG]=0
	yrng=[min([min(Prho)-1000,0]),max(Prho)+1000]

	minAccept=min([-6*10^3,min(Pxx)*1.3])
	maxAccept=max([6*10^3,max(Pxx)*1.3])

	;yrng=[-5*10^3,max(Prho)+1000]
	sInPlot=where((s0x gt minAccept) and (s0x lt maxAccept)) ; the conic section's x values have a minimum far beyond the region of interest, need to delete elements that aren't relevant

	shock0x=s0x[sInPlot]

	shock0rho=s0rho[sInPlot]


	;print,"max(Nphi1)=",max(Nphi1)

	;print,"mean(Nphi1)=",mean(Nphi1)

	;print,"min(Nphi1)=",min(Nphi1)


	;print,"max(Nphi3)=",max(Nphi3)

	;print,"mean(Nphi3)=",mean(Nphi3)

	;print,"min(Nphi3)=",min(Nphi3)

	;print,"max(Pxx)=",max(Pxx)

	;print,"max(Prho)=",max(Prho)
	;print,"min(Pr)=",min(Pr)
	pos1=[0.10,0.22,0.9,0.9];pos1
	cpos1=[.10,.09,.9,.15]

;	tplot_element,"dateDataCurrent",'shock0Dist',minDists,/add
;	tplot_element,"dateDataCurrent",'shock0Acc',dotprods,/add
if 1 then begin
	p1=plot(/buffer,Pxx,Prho,POSITION=pos1, $


	XTITLE='X', YTITLE='$\sqrt{Y^2+Z^2}$',xrange=[minAccept,maxAccept],yrange=yrng,color="grey")
	;print,"p1 done"

	;print,"pshock0 done"
	pmars=plot(R_mars *sin(findgen(360)*!pi/180),R_mars*cos(findgen(360)*!pi/180),/overplot,xrange=[minAccept,maxAccept])
	p2=vector(Nxx,Nrho,Pxx,Prho,POSITION=pos1,RGB_table=25,vector_colors= BYTSCL(Nphi,max=1,min=-1),Symbol='*',/overplot) 
	ps0=plot(shock0x,shock0rho,color='brown',/overplot,xrange=[minAccept,maxAccept],yrange=yrng)           
	;;print,"pshock done"
	c = COLORBAR(position=cpos1,RGB_table=25,TITLE='Tangential component of unit vector (radians), '+string(startdate)+days, range=[-1,1])
	;print,"colorbar done"
	p2.Save,dire+"shockPlot.png", BORDER=10, $

   RESOLUTION=300
	;print,"saving done"
	p1.close
;-------MX1


	p3=plot(/buffer,Pxx,Prho,POSITION=pos1, $

	XTITLE='X, MX1 normals', YTITLE='$\sqrt{Y^2+Z^2}$',xrange=[minAccept,maxAccept],yrange=yrng,color="grey")
	;print,"p3 (MX1) done"
	ps0=plot(shock0x,shock0rho,color='brown',/overplot,xrange=[minAccept,maxAccept],yrange=yrng)
	;print,"pshock0 (MX1) done"
	pmars=plot(R_mars *sin(findgen(360)*!pi/180),R_mars*cos(findgen(360)*!pi/180),/overplot)
	p4=vector(Nxx1,Nrho1,Pxx,Prho,POSITION=pos1,RGB_table=25,vector_colors=bytscl(Nphi1,max=1,min=-1),Symbol='*',/overplot)            
	;print,"pshock (MX1) done"
	c = COLORBAR(position=cpos1,RGB_table=25,$;TARGET=p4, $
	TITLE='Tangential component of MX1 unit vector (radians), '+string(startdate)+days,range=[-1,1])
	;print,"colorbar (MX1) done"
	p4.Save,dire+"shockPlotMX1"+date+".png", BORDER=10, $

   RESOLUTION=300
	print,"saving (MX1) done"
	p3.close
;------Mx2
	get_data,"Shock_Accuracy_MX2",data=datShAcc

	shAcc=datShAcc.y
	GG=where(MX2 ne 0.0,gcount)
	textX=Pxx[GG]+100
	textY=Prho[GG]-100

	;textProd=strarr(gcount)
	;;help,textProd
	;;print,gcount
	;for i=0,gcount-1 do begin
	;	;print,"i=",i
	;	el=GG[i]
	;	;print,"textProd[i]=",textProd[i]
	;	;help,textProd[i]
	;	sNCyl=strjoin(strjoin((strtrim(Ncyl2[GG[i],*],2)).substring(0,4),','),',')
	;	sSCyl=strjoin(strjoin((strtrim(S0n[GG[i],*],2)).substring(0,4),','),',')
	;	dp=strtrim(vecDotProduct(Ncyl2[el,*],S0n[el,*]),2)
	;	;print,sNCyl
	;	;help,sNCyl
	;	txt="{"+ sNCyl+"}$\cdot${"+sSCyl+"}=$\n$="+dp+"="
	;	;print,"txt=",txt
	;	;help,txt
	;	textProd[i]=txt
	;endfor
	p5=plot(/buffer,Pxx,Prho,POSITION=pos1, $

	XTITLE='X, MX2 normals [km]', YTITLE='$\sqrt{Y^2+Z^2}$ [km]',xrange=[minAccept,maxAccept],yrange=yrng,color="grey")
	;print,"p5 (MX2) done"
	ps1=plot(shock0x,shock0rho,color='brown',/overplot,xrange=[minAccept,maxAccept],yrange=yrng)
	;print,"pshock0 (MX2) done"
	pmars=plot(R_mars *sin(findgen(360)*!pi/180),R_mars*cos(findgen(360)*!pi/180),/overplot)
	p6=vector(Nxx2,Nrho2,Pxx,Prho,POSITION=pos1,RGB_table=25,vector_colors=bytscl(Nphi2,max=1,min=-1),Symbol='*',/overplot)            
	print,"pshock (MX2) done"
	;tbest=text(textX,textY,"$N^{MX2}_{SW}\cdot N_{0}=$ "+textProd+strtrim(shAcc[GG],2),/DATA, FONT_SIZE=6)

	c = COLORBAR(position=cpos1,RGB_table=25,$;TARGET=p6, $
	TITLE='Tangential component of MX2 unit vector (radians), '+string(startdate)+days,range=[-1,1])
	;print,"colorbar (MX2) done"
	p6.Save,dire+"shockPlotMX2"+date+".png", BORDER=10, $

   RESOLUTION=300
	p5.CLOSE

;------Mx3
	get_data,"Shock_Accuracy_MX3",data=datShAcc

	shAcc=datShAcc.y
	GG=where(MX3 ne 0.0,gcount)
	textX=Pxx[GG]+100
	textY=Prho[GG]-100

	;textProd=strarr(gcount)
	;;help,textProd
	;;print,gcount
	;for i=0,gcount-1 do begin
	;	;print,"i=",i
	;	el=GG[i]
	;	;print,"textProd[i]=",textProd[i]
	;	;help,textProd[i]
	;	sNCyl=strjoin(strjoin((strtrim(Ncyl3[GG[i],*],2)).substring(0,4),','),',')
	;	sSCyl=strjoin(strjoin((strtrim(S0n[GG[i],*],2)).substring(0,4),','),',')
	;	dp=strtrim(vecDotProduct(Ncyl3[el,*],S0n[el,*]),2)
	;	;print,sNCyl
	;	;help,sNCyl
	;	txt="{"+ sNCyl+"}$\cdot${"+sSCyl+"}=$\n$="+dp+"="+strtrim(shAcc[el],2)+"="
	;	;print,"txt=",txt
	;	;help,txt
	;	textProd[i]=txt
	;endfor

	p7=plot(/buffer,Pxx,Prho,POSITION=pos1, $

	XTITLE='X, MX3 normals', YTITLE='$\sqrt{Y^2+Z^2}$',yrange=yrng,color="grey")
	;print,"p7 (MX3) done"
	ps2=plot(shock0x,shock0rho,color='brown',/overplot,xrange=[minAccept,maxAccept],yrange=yrng)
	;print,"pshock0 (MX3) done"
	pmars=plot(R_mars *sin(findgen(360)*!pi/180),R_mars*cos(findgen(360)*!pi/180),/overplot)

	p8=vector(Nxx3,Nrho3,Pxx,Prho,POSITION=pos1,RGB_table=25,vector_colors=bytscl(Nphi3,max=1,min=-1),Symbol='*',/overplot)            
	;print,"pshock (MX3) done"
	tbest=text(textX,textY,"$N^{MX3}_{SW}\cdot N_{0}=$"+strtrim(shAcc[GG],2),/DATA, FONT_SIZE=6)

	c = COLORBAR(position=cpos1,RGB_table=25,$;TARGET=p8, $
	TITLE='Tangential component of MX3 unit vector (radians)'+string(startdate)+days,range=[-1,1])
	;print,"colorbar (MX2) done"
	;;print,"p8 done"
	c.Save,dire+"shockPlotMX3"+date+".png", BORDER=10, $

   RESOLUTION=300
	;print,"c saved"
print,"saving (MX3) done"

	;p2.close

	;p4.close

	;p6.close
	p7.close
	;p8.close


	;;;COMPONENT WISE AVERAGE

;------COMPONENT WISE AVERAGE

	get_data,"Shock_Accuracy_AVG",data=datShAcc

	shAcc=datShAcc.y
	GG=where(shAcc ne 0)
	textX=Pxx[GG]+100
	textY=Prho[GG]-100

	p7=plot(/buffer,Pxx,Prho,POSITION=pos1, $

	XTITLE='X, AVG normals', YTITLE='$\sqrt{Y^2+Z^2}$',yrange=yrng,color="grey",aspect_ratio=1)
	;print,"p7 (AVG) done"
	ps2=plot(shock0x,shock0rho,color='brown',/overplot,xrange=[minAccept,maxAccept],yrange=yrng,aspect_ratio=1)
	;print,"pshock0 (AVG) done"
	;pmars=plot(R_mars *sin(findgen(360)*!pi/180),R_mars*cos(findgen(360)*!pi/180),/overplot)
	
	p8=vector(NxxA,NrhoA,Pxx,Prho,POSITION=pos1,RGB_table=25,vector_colors=bytscl(NphiA,max=1,min=-1),Symbol='*',/overplot,aspect_ratio=1)            
	;print,"pshock (AVG) done"
	tbest=text(textX,textY,"$N^{AVG}_{SW}\cdot N_{conic}=$"+strtrim(shAcc[GG],2),/DATA, FONT_SIZE=6)
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over)
	tmars=text(/data,0,100,"Mars",align=.5,color=[240,231,231])
	c = COLORBAR(position=cpos1,RGB_table=25,$;TARGET=p8, $
	TITLE='Tangential component of Component wise average unit vector (radians)'+string(startdate)+days,range=[-1,1])
	c.title='Tangential component of Component wise average unit vector (radians)'+string(startdate)+days
	;print,"colorbar (AVG) done"
	;;print,"p8 done"
	c.Save,dire+"shockPlotAVG"+date+".png", BORDER=10, $

   RESOLUTION=300
	;print,"c saved"


	;p2.close

	;p4.close

	;p6.close
	p7.close
	;p8.close

print,"saving (AVG) done"
	;get_data,"Shock_Accuracy_MX3",data=datShAcc

	;shAcc=datShAcc.y
	GG=where(MX3 ne 0.0,gcount)
	textX=Pxx[GG]+100
	textY=Prho[GG]-100

	;textProd=strarr(gcount)
	;;help,textProd
	;;print,gcount
	;for i=0,gcount-1 do begin
	;	;print,"i=",i
	;	el=GG[i]
	;	;print,"textProd[i]=",textProd[i]
	;	;help,textProd[i]
	;	sNCyl=strjoin(strjoin((strtrim(Ncyl3[GG[i],*],2)).substring(0,4),','),',')
	;	sSCyl=strjoin(strjoin((strtrim(S0n[GG[i],*],2)).substring(0,4),','),',')
	;	dp=strtrim(vecDotProduct(Ncyl3[el,*],S0n[el,*]),2)
	;	;print,sNCyl
	;	;help,sNCyl
	;	txt="{"+ sNCyl+"}$\cdot${"+sSCyl+"}=$\n$="+dp+"="+strtrim(shAcc[el],2)+"="
	;	;print,"txt=",txt
	;	;help,txt
	;	textProd[i]=txt
	;endfor

	p7=plot(/buffer,Pxx,Prho,POSITION=pos1, $

	XTITLE='X, AVG normals', YTITLE='$\sqrt{Y^2+Z^2}$',yrange=yrng,color="grey")
	;print,"p7 (MX3) done"
	ps2=plot(shock0x,shock0rho,color='brown',/overplot,xrange=[minAccept,maxAccept],yrange=yrng)
	;print,"pshock0 (MX3) done"
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over)
	;pmars=plot(R_mars *sin(findgen(360)*!pi/180),R_mars*cos(findgen(360)*!pi/180),/overplot)

	p8=vector(NxxA,NrhoA,Pxx,Prho,POSITION=pos1,RGB_table=25,vector_colors=bytscl(NphiA,max=1,min=-1),Symbol='*',/overplot)            
	;print,"pshock (MX3) done"
	;tbest=text(textX,textY,"$N^{AVG}_{SW}\cdot N_{0}=$"+strtrim(shAcc[GG],2),/DATA, FONT_SIZE=6)
	tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231])
	c = COLORBAR(position=cpos1,RGB_table=25,$;TARGET=p8, $
	TITLE='Tangential component of AVG unit vector'+string(startdate)+days,range=[-1,1])
	;print,"colorbar (MX2) done"
	;;print,"p8 done"
	c.Save,dire+"shockPlotAVG2"+date+".png", BORDER=10, DIMENSIONS=[4000,3200];$

   RESOLUTION=600;, BORDER=10
	;print,"c saved"
print,"saving (MX3) done"

	;p2.close

	;p4.close

	;p6.close
	p7.close
	;p8.close
;------best dot products


	get_data,"Shock_Accuracy_best",data=datShAcc

	shAcc=datShAcc.y
	GG=where(shAcc ne 0)
	textX=Pxx[GG]+100
	textY=Prho[GG]-100


	p7=plot(/buffer,Pxx,Prho,POSITION=pos1, $

	XTITLE='X, best normals', YTITLE='$\sqrt{Y^2+Z^2}$',yrange=yrng,color="grey")
	;print,"p7 (best) done"
	ps2=plot(shock0x,shock0rho,color='brown',/overplot,xrange=[minAccept,maxAccept],yrange=yrng)
	;print,"pshock0 (AVG) done"
	pmars=plot(R_mars *sin(findgen(360)*!pi/180),R_mars*cos(findgen(360)*!pi/180),/overplot)

	p8=vector(NxxB,NrhoB,Pxx,Prho,POSITION=pos1,RGB_table=25,vector_colors=bytscl(NphiB,max=1,min=-1),Symbol='*',/overplot)            
	;print,"pshock (best) done"

	tbest=text(textX,textY,"$N^{best}_{SW}\cdot N_{conic}=$"+strtrim(shAcc[GG],2),/DATA, FONT_SIZE=6)

	c = COLORBAR(position=cpos1,RGB_table=25,$;TARGET=p8, $
	TITLE='Tangential component of best $N_{SW}\cdot N_{conic}\prime N_{SW}$s  unit vector (radians)'+string(startdate)+days,range=[-1,1])
	;print,"colorbar (best) done"
	;;print,"p8 done"
	c.Save,dire+"shockPlotbest"+date+".png", BORDER=10, $

   RESOLUTION=300
	;print,"c saved"


	;p2.close

	;p4.close

	;p6.close
	p7.close
	;p8.close
print,"saving (BEST) done"
endif

  
slocs=where(AN ne 0,nsh)

SNph=Nphi[slocs]*180/!pi
SNph1=Nphi1[slocs]*180/!pi
SNph2=Nphi2[slocs]*180/!pi
SNph3=Nphi3[slocs]*180/!pi

SNphA=NphiA[slocs]*180/!pi

sA=AN[slocs]*180/!pi
sA1=MX1[slocs]*180/!pi
sA2=MX2[slocs]*180/!pi
sA3=MX3[slocs]*180/!pi

sAA=AVG[slocs]*180/!pi
if 0 then begin
for i=0,nsh-1 do begin

	if sA[i] gt 90 then sA[i]=90-sA[i]
	if sA1[i] gt 90 then sA1[i]=90-sA1[i]
	if sA2[i] gt 90 then sA2[i]=90-sA2[i]
	if sA3[i] gt 90 then sA3[i]=90-sA3[i]
	if sAA[i] gt 90 then sA3[i]=90-sA3[i]
endfor
endif

tims=xx[slocs]
fday=(tims-xx[0])/(max(xx)-xx[0])
;print,"fday=",fday
ffday=fday
col=38
p10=scatterplot(/buffer,SNph,sA,XTITLE='$\phi$ (^$\circ$)', YTITLE='Shock Angle $\theta$ ($^\circ$)',Symbol="*",NAME="Magnetic Coplanarity method",rgb_table=col,magnitude=fday,POSITION=pos1)
p11=scatterplot(SNph1,sA1,XTITLE='$\phi$ ($^\circ$)', YTITLE='Shock Angle $\theta$ ($^\circ$)',Symbol="s",NAME="Mixed Mode 1 method",rgb_table=col,magnitude=fday,/overplot)
p12=scatterplot(SNph2,sA2,XTITLE='$\phi$ ($^\circ$)', YTITLE='Shock Angle $\theta$ ($^\circ$)',Symbol="D",NAME="Mixed Mode 2 method",rgb_table=col,magnitude=fday,/overplot)
p13=scatterplot(SNph3,sA3,XTITLE='$\phi$ ($^\circ$)', YTITLE='Shock Angle $\theta$ ($^\circ$)',Symbol="X",NAME="Mixed Mode 3 method",rgb_table=col,magnitude=fday,/overplot)
p13=scatterplot(SNphA,sAA,XTITLE='$\phi$ ($^\circ$)', YTITLE='Shock Angle $\theta$ ($^\circ$)',Symbol="o",NAME="Component-wise Average",rgb_table=col,magnitude=fday,/overplot)
;c1 = COLORBAR(TARGET=p13, $
c1 = COLORBAR(TITLE='time of shock, '+startdate,range=[0,24],POSITION=cpos1,rgb_table=col)
cc0 = LEGEND(target=[p10,p11,p12,p13])


cc0.Save,dire+"All_Phi_vs_theta"+date+".png", BORDER=10, $

   RESOLUTION=300
cc0.close
;p10.close

TOC


sA=thetanormalize(sA,/deg)
sA1=thetanormalize(sA1,/deg)
sA2=thetanormalize(sA2,/deg)
sA3=thetanormalize(sA3,/deg)
sAA=thetanormalize(sAA,/deg)
p10=scatterplot(/buffer,SNph,sA,XTITLE='$\phi$ (^$\circ$)', YTITLE='Shock Angle $\theta$ ($^\circ$)',Symbol="*",NAME="Magnetic Coplanarity method",rgb_table=col,magnitude=fday,POSITION=pos1)
p11=scatterplot(SNph1,sA1,XTITLE='$\phi$ ($^\circ$)', YTITLE='Shock Angle $\theta$ ($^\circ$)',Symbol="s",NAME="Mixed Mode 1 method",rgb_table=col,magnitude=fday,/overplot)
p12=scatterplot(SNph2,sA2,XTITLE='$\phi$ ($^\circ$)', YTITLE='Shock Angle $\theta$ ($^\circ$)',Symbol="D",NAME="Mixed Mode 2 method",rgb_table=col,magnitude=fday,/overplot)
p13=scatterplot(SNph3,sA3,XTITLE='$\phi$ ($^\circ$)', YTITLE='Shock Angle $\theta$ ($^\circ$)',Symbol="X",NAME="Mixed Mode 3 method",rgb_table=col,magnitude=fday,/overplot)
p13=scatterplot(SNphA,sAA,XTITLE='$\phi$ ($^\circ$)', YTITLE='Shock Angle $\theta$ ($^\circ$)',Symbol="o",NAME="Component-wise Average",rgb_table=col,magnitude=fday,/overplot)
c1 = COLORBAR(TITLE='time of shock, '+startdate,range=[0.,24.],POSITION=cpos1)
cc0 = LEGEND(target=[p10,p11,p12,p13])


cc0.Save,dire+"All_normalized_Phi_vs_theta"+date+".png", BORDER=10, $

   RESOLUTION=300
cc0.close
print,"finished shock plotting"

return

for i=0, nsh-1 do begin
	if SNph[i] gt 90 then SNph[i]=90-SNph[i]
	if SNph1[i] gt 90 then SNph1[i]=90-SNph1[i]
	if SNph2[i] gt 90 then SNph2[i]=90-SNph2[i]
	if SNph3[i] gt 90 then SNph3[i]=90-SNph3[i]
endfor
fday=ffday
;p20=scatterplot(SNph,sA,XTITLE='$\phi$ (degrees)', YTITLE='$\theta$ (degrees)',Symbol="*",NAME="Magnetic Coplanarity method",rgb_table=col,magnitude=fday)
;p21=scatterplot(SNph1,sA1,XTITLE='$\phi$ (degrees)', YTITLE='$\theta$ (degrees)',Symbol="s",NAME="Mixed Mode 1 method",rgb_table=col,magnitude=fday,/overplot)
;p22=scatterplot(SNph2,sA2,XTITLE='$\phi$ (degrees)', YTITLE='$\theta$ (degrees)',Symbol="D",NAME="Mixed Mode 2 method",rgb_table=col,magnitude=fday,/overplot)
;p23=scatterplot(SNph3,sA3,XTITLE='$\phi$ (degrees)', YTITLE='$\theta$ (degrees)',Symbol="X",NAME="Mixed Mode 3 method",rgb_table=col,magnitude=fday,/overplot)
;c1 = COLORBAR(TARGET=p23, $
;	TITLE='fractional time of shock (($t_{shock} -t_0)/(t_max-t_0)$)',range=[0,max(fday)])
;!null = LEGEND(target=[p20,p21,p22,p23])

phis=[[sA],[sA1],[sA2],[sA3],[fday]]
;print,"phis=",transpose(phis)
mnstd=list()

for i=0,nsh-1 do mnstd.add,[mean(phis[i,0:3]),stddev(phis[i,0:3]),mean(phis[i,1:3]),stddev(phis[i,1:3])]

;mnstd=mnstd.toarray()

;if isa(mnstd) then begin 
;;print,"mnstd=", transpose(mnstd)

;cG=where((mnstd[*,1] gt 5) or (mnstd[*,3] gt 5),count)

;;print,"mstd[cG]=",transpose(phis[cG,*])
;ENDIF
;wdelete
end
