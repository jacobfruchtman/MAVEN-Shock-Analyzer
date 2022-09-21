pro projplot,plt,asym=asym,indcoord=indcoord,magrange=magrange,ct=ct,zlog=zlog
	;;todo: make indcoord keyword capable of choosing which of x,y,z is independent
	if not keyword_set(ct) then ct=72
	dire="Documents/Plots/CombinedPlots/"
	R_mars=3389.5
	pos1=[0.10,0.22,0.9,0.9]
	cpos1=[.10,.09,.9,.15]
	
	get_data,plt,data=dat
	get_data,'X_MSO',data=datXMSO
	get_data,'RHO_MSO',data=datRMSO
	
	Z=dat.y
	if keyword_Set(zlog) then Z=alog10(Z)
	zfn=dat.fn
	zttl=dat.ytitle
	ZT=dat.YN
	XMSO=datXMSO.y
	RHO=datRMSO.y
	yttl=datRMSO.ytitle
	xttl=datXMSO.ytitle
	
	TITLE=ZT+" projection"
	plotFNAME=zfn+"_projection.png"		
	if keyword_set(asym) then begin
	;get_data,'Y_MSO',data=datYMSO
	;get_data,'Z_MSO',data=datZMSO
		asym=asym.toupper()
		get_data,asym+'_MSO',data=datLMSO
		LMSO=datLMSO.y
		sgn=sign(LMSO)
		yttl=yttl.replace('\sqrt','sign('+asym+'_{MSO})\sqrt')
		;YMSO=datYMSO.y
		;ZMSO=datYMSO.y
		RHO*=sgn
		;sgny=sign(YMSO)
		;sgnz=sign(ZMSO)
		plotFNAME=asym+'_Asymmetric_'+zfn+"_projection.png"		
	endif
	if not keyword_set(magrange) then begin
		Zbyt=bytscl(Z)
		Zmax=max(Z)
		Zmin=min(Z)
	endif else begin
		Zmax=max(magrange)
		Zmin=min(magrange)
		Zbyt=bytscl(Z,max=Zmax,min=Zmin)
	endelse	
	p1=scatterplot(XMSO,RHO,symbol='.',/sym_filled,RGB_TABLE=ct,xtitle=xttl,ytitle=yttl,title=TITLE,$
					POSITION=pos1,MAGNITUDE=Zbyt,aspect_ratio=1)
					
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over)
	tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231])
					
	cbb2b2=colorbar(ORIENTATION=0,POSITION=cpos1, $; POSITION=cpos1, $
					title=zttl,RGB_TABLE=rwb,RANGE=[Zmin,Zmax])

	fname=dire+plotFname
	cbb2b2.save,fname,RESOLUTION=600
	p1.close
end
