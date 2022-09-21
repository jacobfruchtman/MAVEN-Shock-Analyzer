pro planeprojector,plane,nX,nY,nZ,X=X,Y=Y,Z=Z,POS=POS

	if keyword_set(POS) then begin
		XX=transose(POS[*,0])
		YY=transpose(POS[*,1])
		ZZ=transpose(POS[*,2])
	endif else if  keyword_set(X) and  keyword_set(Y) and  keyword_set(Z) then begin
		XX=X
		YY=Y
		ZZ=Z
	endif else return
	
	RR=Sqrt(XX^2+YY^2+ZZ^2)
	TH=atan(SQRT(YY^2+ZZ^2),XX)
	PH=atan(ZZ,YY)
	
	if plane.tolower() eq 'x' then begin
		 TH=!pi/2 
		 RR+=XX
	endif else $
	if plane.tolower() eq 'y' then PH=!pi/2*sign(ZZ) else $
	if plane.tolower() eq 'z' then PH=!pi/2*(1-sign(YY)) else return
	
	nX=RR*Cos(TH)
	nY=RR*sin(TH)*COS(PH)
	nZ=RR*sin(TH)*SIN(PH)
end

pro plotplane,plane,normal=normal

	ecc=1.026;.6
	tplot_element,'X_MSO','y',X
	tplot_element,'Y_MSO','y',Y
	tplot_element,'Z_MSO','y',Z
	R_mars=3389.5
	rm=R_mars
	unt='[$R_M$]'
	if not keyword_set(normal) then begin
		rm=1.
		unt='[km]'
	endif
	X/=rm
	Y/=rm
	Z/=rm
	tplot_element,68,'y',dConic
	planeprojector,plane,nX,nY,nZ,X=X,Y=Y,Z=Z
	
	if plane.tolower() eq 'x' then p1=scatterplot(nY,nZ,xtitle='Y_MSO '+unt,ytitle='Z_MSO '+unt,aspect_ratio=1,magn=dConic,RGB=25) else $
	if plane.tolower() eq 'y' then p1=scatterplot(nX,nZ,xtitle='X_MSO '+unt,ytitle='Z_MSO '+unt,aspect_ratio=1,magn=dConic,RGB=25) else $
	if plane.tolower() eq 'z' then p1=scatterplot(nX,nY,xtitle='X_MSO '+unt,ytitle='Y_MSO '+unt,magn=dConic,RGB=25) else return
	
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars/rm,fill_color=[193,68,14],/over)
	;tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231])
	
	if plane.tolower() eq 'x' then begin
		RR=SQRT(nY^2+nZ^2)
		TH=ATAN(nZ,nY)
		medliner,TH,RR,!pi/18.,mlow,mhigh,medR
		medTH=(mlow+mhigh)/2.
		medX=medR*cos(medTH)
		medY=medR*sin(medTH)
		p2=plot(medX,medY,color='black',/over)
	endif
	if plane.tolower() eq 'y' then begin
		varX=nX
		varY=nZ
	
		RR=SQRT(varX^2+varY^2)
		TH=atan(varY,varX) ;; acos(varX/(RR));ATAN(nZ,nY)
		LL=RR+ecc*varX
		medliner,TH,LL,!pi/18.,mlow,mhigh,medL
		medTH=(mlow+mhigh)/2.
		medR=medL/(1+ecc*cos(medTH))
		medX=medR*cos(medTH)
		medY=medR*sin(medTH)
		p2=plot(medX,medY,color='black',/over)
	endif
	if plane.tolower() eq 'z' then begin
		varX=nX
		varY=nY
	
		RR=SQRT(varX^2+varY^2)
		TH=atan(varY,varX) ;acos(varX/(RR));ATAN(nZ,nY)
		LL=RR+ecc*varX
		medliner,TH,LL,!pi/18.,mlow,mhigh,medL
		medTH=(mlow+mhigh)/2.
		medR=medL/(1+ecc*cos(medTH))
		medX=medR*cos(medTH)
		medY=medR*sin(medTH)
		p2=plot(medX,medY,color='black',/over)
	endif
end
