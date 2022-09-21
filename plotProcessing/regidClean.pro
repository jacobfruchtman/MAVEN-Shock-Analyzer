


function monotize,y;,B ;returns an array with 1 where monotonically increasing and starts returning 0  when that stops being true until things start increasing again


;for example, 
;   the array y=[2,0,4,3,2,3,5,0,3,2,2,3] 
; will return m=[0,0,1,0,0,1,1,0,1,0,0,1], ignoring 0s


	;count=Size(B,/n_el)
	
	N=Size(y,/n_el)

	isinc=0

	;monoB=B*0

	mono=fltarr(N);y*0
	
	for i=1,N-1 do begin  ;may want to modify this so that it makes a distinction between if the previous element is zero or not
		;bi=B[i]
		;bim=B[i-1]
		;if y[i] eq 0 then continue
		;if y[bi]  lt y[bim] then isinc=0
		;if y[bi] gt y[bim] then isinc=1
	
		;if (isinc and (y[bi] ge y[bim])) then mono[bi]=1
		
		if y[i]  lt y[i-1] then isinc=0
		if y[i] gt y[i-1] then isinc=1
	
		if (isinc and (y[i] ge y[i-1])) then mono[i]=1

	endfor
	

	return, mono
end  





pro regidClean, newName=newName
	plt='regid'
	get_data,'regid',data=dat
	if NOT KEYWORD_SET(newName) THEN newName=plt+'_cleaned'
	ys=dat.y
	xs=dat.x
	zs=ys
	;print,xs[1]-xs[0]
	;print,xs[2]-xs[1]
	N=size(dat.x,/n_el)
	lastOn=0
	on=0
	;firstFlip=0
	lastOnFlag=[-1,-1]
	;print,ys[0,*]
	ons=fltarr(N);(dat.x)*0
	if total(ys[0,*] ne 0) ne 0 then begin
		;print,"startOn"
		on =1 
		ons[0]=on
		lastOn=0
		lastOnflag=ys[0,*]
	endif else begin
		 on=0
		ons[0]=0
	endelse
	;print,lastOn
	;print,lastOnflag
	loops=0
	miniloops=0
	microloops=0
	yy=ys[*,0]

	y0=yy
	zz=zs[*,0]
	ww=zz
	yy2=yy
	yi=yy
	yii=yy

	lastOn0=lastOn
	lastOnflag=lastOnflag
	;here we will create a zeroth order solution of setting zeros bewteen equal nonzeros to those non zeros

	for i=1, N-1 do begin

		if ((y0[i] eq 0) or (y0[i-1] ne 0)) then continue  ;if we're on on empty flag or the last one is nonempty, nothing to do
		;;print,"y[i=",i,"]=",y0[i],", y[i-1=",i-1,"]=",y0[i-1]
		for j=1, i do begin
			st=i-j
			if(y0[st] eq 0) then begin
				if st ne 0 then continue else y0[0]=y0[i] ;in case the array starts with 0s, we set the first one to the first nonzero y; if st != 0, we continue on until st==0 or y[st] != 0
			endif
			;;print,"y[st=",st,"]=",y0[st]
			
			if y0[st] eq y0[i] then begin
				y0[st:i] = y0[st]

			
				break
			endif else break
		endfor	

	endfor

	flagNew = {x:dat.x,y:[[y0],[y0]],v:[0,1],spec:1}
	store_data,newName+'0',data=flagNew,limits = {panel_size:0.1, no_interp:1}

	plotName=newName+'_plot0'
	plotflag={x:dat.x,y:y0,ytitle:'flag Height'}
	store_data,plotName,data=plotFlag

	y1=y0;for first order results

	;here we will give a first order solution by recursively setting zeros to adjacent nonzeros
	

	while total(y1 eq 0) ne 0 do begin
		for i=1, N-2 do begin
			if y1[i] ne 0  then continue

			if y1[i-1] ne 0  then y1[i]=y1[i-1]
			if y1[i+1] ne 0  then y1[i]=max([y1[i+1],y1[i]])
		endfor

	endwhile

	flagNew0 = {x:dat.x,y:[[y1],[y1]],v:[0,1],spec:1}
	store_data,newName+'1',data=flagNew0,limits = {panel_size:0.1, no_interp:1}

	plotName=newName+'1_plot'
	plotflag={x:dat.x,y:y1,ytitle:'flag Height'}
	store_data,plotName,data=plotFlag
	;here we will use a 'reversi' algorithm to create a seperate structure which sets sheaths inbetween wind to wind

	FirstWind=-1
	
	

	sheathOnly=1
	lastWind=FirstWind
	nextWind=FirstWind
	lastN=lastWind
	thisFlag=0

	NZ=where(y0 ne 0,count)

	ynz=y0[NZ]

	for i=0, count-1 do begin

		if ynz[i] eq 1 then begin
			FirstWind=i
			break
		endif

	endfor

	for i=FirstWind+1,count-1 do begin
		
		if ((ynz[i] ne 1) or (ynz[i-1] gt 2)) then continue
		j=1
		st=i-j
		while ynz[st] ne 1 do st--

		if total(ynz[st:i] gt 2) eq 0 then y0[NZ[st]:NZ[i]]=1
		

	endfor




	flagReversi = {x:dat.x,y:[[y0],[y0]],v:[0,1],spec:1}
	store_data,newName+"-Reversi0",data=flagReversi,limits = {panel_size:0.1, no_interp:1}

	plotName=newName+'_plot'
	plotReversi={x:dat.x,y:y0,ytitle:'flag Height'}
	store_data,plotName+"_Reversi0",data=plotReversi

	for i=0, N-1 do begin

		if yi[i] eq 1 then begin
			FirstWind=i
			break
		endif

	endfor

	for i=FirstWind+1,N-1 do begin
		
		if ((y1[i] ne 1) or (y1[i-1] gt 2)) then continue
		j=1
		st=i-j
		while y1[st] ne 1 do st--

		if total(y1[st:i] gt 2) eq 0 then y1[st:i]=1
		

	endfor
	
	flagReversi = {x:dat.x,y:[[y1],[y1]],v:[0,1],spec:1}
	store_data,newName+"_Reversi",data=flagReversi,limits = {panel_size:0.1, no_interp:1}
	;store_data,newName,data=flagReversi,limits = {panel_size:0.1, no_interp:1}

	plotName=newName+'_plot'
	plotReversi={x:dat.x,y:y1,ytitle:'flag Height'}
	store_data,plotName+"-reversi",data=plotReversi


	;here we do a first order solution of replacing zeros with linear slope connecting surrounding nonzero elements
	for i=1,N-1 do begin
		if ((yy[i] eq 0) or (yy[i-1] ne 0)) then continue
		

		for j=1, i do begin
			st=i-j
			if(yy[st] eq 0) then begin
				if st ne 0 then continue else yy[0]=yy[i] ;in case the array starts with 0s, we set the first one to the first nonzero y; if st != 0, we continue on until st==0 or y[st] != 0
			endif
			
			;turns out the compsci 1 solution using simple algebra is the simplest implementation: just create a linear slope connecting this nonzero to the last time y[i] was nonzero  (ie, at i==st)
			xstart=xs[st]
			xend=xs[i]
			dx=xend-xstart
			if abs(dx) lt .01 then print,"dx=0 at (j,i)=("+string(j)+","+string(i)+")"
			if abs(dx) lt .01 then break
			ystart=yy[st]
			yend=yy[i]
			
			m=1.0*(yend-ystart)/(xend-xstart)
			b=ystart-m*xstart
			for k=st,i do begin
				yy[k]=round(m*xs[k]+b)
				yi[k]=1.0*(m*xs[k]+b)
			endfor  
			break
		endfor	
	endfor
	;turning everything into integers introduces the problem of 'how do we do thresholds' lets try a different way


	B = WHERE(zz ne 0, count, COMPLEMENT=B_C, NCOMPLEMENT=count_c);new array with locations of nonzero elements
	B2=B
	newY=fltarr(count);B*0;an array which will hold the nonzero elements

	for i=1,count-1 do newY[i]=zz[B[i]];newY holds the nonzeros
 
	;lmax=newY[0]
	;nmax=lmax
	;leq=lmax
	;neq=lmax
	;here we try interpolating between local maxima

	;we start by identifying them
	mono=monotize(zz)

	backmono=reverse(monotize(reverse(zz)))

	flagNewMon = {x:dat.x,y:[[yy],[yy]],v:[0,1],spec:1}
	store_data,newName+'mono',data=flagNewMon,limits = {no_interp:1}

	peak=mono*backmono; this sets local maxima to 1 and everything else to zero, but doesn't eliminate plateaus

	;peaky=zz*peaks
	

	;need to find the medians of the maxima to use as bounds for interpolation

	starton=0
	newon=0
	on=peak[0]
	meds=fltarr(N);yy*0
	
	endfirst=0
	if on then begin ;if y[0] is at a plateau peak, set median to middle (because where else would we put  it?) and move our 'pointer' to the end of the plateau 
		meds[0]=1
		j=0
		while (on) do begin
			on=peak[j]
			j++
		ENDWHILE
		endfirst=j
	endif 
	
	for i=endfirst,N-1 do begin
	
		if peak[i] eq 0 and peak[i-1] eq 0 then continue
		
		if peak[i] ne 0 and peak[i-1] eq 0 then starton=i

		if peak[i] eq 0 and peak[i-1] ne 0 then begin
			 newon=i-1
			
			mn=mean([newon,starton])
			meds[ceil(mn)]=1
			meds[floor(mn)]=1
		endif	

	endfor

	BB = WHERE(meds ne 0, cc, COMPLEMENT=BB_C, NCOMPLEMENT=cc_c);new array with locations of nonzero elements

	for i=1,cc-1  do begin
		bf=BB[i]
		bi=BB[i-1]

		yr=zz[bi:bf]

		xa=xs[bi:bf]
		Bx=WHERE(yr ne 0, cn, COMPLEMENT=BB_C, NCOMPLEMENT=cc_c)
		Br=WHERE(yr ne -1, ct, COMPLEMENT=BB_C, NCOMPLEMENT=cc_c)
		inty=INTERPOL(yr[Bx],Bx,Br)
		zz[bi:bf]=inty
	endfor

	;print,"size(B)="+string(Size(B,/n_el))
	;print,"count="+string(count)
	;for i=1,count-1 do begin
	;	ni=B2[i];want the nth nonzero
	;	;if ((zz[i] eq 0) or (zz[i-1]  eq 0) ) then continue
	;	
	;	if(ww[ni] eq ww[ni-1]) then begin
	;		neq=ww[ni]
	;	endif
	;	
	;	if(newY[i] lt newY[i-1]) then continue

	;	if(newY[i] gt newY[i-1])  then begin
	;		 nmax=newY[i]
	;		;neq=nmax
	;		leq=nmax
	;	endif
	;	
	;	for j=1, i do begin
	;		st=i-j
	;		if(ww[st] eq 0) then begin
	;			if st ne 0 then continue else yy2[0]=ww[i] ;in case the array starts with 0s, we set the first one to the first nonzero y; if st != 0, we continue on until st==0 or y[st] != 0
	;		endif
	;		
	;		;turns out the compsci 1 solution using simple algebra is the simplest implementation: just create a linear slope connecting this nonzero to the last time y[i] was nonzero  (ie, at i==st)
	;		xstart=xs[st]
	;		xend=xs[i]
	;		dx=xend-xstart
	;		if abs(dx) lt .01 then print,"dx=0 at (j,i)=("+string(j)+","+string(i)+")"
	;		if abs(dx) lt .01 then break
	;		ystart=ww[st]
	;		yend=ww[i]
	;		
	;		m=1.0*(yend-ystart)/(xend-xstart)
	;		b=ystart-m*xstart
	;		for k=st,i do begin
	;			yy2[k]=round(m*xs[k]+b)
	;			yii[k]=1.0*(m*xs[k]+b)
	;		endfor  
	;		break
	;	endfor	
	;endfor

	zs[*,0]=yy
	zs[*,1]=yy
	numON=total(ons)

	B = WHERE(ons EQ 1, count, COMPLEMENT=B_C, NCOMPLEMENT=count_c)
	;print,'size(B)'
	;print,size(B)
	
	
	
	;print,'size(B_C)'
	;print,size(B_C)
	;print,"count,count_c,B[count-1],xs[B[count-1]]"
	;print,string(count)+", "+string(count_c)+", "+string(B[count-1])+", "+string(xs[B[count-1]])+", "+string(xs[B[count-1]]-xs[0])
	;;print,count_c
	;YYY=B*0
	;print,xs[1]-xs[0]
	XXX=B+xs[0]

	YYY=zs[B,0]
	;FOR j=0, count-1 DO Begin
		;el=B[j]
		;XXX=xs[el]
		;YYY=ys[B]
	;ENDFOR
	
	xsNormalized=xs-xs[0]
	XXXNormalized=XXX-xs[0]
	
;	ZZZ=Interpol(YYY,XXXNormalized,xsNormalized,/NAN)
	;;print,ys[0,*]
	;print,'max,min,mean'
	;print,max(yy)
	;print,min(yy)
	;print,mean(yy)
	;print,""
	ZZZ=Interpol(YYY,B,xsNormalized,/NAN,/QUADRATIC)
	;print,max(ZZZ,/NAN)
	;print,min(ZZZ,/NAN)
	;print,mean(ZZZ,/NAN)
	ZZZ=FIX(ZZZ)
	;print,""
	;print,max(YYY)
	;print,min(YYY)
	;print,mean(YYY)
	;dat.y=ys
	;yy=ys[*,0]

	;print,'size'
	;print,size(yy)
	;print,size(ZZZ)
	;;print,yy[0:1000]
	;;print,'max,min,mean,stddev,variance,'
	;result = MOMENT(yy)
	;print, 'Mean: ', result[0] & ;print, 'Variance: ', result[1] & $
   ;print, 'Skewness: ', result[2] & ;print, 'Kurtosis: ', result[3]
	;print,"loops="+string(loops)

	;print,"miniloops="+string(miniloops)
	;print,"microloops="+string(microloops)
	;print,'finishedCleaning'

	pltName=plt+'_plot'
	pltflag={x:dat.x,y:ww,ytitle:'flag Height'}
	store_data,pltName,data=pltFlag


	flagNew = {x:dat.x,y:[[yy],[yy]],v:[0,1],spec:1}
	;store_data,newName+'lin',data=flagNew,limits = {panel_size:0.1, no_interp:1}
	store_data,newName,data=flagNew,limits = {panel_size:0.1, no_interp:1}



	;plotName=newName+'_lin_plot'
	plotName=newName+'_plot'
	plotflag={x:dat.x,y:yy,ytitle:'flag Height'}
	store_data,plotName,data=plotFlag

	flagNew3 = {x:dat.x,y:[[zz],[zz]],v:[0,1],spec:1}
	store_data,newName+"3",data=flagNew3,limits = {panel_size:0.1, no_interp:1}

	plotName=newName+'3_plot'
	plotflag3={x:dat.x,y:zz,ytitle:'flag Height'}
	store_data,plotName,data=plotFlag3


	intName=newName+"_int"

	iflagNew = {x:dat.x,y:[[yi],[yi]],v:[0,1],spec:1}
	store_data,intName,data=iflagNew,limits = {panel_size:0.1, no_interp:0}

	iplotName=intName+'_plot'
	iplotflag={x:dat.x,y:yi,ytitle:'flag Height'}
	store_data,iplotName,data=iplotFlag




	flagNew2 = {x:dat.x,y:[[ZZZ],[ZZZ]],v:[0,1],spec:1}


	store_data,newName+'2',data=flagNew2,limits = {panel_size:0.1, no_interp:1}
end
