pro correctupstreamdensities


	srctxt="Documents/overmachDays.txt"	

	read_strDATA,H,srctxt,numLines=numPlots
	H = H[UNIQ(H, SORT(H))]
	numPlots=numel(H)
	noCslst=list()
	for i=0,numPlots-1 do begin
		;fname=H[i]
		print,H[i]
		if H[i] eq 'OverVsMach_2014-11-15.tplot' or H[i] eq 'OverVsMach_2014-11-30.tplot' or H[i] eq 'OverVsMach_2014-12-03.tplot' then continue
		corrFactor=1/.7

		;name=(strsplit(((strsplit(H[i],"." , /extract))[0]),'/',/extract))

		tplot_restore,filename="Documents/overVsMachData/"+H[i]
		;del_data,'A*'
		;del_data,'B*'
		;del_data,'C*'
		;del_data,'D*'
		;del_data,'E*'
		;del_data,'F*'
		;del_data,'G*'
		;del_data,'H*'
		;del_data,'I*'
		;del_data,'L*'
		;del_data,'l*'
		;del_data,'m*'
		;del_data,'M*'
		;del_data,'N*'
		;del_data,'P*'
		;del_data,'Q*'
		;del_data,'q*'
		;del_data,'R*'
		;del_data,'r*'
		;del_data,'s*'
		name=(strsplit(((strsplit(H[i],"." , /extract))[0]),'/',/extract))

		get_data,name,data=dat


		if size(dat,/typ) eq 2 then continue

		betas=dat.betas
		ThetaNBs=dat.ANGLE

		N=numel(betas)
		crits=fltarr(N)
		for k=0,N-1 do begin
			ThetaNBn=!pi/2-abs(ThetaNBs[k]-!pi/2)	
			crits[k]=calccritmachnumber(ThetaNBn,betas[k])

		endfor

		dat.crits=crits
		store_data,name[0],data=dat


		

		tplot_save,name[0],filename="Documents/overVsMachData/"+name[0]

		del_data,'*'
		continue


		str_element,dat,'Cs',Cs,success=Cexists
		if ~Cexists then begin
			noCslst.add,H[i]

		;	 continue

		endif
		str_element,dat,'Corrected_density',var,success=densityCorrected

;		if densityCorrected then begin
;			print,H[i]
;			continue

;		endif
		if densityCorrected then begin
		print,'Correcting....'
		;if dat.t[0] lt time_double('2017-07-29/00:00') then corrFactor=1.0

		;{maxlocs:GG,t:shocksUnix,mfms:s[GG],crits:crits[GG],Bmaxs:ymaxs[GG],overDiffs:overs,fracOverDiffs:fracOvers,$
		;B2B1_Fit:B2B1_Fit[GG],ANGLE:AVG[GG],downs:datff.downs[GG],ups:datff.ups[GG],overshoot:datO.y,lat:lat[GG],Ls:Ls[GG],betas:betas,shock0dist:shock0dist,$
	;shockUnix:shocksUnix,shock0Acc:shA[GG],downMeasured:downMeasured,upMeasured:upMeasured,upSTD:upSTD,downSTD:downSTD,pos:POS[GG,*],FF:datff.y,Alfven:datAlfven.y[GG],$
	;uindices:uis[GG,*],dindices:dis[GG,*],Cs:Cs[GG],flowangle:ThetaVB[GG],imins:datff.imins[GG],imaxs:datff.imaxs[GG],N_p:upNp,N_e:upNe}
		beta0=dat.betas_old
		alfven0=dat.Alfven
		alfven=alfven0*sqrt(corrFactor)
		dat.Alfven=alfven
		Cs=dat.Cs
		str_element,dat,'betas',beta0,/add
		

		N=numel(Cs)
		betaNew=fltarr(N)
	
		for j=0,N-1 do betaNew[j]=(Cs[j]/alfven[j])^2

		betas=betaNew
		str_element,dat,'betas_old',betaNew,/add
		ThetaNB=dat.ANGLE
		ThetaNBn=!pi/2-abs(ThetaNB-!pi/2)	
		;fms_old=SQRT(1/2. *(( Cs^2+alfven0^2)+ SQRT( ( Cs^2+alfven0^2)^2 -4 *Cs^2 * alfven0^2*(cos(ThetaNBn))^2 )))
		;fms=SQRT(1/2. *(( Cs^2+alfven^2)+ SQRT( ( Cs^2+alfven^2)^2 -4 *Cs^2 * alfven^2*(cos(ThetaNBn))^2 )))
		MfmsOld=dat.Mfms_old
		mfms=dat.mfms
		;Mfms=MfmsOld
		;for j=0,N-1 do Mfms[j]*=fms_old[j]/fms[j]
		str_element,dat,'mfms',MfmsOld,/add
		dat.Mfms_old=mfms
		
		crits=dat.crits
		dat.crit_old=crits
		;str_element,dat,'crits',critOld,/add
		


		crits=fltarr(N)
		for j=0,N-1 do if finite(betas[j]) ne 1 then betas[j]=0
		for j=0,N-1   do begin
	
			th1=ThetaNBn[j]
	
		
			bta=betas[j]
			MfmsTest=findgen(9001,start=1.,increment=.001)  ;; Going beyond 0.001 precession isn't really necessary
 			;help,MfmsTest
			m2test=fltarr(9001)

			for k=0,9001-1 do begin
			;print,"i=",j
				m2test[k]=m2find(MfmsTest[k],bta,th1)
			endfor

		

			m2loc=-1
			m2closest=min(ABS(m2test),m2loc)
		
			crit=Mfmstest[m2loc]
			;trueCrits[i]=KcalcN(bta,th1)
		
			crits[j]=crit

		endfor
		dat.crits=crits
		str_element,dat,'Corrected_density',0,/add
		dat.N_p/=corrFactor
		print,'Corrected ',H[i]
		;print,H[i]
		;print,name
		;Help,dat


		store_data,name[0],data=dat


		

		tplot_save,name,filename="Documents/overVsMachData/"+name[0]
		endif
		del_data,'*'

	endfor
	foreach el,noCslst do print,el
end
