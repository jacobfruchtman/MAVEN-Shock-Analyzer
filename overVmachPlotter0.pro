
  
function marsSeasonFinder,Ls,lat
	;print,"dls in function"
	;help,Ls
	season=Ls*0
	N=numel(Ls)
	;if (Ls ge 0) doesn't work because seasons are assymetric. Using southern seasons only
	for i=0,N-1 do if 0 then season[i]=floor(Ls[i]/90) mod 4 else season[i]= (floor(Ls[i]/90) +2) mod 4

	;YOU FUCKED UP HERE. YOU IDIOT!
;	for i=0,N-1 do if 1 then season[i]=floor(Ls[i]/90) mod 4 else season[i]= (floor(Ls[i]/90) +2) mod 4

	return,season
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


function normalrefinder, Buvec,thetaBN,Vuvec,thetaVN,POS,N_conic,dop

	Y=POS[1]
	Z=POS[2]
	INV=1/sqrt(Y^2+Z^2)
	a =[[Buvec[0],Buvec[1],Buvec[2]],$
	   [Vuvec[0],Vuvec[1],Vuvec[2]],$
	   [N_conic[2],N_conic[0]*Y*INV,N_conic[0]*Z*INV]]

	b=[SQRT(TOTAL(Buvec^2)) *cos(thetaBN),SQRT(TOTAL(Vuvec^2)) *cos(thetaVN),dop]
	N_SN= LA_LINEAR_EQUATION(a, b)
	return,N_SN
end	



function medianator, dat,quasiperp=quasiperp,par=par
	;print,"===========MEDIANATOR================="

	FM=dat.mfms/dat.crits

			
			
	Bmax=dat.Bmaxs
	overDiffs=dat.overDiffs
	fracOverDiffs=dat.fracOverDiffs
	B2B1_Fit=dat.downups
	ThetaNB=dat.ANGLE
	downs=dat.downs
	ups=dat.ups
	overs=dat.overshoot
	lat=dat.lat
	Ls=dat.Ls
	
	FOD=(Bmax-downs)/downs

	FOU=(Bmax-ups)/ups
	OD=(Bmax-downs)
	OU=(Bmax-ups)
	;overDiffs,fracOverDiffs,B2B1_Fit,(Bmax-downs)/downs,(Bmax-ups)/ups,Bmax-downs,Bmax-ups
	
	;[mx,medODiffs,medFOvDiffs,medFOD,medFOU,medDU,medOD,medOU]
	nullBin=[0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
	bins=list(nullBin)
	aubins=list(nullBin)
	wibins=list(nullBin)
	spbins=list(nullBin)
	subins=list(nullBin)
	SEASONS=marsSeasonFinder(Ls,lat)
	spI=where(SEASONS eq 0,spcount)
	suI=where(SEASONS eq 1,sumcount)
	auI=where(SEASONS eq 2,fallcount)
	wiI=where(SEASONS eq 3,wntrcount)
	;;print,"spcount=",spcount
	quI=where(abs(ThetaNB -!pi/2) lt !pi/4,complement=nHH)

	if keyword_set(quasiperp) then begin

		spI = intersect(spI,quI)
		suI = intersect(suI,quI)
		auI = intersect(auI,quI)
		wiI = intersect(wiI,quI)

	endif

	if keyword_set(par) then begin

		spI = intersect(spI,nHH)
		suI = intersect(suI,nHH)
		auI = intersect(auI,nHH)
		wiI = intersect(wiI,nHH)

	endif

	for i=1, 2*Max(FM) do begin

		mx=i/2.0

		mn=mx-.5

		GGG=where((FM le mx) and (FM gt mn) ,bincount)

		if bincount le 0 then continue
	;overDiffs,fracOverDiffs,B2B1_Fit,(Bmax-downs)/downs,(Bmax-ups)/ups,Bmax-downs,Bmax-ups
		medODiffs=median(/even,overDiffs[GGG])
		medFOvDiffs=median(/even,fracOverDiffs[GGG])
		medDU=median(/even,B2B1_Fit[GGG])			
		medFOD=median(/even,FOD[GGG])
		medFOU=median(/even,FOU[GGG])
		medDU=median(/even,B2B1_Fit[GGG])
		medOD=median(/even,OD[GGG])
		medOU=median(/even,OU[GGG])	
		bin=	[mx,medODiffs,medFOvDiffs,medFOD,medFOU,medDU,medOD,medOU]
		;print,"bin=",bin
		bins.add,bin
	endfor

	for i=1, 2*Max(FM[spI]) do begin

		mx=i/2.0

		mn=mx-.5

		GGG=where((FM le mx) and (FM gt mn) ,bincount)

		if bincount le 0 then continue
		GGG=intersect(GGG,spI)
		if size(GGG,/n_dim) eq 0 then continue
		;overDiffs,fracOverDiffs,B2B1_Fit,(Bmax-downs)/downs,(Bmax-ups)/ups,Bmax-downs,Bmax-ups
		medODiffs=median(/even,overDiffs[GGG])
		medFOvDiffs=median(/even,fracOverDiffs[GGG])
		medDU=median(/even,B2B1_Fit[GGG])			
		medFOD=median(/even,FOD[GGG])
		medFOU=median(/even,FOU[GGG])
		medDU=median(/even,B2B1_Fit[GGG])
		medOD=median(/even,OD[GGG])
		medOU=median(/even,OU[GGG])		
		;;print,"numel(medOU)=",numel(medOU)
		spbin=	[mx,medODiffs,medFOvDiffs,medFOD,medFOU,medDU,medOD,medOU]
		;;print,"spbin=",spbin
		spbins.add,spbin
	endfor
	for i=1, 2*Max(FM[suI]) do begin

		mx=i/2.0

		mn=mx-.5

		GGG=where((FM le mx) and (FM gt mn) ,bincount)

		if bincount le 0 then continue
		GGG=intersect(GGG,suI)
		if size(GGG,/n_dim) eq 0 then continue
		;overDiffs,fracOverDiffs,B2B1_Fit,(Bmax-downs)/downs,(Bmax-ups)/ups,Bmax-downs,Bmax-ups
		medODiffs=median(/even,overDiffs[GGG])
		medFOvDiffs=median(/even,fracOverDiffs[GGG])
		medDU=median(/even,B2B1_Fit[GGG])			
		medFOD=median(/even,FOD[GGG])
		medFOU=median(/even,FOU[GGG])
		medDU=median(/even,B2B1_Fit[GGG])
		medOD=median(/even,OD[GGG])
		medOU=median(/even,OU[GGG])	
		bin=	[mx,medODiffs,medFOvDiffs,medFOD,medFOU,medDU,medOD,medOU]
		;;print,"subin=",bin	
		subins.add,bin
	endfor
	for i=1, 2*Max(FM[auI]) do begin

		mx=i/2.0

		mn=mx-.5

		GGG=where((FM le mx) and (FM gt mn) ,bincount)

		if bincount le 0 then continue
		GGG=intersect(GGG,auI)
		;;print,GGG
		if size(GGG,/n_dim) eq 0 then continue
		;overDiffs,fracOverDiffs,B2B1_Fit,(Bmax-downs)/downs,(Bmax-ups)/ups,Bmax-downs,Bmax-ups
		medODiffs=median(/even,overDiffs[GGG])
		medFOvDiffs=median(/even,fracOverDiffs[GGG])
		medDU=median(/even,B2B1_Fit[GGG])			
		medFOD=median(/even,FOD[GGG])
		medFOU=median(/even,FOU[GGG])
		medDU=median(/even,B2B1_Fit[GGG])
		medOD=median(/even,OD[GGG])
		medOU=median(/even,OU[GGG])	
		aubin=	[mx,medODiffs,medFOvDiffs,medFOD,medFOU,medDU,medOD,medOU]
		;;print,"aubin=",aubin	
		aubins.add,aubin
	endfor

	for i=1, 2*Max(FM[wiI]) do begin

		mx=i/2.0

		mn=mx-.5

		GGG=where((FM le mx) and (FM gt mn) ,bincount)

		if bincount le 0 then continue
		GGG=intersect(GGG,wiI)
		if size(GGG,/n_dim) eq 0 then continue
		;overDiffs,fracOverDiffs,B2B1_Fit,(Bmax-downs)/downs,(Bmax-ups)/ups,Bmax-downs,Bmax-ups
		medODiffs=median(/even,overDiffs[GGG])
		medFOvDiffs=median(/even,fracOverDiffs[GGG])
		medDU=median(/even,B2B1_Fit[GGG])			
		medFOD=median(/even,FOD[GGG])
		medFOU=median(/even,FOU[GGG])
		medDU=median(/even,B2B1_Fit[GGG])
		medOD=median(/even,OD[GGG])
		medOU=median(/even,OU[GGG])
		bin=	[mx,medODiffs,medFOvDiffs,medFOD,medFOU,medDU,medOD,medOU]
		;;print,"wibin=",bin		
		wibins.add,bin
	endfor
	
	;;print,"spbins=",spbins

	;;print,"subins=",subins

	;;print,"aubins=",aubins

	;;print,"wibins=",wibins
	;print,"===========END MEDIANATOR================="
	return,list(bins.toarray(),spbins.toarray(),subins.toarray(),aubins.toarray(),wibins.toarray())


end

pro datalistreader,datalist,interlist,filename,numParameters=numParameters,numIntervals=numIntervals
OPENR,1,filename
;IF N_ELEMENTS(cols) LE 0 THEN cols=1 ;Default value for cols
cols=1
rows=500.    ;Default value for rows
datalist=STRARR(3,rows) ;A big array to hold the data
interlist=STRARR(3,rows) ;A big array to hold the data
S=STRARR(cols)      ;A small array to read a line
ON_IOERROR,ers     ;Jump to statement ers when I/O error is detected
n=0 ; Create a counter
m=0
READF,1,S    ;Read a line of data
WHILE n LT rows DO BEGIN
    READF,1,S    ;Read a line of data
    if S.startswith('=') then break
    if NOT S.startswith(';') then begin
		;S=(S.split(';'))[0]
		datalist[*,m]=strsplit(S,',',/extract)     ;Store it in H
		m=m+1
    endif
    n=n+1        ;Increment the counter
ENDWHILE          ;End of while loop
print,m
datalist=datalist[*,0:m-1];n-1]
numParameters=m
m=0
WHILE n LT rows DO BEGIN
    READF,1,S    ;Read a line of data
    if S.startswith('=') then break
    if NOT S.startswith(';') then begin
		;S=(S.split(';'))[0]
		interlist[*,m]=strsplit(S,',',/extract)     ;Store it in H
		m=m+1
    endif
    n=n+1        ;Increment the counter
ENDWHILE          ;End of while loop
ers: CLOSE,1         ;Jump to this statement when an end of file is detected
interlist=interlist[*,0:m-1];n-1]
numIntervals=m
END


function secondBinner, binList
	;print,"=====SECONDBINNER============="

	datapoints=list()

	foreach el,binList do begin

		NN=numel(el[*,0])
		;print,NN
		for i=0,NN-1 do begin
			;print,"i=",i	
		
			column=transpose(el[i,*])
			;print,"column=",column
			datapoints.add,column
		endfor
	endforeach

	binArr=datapoints.toarray()
	;print,"size(binArr)=", SIZE(binArr)
	;print,"size(binArr,/n_dim)=", SIZE(binArr,/n_dim)

	;print, numel(binArr[*,0])
	;print, numel(binArr[0,*])




	maxFM=-1
	;print,"binArr:"
	;print,binArr
	;print,"binArr[*,0]:"
	;print,binArr[*,0]
	;print,"binArr[*,1]:"
	;print,binArr[*,1]
	;print,"binArr[1,*]:"
	;print,binArr[1,*]
	maxFM=max(binArr[*,0])
	bins=list()

	FMcolumn=list()
	OvDfcolumn=list()
	FOvDfcolumn=list()
	DUcolumn=list()
	FODcolumn=list()
	FOUcolumn=list()
	ODcolumn=list()
	OUcolumn=list()

	FMbin=list()
	OvDf=list()
	FOvDf=list()
	DUbin=list()
	FODbin=list()
	FOUbin=list()
	ODbin=list()
	OUbin=list()

	;print,"maxFM*2=",maxFM*2

	for i=1,maxFM*2 do begin
		j=i/2.0
		
		;overDiffs,fracOverDiffs,B2B1_Fit,(Bmax-downs)/downs,(Bmax-ups)/ups,Bmax-downs,Bmax-ups
		ThisOvDf=list()
		ThisFOvDf=list()
		ThisDUbin=list()
		ThisFODbin=list()
		ThisFOUbin=list()
		ThisODbin=list()
		ThisOUbin=list()
		G=where(binArr[*,0] eq j,gcount)
		if gcount le 0 then continue

		FMbin.add,j
		
		;list2.add, median(/even,binArr[G,2])		
;		foreach el,binList do begin
;			if numel(el) eq 0 then continue
;			G=WHERE(el[0] eq j,gcount)
;			if gcount le 0 then continue
;
		;	;k=G[0]
		;	if (numel(FMbin) eq 0) or  (where(FMbin.toarray() eq j) eq -1) then FMbin.add,j
		;	;overDiffs,fracOverDiffs,B2B1_Fit,(Bmax-downs)/downs,(Bmax-ups)/ups,Bmax-downs,Bmax-ups
		;	;print,"numel(el)=",numel(el)
		;	ThisOvDf.add,el[1]
		;	ThisFOvDf.add,el[2]
		;	ThisDUbin.add,el[3]
		;	ThisFODbin.add,el[4]
		;	ThisFOUbin.add,el[5]
		;	ThisODbin.add,el[6]
		;	ThisOUbin.add,el[7]

		;endforeach
		;if numel(ThisFODbin) eq 0 then continue
		

		OvDf.add,median(/even,binArr[G,1])
		FOvDf.add,median(/even,binArr[G,2])
		DU=median(/even,binArr[G,3])
		FOD=median(/even,binArr[G,4])
		FOU=median(/even,binArr[G,5])
		
		OD=median(/even,binArr[G,6])
		OU=median(/even,binArr[G,7])


		
	
		DUbin.add,DU
		FODbin.add,FOD
		FOUbin.add,FOU
		ODbin.add,OD
		OUbin.add,OU
		
		;ThisOvDf=ThisOvDf.toarray()
		;ThisFOvDf=ThisFOvDf.toarray()
		;ThisDUbin=ThisDUbin.toarray()

		;ThisFODbin=ThisFODbin.toarray()
		;ThisFOUbin=ThisFOUbin.toarray()
		;ThisODbin=ThisODbin.toarray()
		;ThisOUbin=ThisOUbin.toarray()


		;bin=[j,FOvDf,OvDf,DU,FOD,FOU,OD,OU]
		;;print,"bin=",bin
		;bins.add,bin
	endfor
	FMbin=FMbin.toarray()
	OvDf=OvDf.toarray()
	FOvDf=FOvDf.toarray()
	DUbin=DUbin.toarray()
	;print,"FODbin:",FODbin
	FODbin=FODbin.toarray()
	FOUbin=FOUbin.toarray()
	ODbin=ODbin.toarray()
	OUbin=OUbin.toarray()	
	bins=list(FMbin,OvDf,FOvDf,DUbin,FODbin,FOUbin,ODbin,OUbin)
;plotTitles=["overshoot difference","fractional overshoot difference","fitted downstream/upstream B's",$ "fractional overVsDown", "fractional overVsUp","over-Down","over-Up vs "]+"M_fms/M_crit"

	;print,bins.toarray()
	;;print,"bins[0]=j=",bins[0]
	;;print,"bins[1]=",bins[1]
	;bins=transpose(bins.toarray())
	;print,"=====END OF SECONDBINNER============="
	RETURN,bins

end


pro overVmachPlotter,islast=islast,currtime=currtime,REMOVENEGATIVES=REMOVENEGATIVES,RemoveBigFracM=RemoveBigFracM,$
					maxAllowedFM=maxAllowedFM,load2=load2,mindop=mindop,maxdist=maxdist,$
					maxUpDiff=maxUpDiff,maxB2B1fracdiff=maxB2B1fracdiff,betamaxallowed=betamaxallowed,$
					maxMagJump=maxMagJump,keepPathologies=keepPathologies,betaparttype=betaparttype,$
					minMfms=minMfms,manualPlotting=manualPlotting,nofixing=nofixing,maxover=maxover
	clockOMP=TIC("overVmachPlotter")
	dire="Documents/Plots/CombinedPlots/"
	FILE_MKDIR,dire
	doClose=1

	if keyword_set(isLast) then doClose=0

	allowfix=1
	if keyword_set(nofixing) then allowfix=0
	if not keyword_set(betaparttype) then betaparttype=2

	;;;;beta partition types:
	;;;;;; 1: beta>1 or beta<=1
	;;;;;; 2: beta>2 or beta<=2
	;;;;;; 3: beta>=upperquartile,beta<=lowerquartile


	firstrun=0
	get_data,'Mfms',data=dat
	if size(dat,/typ) eq 2 then firstrun=1

	;;;;;;m<0: beta>= (sort(beta))[(1+1./m)*N-1],beta< (sort(beta))[(-1./m)*N-1] <--TO BE IMPLEMENTED

	killBigFM=0
	killNegOverDiff=0
	if keyword_Set(REMOVENEGATIVES) then killNegOverDiff=1
	if keyword_Set(RemoveBigFracM) then killBigFM=1
	if not keyword_set(maxAllowedFM) then maxAllowedFM=10 else killBigFM=1
	;if not keyword_set(maxUpDev) then maxUpDev=5
	if not keyword_set(maxUpDiff) then maxUpDiff=1.3
	if not keyword_set(maxB2B1fracdiff) then maxB2B1fracdiff=.6
	if not keyword_set(maxMagJump) then maxMagJump=7.
	if not keyword_set(mindop) then mindop=.5;.4
	if not keyword_set(minMfms) then minMfms=1.;0.;1.0
	if not keyword_set(betamaxallowed) then betamaxallowed=20.;9.;20.
	if not keyword_set(maxover) then maxover=5.
	;first get names of tplot files from txt file

	;H=0
	corrText=""
	biglistnum=""
	if keyword_set(load2) then biglistnum="2"
	srctxt="Documents/overmachDays.txt"


	if keyword_set(load2) then srctxt="Documents/overmachDays2.txt"
	if killBigFM or killNegOverDiff then corrText="_WithoutPathologies"
	read_strDATA,H,srctxt,numLines=numPlots
	H = H[UNIQ(H, SORT(H))]
	numPlots=numel(H)

	;numDates=numel(dates)
	
	manualList="Documents/savedPoints.txt"
	print,manualList
	read_strDATA2,shocklist,manualList,numLines=numLines2
;	read_strDATA2,shocklist,"Documents/savedPoints_withoutPathologies.txt",numLines=numLines2
	shocklistanom=strarr(numLines2)

	for i=0,numLines2-1 do shocklistanom[i]=(strsplit(shocklist[i],";",/extract))[0] 

	shocklist2=shocklistanom
	instructlist=strarr(numLines2)
	;for i=0,numLines2-1 do if shocklist2[i].startswith('2') then shocklist2[i]='T@'+shocklist2[i]
	for i=0,numLines2-1 do begin
		if shocklist2[i].startswith('2') then instructlist[i]='G' else begin
		;if ~shocklist2[i].startswith('2') then begin
			var=(strsplit(shocklist2[i],"@",/extract)) ;shocklist2[i].remove(0,0)
			
			shocklist2[i]=var[1]
			instructlist[i]=var[0]
		endelse	
	endfor
	;in for loop, restore these files and create an array of data structures

	tmins=list()
	tmaxs=list()
	datasets=list()
	seasons=list()

	;{maxlocs:GG,t:shocksUnix,mfms:s[GG],crits:crits[GG],Bmaxs:ymaxs[GG],overDiffs:overs,fracOverDiffs:fracOvers,$
		;B2B1_Fit:B2B1_Fit[GG],ANGLE:AVG[GG],downs:datff.downs[GG],ups:datff.ups[GG],overshoot:datO.y,lat:lat[GG],Ls:Ls[GG],betas:betas,shock0dist:shock0dist,$
	;shockUnix:shocksUnix,shock0Acc:shA[GG],downMeasured:downMeasured,upMeasured:upMeasured,upSTD:upSTD,downSTD:downSTD,pos:POS[GG,*],FF:datff.y,Alfven:datAlfven.y[GG],$
	;uindices:uis[GG,*],dindices:dis[GG,*],Cs:Cs[GG],flowangle:ThetaVB[GG],imins:datff.imins[GG],imaxs:datff.imaxs[GG],N_p:upNp,N_e:upNe}
	locsLst=list()	
	tLst=list()
	tjLst=list()
	tjdLst=list()
	tdays=list()
	t0dayLst=list()

	totperday=list()
	perdaylst=list()
	MfmsLst=list()
	critList=list()
	BmaxLst=list()
	overDiffsLst=list()
	fracOverDiffsLst=list()
	B2B1_FitLst=list()
	angleLst=list()
	downlst=list()
	uplst=list()
	downstdlst=list()
	upstdlst=list()
	thetaNVlst=list()
	Nplst=list()
	Nelst=list()	

	POSlst=list()

	upsPOSlst=list()
	upmPOSlst=list()
	upePOSlst=list()
	downsPOSlst=list()
	downmPOSlst=list()
	downePOSlst=list()

	usDXlst=list()
	umDXlst=list()
	ueDXlst=list()
	dsDXlst=list()
	dmDXlst=list()
	deDXlst=list()
	usALTlst=list()
	umALTlst=list()
	ueALTlst=list()
	dsALTlst=list()
	dmALTlst=list()
	deALTlst=list()
	

	

	altlst=list()
	alfvenlst=list()
	Soundlst=list()
	;soldistlst=list()

	downMlst=list()
	upMlst=list()



	overLst=list()
	latLst=list()
	LsLst=list()
	betas=list()
	shock0dist=list()
	shock0Acc=list()

	betaIionLst=list()
	TionLst=list()
	TelecLst=list()
	VuvecfLst=list()
	VuvecLst=list()
	vdvecLst=list()
	BuvecLst=list()
	BdvecLst=list()
	NconicsLst=list()
	POSconicsLst=list()
	N_AVG=list()

	pltpos=[0.15,0.25,0.95,0.8]
	cbpos=[0.30,0.07,0.70,0.15]
	;allbins=list()
	;qallbins=list()
	;pallbins=list()
	;allspbins=list()
	;allsubins=list()
	;allaubins=list()
	;allwibins=list()


;	bins=list()
;	spbins=list()
;;	subins=list()
;	aubins=list()
;	wibins=list()

;	qallspbins=list()
;	qallsubins=list()
;	qallaubins=list()
;	qallwibins=list()

;	qbins=list()
;	qspbins=list()
;	qsubins=list()
;	qaubins=list()
;	qwibins=list()

;	pallspbins=list()
;	pallsubins=list()
;	pallaubins=list()
;	pallwibins=list()

;	pbins=list()
;	pspbins=list()
;	psubins=list()
;	paubins=list()
;	pwibins=list()

	maxFM=0.
	maxFMplot=0.
	maxFMplotDate=""
	maxViableFM=0.

	maxViableBeta=0.
	maxBetaPlot=""

	minDiff=0.
	minDiffplot=0.
	minDiffplotDate=""

	maxFUPDiff=0.
	maxFUPfm=0.
	maxFUPDiffplot=0.
	maxFUPDiffplotDate=""
	;maxUPDiff=0.5
	maxBUPDiff=0.
	maxUPDiffplot=0.
	maxUPDiffplotDate=""
	maxUPfm=0.

	maxDU=0.
	maxDUplot=0.
	maxDUplotDate=""

	goodPoints=0
	points=0

	upintervaloffsetlst=list()
	downintervaloffsetlst=list()

	baddists=0
	baddots=0
	badFMs=0
	badBmax=0
	badMeasure=0
	badManual=0
	badLS=0
	badbeta=0
	badB2B1=0
	badMfms=0
	badDens=0
	badNANs=0
	badInstab=0
	badSlope=0
	inOvers=0
	noOvers=0
	wrongBmaxs=0
	nonlocals=0
	BadMultis=0
	badCrusts=0
	badN_SN=0
	badN_SNlst=list()
	whereBadSN=list()
	missedanomolies=0
	missedmanual=0
	clockld = TIC('load')

	badthetaVals=list()
	badthetaManVals=list()
	badthetaAutoVals=list()

	ssnbound=list()
	ssn=-1
	tv=-1
	;tjv=-1
	ssnchanges=0
	wherebaddist=list()

	R_mars=3389.5
	semimajor=227.956 *10.^6.
	ecc=0.0935
	semilat=semimajor*(1-ecc^2)

	noconiclst=list()
	numnoconic=0

	for i=0,numPlots-1 do begin
		;
		error_status=0
		if i gt 0 and H[i] eq H[i-1] then continue

		;tplot_restore,filename="Documents/overVsMachData2/"+H[i]
	

		;name=(strsplit(((strsplit(H[i],"." , /extract))[0]),'/',/extract))
		;get_data,name,data=dat
		name=(strsplit(((strsplit(H[i],"." , /extract))[0]),'/',/extract))
		date=name.remove(0,10)
		t00=time_double(date)
		;if t00 lt time_double('2018-09-16') then continue
		;if t00 gt time_double('2018-09-16')+1 then break
		print,H[i]
		get_data,name,data=dat
		;catch,error_status
		;	if error_status ne 0 then begin
		;	print,i
		;	print,H[i],"doesn't exist ERROR"

		;		catch,/cancel
				;continue
		;endif
		if size(dat,/typ) eq 2 then begin
			
			tplot_restore,filename="Documents/overVsMachData"+biglistnum+"/"+H[i]
	
			
			name=(strsplit(((strsplit(H[i],"." , /extract))[0]),'/',/extract))

			get_data,name,data=dat

			if size(dat,/typ) eq 2 then continue

		endif
		;if size(dat,/typ) eq 2 then continue	
		datasets.add,dat
		date=name.remove(0,10)
		t00=time_double(date)
		;help,dat

		if dat.t[0] ge time_double('2019-11-16/00:00:00') or H[i] eq 'OverVsMach_2019-11-16.tplot' or t00 ge time_double('2019-11-16/00:00:00') then break

		;newBins=medianator(dat)
		;allbins.add,newBins[0]
		;print,transpose(newBins[0])
		;print,(newBins[0])[0,*]
		;z=newBins[10]
		;if numel(newBins[1]) gt 0 then  allspbins.add,newBins[1]
		;;print,"newBins=",newBins
		;;print,newBins[10]
		;if numel(newBins[2]) gt 0 then  allsubins.add,newBins[2]
		;if numel(newBins[3]) gt 0 then allaubins.add,newBins[3]
		;if numel(newBins[4]) gt 0 then allwibins.add,newBins[4]

		;qnewBins=medianator(dat,/qua)
		
		;qallbins.add,qnewBins[0]
		;if numel(qnewBins[1]) gt 0 then  qallspbins.add,qnewBins[1]		
		;if numel(qnewBins[2]) gt 0 then qallsubins.add,qnewBins[2]
		;if numel(newBins[3]) gt 0 then qallaubins.add,qnewBins[3]
		;if numel(newBins[4]) gt 0 then qallwibins.add,qnewBins[4]




		;pnewBins=medianator(dat,/par)
		
		;pallbins.add,pnewBins[0]
		;if numel(pnewBins[1]) gt 0 then  pallspbins.add,pnewBins[1]		
		;if numel(pnewBins[2]) gt 0 then pallsubins.add,pnewBins[2]
		;if numel(newBins[3]) gt 0 then pallaubins.add,pnewBins[3]
		;if numel(newBins[4]) gt 0 then pallwibins.add,pnewBins[4]
		;print,"filename=",H[i]
		;foreach el, newBins do ;print,"el=", el
		dayCrosses=0
		tday=-1
		if numel(dat.t) ne numel(dat.shock0dist) then begin
			print,"BAD dataset at ",H[i]
			wherebaddist.add,H[i]+" ## numel(dat.t),numel(dat.shock0dist)="+strtrim(numel(dat.t),2)+","+strtrim(numel(dat.shock0dist),2)
			continue

		endif

		str_element,dat,'N_SN',var,success=NSNexists

		str_element,dat,'pos_conic',var,success=PSexists

		;catch,error_staus

		if 0 and error_status ne 0 then begin
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			help,dat
			del_data,'*'
			continue	

		endif

		str_element,dat,'Cs',var,success=Cexists
		if ~Cexists then begin
			Cspeed=Sqrt(dat.betas) * dat.Alfven
			str_element,dat,'Cs',Cspeed,/add
		endif
		str_element,dat,'mfms',var,success=Mexists
		if ~Mexists then begin
			print,H[i]
			help,dat
		endif

		str_element,dat,'T_ion',var,success=Texists
		if ~Texists then begin
			print,time_string(dat.t[0])
			print,H[i]
			print,t00
			print,time_double('2019-11-16/00:00:00')
			print,t00 ge time_double('2019-11-16/00:00:00')
			;help,dat
			del_data,name
			continue;break
			;return
		endif

		if 0 and dat.t[0] lt time_double('2018-09-16') then begin
			print,"goodpoint/points=",goodPoints,"/",points
					print,"[baddists,baddots,badFMs,badBmax,BadManual,badLS,badMeasure,missedAnomolies]:"
			print,[baddists,baddots,badFMs,badBmax,BadManual,badLS,badMeasure,missedanomolies]
			del_data,name
			continue;break
		endif	
		if 0 and numel(dat.t) eq 1 and H[i] ne 'OverVsMach_2015-07-14.tplot' then begin
			print,H[i]
			;return
		endif

		if ~PSexists then begin
			print,H[i]
			PRINT,numnoconic
			help,dat
			noconiclst.add,H[i]
			numnoconic++
			del_data,name
			continue;break

		endif
	
		

		;if   t00 gt time_double('2018-09-16')+1 then break
		for j=0,numel(dat.t)-1 do begin
			badNAN=0
			bdmanpoint=0
			badPoint=0
			points++


			t=dat.t[j];shockUnix[j];dat.t[j]
			if j gt 0 and numel(tLst) gt 1 and total(tLst eq t) gt 0 then continue
			xt=x2Greg(t,/strformat)
			;print,xt
			Ls=dat.Ls[j]
			;print,j
			;print,numel(dat.t)
			;print,size(dat.Ls,/n_el)
			;print,size(Ls,/typ)
			;print,Ls

			;lll=dat.Ls
			;seasons.add,marsSeasonFinder(lll,dat.lat)
			;sss=marsSeasonFinder(lll,dat.lat)
			listloc=(where(xt eq shocklist2))[0]
			if   listloc eq -1 then begin
				badManual++
				badPoint=1
				continue
			endif

			if 0 and (dat.N_p[j] lt 0 or dat.N_p[j] gt 1000) and finite(dat.N_e[j]) eq 1 then begin
				print,xt
				;return
				continue
			endif
			anomtext=shocklistanom[listloc]
			instruct=instructlist[listloc]
			print,instruct

			N_SN=dat.N_SN[j,*]

			nurm=sqrt(N_SN[0]^2+N_SN[1]^2+N_SN[2]^2)
			if nurm ne 1. then begin

				if  abs(1.-nurm) lt .00001 then N_SN=N_SN/nurm else begin
					print,"abs(1-nurm(N_SN))=",abs(1.-nurm)
					print,"abs(1-sqrt(total(N_SN^2)))=",abs(1-sqrt(total(N_SN^2)))
					
					print,"N_SN=",N_SN

					print,xt
					

				
				badN_SN++
				badN_SNlst.add,N_SN
				whereBadSN.add,xt
				if nurm lt .7 then begin
					print,numel(dat.t)
					print,j
					print,xt
					;return

				endif

				continue
				endelse
			endif

			if instruct.StartsWith('N')  then begin

				badNANs++
				;badNAN=1
				badPoint=1
				if finite(dat.ANGLE[j]) ne 1 then begin
					print,time_string(t)
					continue
				endif
				th=90-abs(dat.ANGLE[j]*180/!pi-90)
				
				badthetaVals.add,th
				badthetaAutoVals.add,th
				continue

			endif

			if ~allowfix and total(instruct.CharAt(0) eq ['U','D','M']) ne 0 then begin
				print,'no fix'
				if instruct.strlen() eq 2 then begin
					print,'instruct=instruct.CharAt[1]'
					print,instruct
					instruct=instruct.CharAt(1)
					if instruct eq 'B' then begin
						badManual++
						badPoint=1
						continue
					endif
				endif else begin
						badManual++
						badPoint=1
						continue
				endelse

			endif

			if instruct.StartsWith('R')  then begin
				badManual++
				badMultis++
				;badNAN=1
				badPoint=1
				th=90-abs(dat.ANGLE[j]*180/!pi-90)
				
				badthetaVals.add,th
				if bdmanpoint then badthetaManVals.add, th else badthetaAutoVals.add,th
				continue

			endif

			if instruct.StartsWith('C')  then begin
				badManual++
				badCrusts++
				;badNAN=1
				badPoint=1
				th=90-abs(dat.ANGLE[j]*180/!pi-90)
				
				badthetaVals.add,th
				if bdmanpoint then badthetaManVals.add, th else badthetaAutoVals.add,th
				continue

			endif


			if instruct.StartsWith('I')  then begin
				badManual++
				badInstab++
				;badNAN=1
				badPoint=1
				th=90-abs(dat.ANGLE[j]*180/!pi-90)
				
				badthetaVals.add,th
				if bdmanpoint then badthetaManVals.add, th else badthetaAutoVals.add,th
				continue

			endif


			if instruct.StartsWith('V')  then begin
				badManual++
				badSlope++
				;badNAN=1
				badPoint=1
				th=90-abs(dat.ANGLE[j]*180/!pi-90)
				
				badthetaVals.add,th
				if bdmanpoint then badthetaManVals.add, th else badthetaAutoVals.add,th
				continue

			endif
			if instruct.StartsWith('O')  then begin
				badManual++
				inOvers++
				;badNAN=1
				badPoint=1
				th=90-abs(dat.ANGLE[j]*180/!pi-90)
				
				badthetaVals.add,th
				if bdmanpoint then badthetaManVals.add, th else badthetaAutoVals.add,th
				continue

			endif
			if instruct.StartsWith('Q')  then begin
				badManual++
				noOvers++
				;badNAN=1
				badPoint=1
				th=90-abs(dat.ANGLE[j]*180/!pi-90)
				
				badthetaVals.add,th
				if bdmanpoint then badthetaManVals.add, th else badthetaAutoVals.add,th
				continue

			endif
			if instruct.StartsWith('L')  then begin
				badManual++
				nonlocals++
				;badNAN=1
				badPoint=1
				th=90-abs(dat.ANGLE[j]*180/!pi-90)
				
				badthetaVals.add,th
				if bdmanpoint then badthetaManVals.add, th else badthetaAutoVals.add,th
				continue

			endif
			;uptime=t00+mean(dat.uindices[j,*])
			;upDT=abs(uptime-t)
			;downtime=t00+mean(dat.dindices[j,*])
			;downDT=abs(downtime-t)


			mfms=dat.mfms[j]
			theta=dat.ANGLE[j]
			

			Bmax=dat.Bmaxs[j]
			down=dat.downs[j]
			up=dat.ups[j]
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
			crit=dat.crits[j];calccritmachnumber(theta,bta);dat.crits[j]
			FM=mfms/crit
			N_e=dat.N_e[j]
			N_p=dat.N_p[j]
			Tion=dat.T_ion[j]
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
				;return
			endif


			if mfms lt minMfms then begin
				badMfms++
				print,"badMfms,",mfms,xt
				;print,"badFMs"
				badPoint=1;continue
			endif
			T_rat=1.0 * Telec/Tion
			if finite(N_e) ne 1 or N_e le 0 or N_p le 0 or N_e /N_p lt 1/15. or N_e /N_p gt 15. or T_rat lt 0  then begin
				;badMfms++
				;print,"badMfms,",mfms,xt
				;print,"badFMs"
				badDens++
				badPoint=1;continue
			endif

			if maxFM lt FM then begin
				maxFM=FM
				maxFMplot=H[i]
				maxFMplotDate=x2Greg(dat.t[j],/strformat)
			endif
			if minDiff gt (Bmax-down)/down  then begin
				minDiff=(Bmax-down)/down
				minDiffplot=H[i]
				minDiffplotDate=x2Greg(dat.t[j],/strformat)
			endif
	
			if maxFUPDiff lt (Bmax-down)/down and finite((Bmax-down)/down) then begin
				maxFUPDiff=(Bmax-down)/down
				maxFUPDiffplot=H[i]
				maxFUPDiffplotDate=x2Greg(dat.t[j],/strformat)
				maxFUPfm=FM
			endif
			if 0 and maxBUPDiff lt (Bmax-up)  then begin
				maxBUPDiff=(Bmax-up)
				maxUPDiffplot=H[i]
				maxUPDiffplotDate=x2Greg(dat.t[j],/strformat)
				maxUPfm=FM
			endif
			if dist0 gt 2500 then begin

				;print,"dist0=",dist0
				baddists++
				badPoint=1
				 ;continue
			endif

			if dop lt mindop then begin
				;print,"mindop=",dop
				baddots++
				badPoint=1;continue
			endif
			if bta gt betamaxallowed or Beta_ion gt betamaxallowed/2.  then begin
				badbeta++
				badPoint=1
				print,xt
				print,bta
				;PRINT,Ls
				

			endif
			if finite(bta) ne 1 or bta le 0. then begin
				badbeta++
				badPoint=1
				print,xt
				print,bta
				;PRINT,Ls
				

			endif
			if size(Ls,/typ) eq 0 then begin
				;print,xt
				;print,H[i]
				badLS++
				;print,"Bad Ls"
				badPoint=1
	
			endif
			if finite(dat.ANGLE[j]) ne 1 then badPoint=1
			if FM le 0.0 then begin
				badFMs++
				;print,"badFMs"
				badPoint=1;continue
			endif
			if (killBigFM) and (FM ge maxAllowedFM) then begin
				badFMs++
				;print,"badFMs"
				badPoint=1;continue
			endif

			if instruct.StartsWith('M') and badPoint eq 0 then begin
				down=downM
				up=upM

			endif
			if instruct.StartsWith('U') and badPoint eq 0 then begin

				up=upM

			endif

			if instruct.StartsWith('D') and badPoint eq 0 then begin

				down=downM

			endif
			downup=down/up
			if ((downup ge maxMagJump or downM/upM ge maxMagJump or (downup lt 1.5 and down-up lt 1.3)) and ~instruct.StartsWith('Y')) OR downup eq 0  then begin
				badPoint=1
				badB2B1++


			endif

	

			;if upM ge downM or fracdiff(dum,downup) gt maxB2B1fracdiff or fracdiff(downM,down)  gt .3 or duRatio gt 3 or duRatio lt .33 or abs(down-downM) gt 3*dstd then begin;
			if ((abs(upM-up) gt max([3*ustd,maxUpDiff]) or fracdiff(up,upM)  gt .25 )  or upM ge downM or fracdiff(dum,downup) gt maxB2B1fracdiff or fracdiff(downM,down)  gt .3 or duRatio gt 3 or duRatio lt .33 or abs(down-downM) gt 3*dstd) and ~instruct.StartsWith('Y') then begin;or abs(downM-down)/mean([downM,down])  gt .3 or duRatio gt 3 or duRatio lt .33 or fracdiff(dum,downup) gt maxB2B1fracdiff then begin
				badMeasure++
				;print,"bad measure"
				;print,H[i]
				badPoint=1
			endif 

			

			if ((killNegOverDiff) and ((Bmax lt down ) or (Bmax lt up) or (Bmax lt downM) ) or (Bmax-down)/down gt maxover ) and ~instruct.StartsWith('Y')  then begin
				badBmax++
				;print,"badBmax"
				badPoint=1;continue
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
	

			listloc=(where(xt eq shocklist2))[0]
			if  0 and listloc eq -1  and not keyword_set(keepPathologies) then begin
				badManual++
				if badPoint eq 0 then missedmanual++
				badPoint=1
				bdmanpoint=1
				;continue
			endif


			if instruct.StartsWith('A') and badPoint eq 0   then begin
				badPoint=1
				missedanomolies++

			endif

			tj=x2jul(t)
			if tj le 0 then begin
				print,t
				print,tj,"<=0"
				print,H[i]
				print,xt

				return
				badPoint=1
			endif

			if badPoint   then begin
				if finite(dat.ANGLE[j]) ne 1 then begin
					print,time_string(t)
					continue
				endif
				th=90-abs(dat.ANGLE[j]*180/!pi-90)
				
				badthetaVals.add,th
				if bdmanpoint then badthetaManVals.add, th else badthetaAutoVals.add,th
				continue
			endif



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


			;upsPOSlst.add,usMSO
			;upmPOSlst.add,umMSO
			;upePOSlst.add,ueMSO
			;downsPOSlst.add,dsMSO
			;downmPOSlst.add,dmMSO
			;downePOSlst.add,deMSO
			;usDX=norm(MSO-usMSO)
			;umDX=norm(MSO-umMSO)
			;ueDX=norm(MSO-ueMSO)
			;dsDX=norm(MSO-dsMSO)
			;dmDX=norm(MSO-dmMSO)
			;deDX=norm(MSO-deMSO)
			MSO=transpose(dat.pos[j,*])


			;if 0 and total([dsDX,dmDX,deDX] gt R_mars) gt 0 then begin


				;th=90-abs(dat.ANGLE[j]*180/!pi-90)
				
				;badthetaVals.add,th
				;if bdmanpoint then badthetaManVals.add, th else badthetaAutoVals.add,th
				;continue


			;endif

			

			N_conic=transpose(dat.N_conic[j,*])
			POSconic=transpose(dat.pos_conic[j,*])

			Vdvec=transpose(dat.Vdvec[j,*])
			Vuvec_fine=transpose(dat.Vuvec_fine[j,*])
			Vuvec_coarse=transpose(dat.Vuvec_coarse[j,*])

			Buvec=transpose(dat.Buvec[j,*])
			Bdvec=transpose(dat.Buvec[j,*])

			if NSNexists then N_SN=transpose(dat.N_SN[j,*]) else begin

				N_SN=normalrefinder( Buvec,theta,Vuvec_coarse,dat.flowangle[j],MSO,N_conic,dop)
				if (size(N_SN,/n_dim))[0] eq 2 then N_SN=transpose(N_SN)
			endelse

			;print,"good point"
			if maxViableFM lt FM then begin
				maxViableFM=FM
			endif

			if maxViableBeta lt bta then begin
				maxViableBeta=bta
				maxBetaPlot=xt
			endif
		
			if maxDU lt downup  then begin
				maxDU=downup
				maxDUplot=H[i]
				maxDUplotDate=xt;timestr

			endif
			goodPoints++
			dayCrosses++

			upintervaloffsetlst.add,upDT
			downintervaloffsetlst.add,downDT

			betaIionLst.add,Beta_ion
			TionLst.add,Tion
			TelecLst.add,Telec
			VuvecfLst.add,Vuvec_fine
			VuvecLst.add,Vuvec_coarse
			vdvecLst.add,Vdvec
			BuvecLst.add,Buvec
			BdvecLst.add,Bdvec
			NconicsLst.add,N_conic
			POSconicsLst.add,POSconic
			N_AVG.add,N_SN

			;print,"M,FM,down/up,(Bmax-down)/down,Ls,date=",mfms,FM,down/up,(Bmax-down)/down,Ls,H[i]
			locsLst.add,dat.maxlocs	[j]
			tLst.add,t
			tjLst.add,tj
			tday=floor(tj)
			tjdLst.add,tday
			t0dayLst.add,t00
			MfmsLst.add,mfms
			critList.add,crit
			BmaxLst.add,Bmax
			overDiffsLst.add,dat.overDiffs[j]
			fracOverDiffsLst.add,dat.fracOverDiffs[j]
			B2B1_FitLst.add,downup;downup
			angleLst.add,theta
			downlst.add,down
			uplst.add,up
			overLst.add,dat.overshoot[j]
			lat=dat.lat[j]
			latLst.add,lat
			LsLst.add,Ls
			Nelst.add,N_e
			Nplst.add,N_p
			betas.add,dat.betas[j]
			shock0dist.add,dist0
			shock0Acc.add,dop
			perdaylst.add,0
			downMlst.add,downM
			upMlst.add,upM
			downstdlst.add,dstd
			upstdlst.add,ustd
			thetaNVlst.add,dat.flowangle[j]
			alt=SQRT(MSO[0]^2+MSO[1]^2+MSO[2]^2)
			if 0 then begin
			usMSO=transpose(dat.upstartpos[j,*])
			umMSO=transpose(dat.upmidpos[j,*])
			ueMSO=transpose(dat.upendpos[j,*])
			dsMSO=transpose(dat.downstartpos[j,*])
			dmMSO=transpose(dat.downmidpos[j,*])
			deMSO=transpose(dat.downendpos[j,*])

			;print,size(umMSO,/n_dim)
			;if size(umMSO,/n_dim) eq 1 then return else print,umMSO[4]
			;return

			usALT=norm(usMSO)
			umALT=norm(umMSO)
			ueALT=norm(ueMSO)
			dsALT=norm(dsMSO)
			dmALT=norm(dmMSO)
			deALT=norm(deMSO)


			upsPOSlst.add,usMSO
			upmPOSlst.add,umMSO
			upePOSlst.add,ueMSO
			downsPOSlst.add,dsMSO
			downmPOSlst.add,dmMSO
			downePOSlst.add,deMSO
			usALTlst.add,usALT
			umALTlst.add,umALT
			ueALTlst.add,ueALT
			dsALTlst.add,dsALT
			dmALTlst.add,dmALT
			deALTlst.add,deALT
			usDXlst.add,displacement(MSO,usMSO)
			umDXlst.add,displacement(MSO,umMSO)
			ueDXlst.add,displacement(MSO,ueMSO)
			dsDXlst.add,displacement(MSO,dsMSO)
			dmDXlst.add,displacement(MSO,dmMSO)
			deDXlst.add,displacement(MSO,deMSO)
			endif
			POSlst.add,MSO
			altlst.add,alt
			alfvenlst.add,dat.Alfven[j]


			Soundlst.add,dat.Cs[j]

			ssn0=crossingSeasonFinder(Ls,lat)
			;if ssn0 ne ssn and tjv ne -1 then begin
			if ssn0 ne ssn and tv ne -1 then begin
				;ssnbound.add,[mean([tjv,tj]),ssn,ssn0]
				ssnbound.add,[mean([tv,t]),ssn,ssn0]
				ssnchanges++
			endif
			;tjv=tj
			tv=t
			ssn=ssn0
			totperday.add,numel(dat.t)
		endfor
		;print,"dls in loop"
		dLs=dat.Ls
		;if ~badPoint then print,"good points"
		tmins.add,min(dat.t)
		tmaxs.add,max(dat.t)
		help,LsLst
		tdays.add,tday
		;wb=where(tjdLst eq tday)
		wb=where(t0dayLst eq t00,wcount)
		if wcount gt 0 then perdaylst[wb]=dayCrosses
		
		print,"goodpoint/points=",goodPoints,"/",points
		;print,"[baddists,baddots,badFMs,badBmax,BadManual,badLS,badMeasure,missedAnomolies,badNANs]:"
		;print,[baddists,baddots,badFMs,badBmax,BadManual,badLS,badMeasure,missedanomolies,badNANs]
	;print,"[badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badDensity,badMfms,badbeta,missedAnomolies,BadManual,missedManual,badCrusts,badMultis]"
	;print, [badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badDens,badMfms,badbeta,missedanomolies,BadManual,missedmanual,badCrusts,badMultis]
		print,"[badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta,badDensity]"
		print, badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta,badDens,format='(I7,",",I8,",",I7,",",I6,",",I7,",",I5,",",I10,",",I7,",",I7,",",I7)'
		print,"[missedAnomolies,BadManual,missedManual,Crust,Multi,Large Instability]"
		print,missedAnomolies,BadManual,missedManual,badCrusts,badMultis,badInstab,format='(I15,",",I9,",",I12,",",I5,",",I5,",",I17)'
		if size(LsLst,/typ) eq 0 then begin
				print,xt
				print,H[i]
				;badPoint=1
	
		endif
		del_data,name
		;seasons.add,marsSeasonFinder(dLs,dat.lat)
	endfor
	TOC,clockld
	print,"finished binning data"

	;foreach nm,plotFNames do print,"name=",nm
	print,"corrText=",corrText
	print,"maxFM=",maxFM,", at t=",maxFMplotDate," ,  from tplot file: ",maxFMplot

	print,"minDiff=",minDiff,", at t=",minDiffPlotDate," ,  from tplot file: ",minDiffPlot
	print,"maxFUPDiff=",maxFUPDiff,", at t=",maxFUPDiffPlotDate," ,  from tplot file: ",maxFUPDiffPlot
	print,"maxBUPDiff=",maxBUPDiff,", at t=",maxUPDiffPlotDate," ,  from tplot file: ",maxUPDiffPlot
	print,"maxDU=",maxDU,", at t=",maxDUplotDate," ,  from tplot file: ",maxDUplot
	print,"goodpoint/points=",goodPoints,"/",points
	;baddists=0
	;baddots=0
	;badFMs=0
	;badBmax=0
	;badManual=0
;print,"[badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badDensity,badMfms,badbeta,missedAnomolies,BadManual,missedManual,badCrusts,badMultis]"
	;print, [badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badDens,badMfms,badbeta,missedanomolies,BadManual,missedmanual,badCrusts,badMultis]
		print,"[badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta,badDensity]"
		print, badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta,badDens,format='(I7,",",I8,",",I7,",",I6,",",I7,",",I5,",",I10,",",I7,",",I7,",",I7)'
		print,"[missedAnomolies,BadManual,missedManual,Crust,Multi,Large Instability]"
		print,missedAnomolies,BadManual,missedManual,badCrusts,badMultis,badInstab,format='(I15,",",I9,",",I12,",",I5,",",I5,",",I17)'
	print,'numnoconic=',numnoconic
	;if numnoconic lt 100 then foreach el,noconiclst do print,el
	;if numnoconic gt 500 then return
	;return
	if error_status ne 0 then begin
			PRINT, 'Error index: ', error_status
			PRINT, 'Error message: ', !ERROR_STATE.MSG
	;		help,dat
			return

	endif
	;;print,"allbins=",transpose(allbins)
	;print,"size(allbins)=",size(allbins)
	;foreach el3, allbins do ;print,"size(el3)=",size(el3)
	;bins=secondBinner(allbins)
	;print,"allspbins=",allspbins
	;foreach el, allspbins do foreach el2,el do ;print, "el2=",el2
	;print,"allsubins=",allsubins
	;print,"allaubins=",allaubins
	;print,"allwibins=",allwibins
	;spbins=secondBinner(allspbins)
	;subins=secondBinner(allsubins)
	;aubins=secondBinner(allaubins)
	;wibins=secondBinner(allwibins)

	;qbins=secondBinner(qallbins)
	;qspbins=secondBinner(qallspbins)
	;qsubins=secondBinner(qallsubins)
	;qaubins=secondBinner(qallaubins)
	;;qwibins=secondBinner(qallwibins)
;
	;pbins=secondBinner(pallbins)
	;pspbins=secondBinner(pallspbins)
	;psubins=secondBinner(pallsubins)
	;paubins=secondBinner(pallaubins)
	;pwibins=secondBinner(pallwibins)

	;sbins=list(spbins,subins,aubins,wibins)
	;qsbins=list(qspbins,qsubins,qaubins,qwibins)
	;psbins=list(pspbins,psubins,paubins,pwibins)
	;badthetaVals=badthetaVals.toArray()
	;badthetaManVals=badthetaManVals.toarray()
	;badthetaAutoVals=badthetaAutoVals.toarray()

	;print,"mean(theta_rejected),median(theta_rejected),stddev:",mean(badthetaVals),median(badthetaVals),stddev(badthetaVals)
	;print,"mean(theta_Auto-rejected),median(theta_Auto-rejected),stddev:",mean(badthetaAutoVals),median(badthetaAutoVals),stddev(badthetaAutoVals)
	;print,"mean(theta_Man-rejected),median(theta_Man-rejected),stddev:",mean(badthetaManVals),median(badthetaManVals),stddev(badthetaManVals)
	locs=locsLst.toArray()	
	t=tLst.toarray()
	Mfms=MfmsLst.toarray()
	crits=critList.toarray()
	FM=Mfms/crits
	Bmax=BmaxLst.toarray()
	overDiffs=overDiffsLst.toarray()
	fracOverDiffs=fracOverDiffsLst.toarray()
	B2B1_Fit=B2B1_FitLSt.toarray()

	ThetaNV=thetaNVlst.toarray()
	ThetaNVn=!pi/2-abs(ThetaNV-!pi/2)	
	ThetaNVnd=ThetaNVn*180/!pi




	ThetaNB=angleLst.toarray()
	ThetaNBn=!pi/2-abs(ThetaNB-!pi/2)	
	ThetaNBnd=ThetaNBn*180/!pi

	;upDT=upintervaloffsetlst.toarray()
	;downDT=downintervaloffsetlst.toarray()


	costh=cos(ThetaNBn)

	N_p=Nplst.toarray()
	N_e=Nelst.toarray()

	downs=downlst.toarray()
	ups=uplst.toarray()
	overshoots=overLst.toarray()
	lat=latLst.toarray()
	Ls=LsLst.toarray()	
	help,LsLst
	downMs=downMlst.toarray()
	upMs=upMlst.toarray()
	B2B1_Measure=downMs/upMs
	downstds=downstdlst.toarray()
	upstds=upstdlst.toarray()

	downfluctuation=downstds/downMs
	upfluctuation=upstds/upMs

	B2B1fitmax=max(crits,mxlc)
	print,B2B1fitmax
	print,time_string(t[mxlc])
	;return
	normalOverheight=(Bmax-downs)/downs
	normalOverheightM=(Bmax-downMs)/downMs



	N=size(lat,/n_el)
	N=numel(betas)
	tjul=tjLst.toarray()
	print,min(tjul,mnlc),time_string(t[mnlc])
	tjdLst=tjdLst.toarray()
	perdaylst=perdaylst.toarray()
	totperday=totperday.toarray()
	;usPOS=upsPOSlst.toarray()
	;umPOS=upmPOSlst.toarray()
	;uePOS=upePOSlst.toarray()
	
	;dsPOS=downsPOSlst.toarray()
	;dmPOS=downmPOSlst.toarray()
	;dePOS=downePOSlst.toarray()
	POS=POSlst.toarray()

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
	whereBadSN=whereBadSN.toarray()
	badN_SNlst=badN_SNlst.toarray()
	for i=0,numel(whereBadSN)-1 do begin
		print,'%%%%%%%%%%%%%%%%%%%%%%%%%%'
		print,'non-unity norm(N_SN) at '+whereBadSN[i]
		neon=transpose(badN_SNlst[i,*])
		print,"N_SN=",neon
		print,norm(neon)
	endfor
	;return

	beta_ion=betaIionLst.toarray()
	T_ion=TionLst.toarray()
	T_el=TelecLst.toarray()
	Vuvec_fine=VuvecfLst.toarray()
	Vucec_coarse=VuvecLst.toarray()
	vdvec=vdvecLst.toarray()
	Buvec=BuvecLst.toarray()
	Bdvec=BdvecLst.toarray()
	N_conic=NconicsLst.toarray()
	POS_conic=POSconicsLst.toarray()
	N_SN=N_AVG.toarray()

	vumag=fltarr(N)
	for i=0,N-1 do vumag[i]=SQRT(TOTAL(vuvec_fine[i,*]^2))

	Euvec=-crossprod(Vucec_coarse,Buvec)
	Edvec=-crossprod(vdvec,Buvec)


	help,POS
	;help,usDX
	;help,usPOS
	;POS=POS[*,0,*];TRANSPOSE(POS)
	help,POS
	;return

	Alfven=alfvenlst.toarray()
	Cs=Soundlst.toarray()

	AlfCalc=upM/Sqrt(!const.mp* 10.0^6 *N_p) /10.0^9
	AlfCalcF=up/Sqrt(!const.mp* 10.0^6 *N_p) /10.0^9

	fms=SQRT(1/2. *(( Cs^2+Alfven^2)+ SQRT( ( Cs^2+Alfven^2)^2 -4 *Cs^2 * Alfven^2*(cos(ThetaNBn))^2 )))
	fms_corr=SQRT(1/2. *(( Cs^2+Alfven^2)+ SQRT( ( Cs^2+Alfven^2)^2 -4 *Cs^2 * Alfven^2*(cos(ThetaNVn))^2 )))
	v_normal=dotproduct(N_SN,Vuvec_fine);Mfms*fms
	
;	ThetaNV=acos(v_normal/vumag)
;	ThetaNVn=!pi/2-abs(ThetaNV-!pi/2)	
;	ThetaNVnd=ThetaNVn*180/!pi
	Mfms_corr=fltarr(N)
	Msound=fltarr(N)
	Malfven=fltarr(N);Mfms*fms/Alfven
	print,N,numel(v_normal)
	for i=0,N-1 do begin
		Msound[i]=vumag[i]/Cs[i];v_normal[i]/Cs[i]
		Malfven[i]=vumag[i]/Alfven[i];v_normal[i]/Alfven[i]
		Mfms_corr[i]=v_normal[i]/fms_corr[i]
	endfor

	;p1=scatterplot(Mfms,vumag*Mfms/v_normal,xtitle='$M_{fms}$',ytitle='$M_{fms} (^{|{\bf v_U}|}/_{-\bf v_U \cdot N_{sw}} )$',$
	;					SYMBOL='.', /SYM_FILLED,$;POSITION=pltpos, $
				;		magnitude=bytscl(ThetaNVnd,max=90.,min=0.),RGB_TABLE=13.,aspect_ratio=1)
	;cbb2b2=colorbar(ORIENTATION=0,POSITION=cpos1, $; POSITION=cpos1, title="$\theta_{VN}$",RGB_TABLE=62,RANGE=[0.,90.])
	;return
	alt=altlst.toarray()


	orbitTHETAd=(71.+Ls) MOD 360.

	orbitTHETAdnn=180-abs(orbitTHETAd-180.)

	orbitTHETA=orbitTHETAd *!pi/180.
	SolDist=fltarr(N)
	for i=0, N-1 do SolDist[i]=semilat /(1+ecc*cos(orbitTHETA[i]))
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
	

	shock0dist=shock0dist.toarray()
	shock0Acc=shock0Acc.toarray()

	SEASONS2=marsSeasonFinder(Ls,lat)
	seasonNums=[0,1,2,3]



	XMSO=fltarr(N);POS[*,0]
	for i=0,N-1 do XMSO[i]=POS[i,0]
	YMSO=fltarr(N);POS[*,0]
	for i=0,N-1 do YMSO[i]=POS[i,1]
	ZMSO=fltarr(N);POS[*,0]
	for i=0,N-1 do ZMSO[i]=POS[i,2]
	XMSOAdj=XMSO-.600*R_mars
	RHO=SQRT(YMSO^2+ZMSO^2)
	R_conic=SQRT(XMSOAdj^2+RHO^2)

	ecc_MAVEN=1.026


	l_MAVEN=R_conic+ecc_MAVEN*XMSOAdj

	help,XMSO
	help,RHO
	


	betas=betas.toarray()
	beta_el=betas-beta_ion
	betapart=betas[sort(betas)]

	betacalc=fltarr(N)
	for i=0,N-1 do betacalc[i]=Cs[i]^2/Alfven[i]^2

	betafrac=betacalc/betas
	betafdiff=fracdiff(betacalc,betas)
	
	B2B1_RH_i=fltarr(N);B2B1_Fit*0.0
	densityjump_RH_i=fltarr(N)
	beta2_i=fltarr(N)

	B2B1_RH=fltarr(N);B2B1_Fit*0.0
	densityjump_RH=fltarr(N)
	beta2=fltarr(N)
	for i=0,numel(B2B1_Fit)-1 do begin


			;th=ThetaNB[i]
			;ourBta=betas[i]

			RH_parameters,Mfms[i],ThetaNBn[i],betas[i],a,b,c,d,yy,delta
			densityjump_RH[i]=1/yy
			B2B1_RH[i]=SQRT((b+c*delta)/(b+c))
			beta2[i]=2*(a+0.5*c*(1-delta)+1-yy)/(b+c*delta)

			RH_parameters,Mfms[i],ThetaNBn[i],beta_ion[i],a,b,c,d,yy,delta
			densityjump_RH_i[i]=1/yy
			B2B1_RH_i[i]=SQRT((b+c*delta)/(b+c))
			beta2_i[i]=2*(a+0.5*c*(1-delta)+1-yy)/(b+c*delta)
	endfor



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

	maxRHERROR=max(RH_error,mxrhloc)
	print,maxRHERROR
	print,time_string(t[mxrhloc])
;	return

	wnegjumps=where(B2B1_RH lt 1)

	foreach el,wnegjumps do begin
		print,"*************"
		print,B2B1_RH[el]
		print,time_string(t[el])
		print,"*************"
	endforeach



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

	betabounds=betaparts[*,betaparttype-1]
	;print,"betabounds:",betabounds
	lowbetabound=betabounds[0]
	highbetabound=betabounds[1]
	;print,lowbetabound,highbetabound
	wheresmallbeta=where(betas le lowbetabound,nsbeta)
	wherebigbeta=where(betas gt highbetabound,nsbeta)


	HH0=where(abs(ThetaNB -!pi/2) lt !pi/4,complement=nHH) ; QUASIPERP locations
	AA0=where(ThetaNB eq ThetaNB) ;All Locations

	springIndices=where(SEASONS2 eq 0,spcount)
	summerIndices=where(SEASONS2 eq 1,sumcount)
	autumnIndices=where(SEASONS2 eq 2,fallcount)
	winterIndices=where(SEASONS2 eq 3,wntrcount)
  	


	sprng=fltarr(N)
	foreach el,springIndices do sprng[el]=1
	smmr=fltarr(N)
	foreach el,summerIndices do smmr[el]=1
	fall=fltarr(N)
	foreach el,autumnIndices do fall[el]=1	
	wntr=fltarr(N)
	foreach el,winterIndices do wntr[el]=1	

	qspringIndices=Intersect(springIndices,HH0)
	qsummerIndices=Intersect(summerIndices,HH0)
	qautumnIndices=Intersect(autumnIndices,HH0)
	qwinterIndices=Intersect(winterIndices,HH0)

	pspringIndices=Intersect(springIndices,nHH)
	psummerIndices=Intersect(summerIndices,nHH)
	pautumnIndices=Intersect(autumnIndices,nHH)
	pwinterIndices=Intersect(winterIndices,nHH)
	
	startSpring=x2Greg(min(t[springIndices]),/strf,/noHour)
	finalSpring=x2Greg(max(t[springIndices]),/strf,/noHour)
	startSummer=x2Greg(min(t[summerIndices]),/strf,/noHour)
	finalSummer=x2Greg(max(t[summerIndices]),/strf,/noHour)
	startAutumn=x2Greg(min(t[autumnIndices]),/strf,/noHour)
	finalAutumn=x2Greg(max(t[autumnIndices]),/strf,/noHour)
	startWinter=x2Greg(min(t[winterIndices]),/strf,/noHour)
	finalWinter=x2Greg(max(t[winterIndices]),/strf,/noHour)

	HHsp=where(abs(ThetaNB[springIndices] -!pi/2) lt !pi/4,complement=nHHsp) ; QUASIPERP locations Spring
	HHsu=where(abs(ThetaNB[summerIndices] -!pi/2) lt !pi/4,complement=nHHsu) ; QUASIPERP locations Summer
	HHfa=where(abs(ThetaNB[autumnIndices] -!pi/2) lt !pi/4,complement=nHHfa) ; QUASIPERP locations fall
	HHwi=where(abs(ThetaNB[winterIndices] -!pi/2) lt !pi/4,complement=nHHwi) ; QUASIPERP locations Winter
	;angleIndices=list(AA0,HH0)

	quasiIndices=list(HHsp,HHsu,HHfa,HHwi)
	puasiIndices=list(HHsp,HHsu,HHfa,HHwi)
	
	angleNames=["","Quasiperpendicular "]
	angleShort=["","_quasiperp_"]
	seasonIndices=list(springIndices,summerIndices,autumnIndices,winterIndices,AA0)
	qseasonIndices=list(qspringIndices,qsummerIndices,qautumnIndices,qwinterIndices)
	pseasonIndices=list(pspringIndices,psummerIndices,pautumnIndices,pwinterIndices)

	seasonColors=list("green","red","orange","blue")
	seasonColors2=list("spring_green","crimson","gold","deep_sky_blue")
	seasonNames0=list("Spring","Summer","Autumn","Winter")
	seasonNames=list("Spring ($180^\circ\leq Ls<270^\circ$)","Summer ($270^\circ \leq Ls<360^\circ$)","Autumn ($0^\circ \leq Ls<90^\circ$)","Winter ($90^\circ \leq Ls<180^\circ$)")
	seasonNames2=seasonNames0;list("Spring, $t\in$["+startSpring+","+finalSpring+"]","Summer,$t\in$["+startSummer+","+finalSummer+"]","Autumn,$t\in$["+startAutumn+","+finalAutumn+"]","Winter,$t\in$["+startWinter+","+finalWinter+"]")


	;wheresmallbeta=where(betas le 1,nsbeta,complement=wherebigbeta)
	wheresmallbeta=where(betas le lowbetabound,nsbeta)
	wherebigbeta=where(betas gt highbetabound,nsbeta)
	thermalspring=intersect(springIndices,wherebigbeta)
	magnetospring=intersect(springIndices,wheresmallbeta)

	thermalsummer=intersect(summerIndices,wherebigbeta)
	magnetosummer=intersect(summerIndices,wheresmallbeta)	

	thermalautumn=intersect(autumnIndices,wherebigbeta)
	magnetoautumn=intersect(autumnIndices,wheresmallbeta)

	thermalwinter=intersect(winterIndices,wherebigbeta)
	magnetowinter=intersect(winterIndices,wheresmallbeta)

	thermalseason=list(thermalspring,thermalsummer,thermalautumn,thermalwinter)
	magnetoseason=list(magnetospring,magnetosummer,magnetoautumn,magnetowinter)



	wherecritical=where(FM gt 1.,nscrit,complement=wheresubcritical)

	criticalspring=intersect(springIndices,wherecritical)
	subcriticalspring=intersect(springIndices,wheresubcritical)

	criticalsummer=intersect(summerIndices,wherecritical)
	subcriticalsummer=intersect(summerIndices,wheresubcritical)	

	criticalautumn=intersect(autumnIndices,wherecritical)
	subcriticalautumn=intersect(autumnIndices,wheresubcritical)

	criticalwinter=intersect(winterIndices,wherecritical)
	subcriticalwinter=intersect(winterIndices,wheresubcritical)

	criticalseason=list(criticalspring,criticalsummer,criticalautumn,criticalwinter)
	subcriticalseason=list(subcriticalspring,subcriticalsummer,subcriticalautumn,subcriticalwinter)


	for i=0,3 do print,"numel("+seasonNames0[i]+"critical)=",numel(criticalseason[i])
	for i=0,3 do print,"numel("+seasonNames0[i]+"subcritical)=",numel(subcriticalseason[i])




	

	
	xttl='$M_{fms}/M_{crit}$'

	;plotYTitles=['overshootHeight-fit height','(overshootHeight-fit height)/fitHeight','$B^{Fit}_{Downstream}$/$B^{Fit}_{Upstream}$','($B_{Max} - B^{Fit}_{Downstream}$)/$B^{Fit}_{Downstream}$','($B_{Max} - B^{Fit}_{Upstream}$)/$B^{Fit}_{Upstream}$','overshootHeight-downstream [nT]]','overshootHeight-upstream [nT]','$B^{Measured}_{Downstream}$/$B^{Measured}_{Upstream}$','($B_{Max} - B^{Measured}_{Downstream}$)/$B^{Measured}_{Downstream}$']

plotYTitles=['$B^{Fit}_{Downstream}$/$B^{Fit}_{Upstream}$','($B_{Max} - B^{Fit}_{Downstream}$)/$B^{Fit}_{Downstream}$','($B_{Max} - B^{Fit}_{Upstream}$)/$B^{Fit}_{Upstream}$','$B^{Measured}_{Downstream}$/$B^{Measured}_{Upstream}$','($B_{Max} - B^{Measured}_{Downstream}$)/$B^{Measured}_{Downstream}$','$[\rho_D /\rho_U ]_{RH}$','$[\beta_D/\beta_U]_{RH}$']

	;plotYs=list(overDiffs,fracOverDiffs,B2B1_Fit,(Bmax-downs)/downs,(Bmax-ups)/ups,Bmax-downs,Bmax-ups,downMs/upMs,(Bmax-downMs)/downMs)


	plotYs=list(B2B1_Fit,normalOverheight,(Bmax-ups)/ups,B2B1_Measure,(Bmax-downMs)/downMs,densityjump_RH,betajump)

	;plotTitles=["overshoot difference","Normalized overshoot difference","fitted downstream/upstream B's",$
	;"Normalized overVsDown", "fractional overVsUp","over-Down","over-Up","measured downstream/upstream B's","Normalized overshoot-Downstream (Measured) vs "]
	plotTitles=["FIT Magnetic Jump","Normalized overshoot height", "(Bmax-Bup)/Bup","measured downstream/upstream B's","Normalized overshoot-Downstream (Measured)","RH predicted Density Jump","RH predicted Beta Jump"]

	plotTitlesFM=plotTitles+" Critical Ratio"
	pfns=["DownstreamFitVsUpstreamFit","Normalized_Overshoot-down","Normalized_Overshoot-up","DownstreamMeasureVsUpstreamMeasure","Normalized_Overshoot-downMeasured","densityjumpRH","betajumpRH"]

	
	


	plotFNames=pfns+"_vs_FMfms"+corrText+".png"

	plotXs=list(FM,Mfms,ThetaNBnd,betas,ups)
	
	xttls=['$M_{fms}/M_{crit}$','$M_{fms}$','$\theta_{NB} [deg]$','$\beta$','$B_{Upstream}$ [nT]']
	vsTitles=['$M_{fms}/M_{crit}$','$M_{fms}$','$\theta_{NB}$','$\beta$','$B_{Upstream}$']
	fvsTitles=["Crit_Ratio","Mfms",'theta','beta','Bup']

	FMdata=list(FM,'$M_{fms}/M_{crit}$','Critical Ratio',"Crit_Ratio",1.)
	Mfmsdata=list(Mfms,'$M_{fms}$','$M_{fms}$',"Mfms",1.)
	ThetaNBnddata=list(ThetaNBnd,'$\theta_{NB} [deg]$','$\theta_{NB}$','theta',5.0)
	betadata=list(betas,'$\beta$','$\beta$','beta',.25)
	betacalcdata=list(betacalc,'$C_S^2 / C_A^2$','$C_S^2 / C_A^2$','betacalc',.25)

	betafracdata=list(betafrac,'$(C_S / C_A)^2/\beta$','$(C_S / C_A)^2/\beta$','betafrac',.1)
	betafdiffdata=list(betafdiff,'% diff ($C_S / C_A)^2,\beta)$','beta fractional diff','betafdiff',.1)
	critdata=list(crits,'$M_{crit}$','Critical Mach Number','Mcrit',.1)


	orbthetadata=list(orbitTheta,'$\Theta_{orb}$ [rad] (0 at periapsis)','Orbital Angle','ThetaOrb',20*!pi/180)

	LsRaddata=list(Ls*!pi/180.,'LS [rad]','Martian Solar Longitude','LsRad',20*!pi/180)


	ThetaNBdata=list(ThetaNB,'$\theta_{NB1}$ [rad]','$\theta_{NB1}$','theta_Rad',5*!pi/180)



	altdata=list(alt,'Altitude [km]','Altitude','Altitude',100)


	altNormdata=list(alt/R_mars,'Altitude [$R_{Mars}$]','Normalized Altitude','Altitude_normalized',.5)


	Lsdata=list(Ls,'Ls [deg]','Mars Solar Longitude','Ls',20.)
	
	lmavendata=list(l_MAVEN,'$\ell=(ALT\prime+1.026X_{MSO}\prime) [km], ALT\prime\equiv ALT(X_{MSO}\prime)=ALT(X_{MSO}-.6 R_M)$ [km]',"Shock's Latus Rectum",'latus_rectum',500.)
	lmavenNormdata=list(l_MAVEN/R_mars,'$\ell=(ALT\prime+1.026X_{MSO}\prime) [R_{Mars}], ALT\prime\equiv ALT(X_{MSO}\prime)=ALT(X_{MSO}-.6 R_M) [R_{Mars}]$',"Shock's Normalized Latus Rectum",'latus_rectum_normalized',.5)
	tjuldata=list(tjul,'time of crossing','time of crossing','time',30)
	SolDistdata=list(SolDist,'$R_{orb}$ [km]','Subsolar distance [km]','solDist',.005*semimajor)
	SolDistNormdata=list(SolDist/semimajor,'$R_{orbit} [Semimajor_{orbit}]$','Subsolar distance','solDistNormalized',.005)


	downdata=list(downs,'$B^{FIT}_{DOWN}$ [nT]','$B^{FIT}_{DOWN}$',"down_fit",2.)
	downMdata=list(downMs,'$B^{Measured}_{DOWN}$ [nT]','$B^{Measured}_{DOWN}$',"down_measured",2.)
	updata=list(ups,'$B^{fit}_{Upstream}$ [nT]','$B^{fit}_{Upstream}$','Bup',2.)
	upMdata=list(upMs,'$B^{Measured}_{Upstream}$ [nT]','$B^{Measured}_{Upstream}$','BupM',2.)
	Bmaxdata=list(Bmax,'$B_{max}$ [nT]','Overshoot Maximum','Bmax',5.)


	downflucdata=list(downfluctuation,'stddev($B_{Downstream}^{Measured}$)/mean($B_{Downstream}^{Measured}$)',"Normalized Downstream Magnetic Fluctuation",'downfluctuation',.2)
	upflucdata=list(upfluctuation,'stddev($B_{Upstream}^{Measured}$)/mean($B_{Upstream}^{Measured}$)',"Normalized Upstream Magnetic Fluctuation",'upfluctuation',.2)


	B2B1fitdata=list(B2B1_Fit,'$B^{Fit}_{Downstream}$/$B^{Fit}_{Upstream}$',"Fitted Magnetic Jump","DownstreamFitVsUpstreamFit",.2)
	B2B1Mdata=list(B2B1_Measure,'$B^{Measured}_{Downstream}$/$B^{Measured}_{Upstream}$',"Measured Magnetic Jump","DownstreamMeasureVsUpstreamMeasure",.2)
	normOverdownfitdata=list(normalOverheight,'($B_{Max} - B^{Fit}_{Downstream}$)/$B^{Fit}_{Downstream}$',"Normalized overshoot height","Normalized_OvershootHeight",.5)
	normOverdownMdata=list(normalOverheightM,'($B_{Max} - B^{Measured}_{Downstream}$)/$B^{Measured}_{Downstream}$',"Normalized overshoot height (Measured)","Normalized_OvershootHeightMeasured",.5)



	normOverfitupdata=list((Bmax-ups)/ups,'($B_{Max} - B^{Fit}_{Upstream}$)/$B^{Fit}_{Upstream}$',"Normalized overshoot-up","Normalized_Overshoot-up",.5)
	normOverupMdata=list((Bmax-upMs)/upMs,'($B_{Max} - B^{Measured}_{Upstream}$)/$B^{Measured}_{Upstream}$',"Normalized overshoot-up (Measured)","Normalized_Overshoot-up_Measured",1.)

	B2B1_RHdata=list(B2B1_RH,'$(B_D/B_U)_{RH}$','RH Predicted Magnetic Jump',"B2B1_RH",.5)


	densityJumpData=list(densityjump_RH,'$[\rho_D /\rho_U ]_{RH}$',"RH predicted Density Jump","densityjumpRH",1.)
	betajumpdata=list(betajump,'$[\beta_D/\beta_U]_{RH}$',"RH predicted Beta Jump","betajumpRH",5.)
	beta2data=list(beta2,'$\beta_{Down}$','$\beta_{Down}$','beta2',.25)

	
	B2B1fdiffdata=list(B2B1fdiff,'$B_d$/$B_u$ % diff(Fit, RH)','RH Fit Magnetic jump % difference',"fit_rankine_B2B1_percentdiff",.1)
	B2B1Mdiffdata=list(B2B1Mdiff,'$B_d$/$B_u$ % diff(Measured, RH)','$B_d$/$B_u$ % diff(Measured, RH)',"measure_rankine_B2B1_percentdiff",1.)
	RH_errordata=list(RH_error,'|RH-FIT|/RH [Magnetic Jump error]','RH Magnetic Jump error','RH_error',.25)
	Fit_errordata=list(Fit_error,'|RH-FIT|/Fit [Magnetic Jump error]','Fit Magnetic Jump error','Fit_error',.25)


	shock0Accdata=list(shock0Acc,'$N_{SN}^{AVG}\cdotN_{Conic}$','$N_{SN}^{AVG}\cdotN_{Conic}$',"shock0acc",.2)
	shock0distdata=list(shock0dist,'|$R^{shock}_{FIT}-R^{Shock}_{Closest conic}$| [km]','Distance from Conic',"shock0dist",100.)

	SoundSpeeddata=list(Cs,'$C_{sound}$ [km/s]','Sound Speed','Cs',5.)
	Alfvendata=list(Alfven,'$V_A$ [km/s]','Alfven Speed','vA',100.)
	fmsdata=list(fms,'$V_{fms}$ [km/s]','fast Magnetosonic Speed','fms',100.)
	MachSounddata=list(Msound,'$M_{sound}$','$M_{sound}$','Msound',1.)
	MachAlfvendata=list(Malfven,'$M_{Alfven}$','$M_{Alfven}$','M_A',1.)
	velocitydata=list(v_normal,'$v^{ion}_{NB}$ [km/s]','Shockward ion Velocity','Vion',50.)


	subintervals=list(AA0,HH0,nHH);,wherebigbeta,wheresmallbeta,wherecritical,wheresubcritical)
	subinterTitles=['','Quasiperpendicular ','Quasiparallel '];,'($\beta >$'+strtrim(highbetabound,2)+') ','($\beta <$'+strtrim(lowbetabound,2)+') ','Supercritical ','Subcritical ']
	subinterfnames=['','Quasiperp_','Quasipar_'];,'thermal_pressure_dominating_','magnetic_pressure_dominating_','supercritical_','subcritical_']
	
	AllSubinter=list(AA0,'','')
	Quasiperpinter=list(HH0,'Quasiperpendicular ','Quasiperp_')
	Quasiparinter=list(nHH,'Quasiparallel ','Quasipar_')
	thermalinter=list(wherebigbeta,'($\beta >$'+strtrim(highbetabound,2)+') ','thermal_pressure_dominating_')
	magnetointer=list(wheresmallbeta,'($\beta <$'+strtrim(lowbetabound,2)+') ','magnetic_pressure_dominating_')
	supercritinter=list(wherecritical,'Supercritical ','supercritical_')
	subcritinter=list(wheresubcritical,'Subcritical ','subcritical_')
	
	;return
if 1 then begin
	store_data,'season',data={x:t,y:SEASONS2}

	store_data,'spring',data={x:t,y:sprng}
	store_data,'summer',data={x:t,y:smmr}
	store_data,'autumn',data={x:t,y:fall}
	store_data,'winter',data={x:t,y:wntr}

	store_data,"FM",data={x:t,y:FM,ytitle:['$M_{fms}/M_{crit}$'],YN:['Critical Ratio'], fn:["Crit_Ratio"],  binsize:[1.],radian:[0],degree:[0]}
	store_data,"FM_NV",data={x:t,y:Mfms_corr/crits,ytitle:['$M_{fms}/M_{crit}$'],YN:['Critical Ratio'], fn:["Crit_Ratio_NV"],  binsize:[1.],radian:[0],degree:[0]}
	store_data,"Mfms",data={x:t,y:Mfms,ytitle:['$M_{fms}$'],YN:['$M_{fms}$'], fn:["Mfms"],  binsize:[1.],radian:[0],degree:[0]}
	store_data,"Mfms_NV",data={x:t,y:Mfms_corr,ytitle:['$M_{fms}$'],YN:['$M_{fms}$'], fn:["Mfms_NV"],  binsize:[1.],radian:[0],degree:[0]}
	store_data,"ThetaNBn",data={x:t,y:ThetaNBnd,ytitle:['$\theta_{NB} [deg]$'],YN:['$\theta_{NB}$'], fn:['theta'],  binsize:[5.0],radian:[0],degree:[1]}
	store_data,"ThetaNB",data={x:t,y:ThetaNB,ytitle:['$\theta_{NB1}$ [rad]'],YN:['$\theta_{NB1}$'], fn:['theta_Rad'],  binsize:[5*!pi/180],radian:[1],degree:[0]}
	store_data,"beta",data={x:t,y:betas,ytitle:['$\beta$'],YN:['$\beta$'], fn:['beta'],  binsize:[.25],radian:[0],degree:[0]}
	store_data,"beta_proton",data={x:t,y:beta_ion,ytitle:['$\beta_{p1}$'],YN:['Proton $\beta$'], fn:['beta_i'],  binsize:[.1],radian:[0],degree:[0]}
	store_data,"beta_electron",data={x:t,y:beta_el,ytitle:['$\beta_{e1}$'],YN:['Electron $\beta$'], fn:['beta_e'],  binsize:[.25],radian:[0],degree:[0]}
	store_data,"Mcrit",data={x:t,y:crits,ytitle:['$M_{crit}$'],YN:['Critical Mach Number'], fn:['Mcrit'],  binsize:[.1],radian:[0],degree:[0]}
	store_data,"ThetaNVn",data={x:t,y:ThetaNVnd,ytitle:['$\theta_{NV} [deg]$'],YN:['$\theta_{NV}$'], fn:['thetaNV'],  binsize:[5.0],radian:[0],degree:[1]}
	store_data,"ThetaNV",data={x:t,y:ThetaNV,ytitle:['$\theta_{NV1}$ [rad]'],YN:['$\theta_{NV1}$'], fn:['thetaNV_Rad'],  binsize:[5*!pi/180],radian:[1],degree:[0]}

	store_data,"N_ion",data={x:t,y:N_p,ytitle:'Ion Density [cm^-3]',YN:'Ion Density',fn:'Ni',binsize:3.,radians:[0],degree:[0]} 
	store_data,"N_e",data={x:t,y:N_e,ytitle:'Electron Density [cm^-3]',YN:'Electron Density',fn:'Ne',binsize:3.,radians:[0],degree:[0]} 

	store_data,'Tproton',data={x:t,y:T_ion,ytitle:'Proton Temperature [eV]',YN:'Proton Temperature',fn:'T_p',binsize:2,radians:[0],degree:[0]} 
	store_data,'Telectron',data={x:t,y:T_el,ytitle:'Electron Temperature [eV]',YN:'Electron Temperature',fn:'T_e',binsize:2,radians:[0],degree:[0]} 
	store_data,'Temp_ratio',data={x:t,y:TempRatio,ytitle:'$T_e/T_p$',YN:'Temperature Ratio',fn:'TempRatio',binsize:.25,radians:[0],degree:[0]} 

	store_data,"Electron_Ion_fraction",data={x:t,y:N_e/N_p,ytitle:'Electron Density/Ion density',YN:'Electron/Ion fraction',fn:'NeNp',binsize:.1,radians:[0],degree:[0]} 
	store_data,"Electron_Ion_fracdiff",data={x:t,y:fracdiff(N_e,N_p),ytitle:' $2|N_e-N_i|/(N_e+N_i)$',YN:'Electron Ion fractional difference',fn:'NeNpfdiff',binsize:.1,radians:[0],degree:[0]} 


	store_data,"OrbitTheta",data={x:t,y:orbitTheta,ytitle:['$\Theta_{orb}$ [rad] (0 at periapsis)'],YN:['Orbital Angle'], fn:['ThetaOrb'],  binsize:[20*!pi/180],radian:[1],degree:[0]}

	store_data,'alt',data={x:t,y:alt,ytitle:['Altitude [km]'],YN:['Altitude'], fn:['Altitude'],  binsize:[100],radian:[0],degree:[0]}
	store_data,"altNorm",data={x:t,y:alt/R_mars,ytitle:['Altitude [$R_{Mars}$]'],YN:['Normalized Altitude'], fn:['Altitude_normalized'],  binsize:[.5],radian:[0],degree:[0]}
	store_data,'Ls',data={x: t ,y: Ls ,ytitle:[ 'Ls [deg]' ], YN:[ 'Mars Solar Longitude' ], fn:[ 'Ls' ], binsize:[ 20. ], radian:[0],degree:[1]}
	store_data,'lmaven',data={x: t ,y: l_MAVEN ,ytitle:[ '$\ell=(ALT\prime+1.026X_{MSO}\prime) [km]; ALT\prime\equiv ALT(X_{MSO}\prime)=ALT(X_{MSO}-.6 R_M)$ [km]' ], YN:[ "Shock's Latus Rectum" ], fn:[ 'latus_rectum' ], binsize:[ 500. ], radian:[0],degree:[0]}
	store_data,'lmavenNorm',data={x: t ,y: l_MAVEN/R_mars ,ytitle:[ '$\ell=(ALT\prime+1.026X_{MSO}\prime) [R_{Mars}]; ALT\prime\equiv ALT(X_{MSO}\prime)=ALT(X_{MSO}-.6 R_M) [R_{Mars}]$' ], YN:[ "Shock's Normalized Latus Rectum" ], fn:[ 'latus_rectum_normalized' ], binsize:[ .5 ], radian:[0],degree:[0]}
	store_data,'tjul',data={x: t ,y: tjul ,ytitle:[ 'time of crossing' ], YN:[ 'time of crossing' ], fn:[ 'time' ], binsize:[ 30 ], radian:[0],degree:[0]}
	store_data,'SolDist',data={x: t ,y: SolDist ,ytitle:[ '$R_{orb}$ [km]' ], YN:[ 'Subsolar distance [km]' ], fn:[ 'solDist' ], binsize:[ .005*semimajor ], radian:[0],degree:[0]}
	store_data,'SolDistNorm',data={x: t ,y: SolDist/semimajor ,ytitle:[ '$R_{orbit} [Semimajor_{orbit}]$' ], YN:[ 'Subsolar distance' ], fn:[ 'solDistNormalized' ], binsize:[ .005 ], radian:[0],degree:[0]}


	store_data,'down',data={x: t ,y: downs ,ytitle:[ '$B^{FIT}_{DOWN}$ [nT]' ], YN:[ '$B^{FIT}_{DOWN}$' ], fn:[ "down_fit" ], binsize:[ 2. ], radian:[0],degree:[0]}
	store_data,'downM',data={x: t ,y: downMs ,ytitle:[ '$B^{Measured}_{DOWN}$ [nT]' ], YN:[ '$B^{Measured}_{DOWN}$' ], fn:[ "down_measured" ], binsize:[ 2. ], radian:[0],degree:[0]}
	store_data,'up',data={x: t ,y: ups ,ytitle:[ '$B^{fit}_{Upstream}$ [nT]' ], YN:[ '$B^{fit}_{Upstream}$' ], fn:[ 'Bup' ], binsize:[ 2. ], radian:[0],degree:[0]}
	store_data,'upM',data={x: t ,y: upMs ,ytitle:[ '$B^{Measured}_{Upstream}$ [nT]' ], YN:[ '$B^{Measured}_{Upstream}$' ], fn:[ 'BupM' ], binsize:[ 2. ], radian:[0],degree:[0]}
	store_data,'Bmax',data={x: t ,y: Bmax ,ytitle:[ '$B_{max}$ [nT]' ], YN:[ 'Overshoot Maximum' ], fn:[ 'Bmax' ], binsize:[ 5. ], radian:[0],degree:[0]}
	store_data,'downfluc',data={x: t ,y: downfluctuation ,ytitle:[ 'stddev($B_{Downstream}^{Measured}$)/mean($B_{Downstream}^{Measured}$)' ], YN:[ "Normalized Downstream Magnetic Fluctuation" ], fn:[ 'downfluctuation' ], binsize:[ .2 ], radian:[0],degree:[0]}
	store_data,'upfluc',data={x: t ,y: upfluctuation ,ytitle:[ 'stddev($B_{Upstream}^{Measured}$)/mean($B_{Upstream}^{Measured}$)' ], YN:[ "Normalized Upstream Magnetic Fluctuation" ], fn:[ 'upfluctuation' ], binsize:[ .2 ], radian:[0],degree:[0]}

	store_data,'B2B1fit',data={x: t ,y: B2B1_Fit ,ytitle:[ '$B^{Fit}_{Downstream}$/$B^{Fit}_{Upstream}$' ], YN:[ "Fitted Magnetic Jump" ], fn:[ "DownstreamFitVsUpstreamFit" ], binsize:[ .2 ], radian:[0],degree:[0]}
	store_data,'B2B1M',data={x: t ,y: B2B1_Measure ,ytitle:[ '$B^{Measured}_{Downstream}$/$B^{Measured}_{Upstream}$' ], YN:[ "Measured Magnetic Jump" ], fn:[ "DownstreamMeasureVsUpstreamMeasure" ], binsize:[ .2 ], radian:[0],degree:[0]}
	
store_data,'overshootAmplitude',data={x: t ,y: normalOverheight ,ytitle:[ '($B_{Max} - B^{Fit}_{Downstream}$)/$B^{Fit}_{Downstream}$' ], YN:[ "Normalized overshoot height" ], fn:[ "Normalized_OvershootHeight" ], binsize:[ .5 ], radian:[0],degree:[0]}

	store_data,'overshootAmplitudeM',data={x: t ,y: normalOverheightM ,ytitle:[ '($B_{Max} - B^{Measured}_{Downstream}$)/$B^{Measured}_{Downstream}$' ], YN:[ "Normalized overshoot height (Measured)" ], fn:[ "Normalized_OvershootHeightMeasured" ], binsize:[ .5 ], radian:[0],degree:[0]}


store_data,'overshootAmplitudeLowRes',data={x: t ,y: normalOverheight-downstds/downs ,ytitle:[ '($B_{Max} - B^{Fit}_{Down} -\sigma^{down}_{interval}(B)$)/$B^{Fit}_{Down}$' ], YN:[ "Normalized overshoot height" ], fn:[ "Normalized_OvershootHeight_lowRes" ], binsize:[ .5 ], radian:[0],degree:[0]}

	store_data,'overshootAmplitudeLowResM',data={x: t ,y: normalOverheightM-downfluctuation ,ytitle:[ '($B_{Max} - B^{Measured}_{Down}-\sigma^{down}_{interval}(B)$)/$B^{Measured}_{Downstream}$' ], YN:[ "Normalized overshoot height (Measured)" ], fn:[ "Normalized_OvershootHeightMeasured_lowRes" ], binsize:[ .5 ], radian:[0],degree:[0]}



	store_data,'normOverfitup',data={x: t ,y: (Bmax-ups)/ups ,ytitle:[ '($B_{Max} - B^{Fit}_{Upstream}$)/$B^{Fit}_{Upstream}$' ], YN:[ "Normalized overshoot-up" ], fn:[ "Normalized_Overshoot-up" ], binsize:[ .5 ], radian:[0],degree:[0]}
	store_data,'normOverupM',data={x: t ,y: (Bmax-upMs)/upMs ,ytitle:[ '($B_{Max} - B^{Measured}_{Upstream}$)/$B^{Measured}_{Upstream}$' ], YN:[ "Normalized overshoot-up (Measured)" ], fn:[ "Normalized_Overshoot-up_Measured" ], binsize:[ 1. ], radian:[0],degree:[0]}

	store_data,'B2B1_RH',data={x: t ,y: B2B1_RH ,ytitle:[ '$(B_D/B_U)_{RH}$' ], YN:[ 'RH Predicted Magnetic Jump' ], fn:[ "B2B1_RH" ], binsize:[ .5 ], radian:[0],degree:[0]}
	store_data,'densityJump',data={x: t ,y: densityjump_RH ,ytitle:[ '$[\rho_D /\rho_U ]_{RH}$' ], YN:[ "RH predicted Density Jump" ], fn:[ "densityjumpRH" ], binsize:[ 1. ], radian:[0],degree:[0]}
	store_data,'betajump',data={x: t ,y: betajump ,ytitle:[ '$[\beta_D/\beta_U]_{RH}$' ], YN:[ "RH predicted Beta Jump" ], fn:[ "betajumpRH" ], binsize:[ 5. ], radian:[0],degree:[0]}
	store_data,'beta2',data={x: t ,y: beta2 ,ytitle:[ '$\beta_{Down}$' ], YN:[ '$\beta_{Down}$' ], fn:[ 'beta2' ], binsize:[ .25 ], radian:[0],degree:[0]}


store_data,'B2B1_RH_ion',data={x: t ,y: B2B1_RH_i ,ytitle:[ '$(B_D/B_U)_{RH ion}$ ($\beta_i$ only)' ], YN:[ 'RH Predicted Magnetic Jump ' ], fn:[ "B2B1_RH_i" ], binsize:[ .5 ], radian:[0],degree:[0]}
	store_data,'densityJump_ion',data={x: t ,y: densityjump_RH_i ,ytitle:[ '$[\rho_D /\rho_U ]_{RH_ion}$ ($\beta_i$ only)' ], YN:[ "RH predicted Density Jump ($\beta_i$ only)" ], fn:[ "densityjumpRH_i" ], binsize:[ 1. ], radian:[0],degree:[0]}
	store_data,'betajump_ion',data={x: t ,y: betajump_i ,ytitle:[ '$[\beta_D/\beta_U]_{RH ion}$ ($\beta_i$ only)' ], YN:[ "RH predicted Beta Jump" ], fn:[ "betajumpRH" ], binsize:[ 5. ], radian:[0],degree:[0]}
	store_data,'beta2_ion',data={x: t ,y: beta2_i ,ytitle:[ '$\beta_{Down}$ ($\beta_i$ only)' ], YN:[ '$\beta_{Down}$ ($\beta^{i}_1$ only)' ], fn:[ 'beta2_i' ], binsize:[ .25 ], radian:[0],degree:[0]}


	store_data,'B2B1fdiff',data={x: t ,y: B2B1fdiff ,ytitle:[ '$B_d$/$B_u$ % diff(Fit; RH)' ], YN:[ 'RH Fit Magnetic jump % difference' ], fn:[ "fit_rankine_B2B1_percentdiff" ], binsize:[ .1 ], radian:[0],degree:[0]}
	store_data,'B2B1Mdiff',data={x: t ,y: B2B1Mdiff ,ytitle:[ '$B_d$/$B_u$ % diff(Measured; RH)' ], YN:[ '$B_d$/$B_u$ % diff(Measured; RH)' ], fn:[ "measure_rankine_B2B1_percentdiff" ], binsize:[ 1. ], radian:[0],degree:[0]}
	store_data,'RH_error',data={x: t ,y: RH_error ,ytitle:[ '|RH-FIT|/RH [Magnetic Jump error]' ], YN:[ 'RH Magnetic Jump error' ], fn:[ 'RH_error' ], binsize:[ .25 ], radian:[0],degree:[0]}
	store_data,'Fit_error',data={x: t ,y: Fit_error ,ytitle:[ '|RH-FIT|/Fit [Magnetic Jump error]' ], YN:[ 'Fit Magnetic Jump error' ], fn:[ 'Fit_error' ], binsize:[ .25 ], radian:[0],degree:[0]}

	store_data,'B2B1fdiff_i',data={x: t ,y: B2B1fdiff_i ,ytitle:[ '$B_d$/$B_u$ % diff(Fit; RH_ion) ($\beta^{i}_1$ only)' ], YN:[ 'RH Fit Magnetic jump % difference ($\beta^{i}_1$ only)' ], fn:[ "fit_rankine_B2B1_percentdiff_i" ], binsize:[ .1 ], radian:[0],degree:[0]}
	store_data,'B2B1Mdiff_i',data={x: t ,y: B2B1Mdiff_i ,ytitle:[ '$B_d$/$B_u$ % diff(Measured; RH_ion) ($\beta^{i}_1$ only)' ], YN:[ '$B_d$/$B_u$ % diff(Measured; RH_ion) ($\beta^{i}_1$ only)' ], fn:[ "measure_rankine_B2B1_percentdiff_i" ], binsize:[ 1. ], radian:[0],degree:[0]}
	store_data,'RH_error_i',data={x: t ,y: RH_error_i ,ytitle:[ '|RH_ion-FIT|/RH_ion [Magnetic Jump error] ($\beta^{i}_1$ only)' ], YN:[ 'RH_ion Magnetic Jump error ($\beta^{i}_1$ only)' ], fn:[ 'RH_error_i' ], binsize:[ .25 ], radian:[0],degree:[0]}
	store_data,'Fit_error_i',data={x: t ,y: Fit_error_i ,ytitle:[ '|RH_ion-FIT|/Fit [Magnetic Jump error] ($\beta^{i}_1$ only)' ], YN:[ 'Fit Magnetic Jump error ($\beta^{i}_1$ only)' ], fn:[ 'Fit_error_i' ], binsize:[ .25 ], radian:[0],degree:[0]}

	store_data,'shock0Acc',data={x: t ,y: shock0Acc ,ytitle:[ '$N_{SN}^{AVG}\cdotN_{Conic}$' ], YN:[ '$N_{SN}^{AVG}\cdotN_{Conic}$' ], fn:[ "shock0acc" ], binsize:[ .2 ], radian:[0],degree:[0]}
	store_data,'shock0dist',data={x: t ,y: shock0dist ,ytitle:[ '|$R^{shock}_{FIT}-R^{Shock}_{Closest conic}$| [km]' ], YN:[ 'Distance from Conic' ], fn:[ "shock0dist" ], binsize:[ 100. ], radian:[0],degree:[0]}

	store_data,'SoundSpeed',data={x: t ,y: Cs ,ytitle:[ '$C_{sound}$ [km/s]' ], YN:[ 'Sound Speed' ], fn:[ 'Cs' ], binsize:[ 5. ], radian:[0],degree:[0]}
	store_data,'Alfven',data={x: t ,y: Alfven ,ytitle:[ '$V_A$ [km/s]' ], YN:[ 'Alfven Speed' ], fn:[ 'vA' ], binsize:[ 100. ], radian:[0],degree:[0]}

	store_data,"AlfCalc",data={x: t ,y: AlfCalc ,ytitle: '$V_A=B^{Meas}_{Up} \sqrt{\mu_0 m_p n^{ion}_{up}}$ [km/s]' , YN:'Calculated Alfven Speed', fn:'vAcalc', binsize:[ 100. ], radian:[0],degree:[0]}

	store_data,'fms',data={x: t ,y: fms ,ytitle:[ '$V_{fms}$ [km/s]' ], YN:[ 'fast Magnetosonic Speed' ], fn:[ 'fms' ], binsize:[ 100. ], radian:[0],degree:[0]}
	store_data,'fms_corr',data={x: t ,y: fms_corr ,ytitle:[ '$V_{fms}$ [km/s]' ], YN:[ 'fast Magnetosonic Speed' ], fn:[ 'fmsCorr' ], binsize:[ 100. ], radian:[0],degree:[0]}
	store_data,'MachSound',data={x: t ,y: Msound ,ytitle:[ '$M_{sound}$' ], YN:[ '$M_{sound}$' ], fn:[ 'Msound' ], binsize:[ 1. ], radian:[0],degree:[0]}
	store_data,'MachAlfven',data={x: t ,y: Malfven ,ytitle:[ '$M_{Alfven}$' ], YN:[ '$M_{Alfven}$' ], fn:[ 'M_A' ], binsize:[ 1. ], radian:[0],degree:[0]}
	store_data,'Vu',data={x: t ,y: vumag ,ytitle:[ '$v^{ion}$ [km/s]' ], YN:[ 'Ion Speed' ], fn:[ 'VionMag' ], binsize:[ 50. ], radian:[0],degree:[0]}
	store_data,'Velocity',data={x: t ,y: Vuvec_coarse ,ytitle:[ '$v^{ion}$ [km/s]' ], YN:[ 'Ion Velocity' ], fn:[ 'Vion' ], binsize:[ 50. ], radian:[0],degree:[0]}
	store_data,'Velocity_N',data={x: t ,y: v_normal ,ytitle:[ '$v^{ion}_{NB}$ [km/s]' ], YN:[ 'Shockward ion Velocity' ], fn:[ 'VionN' ], binsize:[ 50. ], radian:[0],degree:[0]}

	;store_data,'upDT',data={x:t,y:upDT/60.,ytitle:'minutes between shock and up measurement',YN:'Upstream Measure temporal displacement',fn:'upDT',binsize:.5, radian:[0],degree:[0]}
	;store_data,'downDT',data={x:t,y:downDT/60.,ytitle:'minutes between shock and down measurement',YN:'Downstream Measure temporal displacement',fn:'downDT',binsize:1., radian:[0],degree:[0]}

	store_data,'N_SN',data={x:t,y:N_SN,ytitle:'Shock Normal Vector',YN:'Shock Normal Vector',fn:'N_SN',binsize:.1,radian:[0],degree:[0]}

	store_data,'POS',data={x:t,y:POS,ytitle:'Shock position (MSO) [km]',YN:'Shock Position',fn:'POS',binsize:500,radian:[0],degree:[0]}

	store_data,'X_MSO',data={x:t,y:XMSO,ytitle:'shock X (MSO) [km]',YN:'Subsolar distance',fn:'Xmso',binsize:500,radian:[0],degree:[0]}

store_data,'Y_MSO',data={x:t,y:YMSO,ytitle:'shock Y (MSO) [km]',YN:'$\rm Y_{MSO}$',fn:'Ymso',binsize:500,radian:[0],degree:[0]}

store_data,'Z_MSO',data={x:t,y:ZMSO,ytitle:'shock Z (MSO) [km]',YN:'$\rm Z_{MSO}$',fn:'Zmso',binsize:500,radian:[0],degree:[0]}


	store_data,'RHO_MSO',data={x:t,y:RHO,ytitle:'shock $\rho=\sqrt{Y^2+Z^2}$ (MSO) [km]',YN:'Transverse shock distance',fn:'RHOmso',binsize:500,radian:[0],degree:[0]}

store_data,'RHO-0_MSO',data={x:t,y:sqrt(XMSO^2+YMSO^2),ytitle:'shock $\rho_0=\sqrt{X^2+Y^2}$ (MSO) [km]',YN:'Equatorial Distance',fn:'RHO-0mso',binsize:500,radian:[0],degree:[0]}

	store_data,'pointsperday',data={x:t,y:perdaylst,ytitle:"# saved crossings on day",YN:'Saved Crossings',fn:"numcrossings",binsize:[1],radian:[0],degree:[0]}


if 0 then begin
	store_data,'usPOS',data={x:t,y:usPOS,ytitle:'upstream start measure position (MSO) [km]',YN:'upstream position 1',fn:'usPOS',binsize:500,radian:[0],degree:[0]}
	store_data,'uPOS',data={x:t,y:umPOS,ytitle:'upstream measure position (MSO) [km]',YN:'upstream position',fn:'uPOS',binsize:500,radian:[0],degree:[0]}
	store_data,'uePOS',data={x:t,y:umPOS,ytitle:'upstream measure end position (MSO) [km]',YN:'upstream 	position 2',fn:'uePOS',binsize:500,radian:[0],degree:[0]}
store_data,'usALT',data={x:t,y:usALT,ytitle:'upstream start measure altitude (MSO) [km]',YN:'upstream position 1',fn:'usPOS',binsize:500,radian:[0],degree:[0]}
	store_data,'umALT',data={x:t,y:umALT,ytitle:'upstream measure altitude (MSO) [km]',YN:'Upstream Altitude',fn:'uALT',binsize:500,radian:[0],degree:[0]}
	store_data,'ueALT',data={x:t,y:ueALT,ytitle:'upstream measure end altitude (MSO) [km]',YN:'upstream 	altitude 2',fn:'ueALT',binsize:500,radian:[0],degree:[0]}
store_data,'usDX',data={x:t,y:usDX,ytitle:'distance between shock and upstream start (MSO) [km]',YN:'upstream displacement 1',fn:'usDX',binsize:500,radian:[0],degree:[0]}
	store_data,'uDX',data={x:t,y:umDX,ytitle:'distance between shock and upstream measurement (MSO) [km]',YN:'upstream displacement',fn:'uDX',binsize:500,radian:[0],degree:[0]}
	store_data,'ueDX',data={x:t,y:ueDX,ytitle:'distance between shock and upstream end (MSO) [km]',YN:'upstream displacement 2',fn:'ueDX',binsize:500,radian:[0],degree:[0]}

	store_data,'dsPOS',data={x:t,y:dsPOS,ytitle:'downstream start measure position (MSO) [km]',YN:'downstream position 1',fn:'dsPOS',binsize:500,radian:[0],degree:[0]}
	store_data,'dPOS',data={x:t,y:dmPOS,ytitle:'downstream measure position (MSO) [km]',YN:'downstream position',fn:'dPOS',binsize:500,radian:[0],degree:[0]}
	store_data,'dePOS',data={x:t,y:dePOS,ytitle:'downstream measure end position (MSO) [km]',YN:'downstream 	position 2',fn:'dePOS',binsize:20,radian:[0],degree:[0]}
store_data,'dsALT',data={x:t,y:dsALT,ytitle:'downstream start measure altitude (MSO) [km]',YN:'downstream position 1',fn:'dsALT',binsize:20,radian:[0],degree:[0]}
	store_data,'dALT',data={x:t,y:dmALT,ytitle:'downstream measure altitude (MSO) [km]',YN:'downstream Altitude',fn:'dALT',binsize:500,radian:[0],degree:[0]}
	store_data,'deALT',data={x:t,y:deALT,ytitle:'downstream measure end altitude (MSO) [km]',YN:'downstream 	altitude 2',fn:'deALT',binsize:20,radian:[0],degree:[0]}
store_data,'dsDX',data={x:t,y:dsDX,ytitle:'distance between shock and downstream start (MSO) [km]',YN:'downstream displacement 1',fn:'dsDX',binsize:20,radian:[0],degree:[0]}
	store_data,'dDX',data={x:t,y:dmDX,ytitle:'distance between shock and downstream measurement (MSO) [km]',YN:'downstream displacement',fn:'dDX',binsize:20,radian:[0],degree:[0]}
	store_data,'deDX',data={x:t,y:deDX,ytitle:'distance between shock and downstream end (MSO) [km]',YN:'downstream displacement 2',fn:'deDX',binsize:20,radian:[0],degree:[0]}
endif

	store_data,'Quasiperp',data={x:t,y:wheretoflag(HH0,N),YN:['Quasiperpendicular '], fn:['Quasiperp_']}
	store_data,'Quasipar',data={x:t,y:wheretoflag(nHH,N),YN:['Quasiparallel '], fn:['Quasipar_']}

	store_data,'thermal',data={x:t,y:wheretoflag(wherebigbeta,N),YN:['($\beta >$'+strtrim(highbetabound,2)+') '], fn:['thermal_pressure_dominating_']}
	
	store_data,'magneto',data={x:t,y:wheretoflag(wheresmallbeta,N),YN:['($\beta <$'+strtrim(lowbetabound,2)+') '], fn:['magnetic_pressure_dominating_']}
	

	store_data,'supercritical',data={x:t,y:wheretoflag(wherecritical,N),YN:['Supercritical '], fn:['supercritical_']}
	store_data,'subcritical',data={x:t,y:wheretoflag(wheresubcritical,N),YN:['Subcritical '], fn:['subcritical_']}

	YN='($\beta^{top}_{quart}\ge$'+strtrim(upperquartpart,2)+') '
	print,YN
	store_data,'where_beta_upperquartile',data={x:t,y:wheretoflag(wherebetalowerq,N),YN:[YN], fn:['upperbetaquart_']}
	YN='($\beta^{top}_{quart}\le$'+strtrim(lowerquartpart,2)+') '
	print,YN
	store_data,'where_beta_lowerquartile',data={x:t,y:wheretoflag(wherebetalowerq,N),YN:[YN], fn:['lowerbetaquart_']}



endif
	binsizes=[1.,1.,5.0,.25]

	
	

	subinters=[AllSubinter,Quasiperpinter,Quasiparinter,thermalinter,supercritinter,subcritinter]
	pos1=[0.10,0.22,0.9,0.9]
	cpos1=[.10,.09,.9,.17]
	for i=0,3 do print,"numel("+seasonNames0[i]+"thermal)=",numel(thermalseason[i])
	for i=0,3 do print,"numel("+seasonNames0[i]+"magneto)=",numel(magnetoseason[i])
	print,"goodpoint/points=",goodPoints,"/",points
	
	print,"total spring:",numel(springIndices)
	print,"total summer:",numel(summerIndices)
	print,"total autumn:",numel(autumnIndices)
	print,"total winter:",numel(winterIndices)
;print,"[badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badDensity,badMfms,badbeta,missedAnomolies,BadManual,missedManual,badCrusts,badMultis]"
	;print, [badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badDens,badMfms,badbeta,missedanomolies,BadManual,missedmanual,badCrusts,badMultis]
	print,"[badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta,badDensity]"
	print, badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta,badDens, format='(I7,",",I8,",",I7,",",I6,",",I7,",",I5,",",I10,",",I7,",",I7,",",I7)'
	print,"[missedAnomolies,BadManual,missedManual,Crust,Multi,Large Instability]"
	print,missedAnomolies,BadManual,missedManual,badCrusts,badMultis,badInstab,format='(I15,",",I9,",",I12,",",I5,",",I5,",",I17)'
	if keyword_set(manualPlotting) then begin

		REQ = ''
; Read input from the terminal:
		READ,REQ, PROMPT='Manually collect data?: Y N'

		if REQ eq 'Y' or REQ eq 'y' then begin
			srctxt="Documents/dataquantitieslist.txt"                                                     

			datalistreader,datalist,interlist,srctxt,numParameters=numParameters,numIntervals=numIntervals
			print,"done with datalistreader"
			mxln0=-1                                                               
			foreach el,datalist[0,*] do if strlen(el) gt mxln0 then mxln0=strlen(el)
			print,mxln0
			mxln1=-1                                                               
			foreach el,datalist[1,*] do if strlen(el) gt mxln1 then mxln1=strlen(el)

			mxln2=-1                                                               
			foreach el,datalist[2,*] do if strlen(el) gt mxln2 then mxln2=strlen(el)

			mxlni0=-1                                                               
			foreach el,interlist[0,*] do if strlen(el) gt mxlni0 then mxlni0=strlen(el)
			mxlni1=-1                                                               
			foreach el,interlist[1,*] do if strlen(el) gt mxlni1 then mxlni1=strlen(el)

			mxlni2=-1                                                               
			foreach el,interlist[2,*] do if strlen(el) gt mxlni2 then mxlni2=strlen(el)

			datformat='%-3d :: %-'+strtrim(mxln0,2)+'s : %-'+strtrim(mxln1,2)+'s , %-'+strtrim(mxln2,2)+'s'
			interformat='%-3d :: %-'+strtrim(mxlni0,2)+'s : %-'+strtrim(mxlni1,2)+'s , %-'+strtrim(mxlni2,2)+'s'
			print,datformat
			print,interformat
			alldatas=[FMdata,Mfmsdata,ThetaNBnddata, betadata, critdata, orbthetadata, LsRaddata, ThetaNBdata, altdata,$
   		 		  altNormdata, Lsdata, lmavendata, lmavenNormdata, tjuldata, SolDistdata, SolDistNormdata, downdata,$
		 		  downMdata,updata, upMdata, Bmaxdata, downflucdata, upflucdata, B2B1fitdata, B2B1Mdata,$
		 		  normOverdownfitdata, normOverdownMdata, normOverfitupdata, normOverupMdata, B2B1_RHdata,$
				  densityJumpData, betajumpdata, beta2data, B2B1fdiffdata, B2B1Mdiffdata, RH_errordata, Fit_errordata,$
				  shock0Accdata, shock0distdata, SoundSpeeddata, Alfvendata,fmsdata,MachSounddata,$
				  MachAlfvendata,velocitydata,betacalcdata,betafracdata,betafdiffdata]

				REQ = ''
				quitting=0
				while ~quitting do begin

				quitting=0
				redo=0
					ychosen=0
					xchosen=0
					datanum=-1
					internum=-1
					byseason=1
					interchosen=0
				while 1 do begin
					quitting=0
					redo=0
					print,' "LIST" to get list of quantities'
					print, '"QUIT" to exit'
					print, '"RESTART" to RESTART'
					print,'ENTER a data name, fnm, or line number to choose that as your Y value'
					READ,REQ, PROMPT='ENTER "LIST","QUIT","RESTART",data name,fnm,or data index: '
					;listchosen=0

					IF REQ.tolower() eq 'restart' then continue

					IF REQ.tolower() eq 'quit' then begin
						quitting=1
						break
					endif

					IF REQ.tolower() eq 'list' then begin
						print,"NUM","dataname","title","fnm",format='%-3s :: %-'+strtrim(mxln0,2)+'s :%-'+strtrim(mxln1,2)+'s , %-'+strtrim(mxln2,2)+'s'

						for i=0, numParameters-1 do begin
							print,i,datalist[*,i],format=datformat
						endfor
						continue
					endif


					IF REQ.matches("^[0-9]+$") then begin
						num=uint(REQ)
						print,num
						if num ge numParameters then continue else begin
							print,num
							ychosen=1
							datanum=num
							break
						endelse
					endif 	

					if not ychosen then begin
						LOC=(where(datalist eq REQ))[0]
						if LOC eq -1 then continue
						datanum=LOC/3
						ychosen=1
						break
					endif

					;break
				endwhile

				

				if quitting then begin
						READ,REQ, PROMPT='Automatically generate data? Y N: '
						if REQ eq 'Y' or REQ eq 'y' then break else return
				endif	
				print,datanum
				ydata=alldatas[datanum]
				YT=ydata[2];YTs[i]
				yfnm=ydata[3];yfns[i]
				Y=ydata[0];plotYs3[i]
				yttl=ydata[1];ytitles[i]

				print,datalist[2,datanum]
				print,yfnm
				if yfnm ne datalist[2,datanum] then begin
					print,"broken"
					return
				endif
				datanum=-1
				while 1 do begin
					quitting=0
					redo=0
					print,' "LIST" to get list of quantities'
					print, '"QUIT" to exit'
					print, '"RESTART" to RESTART'
					print,'ENTER a data name, fnm, or line number to choose that as your X value'
					READ,REQ, PROMPT='ENTER "LIST","QUIT","RESTART",data name,fnm,or data index: '
					;listchosen=0

					IF REQ.tolower() eq 'restart' then begin
						redo=1
						break
					endif

					IF REQ.tolower() eq 'quit' then begin
						quitting=1
						break
					endif

					IF REQ.tolower() eq 'list' then begin
						print,"NUM","dataname","title","fnm",format='%-3s :: %-'+strtrim(mxln0,2)+'s :%-'+strtrim(mxln1,2)+'s , %-'+strtrim(mxln2,2)+'s'


						for i=0, numParameters-1 do begin
							print,i,datalist[*,i],format=datformat
						endfor
						continue
					endif


					IF REQ.matches("^[0-9]+$") then begin
						num=uint(REQ)
						if num gt numParameters then continue else begin
							xchosen=1
							datanum=num
							break
						endelse
					endif 	

					if not xchosen then begin
						LOC=(where(datalist eq REQ))[0]
						if LOC eq -1 then continue
						datanum=LOC/3
						xchosen=1
					endif

					;break
				endwhile
	
				if redo eq 1 then continue
				if quitting then begin
						READ,REQ, PROMPT='Automatically generate data? Y N: '
						if REQ eq 'Y' or REQ eq 'y' then break else return
				endif	
				print,datanum
				xdata=alldatas[datanum]
				xttl=xdata[1];xttls[II]
				X=xdata[0];plotXs[II]
		;print,max(ThetaNBn),min(ThetaNBn)
				xfnm=xdata[3];fvsTitles[II]
				XT=xdata[2];vsTitles[II];'$\theta_{bn}$'
				binsize=xdata[4];binsizes[II]
				datanum=-1

				READ,REQ, PROMPT='Do you wish to use only a subset of the data? Y N: '
				if REQ eq 'Y' or REQ eq 'y' then begin
				while 1 do begin
					print,' "LIST" to get list of subsets'
					print, '"QUIT" to exit'
					print, '"CANCEL" to use everything'
					print, '"RESTART" to RESTART'
					print,'ENTER a data name, fnm, or line number to choose that as your Y value'
					READ,REQ, PROMPT='ENTER "LIST","QUIT","RESTART","CANCEL", data name,fnm,or data index: '
					;listchosen=0
					IF REQ.tolower() eq 'restart' then begin
						redo=1
						break
					endif

					IF REQ.tolower() eq 'cancel' then begin
						;redo=1
						break
					endif

					IF REQ.tolower() eq 'quit' then begin
						quitting=1
						break
					endif
					IF REQ.tolower() eq 'list' then begin
						print,"NUM","subsetname","title","fnm",format='%-3s :: %-'+strtrim(mxlni0,2)+'s :%-'+strtrim(mxlni1,2)+'s , %-'+strtrim(mxlni2,2)+'s'


						for i=0, numIntervals-1 do begin
							print,i,interlist[*,i],format=interformat
						endfor
						continue
					endif


					IF REQ.matches("^[0-9]+$") then begin
						num=uint(REQ)
						if num ge numIntervals then continue else begin
							interchosen=1
							internum=num
							break
						endelse
					endif 	

					if not interchosen then begin
						LOC=(where(interlist eq REQ))[0]
						if LOC eq -1 then continue
						internum=LOC/3
						interchosen=1
						break
					endif

					;break
				endwhile


				endif 
				if redo eq 1 then continue
				if quitting then begin
						READ,REQ, PROMPT='Automatically generate data? Y N: '
						if REQ eq 'Y' or REQ eq 'y' then break else return
				endif	
				if internum le 0 then subint=AllSubinter else subint=subinters[internum]
				subtitle=subint[1]
				subinterfname=subint[2]
				subinterval=subint[0]	
				READ,REQ, PROMPT='Color points by season or third parameter? Season Magnitude: '

				IF total(REQ.tolower() eq ['magnitude','mag','m']) eq 1 then begin
					while 1 do begin
					quitting=0
					redo=0
					;byseason=0
					print,' "LIST" to get list of quantities'
					print, '"QUIT" to exit'
					print, '"RESTART" to RESTART'
					print, '"CANCEL" to use seasons'

					print,'ENTER a data name, fnm, or line number to choose that as your Magnitude value'
					READ,REQ, PROMPT='ENTER "LIST","QUIT","RESTART",data name,fnm,or data index: '
					;listchosen=0

					IF REQ.tolower() eq 'restart' then begin
						redo=1
						break
					endif

					IF total(REQ.tolower() eq ['cancel' ,'season','seasons']) gt 0 then begin
						;redo=1
						break
					endif


					IF REQ.tolower() eq 'quit' then begin
						quitting=1
						break
					endif

					IF REQ.tolower() eq 'list' then begin
						print,"NUM","dataname","title","fnm",format='%-3s ::%-'+strtrim(mxln0,2)+'s :%-'+strtrim(mxln1,2)+'s , %-'+strtrim(mxln2,2)+'s'


						for i=0, numParameters-1 do begin
							print,i,datalist[*,i],format=datformat
						endfor
						continue
					endif


					IF REQ.matches("^[0-9]+$") then begin
						num=uint(REQ)
						if num gt numParameters then continue else begin
							byseason=0							
							datanum=num
							break
						endelse
					endif 	

					if  	~byseason then begin
						LOC=(where(datalist eq REQ))[0]
						if LOC eq -1 then continue
						datanum=LOC/3
						byseason=0
					endif

					;break
				endwhile
	
				if redo eq 1 then continue
				if quitting then begin
						READ,REQ, PROMPT='Automatically generate data? Y N: '
						if REQ eq 'Y' or REQ eq 'y' then break else return
				endif	


	
				endif

				if byseason then begin
						READ,REQ, PROMPT='Median Lines? 0 4 5: '
						domedian=uint(REQ)
						print,"seasonsubIndices"
					seasonsubIndices=list(intersect(springIndices,subinterval),intersect(summerIndices,subinterval),$	
						intersect(autumnIndices,subinterval),intersect(winterIndices,subinterval))
					print,"plotFName"
					plotFName=subinterfname+yfnm+"_vs_"+xfnm+corrText+".eps"
					plotlist=list()
					print,"TITLE="
					Title=YT+" vs "+XT
					for KK=0,3 do begin
						xsub=X[seasonsubIndices[KK]]
						ysub=Y[seasonsubIndices[KK]]
						plotlist.add,scatterplot(xsub,ysub,xtitle=xttl,ytitle=yttl,title=Title,$
						SYMBOL='.', /SYM_FILLED,POSITION=pltpos, $
						sym_color=seasonColors[KK],Name=seasonNames[KK],/overplot)
						print,"if domedian ne 0 then begin"
						if domedian ne 0 then begin
							maxval=max(xsub)
							mlow = findgen(maxval/binsize+1)*binsize
							mhigh = mlow + binsize
							medovershoot = fltarr(maxval/binsize+1)

							for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
			 					 w = where(xsub ge mlow[zzz] and xsub lt mhigh[zzz],nw)
  								if nw ge 2 then medovershoot[zzz] = median(/even,( ysub )[w])

							endfor
							nonzero=where(medovershoot ne 0.0,zzz)
							mlow2=mlow[nonzero]
							mhigh2=mhigh[nonzero]
							medovershoot2=medovershoot[nonzero]
							print,"if zzz gt 0 then"
							if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=seasonColors[KK],name="Median"+seasonNames2[KK],/overplot)

						endif

				;		maxval=max(xvals[seasonsubIndices[KK]])
					endfor
					if  xfnm eq "time"  then plotlist[0].xtickunits="time"
					if domedian eq 5 then begin
						maxval=max(X[subinterval])
						mlow = findgen(maxval/binsize+1)*binsize
						mhigh = mlow + binsize
						medovershoot = fltarr(maxval/binsize+1)
						for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
		 					 w = where(X[subinterval] ge mlow[zzz] and X[subinterval] lt mhigh[zzz],nw)
;  					if nw ge 2 then medovershoot[zzz] = median(/even,( (plotYs[JJ])[subinterval] )[w])
  							if nw ge 2 then medovershoot[zzz] = median(/even,( Y[subinterval] )[w])
						endfor
						nonzero=where(medovershoot ne 0.0,zzz)
						mlow2=mlow[nonzero]
						mhigh2=mhigh[nonzero]
						medovershoot2=medovershoot[nonzero]
						if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot)

		


					endif


					;ccf = LEGEND(target=plotlist, font_size=7,transparency=80);[p10,p11,p12,p13])
					ccf=plotlist[-1]
					fname=dire+plotFname
					ccf.save,fname,RESOLUTION=600
					plotlist[0].close
					

				endif else begin
					READ,REQ, PROMPT='Median Lines? Y N: '
					
					zdata=alldatas[datanum]
					Z=zdata[0]
					zttl=zdata[1];xttls[II]

		;print,max(ThetaNBn),min(ThetaNBn)
					zfnm=zdata[3];fvsTitles[II]
					ZT=zdata[2];vsTitles[II];'$\theta_{bn}$'
					Ysub=Y[subinterval]
					Xsub=X[subinterval]
					Zsub=Z[subinterval]
					Zbyt=bytscl(Zsub)
					Zmax=max(Zsub)
					Zmin=min(Zsub)
					plotFName=subinterfname+zfnm+"_vs_"+yfnm+"_vs_"+xfnm+corrText+".eps"
				
					
					pos11=[0.10,0.22,0.9,0.9]

					TITLE=ZT+" vs "+YT+" vs "+XT
					sctrFIT=scatterplot(Xsub,Ysub,symbol='.',/sym_filled,RGB_TABLE=62,xtitle=xttl,ytitle=yttl,title=TITLE,$
					POSITION=pos1,MAGNITUDE=Zbyt)

					IF REQ.tolower() eq 'y' then begin
						maxval=max(Xsub)
						mlow = findgen(maxval/binsize+1)*binsize
						mhigh = mlow + binsize
						medovershooty = fltarr(maxval/binsize+1)
						
						medovershootz = fltarr(maxval/binsize+1)
						for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
		 					 wy = where(Xsub ge mlow[zzz] and Xsub lt mhigh[zzz],nw)
;  					if nw ge 2 then medovershoot[zzz] = median(/even,( (plotYs[JJ])[subinterval] )[w])
  							if nw ge 2 then medovershooty[zzz] = median(/even,( Ysub )[w])
;  					if nw ge 2 then medovershoot[zzz] = median(/even,( (plotYs[JJ])[subinterval] )[w])
  							if nw ge 2 then medovershootz[zzz] = median(/even,( Zsub )[w])
						endfor
						
						mlow2=[mlow[0],mlow[0],mlow]
						mhigh2=[mhigh[0],mhigh[0],mhigh]
						medovershoot2y=[medovershooty[0],medovershooty[0],medovershooty]
						medovershoot2z=[Zmin,Zmax,medovershootz]



						if zzz gt 0 then medplot=plot((mlow2+mhigh2)/2.0, medovershoot2y,vert_color=bytscl(medovershoot2z),name="Median",/overplot)

		

					endif
	
					cbb2b2=colorbar(ORIENTATION=0,POSITION=cpos1, $; POSITION=cpos1, $
					title=zttl,RGB_TABLE=62,RANGE=[Zmin,Zmax])

		;dir='Documents/Plots/'+shockDate+'/'
					fname=dire+plotFname
					cbb2b2.save,fname,RESOLUTION=600
					sctrFIT.close
					zmax=max(Zsub,zmloc)
					print,"max("+zfnm+")="+strtrim(zmax,2)+" at ",time_string(t[subinterval[zmloc]])
				endelse
				ymax=max(Ysub,ymloc)
				xmax=max(Xsub,xmloc)
				print,"max("+xfnm+")="+strtrim(xmax,2)+" at ",time_string(t[subinterval[where(Xsub eq max(Xsub))]])
				print,"max("+yfnm+")="+strtrim(ymax,2)+" at ",time_string(t[subinterval[where(Ysub eq max(Ysub))]])
				print,downs[subinterval[ymloc]],ups[subinterval[ymloc]]
				READ,REQ, PROMPT='CONTINUE? Y N: '
				if REQ eq 'Y' or REQ eq 'y' then continue else begin
						READ,REQ, PROMPT='Automatically generate data? Y N: '
						if REQ eq 'Y' or REQ eq 'y' then break else return

				endelse
								endwhile
; Read input from the terminal:
			
		endif

	endif; else begin
		
		;READ,REQ, PROMPT='Automatically generate data? Y N: '
					;	if ~(REQ eq 'Y' or REQ eq 'y') then begin
						;	p1=scatterplot(tjul,size(Alven,/typ),symbol='.')
						;	p1.xtickunits="time"
						;	 return
						;endif
	;endelse

	ydatas=[B2B1fitdata,B2B1Mdata,normOverdownfitdata,normOverdownMdata,normOverfitupdata,normOverupMdata];,densityJumpData,downflucdata,upflucdata]


	xdatas=[FMdata,Mfmsdata,ThetaNBnddata,betadata];,updata]
	;BETA:
	;mlow = findgen((maxBeta)/.25+1)*.25;16)
	;mhigh = mlow + .25
	;medovershoot = fltarr((maxBeta)/.25+1)*.25;16)
	;THETA:
	;mlow = findgen((maxAngle)/5+1)*5.0;16)
	;mhigh = mlow + 5
	;medovershoot = fltarr((maxAngle)/5+1)*5.0;16) 
	;FM
	;mlow = findgen(maxViableFM+2);16)
	;mhigh = mlow + 1
	;medovershoot = fltarr(maxViableFM+2);16)

	;M
	;mlow = findgen(maxM+2);maxViableFM+2);16)
	;mhigh = mlow + 1
	;medovershoot = fltarr(maxM+2);maxViableFM+2);16)

	;IN GENERAL: 
	;mlow=findgen(maxVal/binsize+1)*binsize
	;mhigh=mlow+binsize
	;medovershoot=fltarr(maxVal/binsize+1)*binsize



	numPlots=numel(pfns)
	doClose=fltarr(numPlots)+1;[1,1,0,0,0,1,0]*0+1
	;print,"(sbins[1])[0]=",(sbins[1])[0]
	;print,"(sbins[1])[1]=",(sbins[1])[1]
	;print,"(sbins[1])[2]=",(sbins[1])[2]
	;print,"(sbins[1])[3]=",(sbins[1])[3]
	;print,"(sbins[1])[4]=",(sbins[1])[4]
	;print,"(sbins[1])[5]=",(sbins[1])[5]
	;print,"(sbins[1])[6]=",(sbins[1])[6]
	;print,"(sbins[1])[7]=",(sbins[1])[7]
	;p000=plot( (sbins[0])[0],(sbins[0])[1],name="spring",'g-')
	


	mlow = findgen(10)
	mhigh = mlow + 1
	medovershoot = fltarr(10)

	;for i = 0,9 do begin

 	;	 w = where((sbins[1])[0] ge mlow[i] and (sbins[1])[0] lt mhigh[i],nw)
  	;	if nw ge 2 then medovershoot[i] = median(/even,((sbins[1])[4])[w])

	;endfor

	mnFM=min(FM,fmnloc)
	print,time_string(t[fmnloc])
	print,mnFM
	print,(plotYs[1])[fmnloc] 

	b=where(plotYs[1] gt 1.2 and FM le 1.5)

	;b=sort(Mfms[springIndices])
	;MfmsSpringSort=Mfms[springIndices[b]]
	;sls=where(Mfms eq MfmsSpringSort[0])
	;xts=t[sls]
	;print,time_string(xts),",",downup[sls],MfmsSpringSort[0]
	;return
	;b=where(ThetaNBnd lt 25 and B2B1_Fit gt 3.3,bcount)
	;print,bcount
	;foreach el,b do begin
	;	print,time_string(t[el])
;		print,ThetaNBnd[el]
;		print,B2B1_Fit[el]
;	endforeach
	;return

	;pq=scatterplot(/buffer,(sbins[1])[0],(sbins[1])[4],sym_color='green',SYMBOL='*')
	;ps=plot((mlow2+mhigh2)/2.0, medovershoot2,'b-',/overplot)
	;pq.close
	;;print,medovershoot[11]
	print,"starting FM plots"

	vsTitle="M_fms/M_crit"

	binsize=1.
	xvals=Mfms
	yvals=B2B1_Fit
	if 0 then begin
	p1=scatterplot(Mfms,B2B1_Fit,xtitle='$M_{fms}$',ytitle='$B_{downstream}/B_{upstream}$',title='Magnetic Jump vs Fast Mach Number',$
						SYMBOL='.', /SYM_FILLED, $
						sym_color='black',name='$B_{Down}^{FIT}/B_{Up}^{FIT}$')
				maxval=max(Mfms)
				mlow = findgen(maxval/binsize+1)*binsize
				mhigh = mlow + binsize
				medovershoot = fltarr(maxval/binsize+1)
				for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
 					 w = where(xvals ge mlow[zzz] and xvals lt mhigh[zzz],nw)
;  					if nw ge 2 then medovershoot[zzz] = median(/even,( (plotYs[JJ])[subinterval] )[w])
  					if nw ge 2 then medovershoot[zzz] = median(/even,( yvals )[w])
				endfor
				nonzero=where(medovershoot ne 0.0,zzz)
				mlow2=mlow[nonzero]
				mhigh2=mhigh[nonzero]
				medovershoot2=medovershoot[nonzero]
				;if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot)

				medplot=plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median FIT",/overplot)


	yvals=B2B1_RH
	p2=scatterplot(Mfms,yvals,$
						SYMBOL='.', /SYM_FILLED, $
						sym_color='red',name='$(B_{Down}/B_{Up})_{RH}$',/over)
		maxval=max(Mfms)
				mlow = findgen(maxval/binsize+1)*binsize
				mhigh = mlow + binsize
				medovershoot = fltarr(maxval/binsize+1)
				for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
 					 w = where(xvals ge mlow[zzz] and xvals lt mhigh[zzz],nw)
;  					if nw ge 2 then medovershoot[zzz] = median(/even,( (plotYs[JJ])[subinterval] )[w])
  					if nw ge 2 then medovershoot[zzz] = median(/even,( yvals )[w])
				endfor
				nonzero=where(medovershoot ne 0.0,zzz)
				mlow2=mlow[nonzero]
				mhigh2=mhigh[nonzero]
				medovershoot2=medovershoot[nonzero]
				;if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot)

				medplot2=plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.scarlet,name="Median RH",/overplot)

	ccf = LEGEND(target=[p1,p12,medplot1,medplot2], font_size=7,transparency=50);[p10,p11,p12,p13])
	ccf.save,dire+"FIT_AND_RH_VS_MFMS.png", BORDER=10, $
  				 RESOLUTION=400

	p1.close
	endif
	if 0 then begin




	;for II=0,4-1 do begin
	foreach xdata,xdatas do begin
		xttl=xdata[1];xttls[II]
		xvals=xdata[0];plotXs[II]
		;print,max(ThetaNBn),min(ThetaNBn)
		fvsTitle=xdata[3];fvsTitles[II]
		vsTitle=xdata[2];vsTitles[II];'$\theta_{bn}$'
		binsize=xdata[4];binsizes[II]
		
	;plotTitlesTH=["overshoot difference","fractional overshoot difference","fitted downstream/upstream B's",$
	;"fractional overVsDown", "fractional overVsUp","over-Down","over-Up vs "]+vsTitle
		for LL=0,numel(subinterfnames)-1 do begin
			subtitle=subinterTitles[LL]
			subinterfname=subinterfnames[LL]
			subinterval=subintervals[LL]
			if (fvsTitle eq "Crit_Ratio" and total(subinterfname eq ['supercritical_','subcritical_']) ne 0) or $
			   (fvsTitle eq 'theta' and total(subinterfname eq ['Quasiperp_','Quasipar_']) ne 0) or $
			   (fvsTitle eq 'beta' and total(subinterfname eq ['thermal_pressure_dominating_','magnetic_pressure_dominating_']) ne 0) then continue
			seasonsubIndices=list(intersect(springIndices,subinterval),intersect(summerIndices,subinterval),$	
						intersect(autumnIndices,subinterval),intersect(winterIndices,subinterval))
			plotFNames=subinterfname+pfns+"_vs_"+fvsTitle+corrText+".png"
			;maxAngle=max(ThetaNBnd)
			print,"starting "+subinterfname+" "+fvsTitle+" plots"
			;for JJ=0,numPlots-1 do begin
			foreach ydata,ydatas do begin
				;if JJ ne 1 and JJ ne 4 then continue
				plotlist=list()
				xsub=xvals[seasonsubIndices[0]]
				yvals=ydata[0]
				ysub=yvals[seasonsubIndices[0]];(plotYs[JJ])[seasonsubIndices[0]]
				YT=ydata[2]
				plotFName=subinterfname+ydata[3]+"_vs_"+fvsTitle+corrText+".png"
				pl0=scatterplot(/buffer,xsub,ysub,SYMBOL='.', /SYM_FILLED,POSITION=pltpos, $
				XTITLE=xttl,sym_color="green",Name=seasonNames[0],YTITLE=ydata[1],$; YTITLE=plotYTitles[JJ]
				TITLE=subtitle+YT+" vs "+vsTitle);plotTitles[JJ]+" vs "+vsTitle)
				plotlist.add,pl0
				maxval=max(xvals[seasonsubIndices[0]])
				mlow = findgen(maxval/binsize+1)*binsize
				mhigh = mlow + binsize
				medovershoot = fltarr(maxval/binsize+1);16)

				for jjj = 0,numel(mlow)-1 do begin;15 do begin
	
	 				 w = where((xsub) ge mlow[jjj] and (xsub) lt mhigh[jjj],nw)
	  				if nw ge 2 then medovershoot[jjj] = median(/even, (ysub)[w])

				endfor
				nonzero=where(medovershoot ne 0.0,zzz)
				mlow2=mlow[nonzero]
				mhigh2=mhigh[nonzero]
				medovershoot2=medovershoot[nonzero]
				if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=seasonColors[0],name="Median"+seasonNames2[0],/overplot)
				if 0 and numel(ysub) gt 1 then begin
					measure_errors = SQRT(ABS(ysub))
					result = LINFIT(xsub, ysub, MEASURE_ERRORS=measure_errors)
					bccc=fltarr(numel(xsub))
					bccc+=measure_errors[0]
					ux=findgen(30.)*(max(xsub)-min(xsub))/(29.)+min(xsub)
					linyy=ux*result[1]+result[0]

					plotlist.add,plot(ux,linyy,linestyle=2,color=seasonColors2[0],/overplot,Name=seasonNames[0]+"linear fit")
				endif
			;plotlist.add,plot( (sbins[0])[0],(sbins[0])[JJ+1], /overplot,name="spring",'g-')

				for KK=1,3 do begin
					;plotlist.add,plot( (sbins[0])[KK],(sbins[JJ+1])[KK], /overplot,color=seasonColors[KK],name="MEDIAN "+seasonNames[KK],'-')
					xsub=xvals[seasonsubIndices[KK]]
					ysub=yvals[seasonsubIndices[KK]];(plotYs[JJ])[seasonsubIndices[KK]]
					plotlist.add,scatterplot(xsub,$
					ysub,$
					SYMBOL='.', /SYM_FILLED,POSITION=pltpos, $
					sym_color=seasonColors[KK],Name=seasonNames[KK],/overplot)
					maxval=max(xvals[seasonsubIndices[KK]])
					mlow = findgen(maxval/binsize+1)*binsize
					mhigh = mlow + binsize
					medovershoot = fltarr(maxval/binsize+1)

					for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
 					 w = where(xsub ge mlow[zzz] and xsub lt mhigh[zzz],nw)
  					if nw ge 2 then medovershoot[zzz] = median(/even,( ysub )[w])

					endfor
					nonzero=where(medovershoot ne 0.0,zzz)
					mlow2=mlow[nonzero]
					mhigh2=mhigh[nonzero]
					medovershoot2=medovershoot[nonzero]
					if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=seasonColors[KK],name="Median"+seasonNames2[KK],/overplot)
					if 0 and numel(xsub) gt 1 then begin
						measure_errors = SQRT(ABS(ysub))
						result = LINFIT(xsub, ysub, MEASURE_ERRORS=measure_errors)
						bccc=fltarr(numel(xsub))
						bccc+=measure_errors[0]
						ux=findgen(30.)*(max(xsub)-min(xsub))/(29.)+min(xsub)
						linyy=ux*result[1]+result[0]

						plotlist.add,plot(ux,linyy,linestyle=2,color=seasonColors2[KK],/overplot,Name=seasonNames0[KK]+"linear fit")
					endif

				endfor	

				maxval=max(xvals[subinterval])
				mlow = findgen(maxval/binsize+1)*binsize
				mhigh = mlow + binsize
				medovershoot = fltarr(maxval/binsize+1)
				for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
 					 w = where(xvals[subinterval] ge mlow[zzz] and xvals[subinterval] lt mhigh[zzz],nw)
;  					if nw ge 2 then medovershoot[zzz] = median(/even,( (plotYs[JJ])[subinterval] )[w])
  					if nw ge 2 then medovershoot[zzz] = median(/even,( yvals[subinterval] )[w])
				endfor
				nonzero=where(medovershoot ne 0.0,zzz)
				mlow2=mlow[nonzero]
				mhigh2=mhigh[nonzero]
				medovershoot2=medovershoot[nonzero]
				if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot)

				medplot=plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot)

			;ccf = LEGEND(target=plotlist, font_size=7,transparency=50);[p10,p11,p12,p13])

			;ccf.save,dire+"All "+	plotFNames[JJ] , BORDER=10, $
				print,plotFName
				;medplot.save,dire+"All "+plotFNames[JJ] , BORDER=10, $
				medplot.save,dire+"All "+plotFName, BORDER=10, $
  				 RESOLUTION=300

				pl0.close
			endforeach
		endfor

		endforeach
endif

print,"Median"+seasonNames0[0]+" RH"
medstr="Median"+seasonNames0[0]+" RH"

if 1 then begin

		for II=0,4-1 do begin
		xttl=xttls[II]
		xvals=plotXs[II]
		;print,max(ThetaNBn),min(ThetaNBn)
		fvsTitle=fvsTitles[II]
		vsTitle=vsTitles[II];'$\theta_{bn}$'
		binsize=binsizes[II]
		
	;plotTitlesTH=["overshoot difference","fractional overshoot difference","fitted downstream/upstream B's",$
	;"fractional overVsDown", "fractional overVsUp","over-Down","over-Up vs "]+vsTitle
		for LL=0,numel(subinterfnames)-1 do begin
			subtitle=subinterTitles[LL]
			subinterfname=subinterfnames[LL]
			subinterval=subintervals[LL]
			;if (fvsTitle eq "Crit_Ratio" and total(subinterfname eq ['supercritical_','subcritical_']) ne 0) or $
			 ;  (fvsTitle eq 'theta' and total(subinterfname eq ['Quasiperp_','Quasipar_']) ne 0) or $
			  ; (fvsTitle eq 'beta' and total(subinterfname eq ['thermal_pressure_dominating_','magnetic_pressure_dominating_']) ne 0) then continue
			seasonsubIndices=list(intersect(springIndices,subinterval),intersect(summerIndices,subinterval),$	
						intersect(autumnIndices,subinterval),intersect(winterIndices,subinterval))
			plotFNames=subinterfname+pfns+"and_RH_vs_"+fvsTitle+corrText+".png"
			;maxAngle=max(ThetaNBnd)
			print,"starting "+subinterfname+" "+fvsTitle+" plots"
			for jn=0,1 do begin
				JJ=jn*3
				plotlist=list()
				xsub=xvals[seasonsubIndices[0]]
				ysub=(plotYs[JJ])[seasonsubIndices[0]]
				ysubRH=B2B1_RH[seasonsubIndices[0]]
				pl0=scatterplot(/buffer,xsub,ysub,SYMBOL='D',sym_size=.2, /SYM_FILLED,POSITION=pltpos, $
				XTITLE=xttl, YTITLE=plotYTitles[JJ]+',$|B_{Down}/B_{up}|^{RH}$',sym_color="green",Name=seasonNames0[0]+(['fitted','measured'])[jn],$
				TITLE=subtitle+(["Fitted","Measured"])[jn]+'and RH calculated Magnetic shock jump vs '+vsTitle)
				plotlist.add,pl0


				plotlist.add,scatterplot(/buffer,xsub,ysubRH,SYMBOL='S',sym_size=.2, /SYM_FILLED,POSITION=pltpos, $
				sym_color=seasonColors2[0],Name=seasonNames0[0]+' RH',/over)
		
				maxval=max(xvals[seasonsubIndices[0]])
				mlow = findgen(maxval/binsize+1)*binsize
				mhigh = mlow + binsize
				medovershoot = fltarr(maxval/binsize+1);16)

				for jjj = 0,numel(mlow)-1 do begin;15 do begin
	
	 				 w = where((xsub) ge mlow[jjj] and (xsub) lt mhigh[jjj],nw)
	  				if nw ge 2 then medovershoot[jjj] = median(/even, (ysub)[w])

				endfor
				nonzero=where(medovershoot ne 0.0,zzz)
				mlow2=mlow[nonzero]
				mhigh2=mhigh[nonzero]
				medovershoot2=medovershoot[nonzero]
				if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=seasonColors[0],name="Median"+seasonNames0[0],/overplot)
				medovershoot = fltarr(maxval/binsize+1);16)

				for jjj = 0,numel(mlow)-1 do begin;15 do begin
	
	 				 w = where((xsub) ge mlow[jjj] and (xsub) lt mhigh[jjj],nw)
	  				if nw ge 2 then medovershoot[jjj] = median(/even, (ysubRH)[w])

				endfor
				nonzero=where(medovershoot ne 0.0,zzz)
				mlow2=mlow[nonzero]
				mhigh2=mhigh[nonzero]
				medovershoot2=medovershoot[nonzero]
				if zzz gt 0 then plotlist.add,plot(/buffer,(mlow2+mhigh2)/2.0,medovershoot2,color=seasonColors2[0],name='Median'+seasonNames0[0]+' RH',/overplot,linestyle=2)
			;plotlist.add,plot( (sbins[0])[0],(sbins[0])[JJ+1], /overplot,name="spring",'g-')

				for KK=1,3 do begin
					;plotlist.add,plot( (sbins[0])[KK],(sbins[JJ+1])[KK], /overplot,color=seasonColors[KK],name="MEDIAN "+seasonNames[KK],'-')
					xsub=xvals[seasonsubIndices[KK]]
					ysub=(plotYs[JJ])[seasonsubIndices[KK]]
					ysubRH=B2B1_RH[seasonsubIndices[KK]]
					plotlist.add,scatterplot(xsub,$
					ysub,$
					SYMBOL='D',sym_size=.2, /SYM_FILLED,POSITION=pltpos, $
					sym_color=seasonColors[KK],Name=seasonNames0[KK]+(['fitted','measured'])[jn],/over);,$
					plotlist.add,scatterplot(xsub,$
					ysubRH,$
					SYMBOL='S',sym_size=.2, /SYM_FILLED,POSITION=pltpos, $
					sym_color=seasonColors2[KK],Name=seasonNames0[KK]+" RH",/over)
					;TITLE=subtitle+plotTitles[JJ]+vsTitle,/overplot)
					maxval=max(xvals[seasonsubIndices[KK]])
					mlow = findgen(maxval/binsize+1)*binsize
					mhigh = mlow + binsize
					medovershoot = fltarr(maxval/binsize+1)

					for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
 					 w = where(xsub ge mlow[zzz] and xsub lt mhigh[zzz],nw)
  					if nw ge 2 then medovershoot[zzz] = median(/even,( ysub )[w])

					endfor
					nonzero=where(medovershoot ne 0.0,zzz)
					mlow2=mlow[nonzero]
					mhigh2=mhigh[nonzero]
					medovershoot2=medovershoot[nonzero]
					if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=seasonColors[KK],name="Median"+seasonNames0[KK],/overplot)

					medovershoot = fltarr(maxval/binsize+1)

					for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
 					 w = where(xsub ge mlow[zzz] and xsub lt mhigh[zzz],nw)
  					if nw ge 2 then medovershoot[zzz] = median(/even,( ysubRH )[w])

					endfor
					nonzero=where(medovershoot ne 0.0,zzz)
					mlow2=mlow[nonzero]
					mhigh2=mhigh[nonzero]
					medovershoot2=medovershoot[nonzero]
					if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0,linestyle=2, medovershoot2,color=seasonColors2[KK],name="Median"+seasonNames0[KK]+" RH",/overplot)



				endfor	

				maxval=max(xvals[subinterval])
				mlow = findgen(maxval/binsize+1)*binsize
				mhigh = mlow + binsize
				medovershoot = fltarr(maxval/binsize+1)
				for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
 					 w = where(xvals[subinterval] ge mlow[zzz] and xvals[subinterval] lt mhigh[zzz],nw)
  					if nw ge 2 then medovershoot[zzz] = median(/even,( (plotYs[JJ])[subinterval] )[w])

				endfor
				nonzero=where(medovershoot ne 0.0,zzz)
				mlow2=mlow[nonzero]
				mhigh2=mhigh[nonzero]
				medovershoot2=medovershoot[nonzero]
				if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot)

				medovershoot = fltarr(maxval/binsize+1)
				for zzz = 0,numel(mlow)-1 do begin;15 do begin
	
 					 w = where(xvals[subinterval] ge mlow[zzz] and xvals[subinterval] lt mhigh[zzz],nw)
  					if nw ge 2 then medovershoot[zzz] = median(/even,( B2B1_RH[subinterval] )[w])

				endfor
				nonzero=where(medovershoot ne 0.0,zzz)
				mlow2=mlow[nonzero]
				mhigh2=mhigh[nonzero]
				medovershoot2=medovershoot[nonzero]
				if zzz gt 0 then plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.dark_slate_grey,name="Median RH",/overplot,linestyle=2)

				;medplot=plot((mlow2+mhigh2)/2.0, medovershoot2,color=!color.slate_grey,name="Median",/overplot)

				ccf = LEGEND(target=plotlist, font_size=7,transparency=50);[p10,p11,p12,p13])

			;ccf.save,dire+"All "+	plotFNames[JJ] , BORDER=10, $
				print,plotFNames[JJ]
				ccf.save,dire+"All "+plotFNames[JJ] , BORDER=10, $

  				 RESOLUTION=300

				pl0.close
			endfor
		endfor

		endfor


	endif


		;;;;;;NOW FOR  B2/B1 scatterplots
	print,"NOW FOR  B2/B1 scatterplots"

	;B2B1_RH=B2B1_Fit*0.0


	for i=0,numel(B2B1_Fit)-1 do begin


			;th=ThetaNB[i]
			;ourBta=betas[i]

			RH_parameters,Mfms[i],ThetaNBn[i],betas[i],a,b,c,d,yy,delta

			B2B1_RH[i]=SQRT((b+c*delta)/(b+c))
	endfor
	print,"max(B2B1_RH)=",max(B2B1_RH)
	

	b2b1Max=max([max(B2B1_Fit),max(B2B1_RH)])
	b2b1Min=min([min(B2B1_Fit),min(B2B1_RH)])

	ThetaNBnd_dummy=[ThetaNBnd,mean([min(ThetaNBnd),0]),max(ThetaNBnd)]
	Mfms_dummy=[Mfms,mean([min(Mfms),0]),max(Mfms)]
	betas_dummy=[betas,mean([min(betas),0]),max(betas)]


	B2B1_Fit_dummy=[B2B1_Fit,b2b1Min,b2b1Max]
	B2B1_RH_dummy=[B2B1_RH,b2b1Min,b2b1Max]

	

	B2B1_Fit_byt=(255+.999)*(B2B1_Fit_dummy-b2b1Min)/(b2b1Max-b2b1Min)
	B2B1_RH_byt=(255+.999)*(B2B1_RH_dummy-b2b1Min)/(b2b1Max-b2b1Min)
	print,"[min(B2B1_Fit_byt),max(B2B1_Fit_byt)]=",[min(B2B1_Fit_byt),max(B2B1_Fit_byt)]
	print,"[min(B2B1_RH_byt),max(B2B1_RH_byt)]=",[min(B2B1_RH_byt),max(B2B1_RH_byt)]
	cpos2=[0.22,0.05,0.29,0.9]

	;sctr1RH0=scatterplot(Mfms,ThetaNBnd,symbol='.',/sym_filled,RGB_TABLE=62,xtitle="Mach Number",ytitle="$\theta_{bn}$ [deg]",title='|B2|/|B1| via RH',MAGNITUDE=bytscl(B2B1_RH))
	;cbb2b10=colorbar(ORIENTATION=1, $
			; POSITION=[0.22,0.05,0.29,0.9], $
			;title='|B2|/|B1|',RGB_TABLE=62,RANGE=[min(B2B1_RH),max(B2B1_RH)])
	;sctr1FIT0=scatterplot(Mfms,ThetaNBnd,symbol='.',/sym_filled,RGB_TABLE=62,xtitle="Mach Number",ytitle="$\theta_{bn}$ [deg]",title='|B2|/|B1| via fit',MAGNITUDE=bytscl(B2B1_Fit))
	;cbb2b102=colorbar(ORIENTATION=1, $
			; POSITION=[0.22,0.05,0.29,0.9], $
			;title='|B2|/|B1|',RGB_TABLE=62,RANGE=[min(B2B1_Fit),max(B2B1_Fit)])

	sctr1RH=scatterplot(Mfms_dummy,ThetaNBnd_dummy,symbol='.',/sym_filled,RGB_TABLE=62,xtitle="Mach Number",ytitle='$\theta_{bn}$ [deg]',title='|B2|/|B1| via RH',MAGNITUDE=B2B1_RH_byt,LAYOUT=[3,1,2],/buffer)
	sctr1RHd1=scatterplot(Mfms_dummy[-2],ThetaNBnd_dummy[-2],symbol='*',sym_color='silver',/over,LAYOUT=[3,1,2])
	sctr1RHd2=scatterplot(Mfms_dummy[-1],ThetaNBnd_dummy[-1],symbol='*',sym_color='gold',/over,LAYOUT=[3,1,2])
	sctr1FIT=scatterplot(Mfms_dummy,ThetaNBnd_dummy,symbol='.',/sym_filled,RGB_TABLE=62,xtitle="Mach Number",ytitle='$\theta_{bn} [deg]$',title='|B2|/|B1| via fit',$
			/CURRENT,MAGNITUDE=B2B1_Fit_byt,LAYOUT=[3,1,3])
	sctr1FITd1=scatterplot(Mfms_dummy[-2],ThetaNBnd_dummy[-2],symbol='*',sym_color='silver',/over,LAYOUT=[3,1,3])
	sctr1FITd2=scatterplot(Mfms_dummy[-1],ThetaNBnd_dummy[-1],symbol='*',sym_color='gold',/over,LAYOUT=[3,1,3])

	
		cbb2b1=colorbar(ORIENTATION=1, $
			 POSITION=[0.22,0.05,0.29,0.9], $
			title='|B2|/|B1|',RGB_TABLE=62,RANGE=[b2b1Min,b2b1Max])

		;dir='Documents/Plots/'+shockDate+'/'
		fname=dire+"ALL_B2B1Scatters.png"
		cbb2b1.save,fname,RESOLUTION=800
		sctr1RH.close

;;;;;;;NOW USING THETA AND BETA

sctr1RH=scatterplot(/buffer,betas_dummy,ThetaNBnd_dummy,symbol='.',/sym_filled,RGB_TABLE=62,xtitle='$\beta$',ytitle='$\theta_{bn}$ [deg]',title='|B2|/|B1| via RH',MAGNITUDE=B2B1_RH_byt,LAYOUT=[3,1,2])
	sctr1RHd1=scatterplot(betas_dummy[-2],ThetaNBnd_dummy[-2],symbol='*',sym_color='silver',/over,LAYOUT=[3,1,2])
	sctr1RHd2=scatterplot(betas_dummy[-1],ThetaNBnd_dummy[-1],symbol='*',sym_color='gold',/over,LAYOUT=[3,1,2])
	sctr1FIT=scatterplot(betas_dummy,ThetaNBnd_dummy,symbol='.',/sym_filled,RGB_TABLE=62,xtitle='$\beta$',ytitle='$\theta_{bn} [deg]$',title='|B2|/|B1| via fit',$
			/CURRENT,MAGNITUDE=B2B1_Fit_byt,LAYOUT=[3,1,3])
	sctr1FITd1=scatterplot(betas_dummy[-2],ThetaNBnd_dummy[-2],symbol='*',sym_color='silver',/over,LAYOUT=[3,1,3])
	sctr1FITd2=scatterplot(betas_dummy[-1],ThetaNBnd_dummy[-1],symbol='*',sym_color='gold',/over,LAYOUT=[3,1,3])

	
		cbb2b1=colorbar(ORIENTATION=1, $
			 POSITION=[0.22,0.05,0.29,0.9], $
			title='|B2|/|B1|',RGB_TABLE=62,RANGE=[b2b1Min,b2b1Max])

		;dir='Documents/Plots/'+shockDate+'/'
		fname=dire+"ALL_B2B1Scatters_vs_theta_vs_beta.png"
		cbb2b1.save,fname,RESOLUTION=800
		sctr1RH.close



;;;;;;;;;;NOW VS MEASURE
print,"NOW VS MEASURE"
	B2B1fdiff=200*abs(B2B1_Fit-B2B1_RH)/abs(B2B1_Fit+B2B1_RH)
	B2B1Mdiff=200*abs(B2B1_Measure-B2B1_RH)/abs(B2B1_Measure+B2B1_RH)
	B2B1FMdiff=200*abs(B2B1_Measure-B2B1_Fit)/abs(B2B1_Measure+B2B1_Fit)




	b2b1Max=max([max(B2B1_Measure),max(B2B1_RH)])
	b2b1Min=min([min(B2B1_Measure),min(B2B1_RH)])

	B2B1_Measure_dummy=[B2B1_Measure,b2b1Min,b2b1Max]
	B2B1_RH_dummy=[B2B1_RH,b2b1Min,b2b1Max]

	B2B1_Measure_byt=(255+.999)*(B2B1_Measure_dummy-b2b1Min)/(b2b1Max-b2b1Min)
	B2B1_RH_byt=(255+.999)*(B2B1_RH_dummy-b2b1Min)/(b2b1Max-b2b1Min)

	cpos2=[0.22,0.05,0.29,0.9]

	sctr1RH=scatterplot(/buffer,Mfms_dummy,ThetaNBnd_dummy,symbol='.',/sym_filled,RGB_TABLE=62,xtitle="Mach Number",ytitle='$\theta_{bn}$ [deg]',title='|B2|/|B1| via RH',MAGNITUDE=B2B1_RH_byt,LAYOUT=[3,1,2])
	sctr1FIT=scatterplot(Mfms_dummy,ThetaNBnd_dummy,symbol='.',/sym_filled,RGB_TABLE=62,xtitle="Mach Number",ytitle='$\theta_{bn} [deg]$',title='|B2|/|B1| via Measure',$
			/CURRENT,MAGNITUDE=B2B1_Measure_byt,LAYOUT=[3,1,3])

	
		cbb2b1=colorbar(ORIENTATION=1, $
			 POSITION=[0.22,0.05,0.29,0.9], $
			title='|B2|/|B1|',RGB_TABLE=62,RANGE=[b2b1Min,b2b1Max])

		;dir='Documents/Plots/'+shockDate+'/'
		fname=dire+"ALL_B2B1MeasureScatters.png"
		cbb2b1.save,fname,RESOLUTION=800
		sctr1RH.close

;;;Now FIT on its own
print,"Now FIT on its own"

	pos1=[0.10,0.22,0.9,0.9]
	cpos1=[.10,.09,.9,.17]
	pos11=[0.10,0.22,0.9,0.9]
	sctr2FIT=scatterplot(/buffer,Mfms,ThetaNBnd,symbol='.',/sym_filled,RGB_TABLE=62,xtitle="Mach Number",ytitle='$\theta_{bn} [deg]$',title='|B2|/|B1| via fit',$
			POSITION=pos1,MAGNITUDE=BYTSCL(B2B1_Fit))

	
			cbb2b2=colorbar(ORIENTATION=0,POSITION=cpos1, $; POSITION=cpos1, $
			title='|B2|/|B1| fit',RGB_TABLE=62,RANGE=[min(B2B1_Fit),max(B2B1_Fit)])

		;dir='Documents/Plots/'+shockDate+'/'
		fname=dire+"ALL_B2B1FitScatters.png"
		cbb2b2.save,fname,RESOLUTION=800
		sctr2FIT.close
	;RETURN


	;;;;;NOW FOR ACCURACY, Distance scatterplots,
print,"NOW FOR ACCURACY, Distance scatterplots"

	b2b1fdiff=2*abs(B2B1_Fit-B2B1_RH)/abs(B2B1_Fit+B2B1_RH)
	B2B1scatterdiff=scatterplot(/buffer,shock0dist,shock0Acc,symbol='.',/sym_filled,RGB_TABLE=62,xtitle='distance from conic',$
					ytitle="$\bf n_{Shock0conic}(closest_point)\cdot \bf{n}_{calculatedShock}$",$
					MAGNITUDE=bytscl(b2b1fdiff),position=pos1)
	cb2b1sctr1=colorbar(range=[min(b2b1fdiff),max(b2b1fdiff)],$;target=cntr10, $
			title='2*abs(RH-FIT)/(RH+FIT)',RGB_TABLE=62,position=cpos1)
	cb2b1sctr1.save,dire+"All_shock0AccVsDistVsBfdiff.png",RESOLUTION=800
	B2B1scatterdiff.close

if 1 then begin

	

	plotYs2=list(downMs,upMs,B2B1_Measure,B2B1_RH ,B2B1_RH     ,shock0Acc,B2B1fdiff,B2B1Mdiff,2*(downs-downMs)/(downs+downMs),B2B1fdiff,downs,B2B1_Fit ,Ls ,ThetaNBnd,B2B1fdiff,downs,alt)
	
	plotXs2=list(downs ,ups ,B2B1_Fit    ,B2B1_Fit,B2B1_Measure,ThetaNBnd  ,shock0Acc,shock0Acc,shock0Acc                  ,shock0dist,ups,Ls,tjul,tjul,B2B1_Fit,Mfms,tjul)
	ytitles=list('$B^{Measured}_{DOWN}$ [nT]','$B^{Measured}_{UP}$ [nT]','$B^{Measured}_{DOWN}$/$B^{Measured}_{UP}$','$(B_D/B_U)_{RH}$','$(B_D/B_U)_{RH}$','$N_{SW}^{AVG}\cdotN_{Conic}$','$B_d$/$B_u$ % diff(Fit, RH)','$B_d$/$B_u$ % diff(Measured, RH)','$2|B^M_D-B^F_D|/|B^M_D+B^F_D|\times100%$','B_d$/$B_u$ % diff(Fit, RH)','$B^{FIT}_{DOWN}$ [nT]','$B^{FIT}_{DOWN}$/$B^{FIT}_{UP}$','Ls [Deg]','$\theta_{BN}$ [deg]','$B_d$/$B_u$ % diff(Fit, RH)','$B^{FIT}_{Down}$ [nT]','Altitude [km]')
	xtitles=list('$B^{FIT}_{DOWN}$ [nT]','$B^{FIT}_{UP}$ [nT]','$B^{FIT}_{DOWN}$/$B^{FIT}_{UP}$','$B^{FIT}_{DOWN}$/$B^{FIT}_{UP}$','$B^{Measured}_{DOWN}$/$B^{Measured}_{UP}$','$\theta_{BN}$ [deg]','$N_{SW}^{AVG}\cdotN_{Conic}$','$N_{SW}^{AVG}\cdotN_{Conic}$','$N_{SW}^{AVG}\cdotN_{Conic}$','|$R^{shock}_{FIT}-R^{Shock}_{Closest conic}$| [km]','$B^{FIT}_{UP}$ [nT]','Ls [Deg]','time of crossing','time of crossing','$B^{FIT}_{DOWN}$/$B^{FIT}_{UP}$','$M_{fms}$','time of crossing')
	yfn=list("down_measured","up_measured","B2B1Measured","B2B1_RH","B2B1_RH","shock0acc","fit_rankine_B2B1_percentdiff","measure_rankine_B2B1_percentdiff","fit_measure_down_percentdiff","fit_rankine_B2B1_percentdiff","down_fit","MagShockJump","Ls","Theta","fit_rankine_B2B1_percentdiff","down_fit",'alt')
xfn=list("down_fit","up_fit","B2B1_Fit","B2B1_Fit","B2B1Measured","theta","shock0acc","shock0acc","shock0acc","shock0dist","up_fit","Ls","time","time","B2B1_Fit","Mfms",'time')
	numplots2=numel(xfn)
	PRINT,numel(plotXs2)
	PRINT,numel(plotYs2)
	print,numel(yfn)
	print,numel(xfn)
	print,numel(xtitles)
	print,numel(ytitles)

	defsize=1
	size0=.1

	print,"max(B2B1_RH)=",max(B2B1_RH)

	for JJ=0,numplots2-1 do begin
			;if xfn[JJ] ne 'time' and yfn[JJ] ne "Ls" then continue
			xs=(plotXs2[JJ])[springIndices]
			maxBeta=max((plotXs2[JJ])[springIndices])
			plotlist=list()
			pl0=scatterplot(/buffer,((plotXs2[JJ])[springIndices]),(plotYs2[JJ])[springIndices],SYMBOL='.', /SYM_FILLED,POSITION=pltpos, $
			XTITLE=xtitles[JJ], YTITLE=ytitles[JJ],sym_color="green",Name=seasonNames[0],$
			TITLE=ytitles[JJ]+' vs '+xtitles[JJ])

			if  xfn[JJ] eq "time"  then pl0.xtickunits="time"
			plotlist.add,pl0
			;mlow = findgen((maxBeta+2)*4)/4.0;16)
			;mhigh = mlow + .25
			;medovershoot = fltarr((maxBeta+2)*4);16)
			if  xfn[JJ] eq "time" and  yfn[JJ] eq "Ls" then pl0.sym_size=size0;*perdaylst[springIndices];then begin
				;print,pl0.sym_size
				;if  yfn[JJ] eq "Ls" then pl0.sym_size=size0*perdaylst[springIndices]

				;if yfn[JJ] eq "Theta" then 
			;endif
			;for jjj = 0,(maxBeta+2)*4-1 do begin;15 do begin
	
 			;	 w = where((betas[springIndices]) ge mlow[jjj] and (betas[springIndices]) lt mhigh[jjj],nw)
  			;	if nw ge 2 then medovershoot[jjj] = median(/even, ( (plotYs[JJ])[springIndices] )[w])

			;endfor

			;plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=seasonColors[0],name="Median"+seasonNames2[0],/overplot)

			;plotlist.add,plot( (sbins[0])[0],(sbins[0])[JJ+1], /overplot,name="spring",'g-')

			for KK=1,3 do begin
				;plotlist.add,plot( (sbins[0])[KK],(sbins[JJ+1])[KK], /overplot,color=seasonColors[KK],name="MEDIAN "+seasonNames[KK],'-')
				p1=scatterplot((plotXs2[JJ])[seasonIndices[KK]],$
				(plotYs2[JJ])[seasonIndices[KK]],$
				SYMBOL='.', /SYM_FILLED,POSITION=pltpos, $
			XTITLE=xtitles[JJ], YTITLE=ytitles[JJ],sym_color=seasonColors[KK],Name=seasonNames[KK],$
			TITLE=ytitles[JJ]+" vs "+xtitles[JJ],/overplot)
			;mlow = findgen((maxBeta+2)*4)/4.0;16)
			;mhigh = mlow + .25
			;medovershoot = fltarr((maxBeta+2)*4);16)
				if  xfn[JJ] eq "time" and yfn[JJ] eq "Ls" then begin
					;print,p1.sym_size
					p1.sym_size=size0;*perdaylst[seasonIndices[KK]]
				endif




				plotlist.add,p1
			;for zzz = 0,(maxBeta+2)*4-1 do begin;15 do begin
	
 			;	 w = where(betas[seasonIndices[KK]] ge mlow[zzz] and betas[seasonIndices[KK]] lt mhigh[zzz],nw)
  			;	if nw ge 2 then medovershoot[zzz] = median(/even,( (plotYs[JJ])[seasonIndices[KK]]  )[w])

			;endfor

			;plotlist.add,plot((mlow2+mhigh2)/2.0, medovershoot2,color=seasonColors[KK],name="Median"+seasonNames2[KK],/overplot)


			endfor	
			if  xfn[JJ] eq "up_fit" then begin

				for KK=0,3 do begin
					dd=(plotYs2[JJ])[seasonIndices[KK]]
					uu=ups[seasonIndices[KK]]
					measure_errors = SQRT(ABS(dd))
					result = LINFIT(uu, dd, MEASURE_ERRORS=measure_errors)
					bccc=fltarr(numel(uu))
					bccc+=measure_errors[0]
					ux=findgen(30.)*(max(uu)-min(uu))/(29.)+min(uu)
					lindd=ux*result[1]+result[0]

					plotlist.add,plot(ux,lindd,linestyle=2,color=seasonColors[KK],/overplot,Name=seasonNames[KK]+"linear fit")
				endfor


			endif

			

			ccf = LEGEND(target=plotlist, font_size=7,transparency=50);[p10,p11,p12,p13])
			if  xfn[JJ] eq "time"  then begin
				
				pl0.xtickunits="time"
				for LL=0,ssnchanges-1 do begin
					tbound=(ssnbound[LL])[0]
					tjbound=x2jul(tbound)
					pressn=(ssnbound[LL])[1]
					postssn=(ssnbound[LL])[2]
					prename=seasonNames0[pressn]
					postname=seasonNames0[postssn]
					print,time_string(tbound),prename,postname
					;print,time_string(jul2x(tbound+.05),postname
					plotlist.add,plot(fltarr(2) + tjbound-.05, p1.yrange,color=seasonColors[pressn], /overplot)
					plotlist.add,plot(fltarr(2) + tjbound+.05, p1.yrange,color=seasonColors[postssn], /overplot)
					if yfn[JJ] eq "Ls" then plotlist.add,text(/data,/over,tbound-.2,max(plotYs2[JJ])/2,prename,ORIENTATION=0,color=seasonColors[pressn])
					if yfn[JJ] eq "Ls" then plotlist.add,text(/data,/over,tbound+.2,max(plotYs2[JJ])/2,postname,ORIENTATION=0,color=seasonColors[postssn])
				endfor
				
			endif

			if yfn[JJ] eq "B2B1_RH" then begin
				lineline=findgen(4000)/1000.
				plotlist.add,plot(lineline,lineline,linestyle=2,/over)
			endif


			fname="All_"+	yfn[JJ]+"_vs_"+ xfn[JJ]+".png"
			print,"now saving: "+fname
			ccf.save,dire+fname, BORDER=10, $

  			 RESOLUTION=300

			pl0.close
			;if  xfn[JJ] eq "time"  then return
	endfor

endif

if 1 then begin
	

	plotYs3=list(RH_error,     Fit_error,       b2b1fdiff)
	ytitles=list('|RH-FIT|/RH [Magnetic Jump error]','|RH-FIT|/FIT [Magnetic Jump error]','$B_d$/$B_u$ % diff(Fit, RH)')
	yfns=list('RH_error',      'Fit_error',	"fit_rankine_B2B1_percentdiff")
	
	YTs=list('RH Magnetic Jump error','Fit Magnetic Jump error','RH Fit Magnetic jump % difference')

	plotXs3=list(B2B1_Fit,B2B1_RH,Bmax,(Bmax-downs)/downs,ThetaNBnd,betas,Mfms,FM,crits)
	xtitles=list('$B^{FIT}_{DOWN}$/$B^{FIT}_{UP}$','$B^{RH}_{DOWN}$/$B^{RH}_{UP}$','$B_{max}$ [nT]','($B_{Max} - B^{Fit}_{Down}$)/$B^{Fit}_{Down}$','$\theta_{NB1}$ [Deg]','$\beta$','$M_{fms}$','$M_{fms}/M_{crit}$','$M_{crit}$')
	xfns=list('B2B1_Fit','B2B1_RH','Bmax','Normalized_overshoot_height','theta','beta','Mfms','crit_ratio','Mcrit')
	XTs=list('FIT Magnetic Jump','RH Predicted Magnetic Jump','$B_{max}$','Normalized Overshoot Height','Shock Normal Angle','Plasma Beta','$M_{fms}$','Critical Ratio','Critical Mach Number')


	RH_errordata=list(RH_error,'|RH-FIT|/RH [Magnetic Jump error]','RH Magnetic Jump error','RH_error',.25)
	Fit_errordata=list(Fit_error,'|RH-FIT|/Fit [Magnetic Jump error]','Fit Magnetic Jump error','Fit_error',.25)
	b2b1diff_errordata=list(b2b1fdiff,'$B_d$/$B_u$ % diff(Fit, RH)','RH Fit Magnetic jump % difference',"fit_rankine_B2B1_percentdiff",.1)
	Bmaxdata=list(Bmax,'$B_{max}$ [nT]','Overshoot Maximum','Bmax',5.)
	critdata=list(crits,'$M_{crit}$','Critical Mach Number','Mcrit',.1)

	ydatas=[RH_errordata,Fit_errordata,b2b1diff_errordata]
	xdatas=[B2B1fitdata,B2B1_RHdata,Bmaxdata,normOverdownfitdata,ThetaNBnddata,betadata,Mfmsdata,FMdata,critdata]

	;For i=0,numel(YTs)-1 do begin
	FOREACH ydata,ydatas do begin

		
		YT=ydata[2];YTs[i]
		yfn=ydata[3];yfns[i]
		Y=ydata[0];plotYs3[i]
		ytitle=ydata[1];ytitles[i]

		;for j=0, numel(XTs)-1 do begin
		foreach xdata,xdatas do begin
			XT=xdata[2];XTs[j]
			xtitle=xdata[1];xtitles[j]
			X=xdata[0];plotXs3[j]
			xfn=xdata[3];xfns[j]

			PLOT_TITLE=YT+" vs "+XT
			fname='All_'+yfn+'_vs_'+xfn+'.png'

			plotlist=list()

			for k=0,3 do begin
				plotlist.add,scatterplot(X[seasonIndices[k]],Y[seasonIndices[k]],SYMBOL='.', /SYM_FILLED,POSITION=pltpos, $
			XTITLE=xtitle, YTITLE=ytitle,sym_color=seasonColors[k],Name=seasonNames0[k],$
			TITLE=PLOT_TITLE,/overplot,/buffer)


			endfor
			ccf = LEGEND(target=plotlist, font_size=7,transparency=50)
			
			print,"now saving: "+fname
			ccf.save,dire+fname, BORDER=10, $

  			 RESOLUTION=300

			plotlist[0].close


		endforeach	


	endforeach
endif


if 1 then begin
	;plotYs4=list(RH_error,     Fit_error,       b2b1fdiff,B2B1_Fit,B2B1_RH,Bmax,(Bmax-downs)/downs,ThetaNBnd,betas,Mfms,FM,crits,alt,l_MAVEN,densityjump_RH,betajump)
	;ytitles=list('|RH-FIT|/RH [Magnetic Jump error]','|RH-FIT|/FIT [Magnetic Jump error]','$B_d$/$B_u$ % diff(Fit, RH)','$B^{FIT}_{DOWN}$/$B^{FIT}_{UP}$','$B^{RH}_{DOWN}$/$B^{RH}_{UP}$','$B_{max}$ [nT]','($B_{Max} - B^{Fit}_{Down}$)/$B^{Fit}_{Down}$','$\theta_{NB1}$ [Deg]','$\beta$','$M_{fms}$','$M_{fms}/M_{crit}$','$M_{crit}$','Altitude [km]','$\ell=(ALT\prime+1.026X_{MSO}\prime), ALT\prime\equiv ALT(X_{MSO}\prime)=ALT(X_{MSO}-.6 R_M)$','$[\rho_D /\rho_U ]_{RH}$','$[\beta_D/\beta_U]_{RH}$')
	;yfns=list('RH_error',      'Fit_error',	"fit_rankine_B2B1_percentdiff",'B2B1_Fit','B2B1_RH','Bmax','Normalized_overshoot_height','theta','beta','Mfms','crit_ratio','Mcrit','alt','latus_rectum','densityjump','betajump')
	
	;YTs=list('RH Magnetic Jump error','Fit Magnetic Jump error','RH Fit Magnetic jump % difference','FIT Magnetic Jump','RH Predicted Magnetic Jump','$B_{max}$','Normalized Overshoot Height','Shock Normal Angle','Plasma Beta','$M_{fms}$','Critical Ratio','Critical Mach Number','Altitude',"Shock's Latus Rectum","RH predicted Density Jump","RH predicted beta jump")

	

	ydatas=[RH_errordata,Fit_errordata,B2B1fdiffdata,B2B1fitdata,B2B1_RHdata,normOverdownfitdata,ThetaNBnddata,betadata,Mfmsdata,FMdata,critdata,altNormdata,lmavenNormdata,densityJumpData,betajumpdata]

	;For i=0,numel(YTs)-1 do begin
	foreach ydata,ydatas do begin
		YT=ydata[2];YTs[i]
		yfn=ydata[3];yfns[i]
		Y=ydata[0];plotYs4[i]
		ytitle=ydata[1];ytitles[i]

		
		XT="Mars Solar Longitude"
		xtitle="Ls [deg]"
		X=Ls
		xfn='Ls'

		PLOT_TITLE=YT+" vs "+XT
		fname='All_'+yfn+'_vs_'+xfn+'.png'

		plotlist=list()

		for k=0,3 do begin
			plotlist.add,scatterplot(X[seasonIndices[k]],Y[seasonIndices[k]],SYMBOL='.', /SYM_FILLED,POSITION=pltpos, $
			XTITLE=xtitle, YTITLE=ytitle,sym_color=seasonColors[k],Name=seasonNames0[k],$
			TITLE=PLOT_TITLE,/overplot,/buffer)


		endfor
		ccf = LEGEND(target=plotlist, font_size=7,transparency=50)
			
		print,"now saving: "+fname
		ccf.save,dire+fname, BORDER=10, $

  		RESOLUTION=300

		plotlist[0].close

	endforeach
endif


;;;;;WHISTLERS

if 0 then begin


	whereSPfastreal=-1
	whereSUfastreal=-1
	whereAUfastreal=-1
	whereWIfastreal=-1

	wbarfast=SQRT(.5*(  (1+betas)+ sqrt( (1+betas)^2 -4*betas*costh^2)))
	whererealfast=where(finite(wbarfast) eq 1,numfinitefms)
	

	



	
	;pos1=[0.10,0.22,0.9,0.9]
;	cpos1=[.10,.09,.9,.17]
	;pos11=[0.10,0.22,0.9,0.9]

	if 0 and numfinitefms gt 0 then begin
		wbarfastreal=wbarfast[whererealfast]
	
		whereSPfastreal=intersect(springIndices,whererealfast)
		whereSUfastreal=intersect(summerIndices,whererealfast)
		whereAUfastreal=intersect(autumnIndices,whererealfast)
		whereWIfastreal=intersect(winterIndices,whererealfast)

		fastseasonindices=list(whereSPfastreal,whereSUfastreal,whereAUfastreal,whereWIfastreal,whererealfast)

		seasonColors.ADD,"grey"
		seasonNames0.add,"All"
		seasonNames.add,"All"
		seasonIndices.add,findgen(N)


		for II=0,4 do begin
			
			print,"for "+seasonNames0[II]+', $\bar{\omega}_{fast}$ is real for '+strtrim(numel(fastseasonindices[II]),2)+"/"+strtrim(numel(seasonIndices[II]),2)+'points.'
			if numel(fastseasonindices[II]) eq 0 or (fastseasonindices[II])[0] eq -1 then continue
			seasonwbar=wbarfast[fastseasonindices[II]]
			
			
			wbarplot=scatterplot(ThetaNBnd[fastseasonindices[II]],betas[fastseasonindices[II]],SYMBOL='.',$
			/sym_filled,RGB_TABLE=62,XTITLE='$\theta_{NB_1}', YTITLE='\beta',TITLE=seasonNames0[II]+' $\bar{\omega}_{fast}=\frac{\omega_{fast}}{k v_A}$ vs Plasma Beta vs Shock Normal Angle',$
					MAGNITUDE=bytscl(seasonwbar),position=pos1)
			wbartext=text(/relative,.4,.1,'$\bar{\omega}_{fast}=\frac{\omega_{fast}}{k v_A}=( ( 1+\beta+((1+\beta)^2-4\beta^2 cos^2 \theta)^{1/2} ) /2 )^{1/2}$')
					cbwbar=colorbar(range=[min(seasonwbar),max(seasonwbar)],$;target=cntr10, $
			title='$\bar{\omega}_{fast}=\frac{\omega_{fast}}{k v_A}=( ( 1+\beta+((1+\beta)^2-4\beta^2 cos^2 \theta)^{1/2} ) /2 )^{1/2}$',RGB_TABLE=62,position=cpos1)

			wbartext=text(/relative,.2,.1,'real solutions to $\bar{\omega}_{fast}=\frac{\omega_{fast}}{k v_A}=( ( 1+\beta+((1+\beta)^2-4\beta^2 cos^2 \theta)^{1/2} ) /2 )^{1/2}$. Real for'+strtrim(numel(fastseasonindices[II]),2)+"/"+strtrim(numel(seasonIndices[II]),2)+'points')

			fname=seasonNames0[II]+"omegabar-fast_vs_beta_vs_theta.png"
			print,"now saving: "+fname
			wbartext.save,dire+fname, BORDER=10, $

  			 RESOLUTION=300

			wbarplot.close
		endfor

	endif
	

	seasonsym=['tr','tu','tl','td']
	whistlerRadX= Mfms *cos(ThetaNB)
	whistlerRadY= Mfms *sin(ThetaNB)
	whistlerRadZ= betas

	plotlist=list()
	for KK=0,3 do begin
			plotlist.add,scatterplot([whistlerRadX[seasonIndices[KK]],0,0],[whistlerRadY[seasonIndices[KK]],0,0],SYMBOL=seasonsym[KK],$
			/sym_filled,RGB_TABLE=62,XTITLE='$M_{fms} cos\theta_{NB_1}$', YTITLE='$M_{fms} sin\theta_{NB_1}$',TITLE="Maximum $M_{fms}$ for Leading Whistler Wavetrains",$
					MAGNITUDE=bytscl([whistlerRadZ[seasonIndices[KK]],min(betas),max(betas)]),position=pos1,/over,name=seasonNames[0],sym_size=.2)
			;print,"for "+seasonNames0[II]+', $\bar{\omega}_{fast}$ is real for '+strtrim(numel(fastseasonindices[II]),2)+"/"+strtrim(numel(seasonIndices[II]),2)+'points.'
			;if numel(fastseasonindices[II]) eq 0 or (fastseasonindices[II])[0] eq -1 then continue
			
	endfor
	whislegend = LEGEND(target=plotlist, font_size=7,transparency=50)
	wplt1=scatterplot(0,0,symbol='*',sym_color='silver',/over)
	whislcbar=colorbar(range=[min(betas),max(betas)],$;target=cntr10, $
			title='plasma beta',RGB_TABLE=62,position=cpos1)
	;wbarslow=SQRT(.5*(  (1+betas)- sqrt( (1+betas)^2 -4*betas*costh^2)))
	;whererealslow=where(finite(wbarslow),numfinitesms)
	;wbarslowreal=wbarslow[whererealslow]
	fname="ALL_Polar_Whistler_speeds.png"
			print,"now saving: "+fname
			whislcbar.save,dire+fname, BORDER=10, $

  			 RESOLUTION=400

	plotlist[0].close

endif

	

	Rvals=list(betas,SolDist/semimajor,FM,l_MAVEN,ThetaNBnd)
	Rtexts=list('$\beta$','Normalized Subsolar distance [km]','Critical Ratio',"Shock's Normalized Latus Rectum",'$\theta_{NB1}$ [deg]')
	Rtitles=list('$\beta$','$R_{orbit}/Semimajor_{orbit}$','$M/M_{crit}$','$\ell/R_M=(ALT\prime+1.026X_{MSO}\prime)/R_M, ALT\prime\equiv ALT(X_{MSO}\prime)=ALT(X_{MSO}-.6 R_M)$','$\theta_{NB1}$')
	Rfns=list('beta','solDist','CriticalRatio','latusRectum','theta')



	;SolDistdata=list(SolDist,'$R_{orb}$ [km]','Subsolar distance [km]','solDist',.2*semimajor)
	;SolDistNormdata=list(SolDist/semimajor,'$R_{orbit} [Semimajor_{orbit}]$','Subsolar distance','solDistNormalized',.2)

	;Rdatas=[betadata,SolDistNormdata,FMdata,lmavenNormdata,ThetaNBdata]

	THvals=list(Ls*!pi/180.,orbitTheta,ThetaNB)
	THtexts=list('LS [rad]','$\Theta_{orb}$ [deg]','$\theta_{NB1}$ [rad]')
	THtitles=list('LS','$\Theta_{orb}$','$\theta_{NB1}$')
	THfns=list('LS','ThetaOrb','theta')


	;THdatas=[LsRaddata,orbthetadata,ThetaNBdata]

	Zvals=list(normalOverheight,B2B1_Fit,FM,l_MAVEN)
	Ztexts=list('Normalized overshoot height','Magnetic Jump','Critical Ratio',"Shock's Normalized Latus Rectum")
	Ztitles=list('$(B_{max}-B^{FIT}_{Down})/B^{FIT}_{Down}$','$B^{FIT}_{Down}/B^{FIT}_{Up}$','$M/M_{crit}$','$\ell/R_M=(ALT\prime+1.026X_{MSO}\prime)/R_M, ALT\prime\equiv ALT(X_{MSO}\prime)=ALT(X_{MSO}-.6 R_M)$')
	Zfns=list('Normalized_overshoot_height','Magnetic_Jump','CriticalRatio','latusRectum')

	;Zdatas=[normOverdownfitdata,B2B1fitdata,FMdata,lmavenNormdata]

	FOR II=0,numel(Zfns)-1 do begin
		;if II lt 1 then II=1
		;if II gt 1 then return;II=1
		Zval=Zvals[II]
		Ztext=Ztexts[II]
		Ztitle=Ztitles[II]
		Zfn=Zfns[II]
		Zbyt=bytscl(Zval)
		if Zfn eq 'Magnetic_Jump' then begin
			print,max(Zval),min(Zval)
			print,max(Zbyt),min(Zbyt)
			;return
		endif
		for JJ=0,numel(Rfns)-1 do begin
			Rfn=Rfns[JJ]
			if Rfn eq Zfn then continue
			Rval=Rvals[JJ]
			Rtext=Rtexts[JJ]
			Rtitle=Rtitles[JJ]
			FOR KK=0, numel(THfns)-1 do begin
				THfn=THfns[KK]
				if Rfn eq THfn then continue


				THval=THvals[KK]
				THtext=THtexts[KK]
				THtitle=THtitles[KK]


				;X=Rval*cos(THval)
				;Y=Rval*sin(THval)
				xtitle="Radial coord := "+Rtitle;Rtitle+"cos("+THtitle+")"
				ytitle="Angular coord :="+THtitle;Rtitle+"sin("+THtitle+")"
				TITLE=Ztext+" vs "+Rtext+" vs "+THtext+" Polar Plot"
				fname=Zfn+"_vs_"+Rfn+"_vs_"+THfn+"_PolarPlot.eps"

				p0=polarplot(Rval,THval,linestyle=6,symbol='.',title=title,xtitle=xtitle,ytitle=ytitle,vert_colors=bytscl(Zval),position=pos1,/sym_filled,RGB_TABLE=62,/buffer)
				cbar=colorbar(range=[min(Zval),max(Zval)],target=p0, $
					title=Ztitle,RGB_TABLE=62,position=cpos1)
				print,"now saving: "+fname
				cbar.save,dire+fname, BORDER=10, $

  			 	RESOLUTION=400

				p0.close

			endfor



		endfor




	endfor
	print,"goodpoint/points=",goodPoints,"/",points
	
	print,"total spring:",numel(springIndices)
	print,"total summer:",numel(summerIndices)
	print,"total autumn:",numel(autumnIndices)
	print,"total winter:",numel(winterIndices)
;print,"[badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badDensity,badMfms,badbeta,missedAnomolies,BadManual,missedManual,badCrusts,badMultis]"
	;print, [badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badDens,badMfms,badbeta,missedanomolies,BadManual,missedmanual,badCrusts,badMultis]
	print,"[badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta,badDensity]"
	print, badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta,badDens, format='(I7,",",I8,",",I7,",",I6,",",I7,",",I5,",",I10,",",I7,",",I7,",",I7)'
	print,"[missedAnomolies,BadManual,missedManual,Crust,Multi,Large Instability]"
	print,missedAnomolies,BadManual,missedManual,badCrusts,badMultis,badInstab,format='(I15,",",I9,",",I12,",",I5,",",I5,",",I17)'
return


	plotlist=list()

	for KK=0,3 do begin
			plotlist.add,scatterplot([ThetaNBnd[seasonIndices[KK]],0,0],[betas[seasonIndices[KK]],0,0],SYMBOL=seasonsym[KK],$
			/sym_filled,RGB_TABLE=62,XTITLE='$\theta_{NB1}$', YTITLE='$\beta$',TITLE="Maximum $M_{fms}$ for Leading Whistler Wavetrains",$
					MAGNITUDE=bytscl([Mfms[seasonIndices[KK]],min(Mfms),max(Mfms)]),position=pos1,/over,name=seasonNames[0],sym_size=.2)
			;print,"for "+seasonNames0[II]+', $\bar{\omega}_{fast}$ is real for '+strtrim(numel(fastseasonindices[II]),2)+"/"+strtrim(numel(seasonIndices[II]),2)+'points.'
			;if numel(fastseasonindices[II]) eq 0 or (fastseasonindices[II])[0] eq -1 then continue
			
	endfor
	whislegend = LEGEND(target=plotlist, font_size=7,transparency=50)
	wplt1=scatterplot(0,0,symbol='*',sym_color='silver',/over)
	
	whislcbar=colorbar(range=[min(Mfms),max(Mfms)],$;target=cntr10, $
			title='Fast Mach Number',RGB_TABLE=62,position=cpos1)
	;wbarslow=SQRT(.5*(  (1+betas)- sqrt( (1+betas)^2 -4*betas*costh^2)))
	;whererealslow=where(finite(wbarslow),numfinitesms)
	;wbarslowreal=wbarslow[whererealslow]
	fname="ALL_Whistler_speeds.png"
			print,"now saving: "+fname
			whislcbar.save,dire+fname, BORDER=10, $

  			 RESOLUTION=400

	plotlist[0].close

	maxApo=max(SolDist)
	minPer=min(SolDist)

	for II=0,4-1 do begin

		subIndices=seasonIndices[II]
		allbows=scatterplot([XMSO[subIndices],0,0],[RHO[subIndices],0,0],/sym_filled,RGB_TABLE=62,XTITLE='$X_{MSO}$ [km]', YTITLE='$\sqrt{Y^2+Z^2}$ [km]',TITLE="BOW SHOCK CROSSINGS IN"+seasonNames0[II],$
					MAGNITUDE=bytscl([SolDist[subIndices],minPer,maxApo]),position=pos1,sym_size=.2)
		bowcbar=colorbar(target=allbows,range=[minPer,maxApo],$;target=cntr10, $
			title='Distance from Sun [km]',RGB_TABLE=62,position=cpos1)


		pmars=plot(R_mars *cos(findgen(180)*!pi/180),R_mars*sin(findgen(180)*!pi/180),/overplot,'r-')
		wplt1=scatterplot(0,0,symbol='*',sym_color='silver',/over)

fname="ALL_BOW_SHOCK_CROSSINGS_in_"+seasonNames0[II]+".png"
			print,"now saving: "+fname
			wplt1.save,dire+fname, BORDER=10, $

  			 RESOLUTION=400

		allbows.close

	endfor

	plotlist=list()
	for JJ=0,1 do begin
		II=JJ*2
		subIndices=seasonIndices[II]
		plotlist.add,scatterplot(XMSO[subIndices],RHO[subIndices],/sym_filled,XTITLE='$X_{MSO}$ [km]', YTITLE='$\sqrt{Y^2+Z^2}$ [km]',TITLE="BOW SHOCK CROSSINGS FOR SPRING AND AUTUMN",name=seasonNames0[II]+" crossings",$
					sym_color=seasonColors[II],$
				position=pos1,sym_size=.1,/over)
	endfor

	whislegend = LEGEND(target=plotlist, font_size=7,transparency=50)

	pmars=plot(R_mars *cos(findgen(180)*!pi/180),R_mars*sin(findgen(180)*!pi/180),/overplot,'r-')
	fname="BOW_SHOCK_CROSSINGS_SEASONAL_DISTANCES.png"
			print,"now saving: "+fname
			pmars.save,dire+fname, BORDER=10, $

  			 RESOLUTION=400

	plotlist[0].close

titles=list('near periapsis, $\Theta_{ORB} <45\deg$','where $45\deg<=\Theta_{ORB} <90\deg$','where $90\deg<=\Theta_{ORB} <135\deg$','near apoapsis, $\Theta_{ORB} >=135\deg$')
fns=list('whereperi','wherelower','wherehigher','whereapo')
	for II=0,4-1 do begin

		subIndices=seasonIndices[II]
		allbows=scatterplot([XMSO[subIndices],0,0],[RHO[subIndices],0,0],/sym_filled,RGB_TABLE=62,XTITLE='$X_{MSO}$ [km]', YTITLE='$\sqrt{Y^2+Z^2}$ [km]',TITLE="BOW SHOCK Crossings "+titles[II],$
					MAGNITUDE=bytscl([SolDist[subIndices],minPer,maxApo]),position=pos1,sym_size=.2)
		bowcbar=colorbar(target=allbows,range=[minPer,maxApo],$;target=cntr10, $
			title='Distance from Sun [km]',RGB_TABLE=62,position=cpos1)
		wbartext=text(/relative,.2,.1,'$\Theta_{ORB}=180\deg -abs(180\deg-((LS-71\deg)$ MOD$ 360\deg))$')
		pmars=plot(R_mars *cos(findgen(180)*!pi/180),R_mars*sin(findgen(180)*!pi/180),/overplot,'r-')
		wplt1=scatterplot(0,0,symbol='*',sym_color='silver',/over)

fname="ALL_BOW_SHOCK_CROSSINGS_"+fns[II]+".png"
			print,"now saving: "+fname
			wplt1.save,dire+fname, BORDER=10, $

  			 RESOLUTION=400

		allbows.close

	endfor

	allbows=scatterplot(XMSO,RHO,/sym_filled,RGB_TABLE=62,XTITLE='$X_{MSO}$ [km]', YTITLE='$\sqrt{Y^2+Z^2}$ [km]',TITLE="BOW SHOCK CROSSINGS AT VARIABLE LS",$
					MAGNITUDE=bytscl(SolDist),position=pos1,sym_size=.2)
	bowcbar=colorbar(target=allbows,range=[min(SolDist),max(SolDist)],$;target=cntr10, $
			title='Distance from Sun',RGB_TABLE=62,position=cpos1)


	fname="ALL_BOW_SHOCK_CROSSINGS_VS_DISTANCE.png"
			print,"now saving: "+fname
			bowcbar.save,dire+fname, BORDER=10, $

  			 RESOLUTION=400

	allbows.close

	allbows=scatterplot(XMSO,RHO,/sym_filled,RGB_TABLE=62,XTITLE='$X_{MSO}$ [km]', YTITLE='$\sqrt{Y^2+Z^2}$ [km]',TITLE="BOW SHOCK CROSSINGS AT VARIABLE Orbital theta",$
					MAGNITUDE=bytscl(orbitTHETAd),position=pos1,sym_size=.2)
	bowcbar=colorbar(target=allbows,range=[min(orbitTHETAd),max(orbitTHETAd)],$;target=cntr10, $
			title='Orbital Theta=Ls-70 DEGREE [0=periapsis, 180=apoapsis]',RGB_TABLE=62,position=cpos1)


	fname="ALL_BOW_SHOCK_CROSSINGS_VS_ORBIT_THETA.png"
			print,"now saving: "+fname
			bowcbar.save,dire+fname, BORDER=10, $

  			 RESOLUTION=400

	allbows.close

	foreach nm,plotFNames do print,"name=",nm
	print,"corrText=",corrText
	print,"maxFM=",maxFM,", at t=",maxFMplotDate," ,  from tplot file: ",maxFMplot
	print,"maxviableBeta=",maxViableBeta,", at t=",maxBetaPlot
	print,"minDiff=",minDiff,", at t=",minDiffPlotDate," ,  from tplot file: ",minDiffPlot
	print,"maxFUPDiff=",maxFUPDiff,", at t=",maxFUPDiffPlotDate," ,  from tplot file: ",maxFUPDiffPlot
	print,"maxUPDiff=",maxUPDiff,", at t=",maxUPDiffPlotDate," ,  from tplot file: ",maxUPDiffPlot
	print,"maxDU=",maxDU,", at t=",maxDUplotDate," ,  from tplot file: ",maxDUplot
	print,"goodpoint/points=",goodPoints,"/",points
	;baddists=0
	;baddots=0
	;badFMs=0
	;badBmax=0
	;badManual=0
	;print,"[baddists,baddots,badFMs,badBmax,BadManual,badLS,badMeasure,badbeta,missedAnomolies]"
	;print, [baddists,baddots,badFMs,badBmax,BadManual,badLS,badMeasure,badbeta,missedAnomolies]
	print,"[badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta,missedAnomolies,BadManual,missedManual]"
	print, [badNANs,baddists,baddots,badFMs,badBmax,badLS,badMeasure,badMfms,badbeta,missedanomolies,BadManual,missedmanual]
	;BFITCOMBINER,[1,2,3,12,13,14]
	;tplot,"bf123121314",/add
	;tplot,'bf46789101112',/add
	foreach el,wherebaddist do print,el
	;tlimit
	;tlimit
	;tlimit
	print,"total spring:",numel(springIndices)
	print,"total summer:",numel(summerIndices)
	print,"total autumn:",numel(autumnIndices)
	print,"total winter:",numel(winterIndices)


	;b=where(ThetaNBnd lt 25 and downup eq max(downup))
	;print,time_string(t[b])
	;print,ThetaNBnd[b]
	;print,downup[b]

	

	TOC,clockOMP
	RETURN
end
