pro randomListGenerator

	dire="Documents/"
	;FILE_MKDIR,dire
	doClose=1




	killBigFM=0
	killNegOverDiff=0
	;if keyword_Set(REMOVENEGATIVES) then killNegOverDiff=1
	;if keyword_Set(RemoveBigFracM) then killBigFM=1
	;if not keyword_set(maxAllowedFM) then maxAllowedFM=14 else killBigFM=1
	;first get names of tplot files from txt file

	;H=0
	corrText=""
	;if killBigFM or killNegOverDiff then corrText="_WithoutPathologies"
	read_strDATA,H,"Documents/overmachDays.txt",numLines=numPlots

	numPlots=numel(H)

	;in for loop, restore these files and create an array of data structures

	tmins=list()
	tmaxs=list()
	datasets=list()
	seasons=list()

	;maxlocs:GG,t:xs[GG],mfms:s[GG],crits:crits[GG],Bmaxs:ymaxs[GG],overDiffs:overs,fracOverDiffs:fracOvers,downups:downups[GG],
	;ANGLE:MX3[GG],downs:datff.downs[GG],ups:datff.ups[GG],overshoot:datO.y,lat:lat[GG],Ls:Ls[GG]}
	locsLst=list()	
	tLst=list()
	
	locList=list()

	bins=list()
	
	pointlist=list()
	selectedlist=list()



	goodPoints=0
	;points=0
	for i=0,numPlots-1 do begin
		;;print,H

		if i gt 0 and H[i] eq H[i-1] then continue

		tplot_restore,filename="Documents/overVsMachData/"+H[i]
	
		name=(strsplit(((strsplit(H[i],"." , /extract))[0]),'/',/extract))

		get_data,name,data=dat



		
		for j=0,numel(dat.t)-1 do begin
			;points++




			;mxFM=max(dat.mfms[j])


	
			



			locsLst.add,dat.maxlocs	[j]
			;t=dat.t[j]
			tLst.add,dat.t[j]
			
			

		
		endfor

		
	endfor
	
	;N=size(tLst,/n_el)
	for i=0,30 do begin
		N=size(tLst,/n_el)
		loc=FIX(N * RANDOMU(seed))

		tsel=tLst[loc]
		tLst.remove,loc
		tt=x2Greg(tsel,/strformat,/noHour)+"/0:00;"+x2Greg(tsel,/strformat)

		selectedlist.add,tt	
	endfor
	selectedlist=selectedlist.sort()
	openW,1,dire+"randomPoints.txt"
	foreach el,selectedlist do printf,1,el
	close,1

end
