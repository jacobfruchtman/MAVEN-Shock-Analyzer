
pro shockTester,ds,oxx,oyy


	plts=		['mvn_B_1sec_Mag','mvn_B_1sec_MAVEN_MSO','B_fitted','mvn_swica_density','Shock_Angle','Shock_Normal','alt','lon','lat','mach_fms_coarse']
	pltlist=list('mvn_B_1sec_Mag','mvn_B_1sec_MAVEN_MSO','B_fitted','mvn_swica_density','Shock_Angle','Shock_Normal','alt','lon','lat','mach_fms_coarse')
	npltlist=list()
	datList=pltList

	
	np=size(plts,/n_el)

	for i=0,np-1 do begin
		
		get_data,plts[i],data=dat

		datList[i]=dat		

	endfor

	for i=0,np-1 do begin
		print,plts[i]

		help,datList[i]
		help,datList[i].y
	endfor

	ds=datList

	len=0
	nlen=0

	for i=0,np-1 do begin
		
		nlen= arrayDims(ds[i].y)
		len+=nlen
		
	endfor
	listLength=len

	oxlist=list()
	oylist=list()
	szsl=list()
	cord=['x','y','z']
	for i=0,np-1 do begin
		
		nlen= arrayDims(datList[i].y)
		;len+=nlen
		for j=0, nlen-1 do begin
			szsl.add,size(datList[i].x,/n_el)
			oxlist.add,datList[i].x;, /No_Copy)
		endfor
		if nlen eq 1 then begin
			print,plts[i]
			npltList.add,plts[i]
			
			oylist.add,datList[i].y;, /No_Copy)
		endif else begin
			
			for k=0, nlen-1 do begin
					name=plts[i]+'_'+cord[k]
					print,name
					npltlist.add,name
					 oylist.add,datList[i].y[*,k];, /No_Copy)
			endfor
		endelse
		
	endfor

	oxx=oxlist
	oyy=oylist
	szs=szsl.toarray()

	B=Where(szs lt max(szs),count,COMPLEMENT=nB,nComplement=ncount)


	listLength=len
	print,size(npltlist)
	;print,npltlist[0]
	plts=		['mvn_B_1sec_Mag','mvn_B_1sec_MAVEN_MSO','B_fitted','mvn_swica_density','Shock_Angle','Shock_Normal','alt','lon','lat','mach_fms_coarse']
	for i=0,len-1 do begin
		;print,plts[i]
		print,npltlist[i]
		print,"x has size"
		print, size(oxlist[i])
		print,"y has size"
		print, size(oylist[i])
	endfor


	bigX=oxlist[nB[0]] 
	msz=size(bigX,/n_el)

	print,"msz=",msz
	datsY=fltarr(msz,listLength); we add +1 because first array will be the 'x' array. Later we may edit this so that [1] will be humn readable time.
	zlist=oylist
	foreach el,B do begin
		print,"el=",el	
		print,size(oylist[el])
		zlist[el]=interpol(oylist[el],oxlist[el],bigX)
	endforeach
	help,oylist[B[0]]
	for i=0,len-1 do begin
	;	print,"q"
		q=zlist[i]
		help,q
		print,q[0]
		help,datsY[*,i]
		for k=0, msz-1 do datsY[k,i]=q[k]
	endfor
	;ylist=oylist.toarray()


	help,datsY

end
