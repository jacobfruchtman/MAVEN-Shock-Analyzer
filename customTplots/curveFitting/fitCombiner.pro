pro fitCombiner, minChi=minChi

	if not keyword_set(minChi) then minChi=0.9

	;get_data,'B_fitted3_inbound',data=dat3in
	;get_data,'B_fitted3_outbound',data=dat3out


	get_data,'B_fitted4_inbound',data=dat4in
	get_data,'B_fitted4_outbound',data=dat4out


	;get_data,'B_fitted5_inbound',data=dat5in
	;get_data,'B_fitted5_outbound',data=dat5out

	get_data,'B_fitted6_inbound',data=dat6in
	get_data,'B_fitted6_outbound',data=dat6out


	get_data,'B_fitted7_inbound',data=dat7in
	get_data,'B_fitted7_outbound',data=dat7out

	get_data,'B_fitted8_inbound',data=dat8in
	get_data,'B_fitted8_outbound',data=dat8out

	get_data,'B_fitted9_inbound',data=dat9in
	get_data,'B_fitted9_outbound',data=dat9out

	get_data,'B_fitted10_inbound',data=dat10in
	get_data,'B_fitted10_outbound',data=dat10out

	get_data,'B_fitted11_inbound',data=dat11in
	get_data,'B_fitted11_outbound',data=dat11out

	get_data,'B_fitted12_inbound',data=dat12in
	get_data,'B_fitted12_outbound',data=dat12out


	help,dat6out
	help,dat7out
	help,dat8out
	help,dat9out
	help,dat10out	
	xs=dat7in.x
	print,"xs[0]=",xs[0]
	zin=xs*0.0
	zout=zin
	inshocks=xs*0.0
	outshocks=xs*0.0
	N=numel(xs)
	iny=[[dat4in.y],[dat6in.y],[dat7in.y],[dat8in.y],[dat9in.y],[dat10in.y],[dat11in.y],[dat12in.y]]
	outy=reverse([[dat4out.y],[dat6out.y],[dat7out.y],[dat8out.y],[dat9out.y],[dat10out.y],[dat11out.y],[dat12out.y]])
	print,outy[0:4,*]
	print,reverse(outy[0:4,*],2)
	allinnums=[[dat4in.innums],[dat6in.innums],[dat7in.innums],[dat8in.innums],[dat9in.innums],[dat10in.innums],[dat11in.innums],[dat12in.innums]]
	alloutnums=reverse([[dat4out.outnums],[dat6out.outnums],[dat7out.outnums],[dat8out.outnums],[dat9out.outnums],[dat10out.outnums],[dat11out.outnums],[dat12out.outnums]])
	allinChis=[[dat4in.inchis],[dat6in.inchis],[dat7in.inchis],[dat8in.inchis],[dat9in.inchis],[dat10in.inchis],[dat11in.inchis],[dat12in.inchis]]
	alloutChis=reverse([[dat4out.outchis],[dat6out.outchis],[dat7out.outchis],[dat8out.outchis],[dat9out.outchis],[dat10out.outchis],[dat11out.outchis],[dat12out.outchis]])
	allinmax=[[dat4in.inimaxs],[dat6in.inimaxs],[dat7in.inimaxs],[dat8in.inimaxs],[dat9in.inimaxs],[dat10in.inimaxs],[dat11in.inimaxs],[dat12in.inimaxs]]
	alloutmax=reverse([[dat4out.outimaxs],[dat6out.outimaxs],[dat7out.outimaxs],[dat8out.outimaxs],[dat9out.outimaxs],[dat10out.outimaxs],[dat11out.outimaxs],[dat12out.outimaxs]])
	allinmin=[[dat4in.inimins],[dat6in.inimins],[dat7in.inimins],[dat8in.inimins],[dat9in.inimins],[dat10in.inimins],[dat11in.inimins],[dat12in.inimins]]
	alloutmin=reverse([[dat4out.outimins],[dat6out.outimins],[dat7out.outimins],[dat8out.outimins],[dat9out.outimins],[dat10out.outimins],[dat11out.outimins],[dat12out.outimins]])
	allinshocks=[[dat4in.inshocks],[dat6in.inshocks],[dat7in.inshocks],[dat8in.inshocks],[dat9in.inshocks],[dat10in.inshocks],[dat11in.inshocks],[dat12in.inshocks]]
	alloutshocks=reverse([[dat4out.outshocks],[dat6out.outshocks],[dat7out.outshocks],[dat8out.outshocks],[dat9out.outshocks],[dat10out.outshocks],[dat11out.outshocks],[dat12out.outshocks]])
	allinups=[[dat4in.inups],[dat6in.inups],[dat7in.inups],[dat8in.inups],[dat9in.inups],[dat10in.inups],[dat11in.inups],[dat12in.inups]]
	alloutups=reverse([[dat4out.outups],[dat6out.outups],[dat7out.outups],[dat8out.outups],[dat9out.outups],[dat10out.outups],[dat11out.outups],[dat12out.outups]])
	allindowns=[[dat4in.indowns],[dat6in.indowns],[dat7in.indowns],[dat8in.indowns],[dat9in.indowns],[dat10in.indowns],[dat11in.indowns],[dat12in.indowns]]
	alloutdowns=reverse([[dat4out.outdowns],[dat6out.outdowns],[dat7out.outdowns],[dat8out.outdowns],[dat9out.outdowns],[dat10out.outdowns],[dat11out.outdowns],[dat12out.outdowns]])
	numCrossings=max([max(allinnums),max(alloutnums)])+1

	insublengths=xs*0.0
	outsublengths=xs*0.0
	insubbegs=xs*0.0
	outsubbegs=xs*0.0
	insubends=xs*0.0
	outsubends=xs*0.0
	ins=xs*0.0

	outs=xs*0.0

	indownups=xs*0.0
	outdownups=xs*0.0

	inups=xs*0.0
	indowns=xs*0.0
	outups=xs*0.0
	outdowns=xs*0.0

	inimins=xs*0-1
	inimaxs=xs*0-1

	noutimins=xs*0-1
	noutimaxs=xs*0-1

	inShockLocs=xs*0-1
	outShockLocs=xs*0-1


	inChis=xs*0.0-1
	outChis=xs*0.0-1

	if numCrossings lt 1 then goto, noCrossings
	print,numCrossings
	numArrays=(size(allinnums))[2]
	print,numArrays

	outChiGrid=fltarr(numCrossings,numArrays)+999
	outMaxGrid=fltarr(numCrossings,numArrays)-1
	outMinGrid=fltarr(numCrossings,numArrays)-1
	outShockGrid=fltarr(numCrossings,numArrays)-1

	inChiGrid=fltarr(numCrossings,numArrays)+999
	inMaxGrid=fltarr(numCrossings,numArrays)-1
	inMinGrid=fltarr(numCrossings,numArrays)-1
	inShockGrid=fltarr(numCrossings,numArrays)-1

	inUpGrid=fltarr(numCrossings,numArrays)-1
	inDownGrid=fltarr(numCrossings,numArrays)-1

	outUpGrid=fltarr(numCrossings,numArrays)-1
	outDownGrid=fltarr(numCrossings,numArrays)-1


	;We will now construct a table listing the parameters of each shock crossing
	for i=0, numCrossings -1 do begin

		for j=0, numArrays-1 do begin

			GG=where(alloutnums[*,j] eq i,count)
			if count ne 0 then begin
				outChiGrid[i,j]=alloutchis[GG[0],j]
				outMaxGrid[i,j]=alloutmax[GG[0],j]
				outMinGrid[i,j]=alloutmin[GG[0],j]
				outShockGrid[i,j]=alloutshocks[GG[0],j]
				outUpGrid[i,j]=alloutups[GG[0],j]
				outDownGrid[i,j]=alloutdowns[GG[0],j]
			endif

		endfor


	endfor
	;print,"outShockGrid:"
	;print,outShockGrid
	;print,"end of outShockGrid"

	outlocs=list()
	outlocs.add,-1

	for i=0, numCrossings -1 do begin
		if total(outlocs ne alloutshocks[i,0]) eq numel(outlocs) then outlocs.add,alloutshocks[i,0]
		print,outlocs
	endfor

	sanityCheck=where (alloutshocks ne -1,ct)
	print,"ct=",ct,", ct/(numArrays*numCrossings)=",ct/(numArrays*numCrossings)
	
	sanityCheck0=where (alloutshocks[*,0] ne -1,ct0)
	print,"ct=",ct0,", ct0/(numCrossings)=",ct0/(numCrossings)
	;for i=1, N-1 do if (alloutshocks[i,0] ne -1 ) and  (alloutshocks[i-1,0] eq -1 ) then print, "[i,crossing,ishock]=",[i,alloutnums[i,0],alloutshocks[i,0]]

	for i=0, numCrossings -1 do begin

		for j=0, numArrays-1 do begin

			GG=where(allinnums[*,j] eq i,count)
			if count ne 0 then begin
				inChiGrid[i,j]=allinchis[GG[0],j]
				inMaxGrid[i,j]=allinmax[GG[0],j]
				inMinGrid[i,j]=allinmin[GG[0],j]
				inShockGrid[i,j]=allinshocks[GG[0],j]
				inUpGrid[i,j]=allinups[GG[0],j]
				inDownGrid[i,j]=allindowns[GG[0],j]
			endif

		endfor


	endfor

	print,outChiGrid
	;Now will compare the fits and stitch them together, where each crossing uses the fit that gives it the lowest CHISQ
	inChiGrid[where(inChiGrid eq 0)]=999
	for i=0, numCrossings -1 do begin
		crossCHI=inChiGrid[i,*]

		if total(crossChi eq 999) eq numArrays then continue
		jchi=-1
		;if total(crossChi eq 999) gt 0 then crossChi[where(crossChi eq 0)]=999
		ourChi=min(crossChi,jchi)

		if ourChi ge minChi then continue		

		imin=inMinGrid[i,jchi]
		imax=inMaxGrid[i,jchi]
		dd=inDownGrid[i,jchi]	
		uu=inUpGrid[i,jchi]
		zin[imin:imax]=iny[imin:imax,jchi]
		ishock=inShockGrid[i,jchi]
		inups[imin:imax]=uu
		indowns[imin:imax]=dd
		insublengths[imin:imax]=1
		insublengths[imin:imax]=1
		insubbegs[imin:ishock-1]=1
		insubends[ishock:imax]=1
		inimins[imin:imax]=imin
		inimaxs[imin:imax]=imax
		inChis[imin:imax]=ourChi
		

		if uu ne 0 then indownups[imin:imax]=dd/uu else indownups[imin:imax]=0

		inshocks[ishock]=1
		
		ins[ishock]=1

		inshockLocs[imin:imax]=ishock
		

		
	endfor
	outChiGrid[where(outChiGrid eq 0)]=999


		;print,"outMinGrid=",TRANSPOSE(outMinGrid)
		;print,"outMaxGrid=",TRANSPOSE(outMaxGrid)
		;print,"outShockGrid=",TRANSPOSE(outShockGrid)


	for i=0, numCrossings -1 do begin
		print,">>>>>>>>>>>>>>>>>"
		print,"i=",i
		crossCHI=outChiGrid[i,*]
		print,"crossCHI=",TRANSPOSE(crossCHI)
		print,"outMinGrid=",TRANSPOSE(outMinGrid[i,*])
		print,"outMaxGrid=",TRANSPOSE(outMaxGrid[i,*])
		print,"outShockGrid=",TRANSPOSE(outShockGrid[i,*])

		if total(crossChi eq 0) eq numArrays then continue
		jchi=-1
;		if total(crossChi eq 0) gt 0 then crossChi[where(crossChi eq 0)]=999
		ourChi=min(crossChi,jchi)

		if ourChi ge minChi then continue		
		ishock=outShockGrid[i,jchi]


		
		imin=outMinGrid[i,jchi]
		imax=outMaxGrid[i,jchi]

		;starti=mean([imin,ishock])

		dd=outDownGrid[i,jchi]
		uu=outUpGrid[i,jchi]
		print,"jchi=",jchi
		zout[imin:imax]=outy[imin:imax,jchi]

		outsublengths[imin:imax]=1
		print,"[i,imin,ishock,imax]=",[i,imin,ishock,imax]
		outsubbegs[imin:ishock-1]=1
		outsubends[ishock:imax]=1
		outups[imin:imax]=uu
		outdowns[imin:imax]=dd
		print,"total(outsublengths[imin:imax]-outsubends[imin:imax])=",total(outsublengths[imin:imax]-outsubends[imin:imax])
		print,"total(outsubbegs[imin:imax])=",total(outsubbegs[imin:imax])
		noutimins[imin:imax]=imin
		noutimaxs[imin:imax]=imax
		print,"numel(zout)=",numel(zout)
		print,"mean(zout)=",mean(zout)
		print,"zout[ishock]=z[",ishock,"]=",zout[ishock]
		if uu ne 0 then outdownups[imin:imax]=dd/uu else outdownups[imin:imax]=0
		outs[ishock]=-1
		outshocks[ishock]=1
		outshockLocs[imin:imax]=ishock
		outChis[imin:imax]=ourChi
	endfor
	outsublengths=reverse(outsublengths)
	outsubbegs=reverse(outsubbegs)
	outsubends=reverse(outsubends)
	outs=reverse(outs)
	zout=reverse(zout)
	outChis=reverse(outChis)
	outshocks=reverse(outshocks)

	outimaxs=reverse(noutimins)

	outimins=reverse(noutimaxs)
	outshockLocs=reverse(outshockLocs)

	FOR i=0, N-1 do begin
		if outshockLocs[i] ne -1 then begin
			outshockLocs[i]=N-1-outshockLocs[i]
			outimins[i]=N-1-outimins[i]
			outimaxs[i]=N-1-outimaxs[i]
		endif
	ENDFOR
	

	outups=reverse(outups)
	outdowns=reverse(outdowns)
	outdownups=reverse(outdownups)
	print,where(outshocks ne 0)
	noCrossings: print,"No Crossings"
	store_data,"Franken_fitted_inbound",data={x:xs,y:zin,ytitle:dat10in.ytitle,inups:inups,indowns:indowns,imins:inimins,imaxs:inimaxs,chis:inChis}

	store_data,"Franken_fitted_outbound",data={x:xs,y:zout,ytitle:dat10in.ytitle,outups:outups,outdowns:outdowns,imins:outimins,imaxs:outimaxs,chis:outChis}
	shocks=inshocks
	print,where(shocks ne 0)

	ups=inups
	downs=indowns
	downups=indownups
	imaxs=fltarr(N)-1;inimaxs
	imins=fltarr(N)-1;inimins
	shockLocs=xs*0.0-1
	chis=xs*0.0-1
	for i=0, N-1 do begin
		if outshocks[i] ne 0 then shocks[i] = outshocks[i]

		outSL=outshockLocs[i]
		inSL=inshockLocs[i]
		isOut=1*(outSL ne -1)
		isIN=1*(inSL ne -1)
		IF ((Not isOut) and isIN) or ( (isOut+isIn eq 2) and (abs(i-outSL) ge abs(i-inSL)))  then begin
			zout[i]=zin[i]
			downs[i] = indowns[i]
			ups[i] = inups[i]
			shockLocs[i] = inSL
			downups[i] = indownups[i]
			imaxs[i]=inimaxs[i]
			imins[i]=inimins[i]
			chis[i]=inChis[i]
		ENDIF ELSE BEGIN 
			IF (isOut and (inSL eq -1)) or  ( (isOut+isIn eq 2) and (abs(i-outSL) lt abs(i-inSL)))  then begin
			zout[i]=zout[i]
			downs[i] = outdowns[i]
			ups[i] = outups[i]
			shockLocs[i] = outSL
			downups[i] = outdownups[i]
			imaxs[i]=outimaxs[i]
			imins[i]=outimins[i]
			chis[i]=outChis[i]
			ENDIF 	
		ENDELSE

		;if zin[i] ne 0 then zout[i]=zin[i]
		
		;if outdowns[i] ne 0 then downs[i] = outdowns[i]
		;if outups[i] ne 0 then ups[i] = outups[i]
		;if outShockLocs[i] ne -1 then shockLocs[i] = outShockLocs[i]
		;if outdownups[i] ne 0 then downups[i] = outdownups[i]
		;if outimaxs[i] ne -1 then imaxs[i]=outimaxs[i]
		;if outimins[i] ne -1 then imins[i]=outimins[i]
	endfor

	print,where(shocks ne 0)


	store_data,'sublengths_inbound',data={x:xs,y:insublengths,ytitle:'flag'}
	store_data,'sublengths_inbound_begin',data={x:xs,y:insubbegs,ytitle:'flag'}
	store_data,'sublengths_inbound_end',data={x:xs,y:insubends,ytitle:'flag'}

	store_data,'sublengths_outbound',data={x:xs,y:outsublengths,ytitle:'flag'}
	store_data,'sublengths_outbound_begin',data={x:xs,y:outsubbegs,ytitle:'flag'}
	store_data,'sublengths_outbound_end',data={x:xs,y:outsubends,ytitle:'flag'}

	store_data,"Franken_fitted",data={x:xs,y:zout,ytitle:dat10in.ytitle,downups:downups,downs:downs,ups:ups,imins:imins,imaxs:imaxs,chis:chis}
	store_data,"bff",data='mvn_B_1sec_Mag Franken_fitted'
	store_data,"shocks_inbound",data={x:xs,y:inshocks,ytitle:'shock'}
	store_data,"shocks_outbound",data={x:xs,y:outshocks,ytitle:'shock'}
	store_data,"downVSups",data={x:xs,y:downups,ytitle:'downstream/upstream'}
	store_data,"downstream_fit",data={x:xs,y:downs,ytitle:'MM[3]+MM[0]'}
	store_data,"upstream_fit",data={x:xs,y:ups,ytitle:'MM[3]-MM[0]'}
	store_data,"shocks",data={x:xs,y:shocks,ytitle:'shock',shockLocs:shockLocs}
	store_data,"shock_locs",data={x:xs,y:shockLocs,ytitle:'shock location'}
	store_data,"shock_locs_inbound",data={x:xs,y:inShockLocs,ytitle:'shock location'}
	store_data,"shock_locs_outbound",data={x:xs,y:outShockLocs,ytitle:'shock location'}

	store_data,"Franken_Fitted_CHISQ",data={x:xs,y:chis,ytitle:"CHISQ"}
	store_data,"Franken_Fitted_CHISQ_inbound",data={x:xs,y:inchis,ytitle:"CHISQ"}
	store_data,"Franken_Fitted_CHISQ_outbound",data={x:xs,y:outchis,ytitle:"CHISQ"}

	tplot_element,"dateDataCurrent",'ximins',xs[imins]
	tplot_element,"dateDataCurrent",'ximaxs',xs[imaxs]

	inouts=ins+outs
	print,"xs[0]=",xs[0]
	store_data,"shockDirection",data={x:xs,y:inouts,ytitle:'1==inbnd,-1==outbnd'}
	
end
