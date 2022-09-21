pro fitCombiner2, nums=nums, minChi=minChi,maxDist=maxDist


	

	R_m = 3389.50D  ; +/- 0.2
	get_data,'shock0rho_interpolated',data=datS0rho
	get_data,'shock0x_interpolated',data=datS0x
	 x0  = 0.600*R_m
 	ecc = 1.026
  	L   = 2.081*R_m

	s0rho=datS0rho.y
	s0x=datS0x.y
	xs=datS0x.x
	N=size(s0x,/n_el)
	s0phi=fltarr(N)

	anyBackwards=0

	sx0=s0x-x0
	srr=SQRT(sx0^2+s0rho^2)
	srt=Sqrt(s0rho^2+(sx0+ecc*srr)^2)
	xx=datS0rho.x
	snxx=(ecc+sx0/srr)*srr/srt   ;<---THIS IS THE TANGENT VECTOR, NOT THE NORMAL VECTOR. NEED TO FIX IT
	snrr=s0rho/srt

	ns0x=REVERSE(s0x)
	nsnxx=REVERSE(snxx)
	nsnrr=reverse(snrr)
	ns0rho=reverse(s0rho)
	SN=[[snxx],[snrr],[fltarr(N)]]
	get_data,'POS_interpolated_(MARS_MSO)',data=dpos
	
		
		xShock=xs[0]
		xsJ=x2Greg(xShock,/strformat)
		shockDate=(xsJ.split('T'))[0]
		print,shockDate
		dir='Documents/Plots/'+shockDate+'/'
	


	post=dpos.x
	oposx=dpos.y[*,0]
	oposy=dpos.y[*,1]
	oposz=dpos.y[*,2]

	pos=dpos.y


	get_data,'mvn_B_1sec_MAVEN_MSO_Mag',data=datB
	ymm=datB.y
	nymm=reverse(ymm)
	
	dists=fltarr(N)
	accs=fltarr(N)
	;Pphi=atan(oposz,oposy)
	;Prho=SQRT(oposz^2+oposy^2)
	;Pxx=oposx

	R_mars=3389.5

	;Prho=Pcyl[*,0]
	;Pphi=Pcyl[*,1]
	;Pxx=Pcyl[*,2]

	Pphi=atan(oposz,oposy)
	Prho=SQRT(oposz^2+oposy^2)
	Pxx=oposx
	Pr=Sqrt(Pxx^2+Prho^2)

	nPxx=REVERSE(Pxx)
	nPrho=REVERSE(Prho)
	nPhi=REVERSE(Pphi)
		
	if not keyword_set(nums) then nums=[0,1,2,4,5,6,7,8,9,10,11,12,13,14,16,17]
	get_data,"mvn_B_1sec_MAVEN_MSO_Mag",data=dat0
	N=numel(dat0.x)
	numDats=numel(nums)
	;datsin=iny
	;datsout=iny

	allinMMs=nanarr(N,4,numDats)	
	alloutMMs=nanarr(N,4,numDats)	

	iny=FLTARR(N,numDats)

	outy=iny
	allinnums=iny
	alloutnums=iny
	allinChis=iny
	alloutChis=iny
	allinmax=iny
	alloutmax=iny
	allinmin=iny
	alloutmin=iny
	allinshocks=iny
	alloutshocks=iny
	allinups=iny
	alloutups=iny
	allindowns=iny
	alloutdowns=iny

	allinalg0=iny
	allinalg1=iny
	alloutalg0=iny
	alloutalg1=iny
	for i=0,numDats-1 do begin
		el=nums[i]
		pltnameIn="B_fitted"+strtrim(el,2)+"_inbound"
		pltnameOut="B_fitted"+strtrim(el,2)+"_outbound"
		get_data,pltnameIn,data=datin
		get_data,pltnameOut,data=datout
		;help,datin
		;help,datout
		iny[*,i]=datin.y
		outy[*,i]=reverse(datout.y)

		allinnums[*,i]=datin.innums
		alloutnums[*,i]=reverse(datout.outnums)

		allinChis[*,i]=datin.inchis
		alloutChis[*,i]=reverse(datout.outchis)

		allinmax[*,i]=datin.inimaxs
		alloutmax[*,i]=reverse(datout.outimaxs)

		allinmin[*,i]=datin.inimins
		alloutmin[*,i]=reverse(datout.outimins)
		allinshocks[*,i]=datin.inshocks
		alloutshocks[*,i]=reverse(datout.outshocks)
		allinups[*,i]=datin.inups
		alloutups[*,i]=reverse(datout.outups)
		allindowns[*,i]=datin.indowns
		alloutdowns[*,i]=reverse(datout.outdowns)
		im=datin.MMs
		om=reverse(datout.MMs)
		allinMMs[*,*,i]=im
		alloutMMs[*,*,i]=om
		allinalg0[*,i]=datin.inalgshocks0
		allinalg1[*,i]=datin.inalgshocks1
		alloutalg0[*,i]=reverse(datout.outalgshocks0)
		alloutalg1[*,i]=reverse(datout.outalgshocks0)
	endfor

	
		;allinmin=transpose(allinmin.torarray())

	xs=datin.x

	if not keyword_set(minChi) then minChi=1.5
	if not keyword_set(maxDist) then maxDist=2500


	;get_data,'B_fitted3_inbound',data=dat3in
	;get_data,'B_fitted3_outbound',data=dat3out


	;get_data,'B_fitted4_inbound',data=dat4in
	;get_data,'B_fitted4_outbound',data=dat4out


	;get_data,'B_fitted5_inbound',data=dat5in
	;get_data,'B_fitted5_outbound',data=dat5out



	
	
	print,"xs[0]=",xs[0]
	zin=fltarr(N)
	zout=zin
	inshocks=fltarr(N)
	outshocks=fltarr(N)
	;N=numel(xs)
	
	numCrossings=max([max(allinnums),max(alloutnums)])+1

	insublengths=fltarr(N)
	outsublengths=fltarr(N)
	insubbegs=fltarr(N)
	outsubbegs=fltarr(N)
	insubends=fltarr(N)
	outsubends=fltarr(N)
	ins=fltarr(N)

	outs=fltarr(N)

	inMMs=nanarr(N,4)
	outMMs=nanarr(N,4)

	indownups=fltarr(N)
	outdownups=fltarr(N)

	inups=fltarr(N)
	indowns=fltarr(N)
	outups=fltarr(N)
	outdowns=fltarr(N)

	inimins=fltarr(N)-1
	inimaxs=fltarr(N)-1

	outimins=fltarr(N)-1
	outimaxs=fltarr(N)-1
	noutimins=fltarr(N)-1
	noutimaxs=fltarr(N)-1

	inShockLocs=fltarr(N)-1
	outShockLocs=fltarr(N)-1
	inShockLocs=fltarr(N)-1
	outShockLocs=fltarr(N)-1

	inalgshock0locs=fltarr(N)-1
	inalgshock1locs=fltarr(N)-1
	outalgshock0locs=fltarr(N)-1
	outalgshock1locs=fltarr(N)-1

	inChis=fltarr(N)-1
	outChis=fltarr(N)-1

	if numCrossings lt 1 then goto, noCrossings
	;print,numCrossings
	numArrays=(size(allinnums))[2]
	;print,numArrays

	outChiGrid=fltarr(numCrossings,numArrays)+999
	outMaxGrid=fltarr(numCrossings,numArrays)-1
	outMinGrid=fltarr(numCrossings,numArrays)-1
	outShockGrid=fltarr(numCrossings,numArrays)-1

	inChiGrid=fltarr(numCrossings,numArrays)+999
	inMaxGrid=fltarr(numCrossings,numArrays)-1
	inMinGrid=fltarr(numCrossings,numArrays)-1
	inShockGrid=fltarr(numCrossings,numArrays)-1

	inAlg0Grid=fltarr(numCrossings,numArrays)-1
	inAlg1Grid=fltarr(numCrossings,numArrays)-1

	outAlg0Grid=fltarr(numCrossings,numArrays)-1
	outAlg1Grid=fltarr(numCrossings,numArrays)-1

	inUpGrid=fltarr(numCrossings,numArrays)-1
	inDownGrid=fltarr(numCrossings,numArrays)-1

	outUpGrid=fltarr(numCrossings,numArrays)-1
	outDownGrid=fltarr(numCrossings,numArrays)-1
	inDistGrid=fltarr(numCrossings,numArrays)+9999
	outDistGrid=fltarr(numCrossings,numArrays)+9999

	inMMGrid=fltarr(numCrossings,4,numArrays)
	outMMGrid=fltarr(numCrossings,4,numArrays)
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
				outMMGrid[i,*,j]=alloutMMs[GG[0],*,j]
				outAlg0Grid[i,j]=alloutalg0[GG[0],j]
				outAlg1Grid[i,j]=alloutalg1[GG[0],j]
				if outShockGrid[i,j] ne -1 then begin
					ishock=outShockGrid[i,j]
					ox=nPxx[ishock]
					;ophi=nPphi[ishock]
					orho=nPrho[ishock]

					mmdiff=nymm[max([0,ishock-5*60]):ishock]-outDownGrid[i,j]
					D=Sqrt( (ox-ns0x)^2+(orho-ns0rho)^2);+ophi^2)
					minDist=min(D,minloc)
					if minDist lt 2500 and total(mmdiff lt 0) gt 60  then begin 
						outDistGrid[i,j]=minDist 
					endif else begin 
						outDistGrid[i,j]=9999
						outChiGrid[i,j]=999
					endelse
				endif
			endif

		endfor


	endfor
	;print,"inShockGrid:"
	;print,inShockGrid
	;print,"end of inShockGrid"

	outlocs=list()
	outlocs.add,-1

	for i=0, numCrossings -1 do begin
		if total(outlocs ne alloutshocks[i,0]) eq numel(outlocs) then outlocs.add,alloutshocks[i,0]
		;print,outlocs
	endfor

	sanityCheck=where (alloutshocks ne -1,ct)
	;print,"ct=",ct,", ct/(numArrays*numCrossings)=",ct/(numArrays*numCrossings)
	
	sanityCheck0=where (alloutshocks[*,0] ne -1,ct0)
	;print,"ct=",ct0,", ct0/(numCrossings)=",ct0/(numCrossings)
	for i=1, N-1 do if (alloutshocks[i,0] ne -1 ) and  (alloutshocks[i-1,0] eq -1 ) then print, "[i,crossing,ishock]=",[i,alloutnums[i,0],alloutshocks[i,0]]

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
				inMMGrid[i,*,j]=allinMMs[GG[0],*,j]



				inAlg0Grid[i,j]=allinalg0[GG[0],j]
				inAlg1Grid[i,j]=allinalg1[GG[0],j]
				if inShockGrid[i,j] ne -1 then begin
					ishock=inShockGrid[i,j]

					if abs(fracdiff(inMMGrid[i,3,j]-inMMGrid[i,0,j], inUpGrid[i,j])) gt .0001 or abs(fracdiff(inMMGrid[i,3,j]+inMMGrid[i,0,j], inDownGrid[i,j])) gt .0001 then begin
						print,"B_fitted"+strtrim(nums[j])					
						print,'i=',i,", t=",time_string(xs[ishock])
						print,"down,up,MM[3]+MM[0],MM[3]-MM[0]=",[inDownGrid[i,j],inUpGrid[i,j],inMMGrid[i,3,j]+inMMGrid[i,0,j],inMMGrid[i,3,j]-inMMGrid[i,0,j]]
						wait,20
					endif

					ox=Pxx[ishock]
					;ophi=Pphi[ishock]
					orho=Prho[ishock]
					mmdiff=ymm[max([0,ishock-5*60]):ishock]-inDownGrid[i,j]
					D=Sqrt( (ox-s0x)^2+(orho-s0rho)^2);+ophi^2)
					;inDistGrid[i,j]=min(D,minloc)
					minDist=min(D,minloc)
					if minDist lt 2500 and total(mmdiff lt 0) gt 60 then begin 
						inDistGrid[i,j]=minDist 
					endif else begin 
						inDistGrid[i,j]=9999
						inChiGrid[i,j]=999
					endelse
				endif
			endif

		endfor


	endfor

	;print,outChiGrid
	;Now will compare the fits and stitch them together, where each crossing uses the fit that gives it the lowest CHISQ
	inChiGrid[where(inChiGrid eq 0)]=999
	for i=0, numCrossings -1 do begin
		print,">>>>>>>>>>>>>>>>>"
		print,"i=",i

		crossCHI=inChiGrid[i,*]
		print,"crossCHI=",TRANSPOSE(crossCHI)
		print,"inMinGrid=",TRANSPOSE(inMinGrid[i,*])
		print,"inMaxGrid=",TRANSPOSE(inMaxGrid[i,*])
		print,"inShockGrid=",TRANSPOSE(inShockGrid[i,*])

		print,crossCHI
		if total(crossChi eq 999) eq numArrays then continue
		jchi=-1
		;if total(crossChi eq 999) gt 0 then crossChi[where(crossChi eq 0)]=999
		ourChi=min(crossChi,jchi)
		print,"ourChi,jchi,nums[jchi]=",ourChi,jchi,nums[jchi]
		if ourChi ge minChi then continue		

		imin=inMinGrid[i,jchi]
		imax=inMaxGrid[i,jchi]
		dd=inDownGrid[i,jchi]	
		uu=inUpGrid[i,jchi]
		zin[imin:imax]=iny[imin:imax,jchi]
		ishock=inShockGrid[i,jchi]
		MM=inMMGrid[i,*,jchi]
		;help,MM
		alg0=inAlg0Grid[i,jchi]
		alg1=inAlg1Grid[i,jchi]
		;help,inMMs
		for k=imin,imax do begin
			inups[k]=MM[3]-MM[0];uu
			indowns[k]=MM[3]+MM[0];dd
			insublengths[k]=1
			insublengths[k]=1
			inimins[k]=imin
			inimaxs[k]=imax
			inChis[k]=ourChi
			inMMs[k,*]=MM
			inalgshock0locs[k]=alg0
			inalgshock1locs[k]=alg1
		endfor
		
		insubbegs[imin:ishock-1]=1
		insubends[ishock:imax]=1
		
		

		if uu ne 0 then indownups[imin:imax]=dd/uu else indownups[imin:imax]=0

		inshocks[ishock]=1
		
		ins[ishock]=1

		inshockLocs[imin:imax]=ishock
		print,"[i,imin,ishock,imax]=",[i,imin,ishock,imax]
		print,"[i,imin,ishock,imax]=",[time_string(xs[imin]),time_string(xs[ishock]),time_string(xs[imax])]
		print,"numel(zin)=",numel(zin[imin:imax])
		print,"mean(zin)=",mean(zin[imin:imax])
		print,"zin[ishock]=zin[",ishock,"]=",zin[ishock]

		print,"~~~~~~~~~~~~~~"

		
	endfor
	outChiGrid[where(outChiGrid eq 0)]=999


		;print,"outMinGrid=",TRANSPOSE(outMinGrid)
		;print,"outMaxGrid=",TRANSPOSE(outMaxGrid)
		;print,"outShockGrid=",TRANSPOSE(outShockGrid)


	for i=0, numCrossings -1 do begin
		print,"<<<<<<<<<<<<<<<<"
		print,"i=",i
		crossCHI=outChiGrid[i,*]
		;print,"crossCHI=",TRANSPOSE(crossCHI)
		;print,"outMinGrid=",TRANSPOSE(outMinGrid[i,*])
		;print,"outMaxGrid=",TRANSPOSE(outMaxGrid[i,*])
		;print,"outShockGrid=",TRANSPOSE(outShockGrid[i,*])

		if total(crossChi eq 0) eq numArrays then continue
		jchi=-1
;		if total(crossChi eq 0) gt 0 then crossChi[where(crossChi eq 0)]=999
		ourChi=min(crossChi,jchi)
		;ourDist=
		if ourChi ge minChi then continue		
		ishock=outShockGrid[i,jchi]
		print,"ourChi,jchi,nums[jchi]=",ourChi,jchi,nums[jchi]

		
		imin=outMinGrid[i,jchi]
		imax=outMaxGrid[i,jchi]

		;starti=mean([imin,ishock])

		dd=outDownGrid[i,jchi]
		uu=outUpGrid[i,jchi]
		print,"jchi=",jchi

		MM=outMMGrid[i,*,jchi]

		alg0=outAlg0Grid[i,jchi]
		alg1=outAlg1Grid[i,jchi]
		zout[imin:imax]=outy[imin:imax,jchi]
		for k=imin,imax do begin
		

		outsublengths[k]=1
		outups[k]=MM[3]-MM[0];uu
		outdowns[k]=MM[3]+MM[0];dd

		noutimins[k]=imin
		noutimaxs[k]=imax
		outChis[k]=ourChi
		outshockLocs[k]=ishock
		outalgshock0locs[k]=alg0
		outalgshock1locs[k]=alg1
		outMMs[k,*]=MM
		endfor
		print,"[i,imin,ishock,imax]=",[i,imin,ishock,imax]
		outsubbegs[imin:ishock-1]=1
		outsubends[ishock:imax]=1

		print,"total(outsublengths[imin:imax]-outsubends[imin:imax])=",total(outsublengths[imin:imax]-outsubends[imin:imax])


		print,"numel(zout)=",numel(zout)
		print,"mean(zout)=",mean(zout)
		print,"zout[ishock]=z[",ishock,"]=",zout[ishock]
		if uu ne 0 then outdownups[imin:imax]=dd/uu else outdownups[imin:imax]=0
		outs[ishock]=-1
		outshocks[ishock]=1
		

	endfor
	;get_data,'B20deriv',data=datB20d


IG2=where(inshocks eq 1,igcount)

	if 0 and igcount gt 1 then begin
			;nB20d=-reverse(datB20d.y)
		foreach shk,IG2 do begin
			imin=inimins[shk]
			imax=inimaxs[shk]

			if total(inshocks[imin:imax]) gt 1 then begin
				IG3=where(inshocks[imin:imax] eq 1,ogcount2)+imin
				shk0=IG3[0]
				shk1=IG3[1]
				;slp0=max(nB20d[shk0-5:shk0+5])
				;slp1=max(nB20d[shk1-5:shk1+5])
				lc0=(where(shk0 eq inShockGrid))[0]
				lc1=(where(shk1 eq inShockGrid))[0]
				chi0=inChiGrid[ lc0]
				chi1=inChiGrid[ lc1]
				
				insubbegs[imin:imax]=0
				insubends[ishock:imax]=0

				zin[imin:imax]=0;iny[imin:imax,jchi]





		
		

		;starti=mean([imin,ishock])

		
				insublengths[imin:imax]=0.
		print,"[i,imin,ishock,imax]=",[i,imin,ishock,imax]
				insubbegs[imin:ishock-1]=0.


				;print,"total(insublengths[imin:imax]-insubends[imin:imax])=",total(insublengths[imin:imax]-insubends[imin:imax])
				for k=imin,imax do begin
				insubends[k]=0.
				inups[k]=0.
				indowns[k]=0.
				inMMs[k,*]=MM*0.
				inimins[k]=-1
				inimaxs[k]=-1
		
				indownups[k]=0.
				ins[k]=0;-1
				inshocks[k]=0
				inshockLocs[k]=-1;ishock
				inChis[k]=-1.;ourChi
				inalgshock0locs[k]=-1
				inalgshock1locs[k]=-1
				endfor
				
				jchi=min([chi0,chi1],mnlc)
				lc=([lc0,lc1])[mnlc]
				ishock=inShockGrid[lc]
			dd=inDownGrid[lc]
		uu=inUpGrid[lc]
		imin=inMinGrid[lc]
		imax=inMaxGrid[lc]		
		alg0=inAlg0Grid[lc]
		alg1=inAlg1Grid[lc]
		MM=inMMGrid[lc]
				zin[imin:imax]=iny[imin:imax,jchi]

		insublengths[imin:imax]=1
		print,"[i,imin,ishock,imax]=",[i,imin,ishock,imax]
		insubbegs[imin:ishock-1]=1
		insubends[ishock:imax]=1

		print,"total(insublengths[imin:imax]-insubends[imin:imax])=",total(insublengths[imin:imax]-insubends[imin:imax])
		for k=imin,imax do begin
		inups[k]=uu
		indowns[k]=dd
		inalgshock0locs[k]=alg0
		inalgshock1locs[k]=alg1
		inimins[k]=imin
		inimaxs[k]=imax
		inMMs[k,*]=MM
		endfor
		print,"numel(zin)=",numel(zin)
		print,"mean(zin)=",mean(zin)
		print,"zin[ishock]=z[",ishock,"]=",zin[ishock]
		if uu ne 0 then indownups[imin:imax]=dd/uu else indownups[imin:imax]=0
		ins[ishock]=-1
		inshocks[ishock]=1
		inshockLocs[imin:imax]=ishock
		inChis[imin:imax]=ourChi

			endif


		endforeach


	endif

	OG2=where(inshocks eq 1,ogcount)

	if ogcount gt 1 then begin
			;nB20d=-reverse(datB20d.y)
		foreach shk,OG2 do begin
			imin=inimins[shk]
			imax=inimaxs[shk]

			if total(inshocks[imin:imax]) gt 1 then begin
				OG3=where(inshocks[imin:imax] eq 1,ogcount2)+imin
				shk0=OG3[0]
				shk1=OG3[1]
				;slp0=max(nB20d[shk0-5:shk0+5])
				;slp1=max(nB20d[shk1-5:shk1+5])
				lc0=(where_xyz(shk0 eq inShockGrid and inChiGrid lt minChi,YIND=jchi0))[0]
				lc1=(where_xyz(shk1 eq inShockGrid and inChiGrid lt minChi,YIND=jchi1))[0]
				chi0=inChiGrid[ lc0]
				chi1=inChiGrid[ lc1]
				
				insubbegs[imin:imax]=0
				insubends[ishock:imax]=0

				zin[imin:imax]=0.;iny[imin:imax,jchi]





		
		

		;starti=mean([imin,ishock])

		
				insublengths[imin:imax]=0.
		print,"[i,imin,ishock,imax]=",[i,imin,ishock,imax]
				insubbegs[imin:ishock-1]=0.
				insubends[imin:imax]=0.

				print,"total(insublengths[imin:imax]-insubends[imin:imax])=",total(insublengths[imin:imax]-insubends[imin:imax])

				;inups[imin:imax]=0.
				;indowns[imin:imax]=0.

			;	inimins[imin:imax]=-1
			;	inimaxs[imin:imax]=-1
		
			;	indownups[imin:imax]=0.
			;	ins[imin:imax]=0;-1
			;	inshocks[imin:imax]=0
			;	inshockLocs[imin:imax]=-1;ishock
			;	inChis[imin:imax]=-1;ourChi
				for k=imin,imax do begin
				insubends[k]=0.
				inups[k]=0.
				indowns[k]=0.
				inMMs[k,*]=MM*0.
				inimins[k]=-1
				inimaxs[k]=-1
		
				indownups[k]=0.
				ins[k]=0.;-1
				inshocks[k]=0.
				inshockLocs[k]=-1;ishock
				inChis[k]=-1.;ourChi
				inalgshock0locs[k]=-1
				inalgshock1locs[k]=-1
				endfor
				ourChi=min([chi0,chi1],mnlc)


				lc=([lc0,lc1])[mnlc]

				jchi=([jchi0,jchi1])[mnlc]

				ishk=([shk0,shk1])[mnlc]
				ishock=inShockGrid[lc]
				print,"ishock,ishk,shk0,shk1,lc0,lc1,chi0,chi1"
				print,ishock,ishk,shk0,shk1,lc0,lc1,chi0,chi1
				;if ishock ne ishk then return
			dd=inDownGrid[lc]
		uu=inUpGrid[lc]
		imin=inMinGrid[lc]
		imax=inMaxGrid[lc]		
		alg0=inAlg0Grid[lc]
		alg1=inAlg1Grid[lc]
		;MM=inMMGrid[lc]
		zin[imin:imax]=iny[imin:imax,jchi]

		insublengths[imin:imax]=1
		print,"[i,imin,ishock,imax]=",[i,imin,ishock,imax]
		insubbegs[imin:ishock-1]=1
		insubends[ishock:imax]=1

		print,"total(insublengths[imin:imax]-insubends[imin:imax])=",total(insublengths[imin:imax]-insubends[imin:imax])
		for k=imin,imax do begin
		inups[k]=uu
		indowns[k]=dd
		inalgshock0locs[k]=alg0
		inalgshock1locs[k]=alg1
		inimins[k]=imin
		inimaxs[k]=imax
		inMMs[k,*]=allinMMs[k,*,jchi]
		inChis[k]=ourChi
		inshockLocs[k]=ishock
		endfor
		
		print,"numel(zin)=",numel(zin)
		print,"mean(zin)=",mean(zin)
		print,"zin[ishock]=z[",ishock,"]=",zin[ishock]
		if uu ne 0 then indownups[imin:imax]=dd/uu else indownups[imin:imax]=0
		ins[ishock]=-1
		inshocks[ishock]=1



			endif


		endforeach


	endif
	OG2=where(outshocks eq 1,ogcount)

	if ogcount gt 1 then begin
			;nB20d=-reverse(datB20d.y)
		foreach shk,OG2 do begin
			imin=noutimins[shk]
			imax=noutimaxs[shk]

			if total(outshocks[imin:imax]) gt 1 then begin
				OG3=where(outshocks[imin:imax] eq 1,ogcount2)+imin
				shk0=OG3[0]
				shk1=OG3[1]
				;slp0=max(nB20d[shk0-5:shk0+5])
				;slp1=max(nB20d[shk1-5:shk1+5])
				lc0=(where_xyz(shk0 eq outShockGrid and outChiGrid lt minChi,YIND=jchi0))[0]
				lc1=(where_xyz(shk1 eq outShockGrid and outChiGrid lt minChi,YIND=jchi1))[0]
				chi0=outChiGrid[ lc0]
				chi1=outChiGrid[ lc1]
				
				imn0=outMinGrid[lc0]
				imn1=outMinGrid[lc1]

				imx0=outMaxGrid[lc0]
				imx1=outMaxGrid[lc1]
				
				imnn=min([imn0,imn1])
				imxx=max([imx0,imx1])
				for k=imnn,imxx do begin
				outsubbegs[k]=0
				outsubends[k]=0

				zout[k]=0.;outy[imin:imax,jchi]



				outMMs[k,*]*=0.

		
		

		;starti=mean([imin,ishock])

		
				outsublengths[k]=0.
		;print,"[i,imin,ishock,imax]=",[i,imin,ishock,imax]
				outsubbegs[k]=0.
				outsubends[k]=0.

			;	print,"total(outsublengths[imin:imax]-outsubends[imin:imax])=",total(outsublengths[imin:imax]-outsubends[imin:imax])

				outups[k]=0.
				outdowns[k]=0.
				outalgshock0locs[k]=-1
				outalgshock1locs[k]=-1
				noutimins[k]=-1
				noutimaxs[k]=-1
		
				outdownups[k]=0.
				outs[k]=0.;-1
				outshocks[k]=0
				outshockLocs[k]=-1;ishock
				outChis[k]=-1.;ourChi
				endfor
				ourChi=min([chi0,chi1],mnlc)


				lc=([lc0,lc1])[mnlc]

				jchi=([jchi0,jchi1])[mnlc]

				ishk=([shk0,shk1])[mnlc]
				ishock=outShockGrid[lc]
				print,"ishock,ishk,shk0,shk1,lc0,lc1,chi0,chi1"
				print,ishock,ishk,shk0,shk1,lc0,lc1,chi0,chi1
				;if ishock ne ishk then return
			dd=outDownGrid[lc]
		uu=outUpGrid[lc]
		imin=outMinGrid[lc]
		imax=outMaxGrid[lc]		
		
		alg0=outAlg0Grid[lc]
		alg1=outAlg1Grid[lc]
				zout[imin:imax]=outy[imin:imax,jchi]
		

		print,"[i,imin,ishock,imax]=",[i,imin,ishock,imax]
		outsubbegs[imin:ishock-1]=1
		outsubends[ishock:imax]=1

		print,"total(outsublengths[imin:imax]-outsubends[imin:imax])=",total(outsublengths[imin:imax]-outsubends[imin:imax])
		for k=imin,imax do begin
			outups[k]=uu
			outdowns[k]=dd
			outsublengths[k]=1
			noutimins[k]=imin
			noutimaxs[k]=imax
			outMMs[k,*]=alloutMMs[k,*,jchi]
			outalgshock0locs[k]=alg0;N-1-outalgshock0locs[i]
			outalgshock1locs[k]=alg1;N-1-outalgshock1locs[i]
		endfor
		print,"numel(zout)=",numel(zout)
		print,"mean(zout)=",mean(zout)
		print,"zout[ishock]=z[",ishock,"]=",zout[ishock]
		if uu ne 0 then outdownups[imin:imax]=dd/uu else outdownups[imin:imax]=0
		outs[ishock]=-1
		outshocks[ishock]=1
		outshockLocs[imin:imax]=ishock
		outChis[imin:imax]=ourChi

			endif


		endforeach


	endif
	outMMs=reverse(outMMs)
	
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
	outalgshock0locs=reverse(outalgshock0locs)
	outalgshock1locs=reverse(outalgshock1locs)
	if not isa(outimins) then outimins=fltarr(N)-1
	FOR i=0, N-1 do begin
		if outshockLocs[i] ne -1 then begin
			outshockLocs[i]=N-1-outshockLocs[i]
			outalgshock0locs[i]=N-1-outalgshock0locs[i]
			outalgshock1locs[i]=N-1-outalgshock1locs[i]
			outimins[i]=N-1-outimins[i]
			outimaxs[i]=N-1-outimaxs[i]
		endif
	ENDFOR
	if not isa(outimins) then outimins=fltarr(N)-1

	outups=reverse(outups)
	outdowns=reverse(outdowns)
	outdownups=reverse(outdownups)
	;print,where(outshocks ne 0)

	
	noCrossings: print,"No Crossings"
	store_data,"Franken_fitted_inbound",data={x:xs,y:zin,ytitle:datin.ytitle,inups:inups,indowns:indowns,imins:inimins,imaxs:inimaxs,chis:inChis,downs:indowns,MMs:inMMs,$
inalgshocks0:inalgshock0locs,inalgshocks1:inalgshock1locs}

	store_data,"Franken_fitted_outbound",data={x:xs,y:zout,ytitle:datin.ytitle,outups:outups,outdowns:outdowns,imins:outimins,imaxs:outimaxs,$
chis:outChis,downs:outdowns,MMs:outMMs,outalgshocks0:outalgshock0locs,outalgshocks1:outalgshock1locs}
	shocks=inshocks
	;print,where(shocks ne 0)

	ups=inups
	downs=indowns
	downups=indownups
	imaxs=fltarr(N)-1;inimaxs
	imins=fltarr(N)-1;inimins
	shockLocs=fltarr(N)-1
	algshock0locs=fltarr(N)-1
	algshock1locs=fltarr(N)-1
	chis=fltarr(N)-1
	shockPOS=nanarr(N,3)
	drctn=fltarr(N)
	MMs=nanarr(N,4)
	for i=0, N-1 do begin
		if outshocks[i] ne 0 then shocks[i] = outshocks[i]

		outSL=outshockLocs[i]
		inSL=inshockLocs[i]
		isOut=1*(outSL ne -1)
		isIN=1*(inSL ne -1)
		;imn=inimins[i]
		;imx=inimins[i]
		;omn=outimins[i]
		;omx=outimaxs[i]
		IF ((Not isOut) and isIN) or ( (isOut+isIn eq 2) and (abs(i-outSL) ge abs(i-inSL))) then begin;and i ge inimins[i] and i le inimaxs[i] then begin
			zout[i]=zin[i]
			downs[i] = indowns[i]
			ups[i] = inups[i]
			shockLocs[i] = inSL
			algshock0locs[i]=inalgshock0locs[i]
			algshock1locs[i]=inalgshock1locs[i]
			downups[i] = indownups[i]
			imaxs[i]=inimaxs[i]
			imins[i]=inimins[i]
			chis[i]=inChis[i]
			shockPOS[i,*]=POS[inSL,*]
			drctn[i]=1
			for ll=0,3 do MMs[i,ll]=inMMs[i,ll]
		ENDIF ELSE BEGIN 
			IF (isOut and (inSL eq -1)) or  ( (isOut+isIn eq 2) and (abs(i-outSL) lt abs(i-inSL))) then begin;and i ge outimins[i] and i le outimaxs[i] then begin
			zout[i]=zout[i]
			downs[i] = outdowns[i]
			ups[i] = outups[i]
			shockLocs[i] = outSL
			algshock0locs[i]=outalgshock0locs[i]
			algshock1locs[i]=outalgshock1locs[i]
			downups[i] = outdownups[i]
			imaxs[i]=outimaxs[i]
			imins[i]=outimins[i]
			chis[i]=outChis[i]
			shockPOS[i,*]=POS[outSL,*]
			drctn[i]=-1
			for ll=0,3 do MMs[i,ll]=outMMs[i,ll]
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
	shockDIST=POS-shockPOS
	;print,where(shocks ne 0)


	store_data,'sublengths_inbound',data={x:xs,y:insublengths,ytitle:'flag'}
	store_data,'sublengths_inbound_begin',data={x:xs,y:insubbegs,ytitle:'flag'}
	store_data,'sublengths_inbound_end',data={x:xs,y:insubends,ytitle:'flag'}

	store_data,'sublengths_outbound',data={x:xs,y:outsublengths,ytitle:'flag'}
	store_data,'sublengths_outbound_begin',data={x:xs,y:outsubbegs,ytitle:'flag'}
	store_data,'sublengths_outbound_end',data={x:xs,y:outsubends,ytitle:'flag'}

	store_data,'sublengths_end',data={x:xs,y:insubends+outsubends,ytitle:'flag'}

	store_data,"Franken_fitted",data={x:xs,y:zout,ytitle:datin.ytitle,downups:downups,$
	downs:downs,ups:ups,imins:imins,imaxs:imaxs,chis:chis,direction:drctn,MMs:MMs,algshocks0:algshock0locs,algshocks1:algshock1locs}
	store_data,"bff",data='mvn_B_1sec_Mag Franken_fitted'
	store_data,"shocks_inbound",data={x:xs,y:inshocks,ytitle:'shock'}
	store_data,"shocks_outbound",data={x:xs,y:outshocks,ytitle:'shock'}
	;store_data,"downVSups",data={x:xs,y:downups,ytitle:'downstream/upstream'}
	;store_data,"down-ups",data={x:xs,y:downs-ups,ytitle:'Bd-Bu [nT]'}
	store_data,"downstream_fit",data={x:xs,y:downs,ytitle:'MM[3]+MM[0]'}
	store_data,"upstream_fit",data={x:xs,y:ups,ytitle:'MM[3]-MM[0]'}
	store_data,"shocks",data={x:xs,y:shocks,ytitle:'shock',shockLocs:shockLocs}
	store_data,"shock_locs",data={x:xs,y:shockLocs,ytitle:'shock location'}
	store_data,"shock_locs_inbound",data={x:xs,y:inShockLocs,ytitle:'shock location'}
	store_data,"shock_locs_outbound",data={x:xs,y:outShockLocs,ytitle:'shock location'}

	;store_data,'1/MM1',data={x:xs,y:1/MMs[*,1]}

	store_data,'shockPOS',data={x:xs,y:shockPOS,ytitle:'position of shock [MSO]'}
	store_data,'shockDIST_MAVEN_MSO',data={x:xs,y:shockDIST,ytitle:'distance of spacecraft from shock crossing [MSO]'}
	tvectot,'shockDIST_MAVEN_MSO',tot='shockDIST_MAVEN_MSO_Mag'
	;store_data,"Franken_Fitted_CHISQ",data={x:xs,y:chis,ytitle:"CHISQ"}
	;store_data,"Franken_Fitted_CHISQ_inbound",data={x:xs,y:inchis,ytitle:"CHISQ"}
	;store_data,"Franken_Fitted_CHISQ_outbound",data={x:xs,y:outchis,ytitle:"CHISQ"}
	;store_data,"Franken_fitted_downstream",data={x:xs,y:downs}
	tplot_element,"dateDataCurrent",'ximins',xs[imins]
	tplot_element,"dateDataCurrent",'ximaxs',xs[imaxs]
 	
	inouts=ins+outs
	;print,"xs[0]=",xs[0]
	store_data,"shockDirection",data={x:xs,y:inouts,ytitle:'1==inbnd,-1==outbnd'}
end
