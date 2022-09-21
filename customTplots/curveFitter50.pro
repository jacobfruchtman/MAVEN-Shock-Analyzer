pro curveFitter5, plt , newName=newName, curveType=curveType , doRoll=doRoll, doSecond=doSecond,doThird=doThird,Debug=Debug,minChi=minChi,secondOrder=secondOrder,orderCustom=orderCustom

	g0=0
	if NOT KEYWORD_SET(newName) THEN newName=newName+'_smoothed'
	if NOT KEYWORD_SET(minChi) THEN minChi=1.20
	rollup=0
	rollsec=0
	rollthird=0
	dobug=0
	minRemain=100

	maxUp=7
	minM1=.04
	if KEYWORD_SET(orderCustom) then begin
		if Total(orderCustom eq 1) gt 0 then rollup=1
		if Total(orderCustom eq 2) gt 0 then rollsec=1
		if Total(orderCustom eq 3) gt 0 then rollthird=1
	endif else begin
		orderCustom=list()

		if KEYWORD_SET(doRoll) THEN begin
			rollup=1
			orderCustom.add,1
		endif
		if KEYWORD_SET(doSecond) THEN begin
			rollsec=1
			orderCustom.add,2
			
		endif
		orderCustom.add,0
		if KEYWORD_SET(doThird) THEN begin
			orderCustom.add,3
			;orderCustom.add,4
			rollthird=1                
			endif
			if numel(orderCustom) gt 3 then orderCustom.add,4
			if numel(orderCustom) gt 4 then orderCustom.add,5
		orderCustom=orderCustom.toArray()
	endelse
	doPerturb=0
	if KEYWORD_SET(secondOrder) THEN doPerturb=1                
	; should we use derivative of smoothed data to adjust our initial guess for foot to inflection point
	if NOT KEYWORD_SET(curveType) THEN curveType="tanhfit"
	typ=2
	get_data,plt,data=dat,limits=limits
	;get_data,'ascend_end',data=datAE
	;print,size(dat)
	ys=dat.y
	print,"=curveFitter5=5=5=5=5=5=5=5=5=5=5=5=5=5=5=5=5=curveFitter5="
	print,"newName=",newName,", rollup=",rollup,", rollsec=",rollsec
	print,"=curveFitter5=5=5=5=5=5=5=5=5=5=5=5=5=5=5=5=5=curveFitter5="
	ys[where(finite(ys) eq 0)]=0

	xs=dat.x
	N=Size(ys,/n_el)
	;print,N
	x00=xs[0]
	mids=fltarr(N)
	shocks=fltarr(N)
	z=fltarr(N)
	insublengths=list()
	outsublengths=list()

	brokenin=list()
	brokenout=list()

	inSubsets=fltarr(N);-1
	inends=fltarr(N);-1
	inbegs=fltarr(N);-1

	outSubsets=fltarr(N);-1
	outends=fltarr(N);-1
	outbegs=fltarr(N);-1

	debugout=fltarr(N)-1


	inimaxs=fltarr(N)-1
	outimaxs=fltarr(N)-1

	inimins=fltarr(N)-1
	outimins=fltarr(N)-1

	inchis=fltarr(N);+0
	outchis=fltarr(N);+0
	chis=fltarr(N)

	innums=fltarr(N)-1
	outnums=fltarr(N)-1
	
	inshocklocs=fltarr(N)-1
	outshocklocs=fltarr(N)-1
	
	inups=fltarr(N)
	indowns=fltarr(N)

	outups=fltarr(N)
	outdowns=fltarr(N)

	if (typ eq 2) then begin
		if dobug gt 0 then print,"type  2: curve fit"
		;.run tanhfit
		;get_data,'ascend_end',data=datae
		;get_data,'descend_end',data=datde

		;get_data,'ascend_begin',data=datab
		;get_data,'descend_begin',data=datdb

		get_data,'monof-B60dsane',data=datMon
		get_data,'bmonof-B60dsane',data=datbMon
		get_data,'regid_cleaned_plot_interpolated',data=datReg
		mono=datMon.y
		bmono=datbMon.y
		reg=datReg.y

		get_data,'B5-3d10',data=datB5d
		get_data,'B20deriv',data=datB20deriv

		hill=datB5d.y
		hill2=datB20deriv.y

		if dobug gt 0 then print,"numel(hill)=",numel(hill)
		get_data,"B_sec_smoothed" ,data=datBSb
		get_data,"B_sec_smoothed_back" ,data=datBS
		get_data,"B_median",data=datBmed
		Bmed=datBmed.y
		;oyae=datae.y
		;oxae=datae.x

		;oydb=datdb.y
		;oxdb=datdb.x
		starti=0
		;oyde=datde.y
		;oxde=datde.x

		;oyab=datab.y
		;oxab=datab.x

		yBS=datBS.y
		yBSb=datBSb.y

	;------------------
		

		;ydb=fltarr(N)
		;yde=fltarr(N)
		;yab=fltarr(N)
		;yae=fltarr(N)
		;for i=0, N-1 do yae[i]=0
		;for i=0, N-1 do yde[i]=0
		;for i=0, N-1 do yab[i]=0
		;for i=0, N-1 do ydb[i]=0
		;print,total(yae)
		;print,total(ydb)
	;	print,"yae is non-finite at:",where(finite(yae) eq 0)
	;	print,"yde is non-finite at:",where(finite(yde) eq 0)
	;	print,"yab is non-finite at:",where(finite(yab) eq 0)
	;	print,"ydb is non-finite at:",where(finite(ydb) eq 0)
	;	print,"total(yae)=",total(yae)
	;	print,"total(ydb)=",total(ydb)
	;	print,"total(yab)=",total(yab)
	;	print,"total(yde)=",total(yde)


		;nl=size(oyae,/n_el)
		;nd=size(oydb,/n_el)
		;"variables that would otherwise be defined in loops"
		;print,N,nl,nd
		mbeg=0
		mend=0
		incfunctions=1
		decfunctions=1
		trust=0
		
		if dobug gt 0 then	print,"numel(hill)=",numel(hill)
		get_data,'ascend_end_interpolated',data=datae
		get_data,'ascend_begin_interpolated',data=datab
		get_data,'descend_end_interpolated',data=datde
		get_data,'descend_begin_interpolated',data=datdb
		yae=datae.y
		yab=datab.y
		yde=datde.y
		ydb=datdb.y

		DD = WHERE(yae ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		;print,yae[DD]

		AA = WHERE(ydb ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

		CC = WHERE(yab ne 0, ccn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
		BB = WHERE(yde ne 0, bbn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock
	if dobug gt 0 then 	PRINT,"[ddn,aan,bbn]=",[ddn,aan,bbn]
	if dobug gt 0 then 	print,"numel(hill)=",numel(hill)
		AA0=AA

		BB0=BB
		CC0=CC
		DD0=DD
	;------------------SHORT CIRCUITING NONCROSSING PERIODS HERE
	zf=fltarr(N)
	if DD[0] eq -1 then begin
		zf=fltarr(N)
		store_data,newName+'_CHISQ_inbound',data={x:xs,y:inchis,ytitle:"REDUCED CHISQ"}
	;store_data,newName+'_ADJCHISQ_inbound',data={x:xs,y:adjChiIn,ytitle:"abs(REDUCED CHISQ-1)"}
		store_data,newName+'_CHISQ_outbound',data={x:xs,y:outchis,ytitle:"REDUCED CHISQ"}
	;store_data,newName+'_ADJCHISQ_outbound',data={x:xs,y:adjChiOut,ytitle:"abs(REDUCED CHISQ-1)"}
		store_data,newName+'_CHISQ',data={x:xs,y:chis,ytitle:"REDUCED CHISQ"}
	;store_data,newName+'_ADJCHISQ',data={x:xs,y:adjChi,ytitle:"abs(REDUCED CHISQ-1)"}
	;	store_data,'shocks'+rl,data={x:xs,y:shocks,ytitle:'flag'}
		store_data,newName+'_shocks',data={x:xs,y:zf,ytitle:'flag'}
		
 		dat.y=fltarr(N)
		;print,size(dat)
		;help,dat

		store_data,newName,data={x:xs,y:zf,ytitle:"Magnetic Field (nT)", inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs,outchis:outchis, 			outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,shocks:shocks}
		store_data,newName+"_inbound",data={x:xs,y:zf,ytitle:"Magnetic Field (nT)", inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs}
		store_data,newName+"_outbound",data={x:xs,y:zf,ytitle:"Magnetic Field (nT)", outchis:outchis, 			outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,shocks:shocks}

;		store_data,'shocks'+rl+'_inbound',data={x:xs,y:shocks,ytitle:'flag'}
		store_data,newName+'_shocks_inbound',data={x:xs,y:shocks,ytitle:'flag'}

;		store_data,'shocks'+rl+'_outbound',data={x:xs,y:shocks,ytitle:'flag'}
		store_data,newName+'_shocks_outbound',data={x:xs,y:shocks,ytitle:'flag'}

		store_data,'sublengths_inbound',data={x:xs,y:inSubsets,ytitle:'flag'}

		store_data,'sublengths_inbound_begin',data={x:xs,y:inbegs,ytitle:'flag'}
		store_data,'sublengths_inbound_end',data={x:xs,y:inends,ytitle:'flag'}

		store_data,'sublengths_outbound',data={x:xs,y:outSubsets,ytitle:'flag'}

		store_data,'sublengths_outbound_begin',data={x:xs,y:outbegs,ytitle:'flag'}
		store_data,'sublengths_outbound_end',data={x:xs,y:outends,ytitle:'flag'}

		return

	endif





	;------------------
		;"variables that would otherwise be defined in loops"

		mbeg=0
		mend=0
		incfunctions=1
		decfunctions=1
		;trust=0
		g0=0;
		b0=0
		;DD = WHERE(oyae ne 0, ddn, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c)
		

		;AA = WHERE(oydb ne 0, aan, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c)
		
		;counterinutitively, will want to curve fit ys[0:DD[0]] after the rest

		;need to determine if we have the whole curve or not

		;iscut=1*(ddn ne aan) ; 
		;bcutoff=1*(DD[0] le AA[0]) ; does the first element of  our data set occur between our first and second trigger?

		doffsetf=1*(BB[0] gt DD[0]) ; does the first element of  our data set occur between our first and second trigger?
		aoffsetf=1*(BB[0] lt AA[0]) ; does the first element of  our data set occur between our first and second trigger?

		backcutoff=1*(DD[0] le CC[0]) ; does the last element of  our data set occur after a start trigger but before an end trigger when looked at backwards?
		fcutoff=1*(BB[bbn-1] le AA[aan-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?
;		print,"a offsetf=",aoffsetf
;		print,"d offsetf=",doffsetf
		;print, "bcutoff: ",bcutoff
		;print, "fcutoff: ",fcutoff

		


		print, xs[1]-xs[0]
		;start loop from second to last end trigger
		;along the way, will keep track of predicted step height
		zwidth=0


;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
print,"==================================================================="
print,"5			inbound side				 5"
print,"==================================================================="

		;z=fitting(ys,xs, AA,BB,CC,DD,aoffsetf,doffsetf,yBS)
		;for i=0,-1 do begin
fullIter=min([ddn,aan,bbn])
		lastmax=0

		for i=1,fullIter-1  do begin
			print,"FORWARDFORWARDFORWARDFORWARDFORWARDFORWARDFORWARD"
			cont=0
			shks=shocks
			sublengths=insublengths
			Subsets=inSubsets
			begs=inbegs
			ends=inends
			brokens=brokenin
			test=-1

			chi=0
			imx=-1
			imn=-1
			shockloc=-1
			Bmin=0
			Bmax=0

			zl=Fitloop(i, xs,ys,z,AA,BB,CC,DD,rollup,rollsec,mono,hill,yBS,curveType,shks,zwidth,trust, sublengths, Subsets,begs,ends,brokens,cont,test,dobug,minChi,minRemain,lastmax,chi,imx,imn,shockloc,Bmin,Bmax,hill2,rollthird,minM1,orderCustom,maxUp,Bmed)
			if dobug gt 0 then print,"test=",test
			if cont eq 1 then continue
			z=zl
			shocks=shks
			inSubsets=Subsets
			insublengths=sublengths
			inbegs=begs
			inends=ends
			brokenin=brokens
		
			inshocklocs[imn:imx]=shockloc

			inimaxs[imn:imx]=imx
			inimins[imn:imx]=imn
			inchis[imn:imx]=chi
			innums[imn:imx]=i

			inups[imn:imx]=Bmin
			indowns[imn:imx]=Bmax

		endfor
	 
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
		;print,"now for the range ys[0:DD[0]]"
		;"now for the range ys[0:DD[0]]"
		b0=BB[0]
		if DD[0] lt BB[0] then d0=DD[0] else d0=0

		yp=ys[0:b0]
		xp=xs[0:b0]
		np=size(yp,/n_el)
		

		;a0=AA[0]

		;"start with standard case"


		if(aoffsetf eq 0) then begin
			print,"-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D-D"
			i=0
			print,"now for the range ys[0:BB[0]]=ys[0:",b0,"]"
			a0=AA[0]
			;print,gi
			;print,"gim=",gim
			dim=d0

			regp=reg[dim:b0]

			if (min(regp) eq 1) then begin
				while (regp[0] gt 1) do begin
					if dim +1 eq a0 then break
					dim++
					regp=regp[1:*]
					yp=yp[1:*]
				endwhile
			endif

			;print,"g0=",b0
			zn=z[b0:*]

			;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)


			ybeg=ys[dim:a0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			nbeg=size(ybeg,/n_el)
			
			yend=ys[a0:b0] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			nend=size(yend,/n_el)

			;will want to try to make sure yp does not start on the previous step, and that our generated step will not be part of the next range

			ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			brefines=0
			frefines=0
	

			mbeg=nbeg
			mend=nend
		


			incfunctions=1
			decfunctions=1
			bi=b0
			d00=dim
			while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((yend[nend-1] lt mean(ybegtrust)) and decfunctions  )) do begin 
				
				;print,"(brefines,frefines)=",brefines,frefines
				;print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
				;we will do these seperately within the loop, but at same time so that one negligably affects the other 
				if ((ybeg[0] gt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
					;print,"pushing forward the end of shock number i=",0,", from gim=",gim,", where a0=",a0,", and g0=",g0, "and size(yp)=",size(yp,/n_el)
					yp=yp[1:*] ;shrinks our range by one
 					ybeg=ybeg[1:*]
					np--
					nbeg--
					dim++
					brefines++
					if ( brefines ge 2 *mbeg/5) or dim +1 eq a0 then begin
						incfunctions=0
						dim=d00
						yp=ys[dim:bi]
						np=size(yp,/n_el)
						ybeg=ys[dim:a0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nbeg=mbeg
						
					endif
				endif

				if ((yend[nend-1] lt mean(ybegtrust)) and (decfunctions eq 1)) then begin
					;print,"pulling back the end of shock number i=",i,", from gi=",gi
					yp=yp[0:np-2]
					np--
					yend=yend[0:nend-2]
					nend--
					bi--
					frefines++
					if ( frefines ge 1 *mbeg/5) or (a0+1 eq bi) or (nend eq 3) or (np eq 3) then begin
						;print,"i=",i,", ddn=",ddn
						decfunctions=0
						bi=BB[0]
						yp=ys[dim:bi]
						np=size(yp,/n_el)
						yend=ys[a0:bi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nend=mend
					endif
				endif

				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
				if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			endwhile 
			
			b0=bi
			;print, "the number of times we refined the beginning is ",brefines,"."
			;print, "the number of times we refined the end is ",frefines,"."
			xp=xs[dim:b0]
			yp=ys[dim:b0]


			xpa=xp-xp[0]
			yc=yp*0.0
			;if (rollthird eq 1) then a0=Third_Roll(hill2,dim,a0,bi,dobug) ; second order correction  
			ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			mn=mean(ybegtrust)
			a0=FalseShockSkipper(a0,bi,Bmed,mn)
			;print,"a0=",a0,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"a0-dim=",a0-dim
			if (rollup eq 1) then a0=monoRoll(a0,bi,dim,mono)
			if (rollsec eq 1) then a0=secondRoll(a0,bi,hill,dobug) ; second order correction 

			a0=aoscillate(a0,bi,dim,yBS,nbeg,nend,dobug)
			if (rollthird eq 1) then a0=Third_Roll(hill2,dim,a0,bi,dobug) ; third order correction 
			;print,"a0=",a0,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"a0-dim=",a0-dim

			;if curve function is f(x)=m0 tanh( m1 *(x-m2))+m3 then
			;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			;mx=mean(yendtrust)
			mx=25
			;print,"max=",mx
			;bottom of  step is at y=mn=m3-m0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			;mn=mean(ybegtrust)
			mn=5
			;print,"min=",mn
			
			m0=(mx-mn)/2
			m3=(mx+mn)/2
			
			;m2  is the x value in approximately at the top of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			;print,"m2  is the x value in the middle of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			m2=xpa[a0-dim]

			;with all this, can calculate guess for  m1 to zeroth order by
			;m1=  atanh((y-m3)/m0)/(x-m2)
			;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
			;m1=atanh( ( yp[nbeg/2] -m3 )/m0)/(xpa[nbeg/2] -m2)
			m1=1
			MM=[m0,m1,m2,m3]
			print,"to zeroth order, guess that MM=",MM
			MM0=MM
			CHISQ=-1
			weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
			status=-19
			yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
			MM1=MM
			PRINT, 'Function parameters: ', MM,", status=",status
			;ishock=round(MM[2]/MM[1]) +dim
			ishock=findShock(MM[2],MM[1],yfit,dim,1-doPerturb)
			imin=dim
			imax=bi
			working=((status eq 0) and (MM[0] gt 1)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt 1) and (ishock gt imin) and (ishock lt imax) and (MM[3]-MM[0]-mn lt maxUp) and (MM[1] gt minM1)
			if doPerturb then begin
				MM1=MM
				chi1=CHISQ
				yfit1=yfit
				working1=working
				ishock1=ishock

				imin=max([dim,ishock-7*60])
				imax=min([bi,ishock+7*60])
				print,"(x)shrunk front by [dim,imin,imin-dim]=",[dim,imin,imin-dim]
				print,"(x)shrunk back by [bi,imax,bi-imax]=",[bi,imax,bi-imax]

				ypp=ys[imin:imax]
				xpp=xs[imin:imax]
				print,"[numel(yp),numel(ypp)]=",[numel(yp),numel(ypp)]
				xppa=xpp-xpp[0]

				MM=coeffCalc(xpp,ypp, ishock,x00)
				weight=1.0/ypp
				yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
				PRINT, 'To second order, Function parameters: ', MM
				print,"status=",status,", CHISQ=",CHISQ,", numel(yfit)=",numel(yfit)
			
				ishock=findShock(MM[2],MM[1],yfit,imin,1)
				working=((status eq 0) and (MM[0] gt 1)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt 1) and (ishock gt imin) and (ishock lt imax) and (MM[3]-MM[0]-mn lt 10) and (MM[1] gt minM1)
					if ((Not working) and (working1)) or (CHISQ gt chi1) then begin
						MM=MM1
						CHISQ=chi1
						ishock=ishock1
						yfit=yfit1
						imin=dim
						imax=b0
						working=working1
					endif 	


			endif
			fracRemain=(bi-ishock)*1.0/(bi-dim)
			;print,"[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]=",[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
			;print,"abs(MM[2]-MM0[2])=",abs(MM[2]-MM0[2]),", 1*(abs(MM[2]-MM0[2]) lt 1000)=",1*(abs(MM[2]-MM0[2]) lt 1000)
			print,"[imin,ishock,imax]=",[imin,ishock,imax] 
			if ishock lt imin then print, "bad ishock: ishock = ",ishock," lt ", imin,"= imin"
			if ishock gt imax then print, "bad ishock: ishock = ",ishock," gt ", imax,"= imax"

			print,"[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]=",[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
			print,"[status,CHISQ lt minChi, bi-ishock gt minRemain]=",[status,CHISQ lt minChi, bi-ishock gt minRemain]
			if working then begin
					print,"0th outbound is working"
					inimaxs[imin:imax]=imax
					inimins[imin:imax]=imin
					inchis[imin:imax]=CHISQ
					innums[imin:imax]=i
					inshocklocs[imin:imax]=ishock
					inups[imin:imax]=MM[3]-MM[0]
					indowns[imin:imax]=MM[3]+MM[0]
				shocks[ishock]=1
				NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
				if( ncount eq 0) then begin;and (abs(MM[2]-MM0[2]) lt 1000) then begin
				;print,"no NANs"
			;	print,"gim=",gim,", g0-1=",b0-1
				
					for k=imin,imax-1 do z[k]=yfit[k-imin]	
				endif else begin
					print,"yfit has ",ncount," NANs. calculating directly."
					;if (abs(MM[2]-MM0[2]) ge 1000) then MM=MM0
					if doPerturb then tanhfit,xppa,MM,F else tanhfit,xpa,MM1,F
					NNN=where(FINITE(F) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
					;for k=gim,gi-1 do z[k]=F[k-gim]
					zn=z[imax:*]
					if ncount eq 0 then for k=imin,imax-1 do z[k]=F[k-imin]


				;	print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)


						
				endelse
				insublengths.add,bi-dim
				ishock=round(MM[2])+dim
				
				;print,"g0"
				zn=z[b0:*]

				;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)
			endif else brokenin.add,[i,dim,b0, xs[dim]-min(xs),xs[b0]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]
			inSubsets[dim:b0]=1;i
			inbegs[dim:a0-1]=1;i
			print,numel(inends),a0,b0
			inends[a0:b0]=1;i
		endif else begin 
			i=0
			;"if the trrigger pair WAS cut off, we try to approximate results here"
			;"want to know if we've started on the step or not. simplest first test is to check if g0 gt zwidth"
			onstep=1*(b0 le zwidth-4)
			;"if onstep eq 1, can assume without loss of generality that we are indeed on the step"
			;ty=yp
			tp=np
			mx=mean(yp)
			mn=mean(trust) 
			if ((onstep eq 1) or(yp[0] gt Mean([mx,mn])))  then begin
				for k=0,b0-1 do z[k]=mx
			endif else begin
			 ;if more than the width of previous, try fitting with m2=0 
				dim=0
				
				
				m0=(mx-mn)/2
				m3=(mx+mn)/2

				m2=0
				m1=atanh( ( yp[np/2] -m3 )/m0)/(xp[np/2] -m2)

				MM=[m0,m1,m2,m3]
				MM0=MM
				print,"to zeroth order, guess that MM=",MM

				weight=1.0/yp
				;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
				status=-11
				CHISQ=-1
				yfit=CURVEFIT(xp, yp, weight, MM, SIGMA, CHISQ=CHISQ, FUNCTION_NAME=curveType,status=status)

;				;yfit=CURVEFIT(xp, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType)
				PRINT, 'Function parameters: ', MM
				print,"status=",status
				ishock=round(MM[2]/MM[1]) +dim

				ishock=findShock(MM[2],MM[1],yfit,dim,1-doPerturb)

				imax=b0
				imin=dim
				;ishock=round(MM[2]/MM[1])+dim;-x00

				ishock1=ishock
				MM1=MM
				chi1=CHISQ
				working =((status eq 0) and (MM[0] gt 1)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[1] gt 0) and (ishock gt imin) and (ishock lt imax) and (MM[3]-MM[0]-mn lt maxUp) and (MM[1] gt minM1)
				

				if doPerturb then begin
					yfit1=yfit
					working1=working
					MM1=MM

					imin=max([dim,ishock-7*60])
					imax=min([bi,ishock+7*60])
					print,"(0) shrunk front by [dim,imin,imin-dim]=",[dim,imin,imin-dim]
					print,"(0) shrunk back by [bi,imax,bi-imax]=",[bi,imax,bi-imax]

					ypp=ys[imin:imax]
					xpp=xs[imin:imax]
					print,"[numel(yp),numel(ypp)]=",[numel(yp),numel(ypp)]
					xppa=xpp-xpp[0]

					MM=coeffCalc(xpp,ypp, ishock,x00)
					weight=1.0/ypp
					yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
					print,"numel(yfit)=",numel(yfit)
					PRINT, 'To second order, Function parameters: ', MM
					print,"status=",status,", CHISQ=",CHISQ

					ishock=findShock(MM[2],MM[1],yfit,imin,1)
					working =((status eq 0) and (MM[0] gt 1)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[1] gt 0) and (ishock gt imin) and (ishock lt imax) and (MM[3]-MM[0]-mn lt maxUp) and (MM[1] gt minM1)

					if ((Not working) and (working1)) or (CHISQ gt chi1) then begin
						MM=MM1
						CHISQ=chi1
						ishock=ishock1
						yfit=yfit1
						imin=dim
						imax=b0
						working=working1
					endif 	

				endif


				if ishock lt imin then print, "bad ishock: ishock = ",ishock," lt ", imin,"= imin"
				if ishock gt imax then print, "bad ishock: ishock = ",ishock," gt ", imax,"= imax"
				fracRemain=(bi-ishock)*1.0/(bi-dim)
				print,"[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]=",[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
				print,"[status,CHISQ lt minChi, bi-ishock gt minRemain]=",[status,CHISQ lt minChi, bi-ishock gt minRemain]
				if ((status eq 0) and (MM[0] gt 1)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[1] gt 0) and (ishock gt imin) and (ishock lt imax) then begin

					print,"0th inbound is working"
					inups[imin:imax]=MM[3]-MM[0]
					indowns[imin:imax]=MM[3]+MM[0]
					inimaxs[imin:imax]=imax
					inimins[imin:imax]=imin
					inchis[imin:imax]=CHISQ
					innums[imin:imax]=i
					inshocklocs[imin:imax]=ishock
					insublengths.add,bi-dim
					NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
					if( ncount eq 0) then begin
						for k=imin,imax-1 do z[k]=yfit[k-imin]	
					endif else begin
			;		print,"yfit has ",ncount," NANs. calculating directly."
						tanhfit,xp,MM,F
					
						for k=imin,imax-1 do z[k]=F[k-imin]
					
					endelse


					shocks[ishock]=1
					inSubsets[dim:b0-1]=1;i
					inbegs[dim:a0-1]=1;i
					inends[a0:b0-1]=1;i
				endif else brokenin.add,[0,dim,b0, xs[dim]-min(xs),xs[b0]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]				
			endelse

		

		endelse
		

		;"finally, we need the remaining bit after DD[ddn-1]"

		;"in hindsight, attempting to conjure one  up with insufficient data would be falsifying things. Not being part of our data set, may as well"
		;"set the remainder to zero for now"
		zn=z[b0:*]

		;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)
		;if DD[ddn-1] gt BB[bbn-1] then GN=DD[ddn-1] else GN=BB[bbn-1]
		;yp=ys[GN:*]
		;print,GN
		for k=lastmax+1,N-1 do z[k]=0
		zn=z[b0:*]

		;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

		;np=size(yp,/n_el)
		;ntrust=size(trust,/n_el)

		;if(np le 6 * ntrust) then for k=bbn-1,N-1 do z[k]=0 ; "if number of remaining element is much less than a period, then the step won't arrive"

		;else begin
		;	;"what we do depends on whether fcutoff eq 0"
		;	yt=yp[4*ntrust:5*ntrust -1]
		;	std=stddev(yt)
		;	meant=mean(yt)
		;	if((yp[N-1] le meant+std) or (fcutoff eq 0)) then for k=bn-1,N-1 do z[k]=0
		;	else begin
		;		mx=yp[N-1]
		;		mn=meant
				 

		;	endif


		;endelse



;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
;----------------------------------OUTBOUND SIDE-------------------------------------;----------------------------------OUTBOUND SIDE-------------------------------------
print,"==================================================================="
print,"			outbound side"
print,"==================================================================="
;nxs=Reverse(xs)
nxs=xs
nys=REVERSE(ys)

nreg=reverse(reg)
nhill2=-1* REVERSE(hill2)
dat.y=nys
dat.x=nxs
nBmed=reverse(Bmed)
;store_data,plt+"_rev",data=dat,limits=limits

;print,"size(nys)=",size(nys)
nyae=reverse(yae)
dat.y=nyae
;store_data,"ascend_end_rev",data=dat
nyab=reverse(yab)
dat.y=nyab
;store_data,"ascend_begin_rev",data=dat
nyde=reverse(yde)
dat.y=nyde
;store_data,"descend_end_rev",data=dat
nydb=reverse(ydb)
dat.y=nydb
;store_data,"descend_begin_rev",data=dat



nyBS=REVERSE(yBSb);REVERSE(yBS)
;datBS.y=nyBS
;datBS.x=nxs
;get_data,'B_sec_smoothed',data=datBS
nAA = WHERE(nyae ne 0, naan, COMPLEMENT=G_C, NCOMPLEMENT=gcount_c);defines the boundaries in  forward direction. beginning  of negative shock
		if dobug gt 0 then print,yae[DD]

nDD = WHERE(ydb ne 0, nddn, COMPLEMENT=H_C, NCOMPLEMENT=hcount_c); beginnging of shock 

nBB = WHERE(yab ne 0, nbbn, COMPLEMENT=K_C, NCOMPLEMENT=kcount_c) ;end of negative shock.  defines boundaries in  negative time  direction
nCC = WHERE(yde ne 0, nccn, COMPLEMENT=L_C, NCOMPLEMENT=lcount_c); end of  negative  shock


;if nDD[0] gt nAA[0] then nAA=nAA[1:*]
;if nDD[0] gt nBB[0] then nBB=nBB[1:*]

nmono=reverse(mono);)bmono)

get_data,'monof-B60dsane_rev',data=datMon
nhill=reverse(hill)

get_data,'B5-3d10_rev',data=datB5d
nz=fltarr(N)
nshocks=fltarr(N)

nDD=N-1-reverse(AA)
nAA=N-1-reverse(DD)
nBB=N-1-reverse(CC)
nCC=N-1-reverse(BB)
;print,"nAA=",nAA
;print,"nBB=",nBB
;print,"nCC=",nCC
;print,"nDD=",nDD
;doffsetb=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
;aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?
;if dobug gt 0 then print,"numel(nys)=",numel(nys)
;fullIter=min([nddn,naan,nbbn])
;for i=1 ,fullIter-1 do begin
;	j=i-aoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
;	k=i-1+doffsetb
;	aj=nAA[j]
;	bi=nBB[i]
;	dim=nDD[k]
;	print,"[nDD[",k,"],nAA[",j,",nBB[",i,"]]=",[dim,aj,bi]
;	if dobug gt 0 then print,"[nDD[",k,"],nAA[",j,",nBB[",i,"]]=",[dim,aj,bi]
;endfor
doffsetb=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?
dboffsetb=1*(nBB[1] lt nDD[0])
daoffsetb=1*(nAA[1] lt nDD[0])
if dobug gt 1 then print,'numel(nys)=',numel(nys)
if dobug gt 1 then print,'[nddn,naan,nbbn]=',[nddn,naan,nbbn]
fullIter=min([nddn,naan,nbbn])
print,'fullIter=',fullIter
for i=1 ,fullIter-1 do begin
	j=i-aoffsetb+daoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
	k=i-1+doffsetb
	ii=i+dboffsetb
	aj=nAA[j]
	bi=nBB[ii]
	dim=nDD[k]
	print,'[nDD[',k,'],nAA[',j,',nBB[',ii,']]=',[dim,aj,bi]
	if dobug gt 1 then print,'[nDD[',k,'],nAA[',j,',nBB[',ii,']]=',[dim,aj,bi]
	
endfor
;stop
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------
;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------;-----------------------BEGINNING OF FOR LOOP --------------


		;z=fitting(ys,xs, AA,BB,CC,DD,aoffsetf,doffsetf,yBS)
		;for i=0,-1 do begin
		lastmax=0
		for i=1,fullIter-1  do begin
			print,"L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-"
			cont=0
			shks=nshocks
			sublengths=outsublengths
			Subsets=outSubsets
			begs=outbegs
			ends=outends
			brokens=brokenout
			test=-1
			shockloc=-1
			imn=-1
			imx=-1
			chi=0

			Bmin=0
			Bmax=0
			zl=Fitloop(i, nxs,nys,nz,nAA,nBB,nCC,nDD,rollup,rollsec,nmono,nhill,nyBS,curveType,shks,zwidth,trust, sublengths, Subsets,begs,ends,brokens,cont,test,dobug,minChi,minRemain,lastmax,chi,imx,imn,shockloc,Bmin,Bmax,nhill2,rollthird,minM1,orderCustom,maxUp,nBmed)
			if dobug gt 0 then print,"test=",test
			if cont eq 1 then continue
			nz=zl
			nshocks=shks
			outsublengths=sublengths
			outSubsets=Subsets
			outbegs=begs
			outends=ends
			brokenout=brokens
			outnums[imn:imx]=i
			outshocklocs[imn:imx]=shockloc
			outimaxs[imn:imx]=imx
			outimins[imn:imx]=imn
			outchis[imn:imx]=chi

			outups[imn:imx]=Bmin
			outdowns[imn:imx]=Bmax



		endfor 
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------;------------------------------END OF STANDARD RANGE--------------
		;print,"now for the range ys[0:DD[0]]"
		;"now for the range ys[0:DD[0]]"
		b0=nBB[0]
		if nDD[0] lt b0 then d0=nDD[0] else d0=0

		yp=nys[0:b0]
		xp=nxs[0:b0]
		np=size(yp,/n_el)
		

		;a0=nAA[0]

		;"start with standard case"


		if(aoffsetb eq 0) then begin
			print,"L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-L-"
			print,"now for the range nys[0:nBB[0]]=nys[0:",nBB[0],"]"


			a0=nAA[0]
			print,"[d0,a0,b0]=",[d0,a0,b0]
			;print,gi
			;print,"gim=",gim
			dim=d0

			nregp=nreg[dim:b0]

			if (min(nregp) eq 1) then begin
				while (nregp[0] gt 1) do begin

					dim++
					nregp=nregp[1:*]
					yp=yp[1:*]
					xp=xp[1:*]
				endwhile
			endif

			;print,"g0=",b0
			nzn=nz[b0:*]

			;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)


			ybeg=nys[dim:a0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			nbeg=size(ybeg,/n_el)
			
			yend=nys[a0:b0] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			nend=size(yend,/n_el)

			;will want to try to make sure yp does not start on the previous step, and that our generated step will not be part of the next range

			ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			brefines=0
			frefines=0
	

			mbeg=nbeg
			mend=nend
		

			incfunctions=1
			decfunctions=1
			bi=b0
			d00=dim
			while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((yend[nend-1] lt mean(ybegtrust)) and decfunctions  )) do begin 
				
				;print,"(brefines,frefines)=",brefines,frefines
				;print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
				;we will do these seperately within the loop, but at same time so that one negligably affects the other 
				if ((ybeg[0] gt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
					;print,"pushing forward the end of shock number i=",0,", from gim=",gim,", where a0=",a0,", and g0=",g0, "and size(yp)=",size(yp,/n_el)
					yp=yp[1:*] ;shrinks our range by one
 					ybeg=ybeg[1:*]
					np--
					nbeg--
					dim++
					brefines++
					if ( brefines ge 2 *mbeg/5) or (nbeg eq 1) or (np eq 2)  then begin
						incfunctions=0
						;dim=DD[i-1]
						dim=d00
						yp=nys[dim:bi]
						np=size(yp,/n_el)
						ybeg=nys[dim:a0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nbeg=mbeg
						
					endif
				endif

				if ((yend[nend-1] lt mean(ybegtrust)) and (decfunctions eq 1)) then begin
					;print,"pulling back the end of shock number i=",i,", from gi=",gi
					yp=yp[0:np-2]
					np--
					nend--
					
					if nend gt 1 then yend=yend[0:nend-1]
					bi--
					frefines++
					if ( frefines ge 1 *mbeg/5) or (nend eq 2) or (a0 +1 eq bi) or (np eq 2) then begin
						;print,"i=",i,", ddn=",ddn
						decfunctions=0
						bi=b0;nBB[0]
						yp=nys[dim:bi]
						np=size(yp,/n_el)
						yend=nys[a0:bi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nend=mend
					endif
				endif

				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
				if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			endwhile 
			b0=bi
			;print, "the number of times we refined the beginning is ",brefines,"."
			;print, "the number of times we refined the end is ",frefines,"."
			xp=nxs[dim:b0]
			yp=nys[dim:b0]
			xpa=xp-min(xp)

			;plotn1=plot(xpa,yp, title="Shock "+newName)
			
		;	plotn2=plot(xpa,thSpk(xpa,-dim+a0),'r-',/overplot)			
			;plotn2=plot(fltarr(2) + a0-dim, plotn1.yrange, name="zerothOrder",color='red', /overplot)
			yc=yp*0.0
			;if curve function is f(x)=m0 tanh( m1 *(x-m2))+m3 then
			;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			mx=max(yend)
			;mx=25
			;print,"max=",mx
			;bottom of  step is at y=mn=m3-m0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			mn=mean(ybegtrust)
			;mn=5
			;print,"min=",mn
			
			m0=(mx-mn)/2
			m3=(mx+mn)/2

			ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			mn=mean(ybegtrust)
			a0=FalseShockSkipper(a0,bi,Bmed,mn)
			
			if (rollup eq 1) then a0=monoRoll(a0,bi,dim,nmono)
			;plotn3=plot(xpa,thSpk(xpa,-dim+a0),'y-',/overplot)		
			;plotn3=plot(fltarr(2) + a0-dim, plotn1.yrange, name="monoroll",color='yellow', /overplot)
			print,"after monorolling"
			print,"[imin,a0,imax]=",[dim,a0,b0]
			if (rollsec eq 1) then a0=secondRoll(a0,bi,nhill,dobug) ; second order correction 
			;p4=plot(xpa,thSpk(xpa,-dim+a0),'g-',/overplot)	
			;plotn4=plot(fltarr(2) + a0-dim, plotn1.yrange, name="rollsec",color='green', /overplot)
			print,"after secondroll"
			print,"[imin,a0,imax]=",[dim,a0,b0]

			nl=300*nhill2[dim:bi]+10
			nl[0:100]=0.0
			
			;plotn6=plot(fltarr(2) + a0-dim, plotn1.yrange, name="third_roll",color='blue', /overplot)			
			print,"after third_roll"
			print,"[imin,a0,imax]=",[dim,a0,b0]
			a0=aoscillate(a0,bi,dim,nyBS,nbeg,nend,dobug)
			;plotn5=plot(xpa,thSpk(xpa,-dim+a0),'c-',/overplot)
			;plotn5=plot(fltarr(2) + a0-dim, plotn1.yrange, name="aoscillate",color='cyan', /overplot)
			print,"after aoscillate"
			print,"[imin,a0,imax]=",[dim,a0,b0]
			;plotnh=plot(xpa,nl,color=56,/overplot)
			if (rollthird eq 1) then a0=Third_Roll(nhill2,dim,a0,bi,dobug) ; third order correction 

			;plotn6=plot(xpa,thSpk(xpa,-dim+a0),'b-',/overplot);



			;m2  is the x value in approximately at the top of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			;print,"m2  is the x value in the middle of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			m2=xpa[a0-dim]

			;with all this, can calculate guess for  m1 to zeroth order by
			;m1=  atanh((y-m3)/m0)/(x-m2)
			;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
			;m1=atanh( ( yp[nbeg/2] -m3 )/m0)/(xpa[nbeg/2] -m2)
			m1=1
			MM=[m0,m1,m2,m3]
			print,"to zeroth order, guess that MM=",MM

			weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly

			status=-11
			CHISQ=-1
			print,numel(xpa),numel(yp),numel(weight)

			tanhfit,xpa,MM,F0
	

			;plotf0=plot(xpa,F0,color="steel_blue",'--',/overplot)

			yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, CHISQ=CHISQ, FUNCTION_NAME=curveType,status=status)
			yfit1=yfit

			print,"status, chisq"
			print,status, CHISQ
			chi1=CHISQ
			PRINT, 'Function parameters'
			print, MM
			ishock=findShock(MM[2],MM[1],yfit,dim,1-doPerturb)
			ishock1=ishock
			print,"[max,min,bi-ishock]"
			print,[MM[3]+MM[0],MM[3]-MM[0],bi-ishock]
			;ishock=round(MM[2]/MM[1])+dim
			print,"[imin,ishock,imax]"
			imin=dim
			imax=bi
			print,[imin,ishock,imax]
			working=((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi) and ( MM[0] gt 1) and (ishock gt imin) and (ishock lt imax) and (MM[3]-MM[0]-mn lt maxUp) and (MM[1] gt minM1)
			working1=working
			;if working then plotn7=plot(fltarr(2)+ ishock-dim, plotn1.yrange, name="ishock",color='magenta', /overplot);plot(xpa,thSpk(xpa,-dim+ishock),'m-',/overplot)
			;if working then plotf1=plot(xpa,yfit1, name="fit",color='purple','-', /overplot);plot(xpa,thSpk(xpa,-dim+ishock),'m-',/overplot)
			;ishock=round(MM[2]/MM[1])+dim;-x00
			if doPerturb then begin
				imin=max([dim,ishock-7*60])
				imax=min([bi,ishock+7*60])
				print,"[imin,ishock,imax]"
				print,[imin,ishock,imax]
				ypp=ys[imin:imax]
				xpp=xs[imin:imax]
				xppa=xpp-xpp[0]
				MM1=MM
				MM=coeffCalc(xpp,ypp, ishock,x00)
				print,"to first order, guess that MM=",MM
				print,"where ishock=",round(MM[2]/MM[1])+imin
				tanhfit,xpp,MM,F01
	

				;plotf01=plot(xpp-xp[0],F01,color="gold",'--',/overplot)
				weight=1.0/ypp
				yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
				print,CHISQ
				print,"status, chisq"
				print,status, CHISQ
				PRINT, 'Function parameters'
				print, MM
				ishock=findShock(MM[2],MM[1],yfit,imin,1)
				working=((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi) and ( MM[0] gt 1) and (ishock gt imin) and (ishock lt imax) and (MM[3]-MM[0]-mn lt maxUp) and (MM[1] gt minM1)
				;if working then plotn8=plot(fltarr(2)+ ishock-dim, plotn1.yrange, name="ishock2",color='peru', /overplot);plot(xpa,thSpk(xpa,-dim+ishock),'-',color='peru',/overplot)	
				;if working then plotf2=plot(xppa-xp[0]+xpp[0],yfit, name="fit2",color=37,'--', /overplot);plot(xpa,thSpk(xpa,-dim+ishock),'m-',/overplot)
				if ((Not working) and (working1)) or (CHISQ gt chi1) then begin
					MM=MM1
					CHISQ=chi1
					ishock=ishock1
					yfit=yfit1
					imin=dim
					imax=bi
					working=working1
				endif 	
			endif

			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)


			if dobug gt 0 then print,"[imin,ishock,imax]"
			if dobug gt 0 then print,[imin,ishock,imax]
			if ishock lt imin then print, "bad ishock: ishock : ",ishock," lt ", imin,": imin"
			if ishock gt imax then print, "bad ishock: ishock : ",ishock," gt ", imax,": imax"
			if dobug gt 0 then print,"[max,min,remain,fracRemain]:[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]"
			fracRemain=(imax-ishock)*1.0/(imax-imin)
			print,[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
			;print,"[status,CHISQ , minChi, minChi-CHISQ, minChi-CHISQ ge 0, bi-ishock gt minRemain]:",[status,CHISQ , minChi,minChi-CHISQ, minChi-CHISQ ge 0,bi-ishock gt minRemain]
			if working then begin
					print,"[imin,ishock,imax]=",[imin,ishock,imax]
					print,"0th outbound is working"
					;t1=TEXT(.1,.8,'Good fit')
					outimaxs[imin:imax]=imax
					outimins[imin:imax]=imin
					outchis[imin:imax]=CHISQ
					outnums[imin:imax]=i
					outshocklocs[imin:imax]=ishock

					outups[imin:imax]=MM[3]-MM[0]
					outdowns[imin:imax]=MM[3]+MM[0]

				nshocks[ishock]=1					
				outsublengths.add,bi-dim
				if( ncount eq 0)  then begin
					;print,"no NANs"
				;	print,"gim=",gim,", g0-1=",b0-1
							nshocks[ishock]=1					
					for k=imin,imax-1 do nz[k]=yfit[k-imin]	
				endif else begin
			;	print,"yfit has ",ncount," NANs. calculating directly."
					tanhfit,xpa,MM,F
					nzn=nz[b0:*]

				;	print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

					for k=imin,imax-1 do nz[k]=F[k-imin]
			
				endelse
			endif else brokenout.add,[0,dim,b0, nxs[dim]-min(nxs),nxs[b0]-min(nxs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]
			;print,"g0"
			nzn=nz[b0:*]

			;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

		endif else begin 
			;"if the trrigger pair WAS cut off, we try to approximate results here"
			print,"0th sheath was cut off"
			;"want to know if we've started on the step or not. simplest first test is to check if g0 gt zwidth"
			onstep=1*(b0 le zwidth-4)
			;"if onstep eq 1, can assume without loss of generality that we are indeed on the step"
			;ty=yp
			tp=np
			mx=mean(yp)
			mn=mean(trust) 
			if ((onstep eq 1) or(yp[0] gt Mean([mx,mn])))  then begin
				for k=0,b0-1 do nz[k]=mx
			endif else begin
			 ;if more than the width of previous, try fitting with m2=0 
				
				
				
				m0=(mx-mn)/2
				m3=(mx+mn)/2

				m2=0
				m1=1;atanh( ( yp[np/2] -m3 )/m0)/(xp[np/2] -m2)

				MM=[m0,m1,m2,m3]
				MM0=MM
				if dobug gt 0 then print,"to zeroth order, guess that MM=",MM

				weight=1.0/yp
				;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
				status=-11
				CHISQ=-1
				yfit=CURVEFIT(xp, yp, weight, MM, SIGMA, CHISQ=CHISQ, FUNCTION_NAME=curveType,status=status)
				if dobug gt 0 then PRINT, 'Function parameters: ', MM

				if NOT finite(MM[1]) then MM=MM0
				ishock=round(MM[2]/MM[1]) +dim
				if dobug gt 0 then print,"[max,min,bi-ishock]=[m3+m0,m3-m0,bi-ishock]=",[MM[3]+MM[0],MM[3]-MM[0],bi-ishock]
				imin=dim
				imax=bi
				if dobug gt 0 then print,"[imin,ishock,imax]=",[imin,ishock,imax]
				if doPerturb then begin
				imin=max([dim,ishock-7*60,0])
				imax=min([bi,ishock+7*60,N-1])
				if dobug gt 0 then print,"[imin,ishock,imax]=",[imin,ishock,imax]
					ypp=nys[imin:imax]
					xpp=nxs[imin:imax]
					xppa=xpp-xpp[0]
					MM1=MM
					MM=coeffCalc(xpp,ypp, ishock,x00)

					weight=1.0/ypp
					yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
	
					ishock=findShock(MM[2],MM[1],yfit,imin,1)
			endif


				ishock=findShock(MM[2],MM[1],yfit,imin,1)
				NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
			if dobug gt 0 then  print,"[imin,ishock,imax]=",[imin,ishock,imax]
			if ishock lt imin then print, "bad ishock: ishock = ",ishock," lt ", imin,"= imin"
			if ishock gt imax then print, "bad ishock: ishock = ",ishock," gt ", imax,"= imax"
			
			if ((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ)  eq 1) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt 1) and (ishock gt imin) and (ishock lt imax) and (MM[3]-MM[0]-mn lt maxUp) and (MM[1] gt minM1) then begin
					outimaxs[imin:imax]=imax
					outimins[imin:imax]=imin
					outchis[imin:imax]=CHISQ
					outnums[imin:imax]=i
					outshocklocs[imin:imax]=ishock
					outups[imin:imax]=MM[3]-MM[0]
					outdowns[imin:imax]=MM[3]+MM[0]
					if( ncount eq 0) then begin
					for k=0,b0-1 do nz[k]=yfit[k]	
					endif else begin
			;			print,"yfit has ",ncount," NANs. calculating directly."
						tanhfit,xp,MM,F

						for k=0,i-1 do nz[k]=F[k]
				
					endelse
					;ishock=round(MM[2]/MM[1]) +imin
					outSubsets[dim:b0]=1
					outbegs[dim:a0]=1
					outends[a0:b0]=1
					nshocks[ishock]=1				
				endif
				
			endelse
		

		endelse
		

		;"finally, we need the remaining bit after DD[ddn-1]"

		;"in hindsight, attempting to conjure one  up with insufficient data would be falsifying things. Not being part of our data set, may as well"
		;"set the remainder to zero for now"
		nzn=nz[b0:*]

		;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)
		;GN=nDD[bbn-1]
		;yp=nys[GN:*]
		;print,GN
		for k=lastmax+1,N-1 do nz[k]=0
		nzn=nz[b0:*]

		;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

		;np=size(yp,/n_el)
		;ntrust=size(trust,/n_el)

		;if(np le 6 * ntrust) then for k=bbn-1,N-1 do nz[k]=0 ; "if number of remaining element is much less than a period, then the step won't arrive"

		;else begin
		;	;"what we do depends on whether fcutoff eq 0"
		;	yt=yp[4*ntrust:5*ntrust -1]
		;	std=stddev(yt)
		;	meant=mean(yt)
		;	if((yp[N-1] le meant+std) or (fcutoff eq 0)) then for k=bn-1,N-1 do z[k]=0
		;	else begin
		;		mx=yp[N-1]
		;		mn=meant
				 

		;	endif


;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
;---------------------------------END OF OUTBOUND-----------------------------------;---------------------------------END OF OUTBOUND-----------------------------------
	endif
	print,"<*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*><*>"
	zb=reverse(nz)
	znb=reverse(nzn)
	shocksb=reverse(nshocks)

	outSubsets=reverse(outSubsets)
	outends=reverse(outends)
	outbegs=reverse(outbegs)


	outimaxs=reverse(outimaxs)
	outimins=reverse(outimins)
	outchis=reverse(outchis)
	outnums=reverse(outnums)

	
	outshocklocs=reverse(outshocklocs)
	;outshocklocs2=outshocklocs
	;for i=0, N-1 do if outshocklocs2[i] ne -1 then outshocklocs2[i]=N-1-outshocklocs2[i]

	outdowns=reverse(outdowns)
	outups=reverse(outups)

	adjChiIn=inchis*0.0
	adjChiOut=fltarr(N)
	for i=0,N-1 do if inchis[i] ne 0 then adjChiIn[i]= abs(inchis[i]-1)
	for i=0,N-1 do if outchis[i] ne 0 then adjChiOut[i]= abs(outchis[i]-1)
	chis=outchis
	for i=0 , N-1 do if inchis[i] ne 0 then chis[i] = inchis[i]
	adjChi=fltarr(N)
	for i=0,N-1 do if chis[i] ne 0 then adjChi[i]= abs(chis[i]-1)
	store_data,newName+'_CHISQ_inbound',data={x:xs,y:inchis,ytitle:"REDUCED CHISQ"}
	store_data,newName+'_ADJCHISQ_inbound',data={x:xs,y:adjChiIn,ytitle:"abs(REDUCED CHISQ-1)"}
	store_data,newName+'_CHISQ_outbound',data={x:xs,y:outchis,ytitle:"REDUCED CHISQ"}
	store_data,newName+'_ADJCHISQ_outbound',data={x:xs,y:adjChiOut,ytitle:"abs(REDUCED CHISQ-1)"}
	store_data,newName+'_CHISQ',data={x:xs,y:chis,ytitle:"REDUCED CHISQ"}
	store_data,newName+'_ADJCHISQ',data={x:xs,y:adjChi,ytitle:"abs(REDUCED CHISQ-1)"}

	rl=""
	cl='c'
	if NOT rollup then begin
		 rl="-unrolled"
		cl='m'
	endif 

	NNN=where(FINITE(z) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	z[where(finite(z,/NAN))]=0
;	print,'size(z)=',size(z)	
;	print,'size of NNN=',size(NNN) ,'with NNN=',NNN
	;print,"end loop"
	result = MOMENT(z)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,"max(z)=",max(z),", min(z)=",min(z)
	
	zn=z[b0:*]

	;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

	store_data,'shocks'+rl+'_inbound',data={x:xs,y:shocks,ytitle:'flag'}
	store_data,newName+'_shocks_inbound',data={x:xs,y:shocks,ytitle:'flag'}
	store_data,'sublengths'+rl+'_inbound',data={x:xs,y:inSubsets,ytitle:'flag'}

	store_data,'sublengths'+rl+'_inbound_begin',data={x:xs,y:inbegs,ytitle:'flag'}
	store_data,'sublengths'+rl+'_inbound_end',data={x:xs,y:inends,ytitle:'flag'}


	if(total(z-ys) eq 0) then print, 'broken'
 	dat.y=z
	;print,size(dat)
	help,dat
	store_data,newName+'_inbound',data={x:xs,y:z,ytitle:"Magnetic Field (nT)", inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs,inups:inups,indowns:indowns}
	;store_data,newName+'CHISQ_inbound',data={x:xs,y:inchis,ytitle:"REDUCED CHISQ"}
	store_data,newName+'_CHISQ_inbound',data={x:xs,y:adjChiIn,ytitle:"abs(REDUCED CHISQ-1)"}
;--------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-----
;--------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-----
;--------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-------------------<><><><>---The outbound fit  -<><><><><><>-----
	store_data,'sublengths'+rl+'_outbound',data={x:xs,y:outSubsets,ytitle:'flag'}

	store_data,'sublengths'+rl+'_outbound_begin',data={x:xs,y:outbegs,ytitle:'flag'}
	store_data,'sublengths'+rl+'_outbound_end',data={x:xs,y:outends,ytitle:'flag'}

	NNN=where(FINITE(zb) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	zb[where(finite(zb,/NAN))]=0
;	print,'size(z)=',size(zb)	
;	print,'size of NNN=',size(NNN) ,'with NNN=',NNN
	;print,"end loop"
	result = MOMENT(zb)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,"max(z)=",max(z),", min(z)=",min(z)
	
	znb=zb[b0:*]

	;print,"mean(zn)=",mean(znb),",  max(zn)=",max(znb)

	store_data,'shocks'+rl+'_outbound',data={x:xs,y:shocksb,ytitle:'flag'}
	store_data,newName+'_shocks_outbound',data={x:xs,y:shocksb,ytitle:'flag'}
	if(total(zb-ys) eq 0) then print, 'broken'
 	dat.y=zb
	;print,size(dat)
	;help,dat
	store_data,newName+'_outbound',data={x:xs,y:zb,ytitle:"Magnetic Field (nT)", outchis:outchis, outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,outups:outups,outdowns:outdowns}
	
;-----------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>--------
;-----------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>--------
;-----------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>---------------------<<><><><><><>>---The Combined Fit---<<><><><>>--------
zf=z
	for i=0, N-1 do begin
		;if outshocks[i] ne 0 then shocks[i] = outshocks[i]

		outSL=outshocklocs[i]
		inSL=inshocklocs[i]
		isOut=1*(outSL ne -1)
		isIN=1*(inSL ne -1)
		IF ((Not isOut) and isIN) or ( (isOut+isIn eq 2) and (abs(i-outSL) ge abs(i-inSL)))  then begin
			zf[i]=z[i]
			;chis[i]=inChis[i]
		ENDIF ELSE BEGIN 
			IF (isOut and (inSL eq -1)) or  ( (isOut+isIn eq 2) and (abs(i-outSL) lt abs(i-inSL)))  then zf[i]=zb[i]
			;downs[i] = outdowns[i]
			;ups[i] = outups[i]
			;shockLocs[i] = outSL
			;downups[i] = outdownups[i]
			;imaxs[i]=outimaxs[i]
			;imins[i]=outimins[i]
			;chis[i]=outChis[i]
			;ENDIF 	
		ENDELSE
	endfor


	;for i=0, N-1 do begin

		;if z[i] ne 0 then zb[i]=z[i]

	;endfor
	;zf=zb
	for i=0, N-1 do begin

		if shocksb[i] ne 0 then shocks[i]=shocksb[i]

	endfor

	;z=z+zb
	;shocks+=shocksb
	
	NNN=where(FINITE(zf) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
	zf[where(finite(zf,/NAN))]=0
;	print,'size(z)=',size(z)	
;	print,'size of NNN=',size(NNN) ,'with NNN=',NNN
	;print,"end loop"
	result = MOMENT(zf)
	;PRINT, 'Mean: ', result[0] & PRINT, 'Variance: ', result[1] & $
   ;PRINT, 'Skewness: ', result[2] & PRINT, 'Kurtosis: ', result[3]
	;print,"max(z)=",max(z),", min(z)=",min(z)
	
	zn=zf[b0:*]

	;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)

	store_data,'shocks'+rl,data={x:xs,y:shocks,ytitle:'flag'}
	store_data,newName+'_shocks',data={x:xs,y:shocks,ytitle:'flag'}
	if(total(zf-ys) eq 0) then print, 'broken'
 	dat.y=zf
	;print,size(dat)
	;help,dat
	store_data,newName,data={x:xs,y:zf,ytitle:"Magnetic Field (nT)", inchis:inchis, inimaxs:inimaxs,inimins:inimins,innums:innums,inshocks:inshocklocs,outchis:outchis, outimaxs:outimaxs,outimins:outimins,outnums:outnums,outshocks:outshocklocs,shocks:shocks}





	insublengths=insublengths.toarray()
	outsublengths=reverse(outsublengths.toarray())
	print,"insublengths=",insublengths
	if numel(insublengths) gt 0 then PRINT, 'Mean: ', mean(insublengths), ', Variance: ', stddev(insublengths)
	print,"outsublengths=",outsublengths
	if numel(outsublengths) gt 0 then PRINT, 'Mean: ', mean(outsublengths), ', Variance: ', stddev(outsublengths)


	store_data,"outnums",data={x:xs,y:outnums,ytitle:"shock number"}


	brokenin=brokenin.toarray()
	brokenout=reverse(brokenout.toarray())
	
	print,"brokenin"
	print,"i,dim,bi, xs[dim]-min(xs),xs[bi]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]"
	
	print,brokenin

	print,"brokenout"
	print,"i,dim,bi, nxs[dim]-min(nxs),nxs[bi]-min(nxs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]"
	
	print,brokenout

	print,".oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo.oOo."
	print,'		finished creating ',newName
	print,"============================================"
	;wdelete
end
