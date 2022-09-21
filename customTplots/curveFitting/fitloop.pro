;+
;  function fitloop
;  CHANGELOG: 2022-01-13T17:49
; 
; ; when defining iminB, in case where d00 ne edim, was:
; 				print,"d00 ne edim"
; 			
; 				mxfore=max(Fore[(d00+ishock)/2:ishock]);Fore[aj]
; 			iminB=(where(Fore[mean([d00,ishock]):ishock] eq mxfore))[-1]
; 			print,iminB
; ; This would define iminB as being in the range [0: ishock-mean([d00,ishock])] rather than [mean([d00,ishock]):ishock] (possibly explaining those times the pointer moved to the beginning of a previous shock)
; ; We could change it to
; 				midloc=(d00+ishock)/2
; 			mxfore=max(Fore[midloc:ishock]);Fore[aj]
; 			iminB=(where(Fore[midloc:ishock] eq mxfore))[-1]+midloc
;  but this doesn't take into the account the possibility that that maximum is the outer edge of the adjacent shock crossing (where Fore[iminB+1] eq 0) or where maxFore eq 1. Better to use
; 		if d00 eq edim or mxfore eq 1 then begin
; 			print,"d00 eq edim"
; 			iminB=max([dim,foot-2*60]);ishock-10*60
; 			print,iminB,foot-2*60
; 			;if ymm[iminB] gt ymf then begin
; 				;ympref=ymm[iminB:foot]
; 				;ympreB=ymm[iminB-2*60:iminB]
; 				;if total(min(ympref) lt [ymm[iminB],ymf]) ge 1 and ymm[iminB-2*60] lt min(ymm[iminB:foot]) then iminB=max([dim,iminB-2*60])
; 				
; 				;endif
; 	endif else begin
; 			print,"d00 ne edim"
; 			;
; 			IN1=where(Fore[d00:ishock] eq 1)+d00
; 			if  foot-IN1[0] ge 60 then begin
; 				iminB=IN1[0]
; 			endif else begin
; 				INMAX=where(Fore[d00:ishock] eq mxfore)+d00
; 				simB=INMAX[0]
; 				eimB=INMAX[-1]
; 				iminB=max([d00,simB+1,eimB-2*60]);;THE -2*60 here is so that we don't just spawn on top of the previous shock crossing	
; 			endelse
; 	endelse
; 				
; 			
;-




function thSpk,x,aj
	maxloc=-1
	n=numel(x)
;print,"n,aj=",n,aj
	b=4
	g=exp(-(abs(x-aj)/10^b)^(2))

	spk= g*50/(abs(x-aj)+1)
	mx=max(spk,maxloc)
	;print,
;print,mx,maxloc,aj,numel(x)
	;print,"spk[0:maxloc+4]=",spk[0:maxloc+4]
;print,"spk[aj-4:aj+4]=",spk[max([aj-4,0]):min([aj+4,n-1])]
	return,spk
end


function fitloop,i, xs,ys,z,AA,BB,CC,DD,rollup,rollsec,mono,B5d,yBS,curveType,shks,zwidth,trust, sublengths, Subsets,begs,ends,brokens,cont,test,dobug,minChi,minRemain,lastmax,chi,imx,imn,shockloc, Bmin,Bmax,B20d,rollthird,minM1,orderCustom,maxUp,Bmed,rhod,Bsd,extr,eDD,Fore,BU,B3,ymm,minM0,foots,foremaxs,phm,$
parallelend,derex,n_e,allownonfore=allownonfore,MMM=MMM,algshock0=algshock0,algshock1=algshock1
			nonfore=0

			if keyword_set(allownonfore) then nonfore=1

			bbn=numel(BB)
			aan=numel(AA)
			ddn=numel(DD)
			N=numel(Bsd)
			x00=xs[0]
			xa=xs-x00
			doffsetf=1*(BB[0] gt DD[0]) ; does the first element of  our data set occur between our first and second trigger?
			aoffsetf=1*(BB[0] lt AA[0]) ; does the first element of  our data set occur between our first and second trigger?


			foffsetf=1*(foots[0] gt AA[0])


			backcutoff=1*(DD[0] le CC[0]) ; does the last element of  our data set occur after a start trigger but before an end trigger when looked at backwards?
			fcutoff=1*(BB[bbn-1] le AA[aan-1]) ; does the last element of  our data set occur after a start trigger but before an end trigger?
			;doffsetb=1*(nBB[0] gt nDD[0]) ; does the first element of  our data set occur between our first and second trigger?
			;aoffsetb=1*(nBB[0] lt nAA[0]) ; does the first element of  our data set occur between our first and second trigger?
			if numel(BB) gt 1 then boffsetb=1*(BB[1] lt DD[0]) else boffsetb=0
			if numel(AA) gt 1 then aoffsetb=1*(AA[1] lt DD[0]) else aoffsetb=0
			;j=i-bcutoff ; if cutoff at beginning, then AA[i-1] and DD[i]  will be paired together, rather than AA[i] and DD[i]
			j=i-aoffsetf +aoffsetb ; if cutoff between AA and BB, then AA[i-1] and BB[i]  will be paired together, rather than AA[i] and DD[i]
print,"i=",i
			;gi=DD[i]; the address of where the fit will end and the next fit begins
			;gim=DD[i-1]; the address where this fit begins
			while BB[i+boffsetb] lt AA[j] do i++
			bi=BB[i+boffsetb]; the address of where the fit will end and the next fit begins
			bim=BB[i-1+boffsetb]; the address where the last fit ended
			;bim=BB[i-1]; the address where this fit begins
			dloc=i-1+doffsetf
			if AA[j] lt DD[dloc] then dloc-- 
			aj=AA[j] ; the index of the start trigger  
			dim=DD[dloc]

			wheredim=(where(eDD ge dim and eDD lt aj))[-1]
			if wheredim ne -1 then edim=eDD[wheredim] else edim=dim
			;edim=eDD[dloc]
			if dobug gt 0 then print,"numel(B5d)=",numel(B5d),", bi=",bi


			mxfore=foremaxs[j]
			if dobug gt 0 then print,"[dim,aj,bi,aj-dim,bi-dim]=",[dim,aj,bi,aj-dim,bi-dim]
			if ((dim gt aj) and (dim lt bi)) or (dim lt bim) then begin
				cont=1
				return, z
			endif  
			n_ep=n_e[dim:bi]
			if total(finite(n_ep) eq 1) eq 0 then begin
;print,"no electron density"
				cont=1
				return, z
			endif

			isFinite=(total(finite(ys[dim:bi]) ne 1) eq 0) and (total(finite(ys[dim:aj]) ne 1) eq 0) and (total(finite(ys[aj:bi]) ne 1) eq 0)
			if ~isFinite then begin
;print,"No MAVEN Mag Field Data here. WAT. Continuing"
				cont=1
				return, z
			endif
			if numel(foots) lt 100 then print, foots
			wherefoot=(where(foots lt aj and foots gt dim))[-1]
			
			;print,"wherefoot=",wherefoot
			if wherefoot eq -1 then begin 
				cont=1
				return, z
			endif else foot=foots[wherefoot]
			;print,"foot=",foot
			bfoot=max([dim,foot-10*60]);120])
			d00=dim
			dim=bfoot;(bfoot*3+dim)/4;mean([bfoot,bfoot,bfoot,bfoot,bfoot,dim])
			if total(finite(N_e[dim:foot]) ne 1) ne 0 then begin
;print,"no electron density"
				whereefin=where(finite(N_e[dim:foot]) eq 1,nefin)+dim

				if foot-whereefin[0] gt 2*60 and whereefin[-1] eq foot then dim=whereefin[0]
			endif

			;print,"[dim,bfoot,foot,aj,aj-bfoot,(aj-bfoot)/60]=",[dim,bfoot,foot,aj,aj-bfoot,(aj-bfoot)/60.]
			
			pcross=max([phm[(where(phm lt bi and phm gt foot))[0]]-60,foot])
			ymf=ymm[foot]
			zeron=aj-dim
			yh=ys[aj]; the y value at the start trigger  
			;print,"gi=DD[i]=G[",i,"]=",gi
			;print,"hj=H[",j,"]=",hj
			;print,"gim=DD[i-1]=G[",i-1,"]=",gim
			;yp=ys[gim:gi]

			;if ymm[dim] eq 0 then begin
			;	dim= (where(ymm[dim:bi] gt 0.))[0]+dim
				
			;	if dim gt foot and wherefoot ne -1 then begin
			;		cont=1
			;		return, z

			;	endif

			;endif

			bi=min([aj+40*60,bi])
			if ymm[dim] le 0 or finite(ymm[dim]) ne 1 then begin
				;KK=where(ymm[dim:aj] gt 0)
				SS=where(ymm[dim:foot] gt 0)
				;if foot ne d00 then begin
					;LL=where(xs[dim:aj] lt xs[foot])
					;SS=INTERSECT(KK,temporary(LL))

					if SS[0] ne -1 and SS[0]+dim lt foot-30 then dim=SS[0]+dim else begin
						cont=1
						return, z

					endelse
				;endif else dim=KK[0]+dim

			endif

			if 0 and total(ymm[bi] le ymm[dim:bi-1]) eq 0 then begin

				while total(ymm[bi] le ymm[dim:bi-1]) eq 0 and bi-aj gt 3*60 and B20d[bi] ne 0 do bi--

			endif
;print,"[dim,aj,bi]=",[dim,aj,bi]
			yp=ys[dim:bi]
			np=size(yp,/n_el)
			;ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			ybeg=ys[mean([dim,aj]):aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step

			nbeg=size(ybeg,/n_el)
			
			;yend=ys[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			yend=ys[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

			nend=size(yend,/n_el)

			yBSfdiff=2*ABS(yBS-ymm)/abs(yBS+ymm)

			;want to make sure we don't include the crustal field in our measurements. When in crustal field, B increases while rho decreases
			;CRUSTST=where((Bsd[aj+1:bi] gt 1.0) and (rhod[aj+1:bi] lt 0),ccount)+aj+1
			;if ccount gt 0  then begin
				;bi=CRUSTST[0]
				;yp=ys[dim:bi]
				;np=numel(yp)
				;yend=ys[aj:bi]
				;nend=numel(yend)
			;endif

			;will want to try to make sure yp does not start on the previous step, and that our generated step will not be part of the next range

			;ybegtrust=ybeg[1 *nbeg/5: 4*nbeg/5] ; we're pretty sure the step will never be  in this region
			mbeg=nbeg
			mend=nend
			;yendtrust=yend[1 * nend/5: 4*nend/5] ; we're pretty sure the step will always be on in this region


			;ybegtrust=ybeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region

			;brefines=0
			;frefines=0
			;incfunctions=1
			;decfunctions=1
			bii=bi
			
			if dobug gt 0 then print,"numel(B5d)=",numel(B5d),", bi=",bi
			;if (rollsec eq 1) then aj=secondRoll(aj,bi,B5d,dobug) ; second order correction 
			;p4=plot(xpa,thSpk(xpa,-dim+aj),'g-',/overplot)			

			;aj1=aj
			if dobug gt 0 then print,"aj=",aj,", aj-dim=",aj-dim,", numel(yBS)=",numel(yBS)
			if dobug gt 0   then print,"[mono[aj-1],mono[aj],mono[aj+1]=",[mono[aj-1],mono[aj],mono[aj+1]]
			;if (rollthird eq 1) then a0=Third_Roll(B20d,dim,a0,bi,dobug) ; second order correction  
			;ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region

			;aj=FalseShockSkipper(aj,bi,Bmed,median(Bmed[2 * nbeg/5+dim: nbeg/2+dim]))
			if dobug gt 0 then print,"aj=",aj
			aj0=aj
			tedim=max([edim,dim])
			if nonfore then Fore[aj:min([bi,aj+20*60])]=1
			aj=ajRefiner(max([edim,foot,pcross]),max([edim,dim]),aj,bi,zeron,dobug,yBS,nbeg,nend, mono,B5d,B20d,orderCustom,extr,rhod,Fore)
			;print,"numel(yBS),dim,aj=",numel(yBS),dim,aj
			parallel=0
			
			if aj+60 lt bi-10 and total(yBSfdiff[aj+60:min([N-1,aj+60+4*60])] lt .2)/120. gt .9 then begin

				maxfdiff=max(yBSfdiff[aj+60:bi-10],mxparfdloc)
				maxpar=max(yBS[aj+60:bi-10],mxparloc)
				if maxfdiff lt .4 and abs(mxparloc-mxparfdloc) lt 120 and min([mxparloc,mxparfdloc])+aj+60 gt aj+4*60 then begin
					parallel=1
					bi=min([mxparloc,mxparfdloc])+aj+60-1
				endif

			endif

			if ymm[bi] eq max(ymm[aj:bi]) then while ymm[bi] eq max(ymm[aj:bi]) and (bi-aj gt 10*60) or (bi-aj gt 6*60 and median(ymm[aj:bi])+2.5*stddev(ymm[aj:bi]) lt ymm[bi] )  do bi--

			if total(abs(ymm[aj-60:aj]-median(ymm[aj:aj+2*60])) gt .4) le 1 or total(abs(ymm[aj]-ymm[aj:aj+2*60]) gt 1) lt 1  then begin
				while (total(abs(ymm[aj-60:aj]-median(ymm[aj:aj+2*60])) gt .4) lt 1 or (total(abs(ymm[aj]-ymm[aj:aj+2*60]) gt 1) lt 1)   ) and stddev( ymm[aj:aj+2*60]) lt .4 and aj-30 gt foot do aj--

			endif
			if max(ymm[bi-20:bi]) eq max(ymm[aj:bi]) then while max(ymm[bi-20:bi]) eq max(ymm[aj:bi]) and (bi-aj gt 10*60) or (bi-aj gt 5*60 and median(ymm[aj:bi])+2.5*stddev(ymm[aj:bi]) lt max(ymm[bi-20:bi]) )  do bi--
			while max(ys[aj-60:aj]) gt max(ys[aj:bi]) and aj-foot gt 30 do aj--


			if foot ne d00 then mn=BU[foot] else mn=BU[mean([dim,aj])]

			if 0 then begin	wblwu=where(ymm[mean([aj,aj,bi]):bi] lt mn,blwuc)+mean([aj,aj,bi])
			if blwuc gt 0 then bi=wblwu[0]-1



			ymmn=min(ymm[mean([aj,bi]):bi],ymmnlc)
			ymmnlc+=mean([aj,bi])
			ymmn2=min(ys[ymmnlc-40:ymmnlc+40],ymmnlc2)
			ymmnlc2+=ymmnlc-20
			if ymmnlc2 lt bi-1 and ymmn2 lt mn*.75+.25*mean(ymm[aj:bi]) then begin
				bi=ymmnlc2-1
				while ys[bi] lt mn*.75+.25*mean(ymm[aj:bi]) and bi gt aj+4*60 do bi--
			endif
			endif
			xp=xs[dim:bi] ; the range of x's between the start and end times 
			xpa=xp-xp[0]; the range of x's normalized so that the first element is zero and last element is gi*(xs[1]-xs[0]) 
			yp=ys[dim:bi]
			np=numel(yp)
			ybeg=ys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step

			nbeg=size(ybeg,/n_el)
			
			;yend=ys[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			yend=ys[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

			nend=size(yend,/n_el)
			ypBS=yBS[dim:bi]
			BSbeg=yBS[mean([dim,aj]):aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step			
			BSend=yBS[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			nbeg=numel(BSbeg)
			nend=numel(BSend)
			;print,2 * (nbeg-1)/5, (nbeg-1)/2
			;BSbegtrust=BSbeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
			;BSendtrust=BSend[2 * (nend-1)/5: 4 * (nend-1)/5] ; we're pretty sure the step will always be on in this region
			;BSendtrust=BSend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			;meanTbeg=mean(BSbegtrust)

			;meanTend=mean(BSendtrust)


			;stdbeg=finite(stddev(BSbegtrust,/nan))

			;stdend=finite(stddev(BSendtrust,/nan))
			;meanT=mean([meanTbeg,meanTend])
			;print,meanTbeg,meanTend,stdbeg,stdend

;			hhj=hj
;			BSh=yBS[hhj]

			



			;aj1=aoscillate(aj,bi,dim,yBS,nbeg,nend,dobug)
			;aj2=aj1
			;if (rollthird eq 1) then aj2=Third_Roll(B20d,dim,aj1,bi,dobug) ; third order correction
			;aj3=aj2 
			;aj3=aoscillate(aj2,bi,dim,yBS,nbeg,nend,dobug)

			;BSh=yBS[aaj]

			;print,"numel(mono)=",numel(mono)
			;print,"[dim,bi]=",[dim,bi]

			;print,"total(monend)-numel(monend)=",total(monend),"-",numel(monend),"=",total(monend)-numel(monend)
			;print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"aj-dim=",aj-dim


			;print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),", aj-dim=",aj-dim
			;hj=hhj
			;print,"hj=",hj,", gim=",gim, ",gi=",gi,", size(xpa,/n_el)=",size(xpa,/n_el),", aj-dim=",aj-dim
			;xp=xs[gim:gi] ; the range of x's between the start and end times 
			;xpa=xp-xp[0]; the range of x's normalized so that the first element is zero and last element is gi*(xs[1]-xs[0]) 
	
			;p5=plot(xpa,thSpk(xpa,-dim+aaj),'c-',/overplot)	
			;aj=aj2
			;ph=plot(xpa,300*B20d[dim:bi]+10,color=56,/overplot)
			;p6=plot(xpa,thSpk(xpa,-dim+aj),'b-',/overplot)			
			;print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"aj-dim=",aj-dim
			;if dobug gt 0 then print,"[zeron,aj0-dim,aj1-dim,aaj-dim,aj2-dim]=",[zeron,aj0-dim,aj1-dim,aaj-dim,aj2-dim]


			yc=yp*0

			;if curve function is f(x)=m0 tanh( m1 (x-m2))+m3 then
			;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			mx=mean(ymm[aj:bi]);max(ymm[aj:bi]);yend)

			print,mx

			;whereendbelow=where(ymm[aj:bi] lt (.3*mn+.7*mx)/2 ,ebcount)
			;ajn0=aj
			;ajnn=aj
			;eedim=edim
			;if 0 and ebcount gt 5*60+1 and whereendbelow[-5*60]+aj lt bi-5*60+10 then begin
				;numidentical=0
				;whereendbelow+=aj
				;for ll=0,min([ebcount-5*60.,bi-aj-60]) do begin
					;if whereendbelow[ll+5*60]-whereendbelow[ll] gt 5*60+2 then  continue
					;numidentical+=5*60
					;if ll+aj ge .4*aj+.6*bi then break
				;	ajnn=whereendbelow[ll+5*60]
				;	eedim=whereendbelow[ll]
				;endfor
				;IF numidentical ge 5*60 then begin 
					;edim=eedim
					
					;aj=ajRefiner(max([eedim,foot,pcross]),max([eedim,dim]),ajnn,bi,zeron,dobug,yBS,nbeg,nend, mono,B5d,B20d,orderCustom,extr,rhod,Fore)
					;mx=mean(ymm[aj:bi]);max(ymm[aj:bi]);yend)

				;ENDIF

			;endif 
			;mn=BU[foot];mean(ybegtrust)
			;mx=25
			;print,"max=",mx
			;bottom of  step is at y=mn=m3-m0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			;mn=mean(ybegtrust)
			;mn=5
			;print,"min=",mn

			m0=(mx-mn)/2.
			m3=(mx+mn)/2.
			Working1=1
			;m2  is the x value in approximately at the top of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			;print,"m2  is the x value in the middle of the curve. to zeroth order, lets use m2=xpa[hj-gim]
;			m2=xpa[hj-gim]
			m2=xpa[aj-dim]
			diim=dim
			imin=dim
			imax=bi
			if dobug ge 1 then	print,"dim,diim=",dim,diim
			;print,"xpa[nbeg/2]=",xpa[nbeg/2]
			;print,"yp[nbeg/2]=",yp[nbeg/2]
			;with all this, can calculate guess for  m1 to zeroth order by
			;m1=  atanh((y-m3)/m0)/(x-m2)
			;m1=atanh( ( yp[nbeg/2] -m3 )/m0)/(xpa[nbeg/2] -m2)
			
			;that gives garbage. lets try 1 as an  arbitrary guess
			m1=1.;.5;.1
			

			MM=[m0,m1,m2,m3]
			MM0=MM
;print,"to zeroth order, guess that MM=",MM0
;print,'max,min=',MM[0]+MM[3],",",MM[3]-MM[0]
			weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
			status=-10
			CHISQ=-1
			yfit=curvefit(itmax=40,xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
;print,"1st order status=",status,", chi^2=",CHISQ
			PRINT, 'Function parameters: ', MM
;print,'max,min=',MM[0]+MM[3],",",MM[3]-MM[0]
			;ishock=round(MM[2]/MM[1])+dim;-x00
			ishock=findshock(aj,MM[2],MM[1],yfit,dim,1,foot)
			;if TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,bi-ishock])] gt MM[0]+MM[3]) eq 0 then 
;print,"[dim,bfoot,foot,aj,ishock]=",[dim,bfoot,foot,aj,ishock]
;print,"[aj-dim,ishock-dim,(ishock-dim)/60]",[aj-dim,ishock-dim,(ishock-dim)/60.]
			if ishock lt foot then begin
				Working1=0
				ishock=aj
			endif
			if 0 and ishock gt 32000 and ishock lt 34000 then p1=plot(xpa,yfit)

			algshock0=aj
			algshock1=aj
			wblwu=where(ymm[mean([ishock,ishock,imax]):imax] lt MM[3]-MM[0],blwuc)+mean([ishock,ishock,imax])
			if  blwuc gt 0 and wblwu[0]-1 gt ishock+60 then begin
				bi=wblwu[0]-1
				xp=xs[dim:bi]
				xpa=xp-xp[0]; the range of x's normalized so that the first element is zero and last element is gi*(xs[1]-xs[0]) 
				yp=ys[dim:bi]
				np=numel(yp)
				ybeg=ys[dim:ishock] ; elements before the first trigger. to zeroth order, this is (probably) before the step

				nbeg=size(ybeg,/n_el)
			
			;yend=ys[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
				yend=ys[ishock:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

				nend=size(yend,/n_el)
				ypBS=yBS[dim:bi]
				BSbeg=yBS[mean([dim,ishock]):ishock] ; elements before the first trigger. to zeroth order, this is (probably) before the step			
				BSend=yBS[ishock:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
				nbeg=numel(BSbeg)
				nend=numel(BSend)				
				MM=coeffCalc(xp,yp, ishock,x00)
				MM_22=MM[2]
				algshock1=MM_22
				weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
				status=-10
				CHISQ=-1
				yfit=curvefit(itmax=40,xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
;print,"1st order status=",status,", chi^2=",CHISQ
				PRINT, 'Function parameters: ', MM
;print,'max,min=',MM[0]+MM[3],",",MM[3]-MM[0]
			;ishock=round(MM[2]/MM[1])+dim;-x00
				ishock=findshock(aj,MM[2],MM[1],yfit,dim,1,foot)
			;if TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,bi-ishock])] gt MM[0]+MM[3]) eq 0 then 
;print,"[dim,bfoot,foot,aj,ishock]=",[dim,bfoot,foot,aj,ishock]
;print,"[aj-dim,ishock-dim,(ishock-dim)/60]",[aj-dim,ishock-dim,(ishock-dim)/60.]
				if 0 and ishock gt 32000 and ishock lt 34000 then p2=plot(xpa,yfit)
				if ishock lt foot then begin
					Working1=0
					ishock=aj
					algshock1=aj
				endif
			endif
			IF Working1 then Working1=(status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi) and (Fore[ishock] eq 1 or nonfore) and imin lt foot-30
			if (ys[ishock-11] lt ys[ishock]) and (max(ys[ishock-11:ishock],maxloc2) gt ys[ishock]) then begin

				MM2=MM
				;print,"MM=",MM
				;print,"[dim,bfoot,foot,aj,ishock]=",[dim,bfoot,foot,aj,ishock]
				;print,x2greg(xs[N-1-aj],/str)
				;print,x2greg(xs[N-1-ishock],/str)
				;print,maxloc2
				;print,"m2=",m2
				;print,"MM[2]",MM[2]
				;print,"aj-dim,ishock-dim=",aj-dim,ishock-dim
				;print,"ishock-aj=",ishock-aj
				;print,maxloc2+MM[2]-11,maxloc2+ishock-dim-11
				;print,xpa[aj-dim],MM[2]
				MM2[2]=xpa[maxloc2+ishock-dim-11]
				MM_22=MM[2]
				yfit2=curvefit(itmax=40,xpa, yp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
				PRINT, 'Function parameters: ', MM2
;print,'max,min=',MM[0]+MM[3],",",MM[3]-MM[0]
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
				ishock2=findshock(aj,MM2[2],MM2[1],yfit2,dim,1,foot)
				Working2=(status2 eq 0) and (MM2[1] gt 0)and ( MM2[0] gt minM0) and (ishock2 gt imin) and (ishock2+60 lt imax) and (abs(MM2[3]-MM2[0]-mn) lt maxUp) and (MM2[1] gt minM1) and (CHISQ2 lt minChi) and (Fore[ishock] eq 1 or nonfore) and imin lt foot-30
				if Working2 and (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and ishock2+1.2/MM2[1] lt bi then begin
					MM=MM2
					status=status2
					yfit=yfit2
					yfit1=yfit
					CHISQ=CHISQ2
					Working1=Working2;1
					ishock=ishock2
					algshock1=MM_22+dim
				endif
			endif

			if dobug ge 1 then	print,"dim,diim=",dim,diim
			if dobug ge 1 then	print,"ishock,ishock-xp[0]=",ishock,ishock-diim

			;Working1=1

			if ishock lt diim or ishock gt bi then begin

				ishock=aj
				Working1=0
				algshock1=aj
			endif
			ishock1=ishock
			imin=dim
			imax=bi
			if Working1 then if ishock-!pi/MM[1] gt dim+60. and ishock-!pi/MM[1] lt foot then foot=ishock-!pi/MM[1]
			;if working then p7=plot(xpa,thSpk(xpa,-dim+ishock),'m-',/overplot)
;Working1=(status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi);((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi)  and (bi-ishock gt 100) and ( MM[0] gt 1) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (yBS[max([dim,ishock-5*60])]-yBS[dim] lt 10) and (yfit[max([dim,ishock-5*60])-dim] -yfit[0] lt 10) and (CHISQ gt 0) and (Fore[ishock] eq 1)

		if d00 eq edim or mxfore eq 1 then begin
				;print,"d00 eq edim"
				iminB=max([dim,foot-3*60]);ishock-10*60
				;print,iminB,foot-2*60
				;if ymm[iminB] gt ymf then begin
					;ympref=ymm[iminB:foot]
					;ympreB=ymm[iminB-2*60:iminB]
					;if total(min(ympref) lt [ymm[iminB],ymf]) ge 1 and ymm[iminB-2*60] lt min(ymm[iminB:foot]) then iminB=max([dim,iminB-2*60])
					

				;endif
		endif else begin
				;print,"d00 ne edim"
				;
				IN1=where(Fore[d00:ishock] eq 1)+d00
				if  foot-IN1[0] ge 60 then begin
					iminB=IN1[0]
				endif else begin
					INMAX=where(Fore[d00:ishock] eq mxfore)+d00
					simB=INMAX[0]
					eimB=INMAX[-1]
					iminB=max([d00,simB+1,eimB-3*60]);;THE -2*60 here is so that we don't just spawn on top of the previous shock crossing	
				endelse
		endelse
		if Working1 then begin
			B20de=B20d[ishock:imax]
			;pointerinup= ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

			;	     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) $
 ;and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])])) ))




			Working1=(status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi) and (Fore[ishock] eq 1 or nonfore) and imin lt foot-30;((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi)  and (bi-ishock gt 100) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (yfit[max([dim,iminB])-dim] -yfit[0] lt 10) and (CHISQ gt 0) and (Fore[ishock] eq 1) and (abs(yfit[imin-dim]-BU[ishock]) lt 4) and ~pointerinup	
			endif
			working=1
			if dobug eq 3 then begin
			
print,"**************************"
print,"status eq 0=",status eq 0
print,"MM[0],minM0,> :=",MM[0],minM0, MM[0] gt minM0
print,"MM[1],minM1,MM[1]>minM1=",MM[1],minM1,MM[1] gt minM1
print,"MM[3]-MM[0],mn,abs(MM[3]-MM[0]-mn),maxup,success=",MM[3]-MM[0],mn,maxup,abs(MM[3]-MM[0]-mn),(abs(MM[3]-MM[0]-mn) lt maxUp)
print,"bi-ishock,>100=",bi-ishock,(bi-ishock gt 100)
print,"Fore[ishock]=",Fore[ishock]
print,"yfit[0],BU[ishock],abs(yfit[0]-BU[ishock]) lt 4=",yfit[0],BU[ishock],abs(yfit[0]-BU[ishock]) lt 4
print,"~pointerinup=",~pointerinup
print,"--------------------------"
print,"Working1=,",Working1

			endif

			smol=min(derex[ishock:bi],smoloc)
			;smoloc+=ishock
			imin0=imin
			imax0=imax
			imin00=imin
			imax00=imax
			imin=max([dim,iminB])
			if imin gt ishock then begin
				if dim lt ishock and foot lt ishock then imin=dim
			endif
			if imin gt foot then begin
				 imin0=max([foot-3*60,dim])
				 if imin0 lt foot-10 then imin=imin0 else begin
					;print,"dim,imin0,foot-10=",dim,imin0,foot-10
					cont=1
					return, z
				endelse
			endif


			;print,"[imin,foot,aj,ishock]=",[imin,foot,aj,ishock]
			;print,"[aj-imin,ishock-imin,(ishock-imin)/60]=",[aj-imin,ishock-imin,(ishock-imin)/60.]
;and (yBS[max([dim,ishock-5*60])]-yBS[dim] lt 10) 
			while ymm[imin]-ymm[dim] gt 4 and imin gt iminB-3*60 and imin gt dim do begin
				imin--
				;if ymm[imin]-ymm[dim] gt 10 and ishock-foot gt 30 then begin 
					;ishock--
					;imax--
				;endif 	
			endwhile

	
			if finite(smol) eq 1 and smoloc gt 7*60 then begin
				imax=min([smoloc+ishock+60,bi])
				;imin0=max([dim,iminB])

				ypp=ys[imin:imax]
				xpp=xs[imin:imax]

				MM1=MM

				idmin=min([dim:imin])
				idmax=max([dim:imin])
				ympref=ymm[imin:foot]
				;smypp=(smooth(ypp,2,/edge_mirror))[0:8*(ishock-imin)/10]
				preAcc=where( abs( MM[3]-MM[0] - ymm[dim:imin]) lt .4,numpre)+dim
				postAcc=where( abs( MM[3]-MM[0] - ymm[imin:ishock]) lt .4,numpost)+imin
				fracPreAcc=1.*numpre/(dim-imin)
				fracPostAcc=1.*numpost/(imin-ishock)

				smmp=ymm[imin:.8*ishock+.2*imin]
				smypp=Bmed[imin:.8*ishock+.2*imin]
				ssp=(smmp+smypp)/2.
				;mn=min(smmp)
				;print,"smmp[0]=",smmp[0]
				;print,"min(smmp)=",min(smmp)
	

				if 0 and  MM[3]-MM[0]  gt min([smmp[0],mean(ymm[imin:foot])])+1 then begin
					;print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " > ",min([smmp[0],mean(ymm[imin:foot])])+1,"=smmp[0]+1"
					mx=MM[3]+MM[0]
					mn=min([smmp[0],mean(ymm[imin:foot])]);smmp[0];smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
					;print,"mx,mn=",mx,mn
					MMtt=(mx+mn)/2.
					MMzz=(mx-mn)/2.
					if MMzz gt 0 and MMtt gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
						MM[0]=MMzz
						MM[3]=MMtt
			
					endif

				endif

				if 0 and MM[3]-MM[0]  lt min(smmp) then begin
				;print,"mm3-mm0=",MM[3]-MM[0], " < ", min(smmp)
					mx=MM[3]+MM[0]
					mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;print,"mx,mn=",mx,mn
					MMtt=(mx+mn)/2.
					MMzz=(mx-mn)/2.
					if MMzz gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
						MM[0]=MMzz
						MM[3]=MMtt
				
					endif

				endif
				if 0 and MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5   then begin
					mx=MM[3]+MM[0]
					mn=min(smmp);smypp);mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;print,"mx,mn=",mx,mn
					MMtt=(mx+mn)/2.
					MMzz=(mx-mn)/2.
					if MMzz gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
						MM[0]=MMzz
						MM[3]=MMtt
			
					endif
				endif
				xppa=xpp-xpp[0]
	
				MM=coeffCalc(xpp,ypp, ishock,x00)


				MM_22=MM[2]

				mnpref=min(ympref)
				preAcc=where( abs( MM[3]-MM[0] - ymm[dim:imin]) lt .4,numpre)+dim
				postAcc=where( abs( MM[3]-MM[0] - ymm[imin:ishock]) lt .4,numpost)+imin
				fracPreAcc=1.*numpre/(dim-imin)
				fracPostAcc=1.*numpost/(imin-ishock)
				;smypp=(smooth(ypp,2,/edge_mirror))[0:8*(ishock-imin)/10]
				;if 0 and MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10])  or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt smypp) lt 5 or total(MM[3]-MM[0] lt smypp) lt 5 then begin;
				;if 0 and MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5  then begin
					;mx=MM[3]+MM[0]
					;mn=mean(smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
					;MM[3]=(mx+mn)/2
					;MM[0]=(mx-mn)/2

				;endif
				;if 0 and MM[3]-MM[0]  lt min(smypp) then begin
					;mx=MM[3]+MM[0]
					;mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
					;MM[3]=(mx+mn)/2
					;MM[0]=(mx-mn)/2

				;endif

				if  MM[3]-MM[0]  gt smmp[0]+1 then begin
					;print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " > ",smmp[0]+1,"=smmp[0]+1"
					mx=MM[3]+MM[0]
					mn=smmp[0];smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
					;print,"mx,mn=",mx,mn
					MMtt=(mx+mn)/2.
					MMzz=(mx-mn)/2.
					if MMzz gt 0 and MMtt gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
						MM[0]=MMzz
						MM[3]=MMtt
			
					endif

				endif

				if 0 and MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5   then begin
			;	print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " < ", min(smmp)
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.
					if MMzz gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
						MM[0]=MMzz
						MM[3]=MMtt
			
					endif

				endif

				if  MM[3]-MM[0]  lt min(smmp) then begin
					;print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " < ", min(smmp)
					mx=MM[3]+MM[0]
					mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
					;print,"mx,mn=",mx,mn
					MMtt=(mx+mn)/2.
					MMzz=(mx-mn)/2.
					if MMzz gt 0 and MMtt gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
						MM[0]=MMzz
						MM[3]=MMtt
			
					endif

				endif

				weight=1.0/ypp
				CHISQ2=CHISQ
				stat2=status
				yfit2=yfit
				yfit=curvefit(itmax=40,xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
;print,"status=",status,", chi^2=",CHISQ

				PRINT, 'Function parameters: ', MM
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smmp)=", min(smmp),", min(smypp)=",min(smypp)
			;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
				ishock=findshock(aj,MM[2],MM[1],yfit,imin,1,foot)
			;print,"[d00,dim,imin,foot,ishock,ishock-imin,(ishock-imin)/60]=",[d00,dim,imin,foot,ishock,ishock-imin,(ishock-imin)/60.]
				print,"ishock=",ishock
				if 0 and ishock gt 32000 and ishock lt 34000 then p3=plot(xppa,yfit)
				if  MM[3]-MM[0]  gt smmp[0]+1 then begin
				;print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " > ",smmp[0]+1,"=smmp[0]+1"
					mx=MM[3]+MM[0]
					mn=smmp[0];smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;print,"mx,mn=",mx,mn
					MMtt=(mx+mn)/2.
					MMzz=(mx-mn)/2.
					MM2=MM
					if MMzz gt 0 and MMtt gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
						MM2[0]=MMzz
						MM2[3]=MMtt
			
					endif

					yfit3=curvefit(itmax=40,xppa, ypp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
					PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
					ishock3=findshock(ishock,MM2[2],MM2[1],yfit3,imin,1,foot)

					if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt minM1) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and ishock3 gt imin and ishock3+1.2/MM2[1] lt bi then begin
						MM=MM2
						status=status2
						yfit=yfit3
					;yfit1=yfit
						CHISQ=CHISQ2
						ishock=ishock3
					Working1=1
					endif

				endif

				if  MM[3]-MM[0]  lt min(smmp) then begin
;print,"mm3-mm0=",MM[3]-MM[0], " < ", min(smmp)
					mx=MM[3]+MM[0]
					mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
					MMtt=(mx+mn)/2.
					MMzz=(mx-mn)/2.
					MM2=MM
					if MMzz gt 0 and MMtt gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
						MM2[0]=MMzz
						MM2[3]=MMtt
			
					endif

					yfit3=curvefit(itmax=40,xppa, ypp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
					PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
					ishock3=findshock(ishock,MM2[2],MM2[1],yfit3,imin,1,foot)

					if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and ishock3 gt imin and ishock3+1.2/MM2[1] lt bi and (MM2[1] gt minM1) then begin
						MM=MM2
						status=status2
						yfit=yfit3
					;yfit1=yfit
						CHISQ=CHISQ2
						ishock=ishock3
					endif	
				endif

				if (ys[ishock-11] lt ys[ishock]) and (max(ys[ishock-11:ishock],maxloc2) gt ys[ishock]) then begin

					MM2=MM
					MM2[2]=xppa[maxloc2+ishock-11-imin];xpa[maxloc2+ishock-11-dim]
					yfit3=curvefit(itmax=40,xppa, ypp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
					PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
					ishock2=findshock(aj,MM2[2],MM2[1],yfit3,imin,1,foot)

					if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and ishock2+1.2/MM2[1] lt bi and (MM2[1] gt minM1) then begin
						MM=MM2
						status=status2
						yfit=yfit3
					;yfit1=yfit
						CHISQ=CHISQ2
						ishock=ishock2
					endif
				endif


				B20de=B20d[ishock:imax]
				pointerinup= ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq 0) $
 and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])]) )) 


				Working2=Working1
				working = ishock+1.2/MM[1] lt bi and ((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1)  and (bi-ishock gt 100) and (CHISQ gt 0) and (Fore[ishock] eq 1 or nonfore) and (abs(yfit[0]-BU[ishock]) lt 4) and ~pointerinup	and imin lt foot-30	; ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) ne 0)
			;if working then p8=plot(xpa,thSpk(xpa,-dim+ishock),'-',color='peru',/overplot)	else p1.close		
				fracRemain=(bi-ishock)*1.0/(bi-dim)
			IF ((CHISQ gt CHISQ2) and Working2) or( (not Working1) and Working2) then begin
;print,"going back to old version (0)"
;print,"IF ((CHISQ gt CHI2) and Working2)",imin0,imin,imax,imax0,imax-imin,numel(yfit), imax0-imin0,numel(yfit2)
				yfit=yfit2
				CHISQ=CHISQ2
				MM=MM1
				ishock=ishock1
				imin=imin0
				imax=imax0
				Working=Working2			

				imin=max([dim,iminB])
				if imin gt ishock then begin
					if dim lt ishock and foot lt ishock then imin=dim
				endif
				if imin gt foot then begin
					 imin01=max([foot-3*60,dim])
					 if imin01 lt foot-10 then imin=imin01 else begin
					;print,"dim,imin0,foot-10=",dim,imin0,foot-10
						cont=1
						return, z
					endelse
				endif


			;print,"[imin,foot,aj,ishock]=",[imin,foot,aj,ishock]
			;print,"[aj-imin,ishock-imin,(ishock-imin)/60]=",[aj-imin,ishock-imin,(ishock-imin)/60.]
;and (yBS[max([dim,ishock-5*60])]-yBS[dim] lt 10) 
				



			ENDIF ;else begin
;
				;imin0=imin	
				;imax0=imax
				;xpa=xppa
				;MM1=MM
				;yfit1=yfit;
;
			;endelse







			endif else begin
	

			imax=min([bi,ishock+7*60])
			while ymm[imin]-ymm[dim] gt 4 and imin gt iminB-3*60 and imin gt dim do begin
					imin--
					if ymm[imin]-ymm[dim] gt 10 and ishock-foot gt 30 then begin 
						ishock--
						imax--
					endif 		
			endwhile
				
			if dobug ge 1 then	print,"shrunk front by [dim,imin,imin-dim]=",[dim,imin,imin-dim]
			if dobug ge 1 then	print,"shrunk back by [bi,imax,bi-imax]=",[bi,imax,bi-imax]
			if imin gt ishock and dim lt ishock then imin=imin0
			ypp=ys[imin:imax]
			xpp=xs[imin:imax]
			xppa=xpp-xpp[0]
			MM1=MM

			idmin=min([dim:imin])
			idmax=max([dim:imin])
			ympref=ymm[imin:foot]
			;smypp=(smooth(ypp,2,/edge_mirror))[0:8*(ishock-imin)/10]
			preAcc=where( abs( MM[3]-MM[0] - ymm[dim:imin]) lt .4,numpre)+dim
			postAcc=where( abs( MM[3]-MM[0] - ymm[imin:ishock]) lt .4,numpost)+imin
			fracPreAcc=1.*numpre/(dim-imin)
			fracPostAcc=1.*numpost/(imin-ishock)

			smmp=ymm[imin:.8*ishock+.2*imin]
			smypp=Bmed[imin:.8*ishock+.2*imin]
			ssp=(smmp+smypp)/2.
			;mn=min(smmp)
			;print,"smmp[0]=",smmp[0]
			;print,"min(smmp)=",min(smmp)


			if  MM[3]-MM[0]  gt min([smmp[0],mean(ymm[imin:foot])])+1 then begin
				;print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " > ",min([smmp[0],mean(ymm[imin:foot])])+1,"=smmp[0]+1"
				mx=MM[3]+MM[0]
				mn=min([smmp[0],mean(ymm[imin:foot])]);smmp[0];smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;print,"mx,mn=",mx,mn
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.
				MM2=MM
				if MMzz gt 0 and MMtt gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
					MM2[0]=MMzz
					MM2[3]=MMtt
			
				endif

				yfit3=curvefit(itmax=40,xppa, ypp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
					PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
				ishock3=findshock(ishock,MM2[2],MM2[1],yfit3,imin,1,foot)

					if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and ishock3 gt imin and (MM2[1] gt minM1) then begin
						MM=MM2
						status=status2
						yfit=yfit3
					;yfit1=yfit
						CHISQ=CHISQ2
						ishock=ishock3
				endif

			endif

			if  MM[3]-MM[0]  lt min(smmp) then begin
				;print,"mm3-mm0=",MM[3]-MM[0], " < ", min(smmp)
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;print,"mx,mn=",mx,mn
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.
				MM2=MM
				if MMzz gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
					MM2[0]=MMzz
					MM2[3]=MMtt
			
				endif
				;;help,xppa
				;;help,ypp
				;;help,weight
				;;help,sigma
				yfit3=curvefit(itmax=40,xppa, ypp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
					PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
				ishock3=findshock(ishock,MM2[2],MM2[1],yfit3,imin,1,foot)

					if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and ishock3 gt imin then begin
						MM=MM2
						status=status2
						yfit=yfit3
					;yfit1=yfit
						CHISQ=CHISQ2
						ishock=ishock3
				endif
			endif
			if MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5   then begin
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;print,"mx,mn=",mx,mn
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.
				MM2=MM
				if MMzz gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
					MM2[0]=MMzz
					MM2[3]=MMtt
			
				endif
				yfit3=curvefit(itmax=40,xppa, ypp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
					PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
				ishock3=findshock(ishock,MM2[2],MM2[1],yfit3,imin,1,foot)
				print,"[imin,ishock3,ishock,imax]=",[imin,ishock3,ishock,imax]
					if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and ishock3 gt imin and ishock3+1.2/MM2[1] lt bi then begin
						MM=MM2
						status=status2
						yfit=yfit3
					;yfit1=yfit
						CHISQ=CHISQ2
						ishock=ishock3
				endif
				print,"[imin,ishock3,ishock,imax]=",[imin,ishock3,ishock,imax]
			endif
			print,"[imin,ishock,imax]=",[imin,ishock,imax]
			xppa=xpp-xpp[0]
			MM1=MM
			MM=coeffCalc(xpp,ypp, ishock,x00)
			MM_22=MM[2]
			algshock1=MM_22+imin			



			mnpref=min(ympref)
			preAcc=where( abs( MM[3]-MM[0] - ymm[dim:imin]) lt .4,numpre)+dim
			postAcc=where( abs( MM[3]-MM[0] - ymm[imin:ishock]) lt .4,numpost)+imin
			fracPreAcc=1.*numpre/(dim-imin)
			fracPostAcc=1.*numpost/(imin-ishock)
			;smypp=(smooth(ypp,2,/edge_mirror))[0:8*(ishock-imin)/10]
			;if 0 and MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10])  or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt smypp) lt 5 or total(MM[3]-MM[0] lt smypp) lt 5 then begin;
			;if 0 and MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5  then begin
				;mx=MM[3]+MM[0]
				;mn=mean(smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;MM[3]=(mx+mn)/2
				;MM[0]=(mx-mn)/2

			;endif
			;if 0 and MM[3]-MM[0]  lt min(smypp) then begin
				;mx=MM[3]+MM[0]
				;mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;MM[3]=(mx+mn)/2
				;MM[0]=(mx-mn)/2

			;endif

			if  MM[3]-MM[0]  gt smmp[0]+1 then begin
				;print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " > ",smmp[0]+1,"=smmp[0]+1"
				mx=MM[3]+MM[0]
				mn=smmp[0];smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				print,"mx,mn=",mx,mn
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.
				if MMzz gt 0 and MMtt gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
					MM[0]=MMzz
					MM[3]=MMtt
			
				endif

			endif

			if MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5   then begin
			;	print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " < ", min(smmp)
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.
					if MMzz gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
						MM[0]=MMzz
						MM[3]=MMtt
			
					endif

			endif

			if  MM[3]-MM[0]  lt min(smmp) then begin
				;print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " < ", min(smmp)
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;print,"mx,mn=",mx,mn
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.
				if MMzz gt 0 and MMtt gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
					MM[0]=MMzz
					MM[3]=MMtt
			
				endif

			endif

			weight=1.0/ypp
			CHI1=CHISQ
			stat1=status
			yfit1=yfit
			;help,yfit1
			;help,xpa
			;help,xppa
			if 0 and ishock gt 32000 and ishock lt 34000 then begin
				 p3=plot(xpa,yfit,'-',color='cyan')
				 p3y=plot(xpa,yp,/over)			
			endif

			yfit=curvefit(itmax=40,xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
;print,"yfit=CURVEFIT",imin,imax,imax-imin,numel(yfit),imax0-imin0
;print,"status=",status,", chi^2=",CHISQ

			PRINT, 'Function parameters: ', MM
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smmp)=", min(smmp),", min(smypp)=",min(smypp)
			;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
			ishock=findshock(aj,MM[2],MM[1],yfit,imin,1,foot)
			;print,"[d00,dim,imin,foot,ishock,ishock-imin,(ishock-imin)/60]=",[d00,dim,imin,foot,ishock,ishock-imin,(ishock-imin)/60.]
			;print,"asdafs"
			if 0 and ishock gt 32000 and ishock lt 34000 then begin
				 p3=plot(xppa,yfit,'b-')
				 p3y=plot(xppa,ypp,/over)			
			endif
			if  MM[3]-MM[0]  gt smmp[0]+1 then begin
				;print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " > ",smmp[0]+1,"=smmp[0]+1"
				mx=MM[3]+MM[0]
				mn=smmp[0];smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;print,"mx,mn=",mx,mn
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.
				MM2=MM
				if MMzz gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
					MM2[0]=MMzz
					MM2[3]=MMtt
			
				endif	

				yfit2=curvefit(itmax=40,xppa, ypp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
				PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
				ishock2=findshock(aj,MM2[2],MM2[1],yfit2,imin,1,foot)
			if 0 and ishock gt 32000 and ishock lt 34000 then begin
				 p3=plot(xppa,yfit,'r-')
				 p3y=plot(xppa,ypp,/over)			
			endif
				if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt minM1) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and ishock2+1.2/MM2[1] lt bi then begin
					MM=MM2
					status=status2
					yfit=yfit2
					;yfit1=yfit
					CHISQ=CHISQ2
					ishock=ishock2
				endif	

			endif

			if  MM[3]-MM[0]  lt min(smmp) then begin
;print,"mm3-mm0=",MM[3]-MM[0], " < ", min(smmp)
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.

				MM2=MM
				if MMzz gt 0 then begin
;print,"setting M0=",MMzz,", M3=",MMtt
					MM2[0]=MMzz
					MM2[3]=MMtt
			
				endif	

				yfit2=curvefit(itmax=40,xppa, ypp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
				PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
				ishock2=findshock(aj,MM2[2],MM2[1],yfit2,imin,1,foot)

				if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt minM1) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and ishock2+1.2/MM2[1] lt bi then begin
					MM=MM2
					status=status2
					yfit=yfit2
					;yfit1=yfit
					CHISQ=CHISQ2
					ishock=ishock2
				endif	
			endif
			if 0 and ishock gt 32000 and ishock lt 34000 then begin
				 p3=plot(xppa,yfit,'g-')
				 p3y=plot(xppa,ypp,/over)			
			endif
			print,"[imin,ishock,imax]=",[imin,ishock,imax]
			if (ys[ishock-11] lt ys[ishock]) and (max(ys[ishock-11:ishock],maxloc2) gt ys[ishock]) then begin

				MM2=MM
				MM2[2]=xppa[maxloc2+ishock-11-imin];xpa[maxloc2+ishock-11-dim]
				MM_22=MM2[2]
				yfit2=curvefit(itmax=40,xppa, ypp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
;print,"status=",status2,", chi^2=",CHISQ2
				PRINT, 'Function parameters: ', MM2
;print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
				ishock2=findshock(aj,MM2[2],MM2[1],yfit2,imin,1,foot)

				if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt minM1) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) and ishock2+1.2/MM2[1] lt bi then begin
					MM=MM2
					status=status2
					yfit=yfit2
					;yfit1=yfit
					CHISQ=CHISQ2
					ishock=ishock2
					algshock1=MM_22+imin
				endif
			endif

			if 0 and ishock gt 32000 and ishock lt 34000 then begin
				 p3=plot(xppa,yfit,'-',color='gold')
				 p3y=plot(xppa,ypp,/over)			
			endif
;print,"after CHISQ2 lt CHISQ,",imin,imax,imax-imin,numel(yfit)

			B20de=B20d[ishock:imax]
			pointerinup= ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq 0) $
 and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])]) )) 



			working = ishock+1.2/MM[1] lt bi and ((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock+60 lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1)  and (bi-ishock gt 100) and (CHISQ gt 0) and (Fore[ishock] eq 1 or nonfore) and (abs(yfit[0]-BU[ishock]) lt 4) and ~pointerinup	and imin lt foot-30	; ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) ne 0)
			;if working then p8=plot(xpa,thSpk(xpa,-dim+ishock),'-',color='peru',/overplot)	else p1.close		
			fracRemain=(imax-ishock)*1.0/(imax-imin)

			if dobug eq 3 then begin
print,"**********************************************************"
				print,"working=,",working
print,"status eq 0=",status eq 0
print,"MM[0],minM0,> :=",MM[0],minM0, MM[0] gt minM0
print,"MM[1],minM1,sucess=",MM[1],minM1,MM[1] gt minM1
print,"MM[3]-MM[0],mn,abs(MM[3]-MM[0]-mn),maxup,success=",MM[3]-MM[0],mn,maxup,abs(MM[3]-MM[0]-mn),(abs(MM[3]-MM[0]-mn) lt maxUp)
print,"bi-ishock,100,success=",bi-ishock,100,(bi-ishock gt 100)
print,"Fore[ishock]=",Fore[ishock]
print,"yfit[0],BU[ishock],abs(yfit[0]-BU[ishock]) lt 4=",yfit[0],BU[ishock],abs(yfit[0]-BU[ishock]) lt 4
print,"~pointerinup=",~pointerinup
print,"--------------------------"
print,"working=,",working
print,"**********************************************************"
				
			endif
			;print,"[imin,ishock,imax]=",[imin,ishock,imax]
			print,CHI1,CHISQ
			IF ((CHISQ gt CHI1) and Working1) or( (not Working) and Working1) then begin
;print,"going back to old version"
;print,"(in) IF ((CHISQ gt CHI1) and Working1)",imin0,imax0, imin,imax,imax-imin,numel(yfit),numel(yfit1),imax0-imin0
				yfit=yfit1
				CHISQ=CHI1
				MM=MM1

				imin=imin00
				imax=imax00
				ishock=findshock(aj,MM[2],MM[1],yfit,imin,1,foot);ishock1
				working=Working1
				xppa=xpa
				ypp=yp		
				algshock1=aj	
			ENDIF

			endelse
			if 0 and ishock gt 32000 and ishock lt 34000 then begin
				 p3=plot(xppa,yfit,'-',color='purple')
				 p3y=plot(xppa,ypp,/over)			
			endif
;print,"IF ((CHISQ gt CHI1) and Working1)", imin,imax,imax-imin,numel(yfit),imax0-imin0

			if dobug ge 1 then	print,"[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-MM[2],(bi-MM[2])/(bi-dim)]=",[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
			if dobug ge 1 then	print,"[imin,ishock,imax]=",[imin,ishock,imax]
			if ishock lt imin then print, "bad ishock: ishock = ",ishock," lt ", imin,"= imin"
			if ishock gt imax then print, "bad ishock: ishock = ",ishock," gt ", imax,"= imax"

			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
			;if ((status eq 0) and (MM[0] gt 0)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt 1) then begin
			;print,'sadasdf'
			if 0 and ishock gt 32000 and ishock lt 34000 then p3=plot(xppa,yfit)
			if working then begin
				;print,'[imin,foot,ishock,imax]=',[imin,foot,ishock,imax]
				;;t1=TEXT(.1,.8,'Good fit')
				;p1.close
;print,"working"
				;tanhfit,xpa,MM,F

				ishockt1=findshock(aj,MM[2],MM[1],yfit,imin,1,foot)
				ishockt2=ishock

				if abs(ishockt2-ishockt1) gt 2. then begin
					imin+=ishock-ishockt1
					if imax-imin gt numel(yfit)+1 or imax-imin lt numel(yfit)-2 then imax+=ishock-ishockt1

				endif
				MMM=MM
				print,MM
				lastmax=imax
				sublengths.add,bi-dim
				if( ncount eq 0) then begin; and (abs(MM[2]-MM0[2]) lt 1000) then begin
					;print,"no NANs"
					;for k=gim,gi-1 do z[k]=yfit[k-gim]	
					;for k=dim,bi-1 do z[k]=yfit[k-dim]
;print,"if( ncount eq 0) then begin",imin,imax,imax-imin,numel(yfit),imax0-imin0
					for k=imin,imax-1 do z[k]=yfit[k-imin]	

				endif else begin
;print,"yfit has ",ncount," NANs. calculating directly."

					;if (abs(MM[2]-MM0[2]) ge 1000) then MM=MM0

					NNN=where(FINITE(F) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
					;for k=gim,gi-1 do z[k]=F[k-gim]
					tanhfit,xppa,MM,F
;					if ncount eq 0 then for k=dim,bi-1 do z[k]=F[k-dim]
					if ncount eq 0 then for k=imin,imax-1 do z[k]=F[k-imin]
				
				endelse
 				;ishock=round(MM[2]/MM[1])+imin;-x00
;print,dobug
				if dobug gt 0 then print,"ishock=",ishock
				shks[ishock]=1
				if parallel then parallelend[ishock]=bi
				shockloc=ishock
				imn=imin
				imx=imax
				chi=CHISQ
				MMM=MM
				;"in case bcutoff eq 1, want an estimate of the step width of the DD[0:1] step to make approximations"
				if(i eq 1) then begin
						zwidth =(imax-imin)-MM[3]
					;trust=ybegtrust[gim+2*nbeg/5:gim+nbeg/2]
					trust=z
				endif
				insubs=Subsets
				Subsets=insubs
				for kk=imin,imax do insubs[kk]=1

				for kk=imin, ishock do begs[kk]=1
				for kk=ishock, imax do ends[kk]=1


				Bmax=MM[3]+MM[0]
				Bmin=MM[3]-MM[0]
			;print,'m0,m3,max,min=',MM[0],MM[3],MM[0]+MM[3],",",MM[3]-MM[0]
			endif else begin
				brokens.add,[i,dim,bi, xs[dim]-min(xs),xs[bi]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]
				cont=1
			endelse
			test=1
			if imin lt bfoot then message,"bad shock"
			return, z
end
