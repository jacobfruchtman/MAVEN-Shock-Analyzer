function thSpk,x,aj
	maxloc=-1
	n=numel(x)
	print,"n,aj=",n,aj
	b=4
	g=exp(-(abs(x-aj)/10^b)^(2))

	spk= g*50/(abs(x-aj)+1)
	mx=max(spk,maxloc)
	;print,
	print,mx,maxloc,aj,numel(x)
	;print,"spk[0:maxloc+4]=",spk[0:maxloc+4]
	print,"spk[aj-4:aj+4]=",spk[max([aj-4,0]):min([aj+4,n-1])]
	return,spk
end


function fitloop,i, xs,ys,z,AA,BB,CC,DD,rollup,rollsec,mono,hill,yBS,curveType,shks,zwidth,trust, sublengths, Subsets,begs,ends,brokens,cont,test,dobug,minChi,minRemain,lastmax,chi,imx,imn,shockloc, Bmin,Bmax,hill2,rollthird,minM1,orderCustom,maxUp,Bmed,rhod,Bsd,extr,eDD,Fore,BU,B3,ymm,minM0,foots

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
			dim=DD[dloc]
			edim=eDD[dloc]
			if dobug gt 0 then print,"numel(hill)=",numel(hill),", bi=",bi
			aj=AA[j] ; the index of the start trigger 
			if dobug gt 0 then print,"[dim,aj,bi,aj-dim,bi-dim]=",[dim,aj,bi,aj-dim,bi-dim]
			if ((dim gt aj) and (dim lt bi)) or (dim lt bim) then begin
				cont=1
				return, z
			endif  

			foot=foots[(where(foots lt aj and foots ge dim))[0]]

			if foot eq -1 then foot=dim

			bfoot=max([dim,foot-120])
			d00=dim
			dim=(bfoot*3+dim)/4;mean([bfoot,bfoot,bfoot,bfoot,bfoot,dim])

			zeron=aj-dim
			yh=ys[aj]; the y value at the start trigger  
			;print,"gi=DD[i]=G[",i,"]=",gi
			;print,"hj=H[",j,"]=",hj
			;print,"gim=DD[i-1]=G[",i-1,"]=",gim
			;yp=ys[gim:gi]
			print,"[dim,aj,bi]=",[dim,aj,bi]
			yp=ys[dim:bi]
			np=size(yp,/n_el)
			;ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			ybeg=ys[mean([dim,aj]):aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step

			nbeg=size(ybeg,/n_el)
			
			;yend=ys[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			yend=ys[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

			nend=size(yend,/n_el)


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
			;print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
			;jbegj=max([1,nend-20])+aj
			;jendj=min([N-1,nend+20+aj])
			;while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((median(ys[jbegj:jendj]) le mean(ybegtrust)+1.5) and (decfunctions eq 1))) do begin 
				;print,"(brefines,frefines)=",brefines,frefines
				;print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
				;we will do these seperately within the loop, but at same time so that one negligably affects the other 

				;ybegtrust=ybeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
				;yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
				;jbegj=max([1,nend-20])+aj
				;jendj=min([N-1,nend+20+aj])
				;if ((ybeg[0] gt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
					;yp=yp[1:*] ;shrinks our range by one
 					;ybeg=ybeg[1:*]
					;np--
					;nbeg--
					;;gim++
					;dim++
					;brefines++
					;if ( brefines ge 2 *mbeg/5) or (nbeg eq 1) then begin
					;	incfunctions=0
					;	;gim=DD[i-1]
					;	dim=DD[i-1]
					;	;yp=ys[gim:gi]
					;	yp=ys[dim:bi]
					;;	
					;	np=size(yp,/n_el)
					;	;ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
					;	ybeg=ys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step					;	
					;	nbeg=numel(ybeg)
			;			
					;endif
		;		endif
;
;				if ((median(ys[jbegj:jendj]) le mean(ybegtrust)+1.5) and (decfunctions eq 1)) then begin
;					;print,"pulling back the end of shock number i=",i,", from gi=",gi
;					np--
;					yp=yp[0:np-1]
;
;					nend--
;					
;					if nend gt 1 then yend=yend[0:nend-1]
;
;					;gi--
;					bi--
;					frefines++
;					if ( frefines ge 1 *mbeg/5) or (nend eq 1) then begin
;						decfunctions=0
;						;gi=DD[i]
						;yp=ys[gim:gi]
;;						bi=bii
;						yp=ys[dim:bi]
;						np=size(yp,/n_el)
;						;yend=ys[hj:gi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
;						yend=ys[aj:bi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
;						nend=numel(yend)
;					endif
;				endif
;
				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
;				if ((incfunctions eq 0) and (decfunctions eq 0)) then break
;			endwhile 
			
			;p1=plot(xpa,yp, title="Shock "+i)
			
			;p2=plot(xpa,1/(abs(xa-aj)+1)-1,'r-',/overplot)			
			;print, "the number of times we refined the beginning is ",brefines,"."
			;print, "the number of times we refined the end is ",frefines,"."


			;ypBS=yBS[gim:gi]
			;BSbeg=yBS[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step			
			;BSend=yBS[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			;monp=mono[dim:bi]
			;bmonp=bmono[dim:bi]
			;monend=mono[aj:bi]
			;aj0=aj
			;if (rollup eq 1) then aj=monoRoll(aj,bi,dim,mono) ; rolls predicted m2 (inflection point of atan up to 
			;p3=plot(xpa,thSpk(xpa,-dim+aj),'y-',/overplot)			
			if dobug gt 0 then print,"numel(hill)=",numel(hill),", bi=",bi
			;if (rollsec eq 1) then aj=secondRoll(aj,bi,hill,dobug) ; second order correction 
			;p4=plot(xpa,thSpk(xpa,-dim+aj),'g-',/overplot)			

			;aj1=aj
			if dobug gt 0 then print,"aj=",aj,", aj-dim=",aj-dim,", numel(yBS)=",numel(yBS)
			if dobug gt 0   then print,"[mono[aj-1],mono[aj],mono[aj+1]=",[mono[aj-1],mono[aj],mono[aj+1]]
			;if (rollthird eq 1) then a0=Third_Roll(hill2,dim,a0,bi,dobug) ; second order correction  
			;ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region

			;aj=FalseShockSkipper(aj,bi,Bmed,median(Bmed[2 * nbeg/5+dim: nbeg/2+dim]))
			if dobug gt 0 then print,"aj=",aj
			aj0=aj
			aj=ajRefiner(edim,aj,bi,zeron,dobug,yBS,nbeg,nend, mono,hill,hill2,orderCustom,extr,rhod,Fore)
			print,"numel(yBS),dim,aj=",numel(yBS),dim,aj

			if ymm[bi] eq max(ymm[aj:bi]) then while ymm[bi] eq max(ymm[aj:bi]) and bi-aj gt 10*60 do bi--
			while max(ys[aj-60:aj]) gt max(ys[aj:bi]) do aj--
			xp=xs[dim:bi] ; the range of x's between the start and end times 
			xpa=xp-xp[0]; the range of x's normalized so that the first element is zero and last element is gi*(xs[1]-xs[0]) 
			yp=ys[dim:bi]
			np=numel(yp)
			ybeg=ys[mean([dim,aj]):aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step

			nbeg=size(ybeg,/n_el)
			
			;yend=ys[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			yend=ys[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

			nend=size(yend,/n_el)
			ypBS=yBS[dim:bi]
			BSbeg=yBS[mean([dim,aj]):aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step			
			BSend=yBS[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			nbeg=numel(BSbeg)
			nend=numel(BSend)
			print,2 * (nbeg-1)/5, (nbeg-1)/2
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
			;if (rollthird eq 1) then aj2=Third_Roll(hill2,dim,aj1,bi,dobug) ; third order correction
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
			;ph=plot(xpa,300*hill2[dim:bi]+10,color=56,/overplot)
			;p6=plot(xpa,thSpk(xpa,-dim+aj),'b-',/overplot)			
			;print,"aj=",aj,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"aj-dim=",aj-dim
			;if dobug gt 0 then print,"[zeron,aj0-dim,aj1-dim,aaj-dim,aj2-dim]=",[zeron,aj0-dim,aj1-dim,aaj-dim,aj2-dim]


			yc=yp*0
			;if curve function is f(x)=m0 tanh( m1 (x-m2))+m3 then
			;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			mx=max(yend)

			if foot ne d00 then mn=BU[foot] else mn=BU[aj0]
			;mn=BU[foot];mean(ybegtrust)
			;mx=25
			;print,"max=",mx
			;bottom of  step is at y=mn=m3-m0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			;mn=mean(ybegtrust)
			;mn=5
			;print,"min=",mn

			m0=(mx-mn)/2
			m3=(mx+mn)/2
			
			;m2  is the x value in approximately at the top of the curve. to zeroth order, lets use m2=xpa[hj-gim]
			;print,"m2  is the x value in the middle of the curve. to zeroth order, lets use m2=xpa[hj-gim]
;			m2=xpa[hj-gim]
			m2=xpa[aj-dim]
			diiim=dim
			imin=dim
			imax=bi
			if dobug ge 1 then	print,"dim,diiim=",dim,diiim
			;print,"xpa[nbeg/2]=",xpa[nbeg/2]
			;print,"yp[nbeg/2]=",yp[nbeg/2]
			;with all this, can calculate guess for  m1 to zeroth order by
			;m1=  atanh((y-m3)/m0)/(x-m2)
			;to be safe, lets use (x,y)=(xp(nbeg/2),yp(nbeg/2))
			;m1=atanh( ( yp[nbeg/2] -m3 )/m0)/(xpa[nbeg/2] -m2)
			
			;that gives garbage. lets try 1 as an  arbitrary guess
			m1=1
			

			MM=[m0,m1,m2,m3]
			MM0=MM
			print,"to zeroth order, guess that MM=",MM0
			print,'max,min=',MM[0]+MM[3],",",MM[3]-MM[0]
			weight=1.0/yp
			;yfit seems to sometimes give garbage out, so will just use it to calculate MM and then calculate directly
			status=-10
			CHISQ=-1
			yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
			print,"1st order status=",status,", chi^2=",CHISQ
			PRINT, 'Function parameters: ', MM
			print,'max,min=',MM[0]+MM[3],",",MM[3]-MM[0]
			;ishock=round(MM[2]/MM[1])+dim;-x00
			ishock=findShock(MM[2],MM[1],yfit,dim,1)
			;if TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,bi-ishock])] gt MM[0]+MM[3]) eq 0 then 

			if (ys[ishock-11] lt ys[ishock]) and (max(ys[ishock-11:ishock],maxloc2) gt ys[ishock]) then begin

				MM2=MM
				MM2[2]=xpa[maxloc2+ishock-11-dim]

				yfit2=CURVEFIT(xpa, yp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
				print,"status=",status2,", chi^2=",CHISQ2
				PRINT, 'Function parameters: ', MM2
				print,'max,min=',MM[0]+MM[3],",",MM[3]-MM[0]
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
				ishock=findShock(MM2[2],MM2[1],yfit2,dim,1)

				if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) then begin
					MM=MM2
					status=status2
					yfit=yfit2
					yfit1=yfit
					CHISQ=CHISQ2

				endif
			endif

			if dobug ge 1 then	print,"dim,diiim=",dim,diiim
			if dobug ge 1 then	print,"ishock,ishock-xp[0]=",ishock,ishock-diiim
			ishock1=ishock
			imin=dim
			imax=bi
			
			;if working then p7=plot(xpa,thSpk(xpa,-dim+ishock),'m-',/overplot)
;working1=((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi)  and (bi-ishock gt 100) and ( MM[0] gt 1) and (ishock gt imin) and (ishock lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (yBS[max([dim,ishock-5*60])]-yBS[dim] lt 10) and (yfit[max([dim,ishock-5*60])-dim] -yfit[0] lt 10) and (CHISQ gt 0) and (Fore[ishock] eq 1)

			if dim eq edim then begin
				
				iminB=ishock-10*60
			endif else begin

				;

				mxfore=Fore[mean([dim,aj0])+2]
				iminB=(where(Fore[mean([dim,aj0])+2:aj0] eq mxfore))[-1]-10*60

				;if ishock gt edim+30 then begin
					;bmdmn=(where(Bmed[edim:ishock] eq min(Bmed[edim:ishock])))+edim

					;iminB=bmdmn
				;endif
			endelse

			hill2e=hill2[ishock:imax]
			pointerinup= ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) $
 and (array_equal(hill2e[0:min([60,imax-ishock])], hill2e[SORT(hill2e[0:min([60,imax-ishock])])])) ))




			working1=((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi)  and (bi-ishock gt 100) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (yfit[max([dim,iminB])-dim] -yfit[0] lt 10) and (CHISQ gt 0) and (Fore[ishock] eq 1) and (abs(yfit[imin-dim]-BU[ishock]) lt 4) and ~pointerinup	
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
				print,"working1=,",working1

			endif
	
			imin=max([dim,iminB])
			imax=min([bi,ishock+10*60])
;and (yBS[max([dim,ishock-5*60])]-yBS[dim] lt 10) 
			while yBS[imin]-yBS[dim] gt 2 do begin
				imin--
				if yBS[imin]-yBS[dim] gt 10 then begin 
					ishock--
					imax--
				endif 	
			endwhile
				
			if dobug ge 1 then	print,"shrunk front by [dim,imin,imin-dim]=",[dim,imin,imin-dim]
			if dobug ge 1 then	print,"shrunk back by [bi,imax,bi-imax]=",[bi,imax,bi-imax]

			ypp=ys[imin:imax]
			xpp=xs[imin:imax]

			MM1=MM
			;smypp=(smooth(ypp,2,/edge_mirror))[0:8*(ishock-imin)/10]
			smmp=ymm[imin:8*(ishock-imin)/10+imin]
			smypp=Bmed[imin:8*(ishock-imin)/10+imin]
			ssp=(smmp+smypp)/2
			mn=min(smmp)
			if MM[3]-MM[0]  lt min(smmp) then begin
				print,"mm3-mm0=",MM[3]-MM[0], " < ", min(smmp)
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				MMtt=(mx+mn)/2
				MMzz=(mx-mn)/2
				if MMzz gt 0 then begin
					print,"setting M0=",MMzz,", M3=",MMtt
					MM[0]=MMzz
					MM[3]=MMtt
			
				endif

			endif
			if MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5   then begin
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				MMtt=(mx+mn)/2
				MMzz=(mx-mn)/2
				if MMzz gt 0 then begin
					print,"setting M0=",MMzz,", M3=",MMtt
					MM[0]=MMzz
					MM[3]=MMtt
			
				endif
			endif
			xppa=xpp-xpp[0]

			MM=coeffCalc(xpp,ypp, ishock,x00)
			;smypp=(smooth(ypp,2,/edge_mirror))[0:8*(ishock-imin)/10]
			;if MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10])  or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt smypp) lt 5 or total(MM[3]-MM[0] lt smypp) lt 5 then begin;
			;if MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5  then begin
				;mx=MM[3]+MM[0]
				;mn=mean(smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;MM[3]=(mx+mn)/2
				;MM[0]=(mx-mn)/2

			;endif
			;if MM[3]-MM[0]  lt min(smypp) then begin
				;mx=MM[3]+MM[0]
				;mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;MM[3]=(mx+mn)/2
				;MM[0]=(mx-mn)/2

			;endif
			if MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5   then begin

				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				MMtt=(mx+mn)/2
				MMzz=(mx-mn)/2
					if MMzz gt 0 then begin
						print,"setting M0=",MMzz,", M3=",MMtt
						MM[0]=MMzz
						MM[3]=MMtt
			
					endif

			endif

			if MM[3]-MM[0]  lt min(smmp) then begin
				print,"mm3-mm0=",MM[3]-MM[0], " < ", min(smmp)
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				MMtt=(mx+mn)/2
				MMzz=(mx-mn)/2
				if MMzz gt 0 then begin
					print,"setting M0=",MMzz,", M3=",MMtt
					MM[0]=MMzz
					MM[3]=MMtt
			
				endif

			endif

			weight=1.0/ypp
			CHI1=CHISQ
			stat1=status
			yfit1=yfit
			yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
			print,"status=",status,", chi^2=",CHISQ
			PRINT, 'Function parameters: ', MM
			print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smmp)=", min(smmp),", min(smypp)=",min(smypp)
			;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
			ishock=findShock(MM[2],MM[1],yfit,imin,1)
			if MM[3]-MM[0]  lt min(smmp) then begin
				print,"mm3-mm0=",MM[3]-MM[0], " < ", min(smmp)
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				MMtt=(mx+mn)/2
				MMzz=(mx-mn)/2
				if MMzz gt 0 then begin
					print,"setting M0=",MMzz,", M3=",MMtt
					MM[0]=MMzz
					MM[3]=MMtt
			
				endif		
			endif

			if (ys[ishock-11] lt ys[ishock]) and (max(ys[ishock-11:ishock],maxloc2) gt ys[ishock]) then begin

				MM2=MM
				MM2[2]=xpa[maxloc2+ishock-11-dim]
				yfit2=CURVEFIT(xppa, ypp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
				print,"status=",status2,", chi^2=",CHISQ2
				PRINT, 'Function parameters: ', MM2
				print,'Bd,Bu=',MM[0]+MM[3],",",MM[3]-MM[0],", min(smypp)=", min(smypp)
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
				ishock=findShock(MM2[2],MM2[1],yfit2,imin,1)

				if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) then begin
					MM=MM2
					status=status2
					yfit=yfit2
					;yfit1=yfit
					CHISQ=CHISQ2

				endif
			endif


			hill2e=hill2[ishock:imax]
			pointerinup= ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq 0) $
 and (array_equal(hill2e[0:min([60,imax-ishock])], hill2e[SORT(hill2e[0:min([60,imax-ishock])])]) )) 



			working=((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1)  and (bi-ishock gt 100) and (CHISQ gt 0) and (Fore[ishock] eq 1) and (abs(yfit[0]-BU[ishock]) lt 4) and ~pointerinup		; ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) ne 0)
			;if working then p8=plot(xpa,thSpk(xpa,-dim+ishock),'-',color='peru',/overplot)	else p1.close		
			fracRemain=(bi-ishock)*1.0/(bi-dim)

			if dobug eq 3 then begin
				print,"**********************************************************"
				;print,"working=,",working
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

			IF ((CHISQ gt CHI1) and Working1) or( (not Working) and Working1) then begin
				print,"going back to old version"
				yfit=yfit1
				CHISQ=CHI1
				MM=MM1
				ishock=ishock1
				imin=dim
				imax=bi	
				working=working1			
			ENDIF



			if dobug ge 1 then	print,"[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-MM[2],(bi-MM[2])/(bi-dim)]=",[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
			if dobug ge 1 then	print,"[imin,ishock,imax]=",[imin,ishock,imax]
			if ishock lt imin then print, "bad ishock: ishock = ",ishock," lt ", imin,"= imin"
			if ishock gt imax then print, "bad ishock: ishock = ",ishock," gt ", imax,"= imax"

			NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
			;if ((status eq 0) and (MM[0] gt 0)) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt 1) then begin
			if working then begin
				;;t1=TEXT(.1,.8,'Good fit')
				;p1.close
				print,"working"
				lastmax=imax
				sublengths.add,bi-dim
				if( ncount eq 0) then begin; and (abs(MM[2]-MM0[2]) lt 1000) then begin
					;print,"no NANs"
					;for k=gim,gi-1 do z[k]=yfit[k-gim]	
					;for k=dim,bi-1 do z[k]=yfit[k-dim]
					for k=imin,imax-1 do z[k]=yfit[k-imin]	

				endif else begin
					print,"yfit has ",ncount," NANs. calculating directly."

					;if (abs(MM[2]-MM0[2]) ge 1000) then MM=MM0
					tanhfit,xpa,MM,F
					NNN=where(FINITE(F) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
					;for k=gim,gi-1 do z[k]=F[k-gim]

;					if ncount eq 0 then for k=dim,bi-1 do z[k]=F[k-dim]
					if ncount eq 0 then for k=imin,imax-1 do z[k]=F[k-imin]
				
				endelse
 				;ishock=round(MM[2]/MM[1])+imin;-x00
				print,dobug
				if dobug gt 0 then print,"ishock=",ishock
				shks[ishock]=1
				shockloc=ishock
				imn=imin
				imx=imax
				chi=CHISQ
				
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
			endif else brokens.add,[i,dim,bi, xs[dim]-min(xs),xs[bi]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]

			test=1
			
			return, z
end
