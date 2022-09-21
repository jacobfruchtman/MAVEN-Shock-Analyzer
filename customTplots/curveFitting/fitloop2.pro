
function fitloop2,i, xs,ys,z,aj,bi,dim,foot,edim,rollup,rollsec,mono,B5d,yBS,curveType,shks,zwidth,trust, sublengths, Subsets,begs,ends,brokens,cont,test,dobug,minChi,minRemain,lastmax,chi,imx,imn,shockloc, Bmin,Bmax,B20d,rollthird,minM1,orderCustom,maxUp,Bmed,rhod,Bsd,extr,Fore,BU,B3,ymm,minM0,mxfore,allownonfore=allownonfore
			nonfore=0

			if keyword_set(allownonfore) then nonfore=1

			
			N=numel(Bsd)
			x00=xs[0]
			xa=xs-x00
			

			
			
			;j=i-bcutoff ; if cutoff at beginning, then AA[i-1] and DD[i]  will be paired together, rather than AA[i] and DD[i]

			print,"i=",i
			
			
			
			if dobug gt 0 then print,"[dim,aj,bi,aj-dim,bi-dim]=",[dim,aj,bi,aj-dim,bi-dim]
			if ((dim gt aj) and (dim lt bi))  then begin
				cont=1
				return, z
			endif  
			isFinite=(total(finite(ys[dim:bi]) ne 1) eq 0) and (total(finite(ys[dim:aj]) ne 1) eq 0) and (total(finite(ys[aj:bi]) ne 1) eq 0)
			if ~isFinite then begin
				print,"No MAVEN Mag Field Data here. WAT. Continuing"
				cont=1
				return, z
			endif
			print,"foot=",foot
			bfoot=max([dim,foot-10*60]);120])
			print,"[dim,bfoot,foot,aj,aj-bfoot,(aj-bfoot)/60]=",[dim,bfoot,foot,aj,aj-bfoot,(aj-bfoot)/60.]
			d00=dim
			dim=bfoot;(bfoot*3+dim)/4;mean([bfoot,bfoot,bfoot,bfoot,bfoot,dim])
			ymf=ymm[foot]
			zeron=aj-dim
			yh=ys[aj]; the y value at the start trigger  
			
			if ymm[dim] le 0 or finite(ymm[dim]) ne 1 then begin
				KK=where(ymm[dim:aj] gt 0)
				if foot ne d00 then begin
					LL=where(xs[dim:aj] lt xs[foot])
					SS=INTERSECT(KK,temporary(LL))

					if SS[0] ne -1 and SS[0]+dim lt foot-60 then dim=SS[0]+dim else begin
						cont=1
						return, z

					endelse
				endif else dim=KK[0]+dim

			endif
			print,"[dim,aj,bi]=",[dim,aj,bi]
			yp=ys[dim:bi]
			np=size(yp,/n_el)
			ybeg=ys[mean([dim,aj]):aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step

			nbeg=size(ybeg,/n_el)
			

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

			bii=bi

			
			
			
			if dobug gt 0 then print,"numel(B5d)=",numel(B5d),", bi=",bi
			
			if dobug gt 0 then print,"aj=",aj,", aj-dim=",aj-dim,", numel(yBS)=",numel(yBS)
			if dobug gt 0   then print,"[mono[aj-1],mono[aj],mono[aj+1]=",[mono[aj-1],mono[aj],mono[aj+1]]

			;ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region


			if dobug gt 0 then print,"aj=",aj
			aj0=aj
			tedim=max([edim,dim])
			aj=ajRefiner(max([edim,foot]),max([edim,dim]),aj,bi,zeron,dobug,yBS,nbeg,nend, mono,B5d,B20d,orderCustom,extr,rhod,Fore)
			print,"numel(yBS),dim,aj=",numel(yBS),dim,aj

			if ymm[bi] eq max(ymm[aj:bi]) then while ymm[bi] eq max(ymm[aj:bi]) and bi-aj gt 10*60 do bi--
			while max(ys[aj-60:aj]) gt max(ys[aj:bi]) and aj -foot gt 60  do aj--
			xp=xs[dim:bi] ; the range of x's between the start and end times 
			xpa=xp-xp[0]; the range of x's normalized so that the first element is zero and last element is gi*(xs[1]-xs[0]) 
			yp=ys[dim:bi]
			np=numel(yp)
			ybeg=ys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step

			nbeg=size(ybeg,/n_el)
			

			yend=ys[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

			nend=size(yend,/n_el)
			ypBS=yBS[dim:bi]
			BSbeg=yBS[mean([dim,aj]):aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step			
			BSend=yBS[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			nbeg=numel(BSbeg)
			nend=numel(BSend)
			print,2 * (nbeg-1)/5, (nbeg-1)/2


			



			
			
			yc=yp*0
			;if curve function is f(x)=m0 tanh( m1 (x-m2))+m3 then
			;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25

			mx=mean(ymm[aj:bi]);max(ymm[aj:bi]);yend)

			if foot ne d00 then mn=BU[foot] else mn=BU[aj0]
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
			ishock=findshock(aj,MM[2],MM[1],yfit,dim,1,foot)
			;if TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,bi-ishock])] gt MM[0]+MM[3]) eq 0 then 
			print,"[dim,bfoot,foot,aj,ishock]=",[dim,bfoot,foot,aj,ishock]
			print,"[aj-dim,ishock-dim,(ishock-dim)/60]",[aj-dim,ishock-dim,(ishock-dim)/60.]
			if ishock lt foot then ishock=aj
			if (ys[ishock-11] lt ys[ishock]) and (max(ys[ishock-11:ishock],maxloc2) gt ys[ishock]) then begin

				MM2=MM
				;MM2[2]=xpa[maxloc2+MM[2]-11 -2]
				MM[2]=xpa[maxloc2+ishock-dim-11]
				yfit2=CURVEFIT(xpa, yp, weight, MM2, SIGMA, FUNCTION_NAME=curveType,status=status2,CHISQ=CHISQ2)
				print,"status=",status2,", chi^2=",CHISQ2
				PRINT, 'Function parameters: ', MM2
				print,'max,min=',MM[0]+MM[3],",",MM[3]-MM[0]
				;ishock=round(MM[2]/MM[1])+xpp[0]-x00;-x00
				ishock=findshock(aj,MM2[2],MM2[1],yfit2,dim,1,foot)

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
			

;working1=(status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi);((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi)  and (bi-ishock gt 100) and ( MM[0] gt 1) and (ishock gt imin) and (ishock lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (yBS[max([dim,ishock-5*60])]-yBS[dim] lt 10) and (yfit[max([dim,ishock-5*60])-dim] -yfit[0] lt 10) and (CHISQ gt 0) and (Fore[ishock] eq 1)

			if d00 eq edim or mxfore eq 1 then begin
				print,"d00 eq edim"
				iminB=max([dim,foot-2*60]);ishock-10*60
				print,iminB,foot-2*60
				;if ymm[iminB] gt ymf then begin
					;ympref=ymm[iminB:foot]
					;ympreB=ymm[iminB-2*60:iminB]
					;if total(min(ympref) lt [ymm[iminB],ymf]) ge 1 and ymm[iminB-2*60] lt min(ymm[iminB:foot]) then iminB=max([dim,iminB-2*60])
					

				;endif
			endif else begin
				print,"d00 ne edim"
				;
				IN1=where(Fore[d00:ishock] eq 1)+d00
				if  foot-IN1[0] ge 60 then begin
					iminB=IN1[0]
				endif else begin
					INMAX=where(Fore[d00:ishock] eq mxfore)+d00
					simB=INMAX[0]
					eimB=INMAX[-1]
					iminB=max([d00,simB+1,eimB-2*60]);;THE -2*60 here is so that we don't just spawn on top of the previous shock crossing	
				endelse
			endelse

			;firstfore=(where(Fore[dim:bi] eq 1))[0]
			;if d00 eq edim or nonfore or  then begin
			;	print,"d00 eq edim"
			;	iminB=max([dim,foot-2*60,min([firstfore,foot])]);ishock-10*60
			;	print,iminB,foot-2*60
				;if ymm[iminB] gt ymf then begin
					;ympref=ymm[iminB:foot]
					;ympreB=ymm[iminB-2*60:iminB]
					;if total(min(ympref) lt [ymm[iminB],ymf]) ge 1 and ymm[iminB-2*60] lt min(ymm[iminB:foot]) then iminB=max([dim,iminB-2*60])
					

				;endif
			;endif else begin
				;print,"d00 ne edim"
				;

				;mxfore=max(Fore[(d00+ishock)/2:ishock]);Fore[aj]
				;iminB=(where(Fore[mean([d00,ishock]):ishock] eq mxfore))[-1]
				;print,iminB
				;if ishock gt edim+30 then begin
					;bmdmn=(where(Bmed[edim:ishock] eq min(Bmed[edim:ishock])))+edim

					;iminB=bmdmn
				;;endif
			;endelse

			B20de=B20d[ishock:imax]
			pointerinup= ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) $
 and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])])) ))




			working1=(status eq 0) and (MM[1] gt 0)and ( MM[0] gt minM0) and (ishock gt imin) and (ishock lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (CHISQ lt minChi) and (Fore[ishock] eq 1 or nonfore) ;((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi)  and (bi-ishock gt 100) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1) and (yfit[max([dim,iminB])-dim] -yfit[0] lt 10) and (CHISQ gt 0) and (Fore[ishock] eq 1) and (abs(yfit[imin-dim]-BU[ishock]) lt 4) and ~pointerinup	
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
			if imin gt ishock and dim lt ishock then imin=dim
			imax=min([bi,ishock+7*60])

			print,"[imin,foot,aj,ishock]=",[imin,foot,aj,ishock]
			print,"[aj-imin,ishock-imin,(ishock-imin)/60]=",[aj-imin,ishock-imin,(ishock-imin)/60.]
;and (yBS[max([dim,ishock-5*60])]-yBS[dim] lt 10) 
			while ymm[imin]-ymm[dim] gt 4 and imin gt iminB-2*60 and imin gt dim do begin
				imin--
				if ymm[imin]-ymm[dim] gt 10  then begin 
					ishock--
					imax--
				endif 	
			endwhile
				
			if dobug ge 1 then	print,"shrunk front by [dim,imin,imin-dim]=",[dim,imin,imin-dim]
			if dobug ge 1 then	print,"shrunk back by [bi,imax,bi-imax]=",[bi,imax,bi-imax]
			if imin gt ishock and dim lt ishock then imin=dim
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
			print,"smmp[0]=",smmp[0]
			print,"min(smmp)=",min(smmp)


			if  MM[3]-MM[0]  gt min([smmp[0],mean(ymm[imin:foot])])+1 then begin
				print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " > ",min([smmp[0],mean(ymm[imin:foot])])+1,"=smmp[0]+1"
				mx=MM[3]+MM[0]
				mn=min([smmp[0],mean(ymm[imin:foot])]);smmp[0];smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				print,"mx,mn=",mx,mn
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.
				if MMzz gt 0 and MMtt gt 0 then begin
					print,"setting M0=",MMzz,", M3=",MMtt
					MM[0]=MMzz
					MM[3]=MMtt
			
				endif

			endif

			if  MM[3]-MM[0]  lt min(smmp) then begin
				print,"mm3-mm0=",MM[3]-MM[0], " < ", min(smmp)
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				print,"mx,mn=",mx,mn
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.
				if MMzz gt 0 then begin
					print,"setting M0=",MMzz,", M3=",MMtt
					MM[0]=MMzz
					MM[3]=MMtt
			
				endif

			endif
			if 0 and MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5   then begin
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				print,"mx,mn=",mx,mn
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.
				if MMzz gt 0 then begin
					print,"setting M0=",MMzz,", M3=",MMtt
					MM[0]=MMzz
					MM[3]=MMtt
			
				endif
			endif
			xppa=xpp-xpp[0]

			MM=coeffCalc(xpp,ypp, ishock,x00)




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
				print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " > ",smmp[0]+1,"=smmp[0]+1"
				mx=MM[3]+MM[0]
				mn=smmp[0];smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				print,"mx,mn=",mx,mn
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.
				if MMzz gt 0 and MMtt gt 0 then begin
					print,"setting M0=",MMzz,", M3=",MMtt
					MM[0]=MMzz
					MM[3]=MMtt
			
				endif

			endif

			if 0 and MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5   then begin
				print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " < ", min(smmp)
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.
					if MMzz gt 0 then begin
						print,"setting M0=",MMzz,", M3=",MMtt
						MM[0]=MMzz
						MM[3]=MMtt
			
					endif

			endif

			if  MM[3]-MM[0]  lt min(smmp) then begin
				print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " < ", min(smmp)
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				print,"mx,mn=",mx,mn
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.
				if MMzz gt 0 and MMtt gt 0 then begin
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
			ishock=findshock(aj,MM[2],MM[1],yfit,imin,1,foot)
			print,"[imin,foot,ishock,ishock-imin,(ishock-imin)/60]=",[imin,foot,ishock,ishock-imin,(ishock-imin)/60.]
			
			if  MM[3]-MM[0]  gt smmp[0]+1 then begin
				print,"mm3,mm0,mm3-mm0=",MM[3],MM[0],MM[3]-MM[0], " > ",smmp[0]+1,"=smmp[0]+1"
				mx=MM[3]+MM[0]
				mn=smmp[0];smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				print,"mx,mn=",mx,mn
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.
				if MMzz gt 0 then begin
					print,"setting M0=",MMzz,", M3=",MMtt
					MM[0]=MMzz
					MM[3]=MMtt
			
				endif

			endif

			if  MM[3]-MM[0]  lt min(smmp) then begin
				print,"mm3-mm0=",MM[3]-MM[0], " < ", min(smmp)
				mx=MM[3]+MM[0]
				mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				MMtt=(mx+mn)/2.
				MMzz=(mx-mn)/2.
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
				ishock=findshock(aj,MM2[2],MM2[1],yfit2,imin,1,foot)

				if (CHISQ2 lt CHISQ) and (status2 eq 0) and (MM2[1] gt 0) and (finite(CHISQ2) eq 1) and (CHISQ2 lt minChi) and ( MM2[0] gt minM0) then begin
					MM=MM2
					status=status2
					yfit=yfit2
					;yfit1=yfit
					CHISQ=CHISQ2

				endif
			endif


			B20de=B20d[ishock:imax]
			pointerinup= ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq 0) $
 and (array_equal(B20de[0:min([60,imax-ishock])], B20de[SORT(B20de[0:min([60,imax-ishock])])]) )) 



			working=((status eq 0) and (MM[1] gt 0)) and (finite(CHISQ) eq 1) and (CHISQ lt minChi) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1)  and (bi-ishock gt 100) and (CHISQ gt 0) and (Fore[ishock] eq 1 or nonfore) and (abs(yfit[0]-BU[ishock]) lt 4) and ~pointerinup		; ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) ne 0)
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
			print,'m0,m3,max,min=',MM[0],MM[3],MM[0]+MM[3],",",MM[3]-MM[0]
			endif else brokens.add,[i,dim,bi, xs[dim]-min(xs),xs[bi]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]

			test=1
			if imin lt bfoot then message,"bad shock"
			return, z
end
