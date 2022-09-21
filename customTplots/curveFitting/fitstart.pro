pro fitstart,xs,ys,z,zn,a0,b0,d0,rollup,rollsec,mono,hill,yBS,curveType,shks, sublengths, Subsets,begs,ends,brokens,cont,test,dobug,minChi,minRemain,lastmax,chi,imx,imn,shockloc, Bmin,Bmax,hill2,rollthird,minM1,orderCustom,maxUp,Bmed,rhod,Bsd,extr,eDD,Fore,BU,B3,ymm,minM0,foots,t,reg,errorloc,doPerturb,upDev


print,"=D=D=D=D=D=D=D=D-D=D=D=D=D=D=D=D=D=D=D=D=D"
			i=0
			edim=d0
			yp=ys[0:b0]
			xp=xs[0:b0]
			x00=min(xs)
			print,"now for the range ys[0:BB[0]]=ys[0:",b0,"]"
			;a0=AA[0]
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
			foot=(where(foots lt a0 and foots ge dim))[0]
			;t=tau[a0]
			if foot eq -1 then foot=dim+60

			bfoot=max([dim,foot-10*60]);20*t]);120])
			d00=dim
			dim=bfoot;(bfoot*2+dim)/3;mean([bfoot,bfoot,bfoot,bfoot,bfoot,dim])
			;print,"g0=",b0
			zn=z[b0:*]

			;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)


			ybeg=ys[dim:a0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			nbeg=size(ybeg,/n_el)
			

			;CRUSTST=where((Bsd[a0+1:b0] gt 1.0) and (rhod[a0+1:b0] lt 0),ccount)+a0+1
			;if ccount gt 0  then begin
				;b0=CRUSTST[0]
				;yp=ys[dim:b0]
				;np=numel(yp)
				;zn=z[b0:*]
			;endif


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

			;	jbegj=max([1,nend-20])+a0
			;	jendj=min([N-1,nend+20+a0])
			;while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((yend[nend-1] lt mean(ybegtrust)) and decfunctions  )) do begin 
			;while  (((ybeg[0] gt mean(yendtrust)) and incfunctions) or ((median(ys[jbegj:jendj]) le mean(ybegtrust)+1.5) and (decfunctions eq 1))) do begin 	
				;print,"(brefines,frefines)=",brefines,frefines
				;print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
				;we will do these seperately within the loop, but at same time so that one negligably affects the other 
			;	;jbegj=max([1,nend-20])+a0
			;	jendj=min([N-1,nend+20+a0])
			;	if ((ybeg[0] gt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
			;		;print,"pushing forward the end of shock number i=",0,", from gim=",gim,", where a0=",a0,", and g0=",g0, "and size(yp)=",size(yp,/n_el)
			;		yp=yp[1:*] ;shrinks our range by one
 			;;;		ybeg=ybeg[1:*]
			;;		np--
			;;		nbeg--
			;		dim++
			;		brefines++
			;		if ( brefines ge 2 *mbeg/5) or (dim +1 eq a0) or (np le 3) or (nbeg le 3) then begin
			;			incfunctions=0
			;			dim=d00
			;			yp=ys[dim:bi]
			;			np=size(yp,/n_el)
			;			ybeg=ys[dim:a0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			;			nbeg=mbeg
						
			;		endif
			;	endif
			;	if ((median(ys[jbegj:jendj]) le mean(ybegtrust)+1.5) and (decfunctions eq 1)) then begin
				;if ((yend[nend-1] lt mean(ybegtrust)) and (decfunctions eq 1))  then begin
					;print,"pulling back the end of shock number i=",i,", from gi=",gi
			;		yp=yp[0:np-2]
			;		np--
			;		yend=yend[0:nend-2]
			;		nend--
			;		bi--
			;;		frefines++
			;		if ( frefines ge 1 *mbeg/5) or (a0+1 eq bi) or (nend le 3) or (np le 3) then begin
			;			;print,"i=",i,", ddn=",ddn
			;			decfunctions=0
			;			bi=b0
			;			yp=ys[dim:bi]
			;			np=size(yp,/n_el)
			;			yend=ys[a0:bi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			;			nend=mend
			;		endif
			;	endif

				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
			;	if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			;endwhile 
			dim=d00
			b0=bi
			;print, "the number of times we refined the beginning is ",brefines,"."
			;print, "the number of times we refined the end is ",frefines,"."
			xp=xs[dim:b0]
			yp=ys[dim:b0]
			if d00 ne 0 then edim=eDD[0]

			xpa=xp-xp[0]
			yc=yp*0.0
			;if (rollthird eq 1) then a0=Third_Roll(hill2,dim,a0,bi,dobug) ; second order correction  
			;ybegtrust=ybeg[2 * nbeg/5: nbeg/2] ; we're pretty sure the step will never be  in this region
			zeron=a0-dim
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
			;mn=mean(ybegtrust)
			;a0=FalseShockSkipper(a0,bi,Bmed,median(Bmed[2 * nbeg/5+dim: nbeg/2+dim]))
			;print,"a0=",a0,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"a0-dim=",a0-dim
			;if (rollup eq 1) then a0=monoRoll(a0,bi,dim,mono)
			;if (rollsec eq 1) then a0=secondRoll(a0,bi,hill,dobug) ; second order correction 
			a00=a0
			print,"[dim,edim,a0,bi]=",[dim,edim,a0,bi]
			help,Fore
			a0=ajRefiner(edim,a0,bi,zeron,dobug,yBS,nbeg,nend, mono,hill,hill2,orderCustom,extr,rhod,Fore)

			while max(ys[a0-60:a0]) gt max(ys[a0:bi]) do a0--
			;a0=aoscillate(a0,bi,dim,yBS,nbeg,nend,dobug)
			;if (rollthird eq 1) then a0=Third_Roll(hill2,dim,a0,bi,dobug) ; third order correction 
			;print,"a0=",a0,", dim=",dim, ",bi=",bi,", size(xpa,/n_el)=",size(xpa,/n_el),"a0-dim=",a0-dim

			;if curve function is f(x)=m0 tanh( m1 *(x-m2))+m3 then
			;top of step is at y=mx=m0+m3 ~  mean (yendtrust) 
			; or a zeroth order estimate of 25
			xp=xs[dim:b0]
			yp=ys[dim:b0]
			;mx=mean(yendtrust)
			mx=25
			;print,"max=",mx
			;bottom of  step is at y=mn=m3-m0 ~mean(ybegtrust)
			;or  a zeroth order estimate of ~5

			;mn=mean(ybegtrust)
			mn=BU[a0]
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
			hill2e=hill2[ishock:imax]
			pointerinup= ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) $
 and (array_equal(hill2e[0:min([60,imax-ishock])], hill2e[SORT(hill2e[0:min([60,imax-ishock])])])) ))



			working=(status eq 0) and (CHISQ lt minChi) and (bi-ishock gt minRemain) and ( MM[0] gt minM0) and (ishock gt imin) and (ishock lt imax) and (abs(MM[3]-MM[0]-mn) lt maxUp) and (MM[1] gt minM1)  and (Fore[ishock] eq 1) and (abs(yfit[0]-BU[ishock]) lt upDev) and ~pointerinup;(TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0)

			if dobug eq 3 then begin
				print,"**************************"
				print,"[imin,ishock,imax]=",[imin,ishock,imax]
				print,"[a00,a0,ishock]=",[a00,a0,ishock]
				print,"Fore[a00,a0,ishock]=",[Fore[a00],Fore[a0],Fore[ishock]]
				print,"~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
				print,"status eq 0=",status eq 0
				print,"MM[0],minM0,> :=",MM[0],minM0, MM[0] gt minM0

				print,"MM[1],minM1,MM[1]>minM1=",MM[1],minM1,MM[1] gt minM1
				print,"MM[3]-MM[0],mn,abs(MM[3]-MM[0]-mn),[<] maxup,success=",MM[3]-MM[0],mn,maxup,abs(MM[3]-MM[0]-mn),(abs(MM[3]-MM[0]-mn) lt maxUp)
				print,"bi-ishock,>100=",bi-ishock,(bi-ishock gt 100)
				print,"Fore[ishock]=",Fore[ishock]
				print,"yfit[0],BU[ishock],abs(yfit[0]-BU[ishock]), [<] upDev, success=",yfit[0],BU[ishock],upDev,abs(yfit[0]-BU[ishock]) lt upDev
				print,"~pointerinup=",~pointerinup

				print,"--------------------------"
				print,"working=,",working
				print,"**************************"
			endif

			if doPerturb then begin
				MM1=MM

				chi1=CHISQ
				yfit1=yfit
				working1=working
				ishock1=ishock

				imin=max([dim,foot-180]);8*t]);ishock-7*60])
				imax=min([bi,ishock+7*60])

				while (yBS[imin]-yBS[dim] gt 2)  and (imin gt imax+100) do begin
					imin--
					if yBS[imin]-yBS[dim] gt 10 then begin
						 ishock--
						imax--
					endif
				endwhile
				print,"(x)shrunk front by [dim,imin,imin-dim]=",[dim,imin,imin-dim]
				print,"(x)shrunk back by [bi,imax,bi-imax]=",[bi,imax,bi-imax]

				ypp=ys[imin:imax]
				xpp=xs[imin:imax]
				print,"[numel(yp),numel(ypp)]=",[numel(yp),numel(ypp)]
				xppa=xpp-xpp[0]				
				smmp=ymm[imin:8*(ishock-imin)/10+imin]
				smypp=Bmed[imin:8*(ishock-imin)/10+imin]

				mn=min(smmp)
			;if MM[3]-MM[0]  lt min(smypp) then begin
				;mx=MM[3]+MM[0]
				;mn=min(smmp);smypp);ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;MM[3]=(mx+mn)/2
				;MM[0]=(mx-mn)/2

			;endif

				;if MM[3]-MM[0]  lt min(smypp)  or MM[3]-MM[0]  gt max(smypp) or total(MM[3]-MM[0] gt smypp) lt 5 or total(MM[3]-MM[0] lt smypp) lt 5 then begin;if MM[3]-MM[0]  lt
				;if MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10])  or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5 then begin;then begin
					;mx=MM[3]+MM[0]
					;mn=min(smmp);mean(ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
					;MM[3]=(mx+mn)/2
					;MM[0]=(mx-mn)/2

				;endif
				MM=coeffCalc(xpp,ypp, ishock,x00)
				;if MM[3]-MM[0]  lt min(smypp)  or MM[3]-MM[0]  gt max(smypp) or total(MM[3]-MM[0] gt smypp) lt 5 or total(MM[3]-MM[0] lt smypp) lt 5 then begin;if MM[3]-MM[0]  lt
				;if MM[3]-MM[0]  lt min(ypp[0:8*(ishock-imin)/10]) or MM[3]-MM[0]  gt max(ypp[0:8*(ishock-imin)/10]) or total(MM[3]-MM[0] gt ypp[0:8*(ishock-imin)/10]) lt 5 or total(MM[3]-MM[0] lt ypp[0:8*(ishock-imin)/10]) lt 5 then begin; then begin
				;	mx=MM[3]+MM[0]
					;mn=min(smmp);mean(ypp[0:8*(ishock-imin)/10]);min(ypp[0:ishock-imin])
				;	MM[3]=(mx+mn)/2
				;	MM[0]=(mx-mn)/2

				;endif
				weight=1.0/ypp
				yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
				PRINT, 'To second order, Function parameters: ', MM
				print,"status=",status,", CHISQ=",CHISQ,", numel(yfit)=",numel(yfit)
			
				ishock=findShock(MM[2],MM[1],yfit,imin,1)
				hill2e=hill2[ishock:imax]
				pointerinup= ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq 0 $
 and (array_equal(hill2e[0:min([60,imax-ishock])], hill2e[SORT(hill2e[0:min([60,imax-ishock])])])) ))




				working=((status eq 0) and (MM[0] gt minM0)) and (CHISQ lt minChi) and (bi-ishock gt minRemain)  and (ishock gt imin) and (ishock lt imax) and (MM[3]-MM[0]-mn lt 10) and (MM[1] gt minM1) and (Fore[ishock] eq 1) and (abs(yfit[0]-BU[ishock]) lt upDev) and ~pointerinup;(TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0)


if dobug eq 3 then begin
				print,"**************************"
				print,"[imin,ishock,imax]=",[imin,ishock,imax]
				print,"[a00,a0,ishock]=",[a00,a0,ishock]
				print,"Fore[a00,a0,ishock]=",[Fore[a00],Fore[a0],Fore[ishock]]
				print,"~~~~~~~~~"
				print,"status eq 0=",status eq 0
				print,"MM[0],minM0,> :=",MM[0],minM0, MM[0] gt minM0

				print,"MM[1],minM1,MM[1]>minM1=",MM[1],minM1,MM[1] gt minM1
				print,"MM[3]-MM[0],mn,abs(MM[3]-MM[0]-mn),[<] maxup,success=",MM[3]-MM[0],mn,maxup,abs(MM[3]-MM[0]-mn),(abs(MM[3]-MM[0]-mn) lt maxUp)
				print,"bi-ishock,>100=",bi-ishock,(bi-ishock gt 100)
				print,"Fore[ishock]=",Fore[ishock]
				print,"yfit[0],BU[ishock],abs(yfit[0]-BU[ishock]), [<] upDev, success=",yfit[0],BU[ishock],upDev,abs(yfit[0]-BU[ishock]) lt upDev
				print,"~pointerinup=",~pointerinup

				print,"--------------------------"
				print,"working=,",working
				print,"**************************"
			endif

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
					if lastmax le 0 then lastmax=imax
					print,"0th outbound is working"
					;inimaxs[imin:imax]=imax
					;inimins[imin:imax]=imin
					;inchis[imin:imax]=CHISQ
					;innums[imin:imax]=i
					;inshocklocs[imin:imax]=ishock
					;inups[imin:imax]=MM[3]-MM[0]
					;indowns[imin:imax]=MM[3]+MM[0]
				;for k=imin,imax do begin
				;	inimaxs[k]=imax
				;	inimins[k]=imin
				;	inchis[k]=CHISQ
				;	innums[k]=i
				;	inshocklocs[k]=ishock
				;	inups[k]=MM[3]-MM[0]
				;	indowns[k]=MM[3]+MM[0]
				;endfor
				shks[ishock]=1
				shockloc=ishock
				imx=imax
				imn=imin
				chi=CHISQ
				Bmax=MM[3]+MM[0]
				Bmin=MM[3]-MM[0]
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
				sublengths.add,bi-dim
				ishock=round(MM[2])+dim
				
				;print,"g0"
				zn=z[b0:*]

				;print,"mean(zn)=",mean(zn),",  max(zn)=",max(zn)
			endif else brokens.add,[i,dim,b0, xs[dim]-min(xs),xs[b0]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]
			Subsets[dim:b0]=1;i
			begs[dim:a0-1]=1;i
			print,numel(inends),a0,b0
			ends[a0:b0]=1;i

end

