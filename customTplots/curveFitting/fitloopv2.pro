pro fitstart,xs,ys,z,zn,a0,b0,d0,rollup,rollsec,mono,hill,yBS,curveType,shks, sublengths, Subsets,begs,ends,brokens,cont,test,dobug,minChi,minRemain,lastmax,chi,imx,imn,shockloc, Bmin,Bmax,hill2,rollthird,minM1,orderCustom,maxUp,Bmed,rhod,Bsd,extr,eDD,Fore,BU,B3,ymm,minM0,foots,t


print,"=D=D=D=D=D=D=D=D-D=D=D=D=D=D=D=D=D=D=D=D=D"
			i=0

			yp=ys[0:b0]
			xp=xs[0:b0]

			print,"now for the range ys[0:BB[0]]=ys[0:",b0,"]"



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

			if foot eq -1 then foot=dim+60

			bfoot=max([dim,foot-20*t]);120])
			d00=dim
			dim=bfoot;(bfoot*2+dim)/3;mean([bfoot,bfoot,bfoot,bfoot,bfoot,dim])

			zn=z[b0:*]



			ybeg=ys[dim:a0] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			nbeg=size(ybeg,/n_el)









			yend=ys[a0:b0] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			nend=size(yend,/n_el)


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
















































			dim=d00
			b0=bi


			xp=xs[dim:b0]
			yp=ys[dim:b0]
			if d00 ne 0 then edim=eDD[0]

			xpa=xp-xp[0]
			yc=yp*0.0


			zeron=a0-dim
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region





			a00=a0
			print,"[dim,edim,a0,bi]=",[dim,edim,a0,bi]
			help,Fore
			a0=ajRefiner(edim,a0,bi,zeron,dobug,yBS,nbeg,nend, mono,hill,hill2,orderCustom,extr,rhod,Fore)

			while max(ys[a0-60:a0]) gt max(ys[a0:bi]) do a0--






			xp=xs[dim:b0]
			yp=ys[dim:b0]

			mx=25




			mn=BU[a0]

			
			m0=(mx-mn)/2
			m3=(mx+mn)/2


			m2=xpa[a0-dim]




			m1=1
			MM=[m0,m1,m2,m3]
			print,"to zeroth order, guess that MM=",MM
			MM0=MM
			CHISQ=-1
			weight=1.0/yp

			status=-19
			yfit=CURVEFIT(xpa, yp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
			MM1=MM
			PRINT, 'Function parameters: ', MM,", status=",status

			ishock=findShock(MM[2],MM[1],yfit,dim,1-doPerturb)
			imin=dim
			imax=bi
			hill2e=hill2[ishock:imax]
			pointerinup= ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) eq0 $
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

				imin=max([dim,foot-8*t]);ishock-7*60])
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













				MM=coeffCalc(xpp,ypp, ishock,x00)







				weight=1.0/ypp
				yfit=CURVEFIT(xppa, ypp, weight, MM, SIGMA, FUNCTION_NAME=curveType,status=status,CHISQ=CHISQ)
				PRINT, 'To second order, Function parameters: ', MM
				print,"status=",status,", CHISQ=",CHISQ,", numel(yfit)=",numel(yfit)
			
				ishock=findShock(MM[2],MM[1],yfit,imin,1)
				hill2e=hill2[ishock:imax]
				pointerinup= ( TOTAL(Bmed[max([ishock-10,0]):min([ishock+2*60,imax])] gt MM[0]+MM[3]) eq 0) or $ ; if the average is less than the downstream for two minutes after crossing the predicted shock, then clearly we're still upstream					

				     (     (TOTAL(Bmed[max([ishock-10,0]):min([ishock+60,imax])] gt MM[0]+MM[3] ) $
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


			print,"[imin,ishock,imax]=",[imin,ishock,imax] 
			if ishock lt imin then print, "bad ishock: ishock = ",ishock," lt ", imin,"= imin"
			if ishock gt imax then print, "bad ishock: ishock = ",ishock," gt ", imax,"= imax"

			print,"[max,min,remain,fracRemain]=[m3+m0,m3-m0,bi-ishock,(bi-ishock)*1.0/(bi-dim)]=",[MM[3]+MM[0],MM[3]-MM[0],bi-ishock,fracRemain]
			print,"[status,CHISQ lt minChi, bi-ishock gt minRemain]=",[status,CHISQ lt minChi, bi-ishock gt minRemain]
			if working then begin
					if lastmax le 0 then lastmax=imax
					print,"0th outbound is working"
















				shks[ishock]=1
				shockloc=ishock
				imx=imax
				imn=imin
				chi=CHISQ
				Bmax=MM[3]+MM[0]
				Bmin=MM[3]-MM[0]
				NNN=where(FINITE(yfit) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)
				if( ncount eq 0) then begin;and (abs(MM[2]-MM0[2]) lt 1000) then begin


				
					for k=imin,imax-1 do z[k]=yfit[k-imin]	
				endif else begin
					print,"yfit has ",ncount," NANs. calculating directly."

					if doPerturb then tanhfit,xppa,MM,F else tanhfit,xpa,MM1,F
					NNN=where(FINITE(F) eq 0, ncount, COMPLEMENT=B_C, NCOMPLEMENT=countnc)

					zn=z[imax:*]
					if ncount eq 0 then for k=imin,imax-1 do z[k]=F[k-imin]



						
				endelse
				sublengths.add,bi-dim
				ishock=round(MM[2])+dim

				zn=z[b0:*]

			endif else brokens.add,[i,dim,b0, xs[dim]-min(xs),xs[b0]-min(xs),m0,m1,m2,m3,MM[0],MM[1],MM[2],MM[3],CHISQ]
			Subsets[dim:b0]=1;i
			begs[dim:a0-1]=1;i
			print,numel(inends),a0,b0
			ends[a0:b0]=1;i

end

