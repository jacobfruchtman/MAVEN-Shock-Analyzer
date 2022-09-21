pro boundaryshrinker, dim,aj,bi, ys,Bsd,rhod,Bmed,neh
		;print,"=====BOUNDARY SHRINKER========="
			brefines=0
			frefines=0
			yp=ys[dim:bi]
			np=size(yp,/n_el)
			;ybeg=ys[gim:hj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
			ybeg=ys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step

			nbeg=size(ybeg,/n_el)
			
			;yend=ys[hj:gi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 
			yend=ys[aj:bi] ; elements after the first trigger. to zeroth order, this is (probably) after the step starts. ish 

			nend=size(yend,/n_el)


			;want to make sure we don't include the crustal field in our measurements. When in crustal field, B increases while rho decreases
			bii=bi

			if aj+20 gt bi-10 then return
			CRUSTST=where((Bsd[aj+1:bi] gt 1.0) and (rhod[aj+1:bi] lt 0),ccount)+aj+1
			if 0 and ccount gt 0  then begin
				for j=0,ccount-1 do begin
					if CRUSTST[-j]  gt aj+ 10*60 then bi=CRUSTST[-j] else break
				endfor
				;bi=CRUSTST[0]
				yp=ys[dim:bi]
				np=numel(yp)
				yend=ys[aj:bi]
				nend=numel(yend)
				;print,"bi,bii=",bi,bii
				frefines+=bii-bi
			endif

				;print,aj,aj+1,aj+5*60, bi-21,bi	
			;print,"dim,aj,bi=",[dim,aj,aj+1,bi-21,bi]			
			while aj lt bi-5*60 and (     max(Bmed[bi-20:bi],mxloc) gt max(Bmed[aj+1:bi-21])) and rhod[bi] lt 0 and aj+5*60 lt mxloc+bi-20-1 do begin
				;print,aj,aj+1,aj+5*60, bi-21, mxloc+bi-20-1,bi				
				bi=mxloc+bi-20-1
			endwhile
			bi0=bi

			while Bmed[bi] gt max(Bmed[aj:bi-1]) and aj+5*60-1 lt bi  do bi--
			;backNinth=9.0*(bi-aj)/10 +aj
			;print,aj,backNinth,bi
			;while max(ys[aj:backNinth])+2 lt max(ys[backNinth+1:bi]) and max(ys[backNinth+1:bi])- mean(ys[backNinth+1:bi]) gt 2 do begin
			;	print,aj,backNinth,bi
			;	bi--
			;	backNinth=9.0*(bi-aj)/10 +aj
		
			;endwhile
				yp=ys[dim:bi]
				np=numel(yp)
				yend=ys[aj:bi]
				nend=numel(yend)
				;print,"bi,bii=",bi,bii
				frefines+=bii-bi
			
			;print,"[dim,aj,bi]=",[dim,aj,bi]
			;will want to try to make sure yp does not start on the previous step, and that our generated step will not be part of the next range

			;ybegtrust=ybeg[1 *nbeg/5: 4*nbeg/5] ; we're pretty sure the step will never be  in this region
			mbeg=nbeg
			mend=nend
			;yendtrust=yend[1 * nend/5: 4*nend/5] ; we're pretty sure the step will always be on in this region


			ybegtrust=ybeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
			yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region


			incfunctions=1
			decfunctions=1
			bii=bi
			d00=dim
			;print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
			bbrefines=brefines
			ffrefines=frefines
			while (Bmed[dim] gt mean(yendtrust) and incfunctions eq 1) or ( (Bmed[aj+nend-1] lt mean(ybegtrust)+1.1) and (decfunctions eq 1  )) do begin; and ( (Bmed[aj+nend-1] lt mean(ybegtrust)+1.1) and (decfunctions eq 1  )) do begin 
				;print,"(brefines,frefines)=",brefines,frefines
				;print,"(incfunctions,decfunctions)=",incfunctions,decfunctions
				;we will do these seperately within the loop, but at same time so that one negligably affects the other 

				ybegtrust=ybeg[2 * (nbeg-1)/5: (nbeg-1)/2] ; we're pretty sure the step will never be  in this region
			
				yendtrust=yend[8 * (nend-1)/10: 9 * (nend-1)/10] ; we're pretty sure the step will always be on in this region
				;jbegj=max([1,nend-20])+aj
				;jendj=min([N-1,nend+20+aj])
				if ((Bmed[dim] gt mean(yendtrust)) and (incfunctions eq 1)) then begin; this is a simple approximation, but checking if on previous step
					;print,'pushing forward the end of shock number i=',0,', from gim=',gim,', where a0=',a0,', and g0=',g0, 'and size(yp)=',size(yp,/n_el)
					yp=yp[1:*] ;shrinks our range by one
 					ybeg=ybeg[1:*]
					np--
					nbeg--
					dim++
					brefines++
					if ( brefines ge 2 *mbeg/5) or (dim +2*60 eq aj) then begin
						incfunctions=0
						dim=d00
						yp=ys[dim:bi]
						np=size(yp,/n_el)
						ybeg=ys[dim:aj] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nbeg=numel(ybeg)
						
					endif
				endif

				if  ((Bmed[aj+nend-1] le mean(ybegtrust)+1.1) and (decfunctions eq 1)) then begin
				;if ((median(ys[jbegj:jendj]) le mean(ybegtrust)+1.5) and (decfunctions eq 1)) then begin
					;print,"pulling back the end of shock number i=",i,", from gi=",gi
					np--
					yp=yp[0:np-1]

					nend--
					
					if nend gt 1 then yend=yend[0:nend-1]

					;gi--
					bi--
					frefines++
					if (nend le 4*60) or (np eq 40) or (frefines gt mend/2) or (aj+2*60 ge bi) then begin
						;print,"too far: aj,bi=",aj,bi
						decfunctions=0
						;gi=DD[i]
						;yp=ys[gim:gi]
						bi=bii
						yp=ys[dim:bi]
						np=size(yp,/n_el)
						;yend=ys[hj:gi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						yend=ys[aj:bi] ; elements before the first trigger. to zeroth order, this is (probably) before the step
						nend=mend

						frefines=ffrefines
						
					endif
				endif
;
				;if this ends up going to the point of overtaking our trusted ranges will restore to original and disable that 'refinement' 
				if ((incfunctions eq 0) and (decfunctions eq 0)) then break
			endwhile 
			;print,"=====end BOUNDARY SHRINKER========="		
end
