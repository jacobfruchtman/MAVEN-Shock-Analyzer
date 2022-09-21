function FalseShockSkipper,aj,bi,Bmed,Bu,minWidth=minWidth

	;print,"Bu=",Bu
	;print,"aj=",aj
	if not keyword_set(minWidth) then minWidth=120
	ts=findgen(bi-aj)+aj
	thirdpoint=(aj+bi)/2.0;(aj+aj+bi)/3
	print,"thirdpoint=",thirdpoint
	BELW = where((Bmed[aj:bi] lt 1.4+Bu) and ts lt thirdpoint,bcount)+aj

	;print,"bcount=",bcount
	if bcount gt 0 then begin
		while((BELW[bcount-1] ge thirdpoint-1)) and (bcount gt 1) do begin
			;print,"ends in hole"
			BELW=BELW[0:bcount-2]
			bcount--
			thirdpoint--
		endwhile

	endif
	if bcount gt 0 then begin
		;z=BELW*0.0
		;for i=0, bcount-2 do z[i]=BELW[i+1]-BELW[i]
		;WHERECONT=where(z lt 3,wcount,complement=nW,ncomplement=nwcount) ;if it is just a brief jump above the upstream, we'll still include it
		;this is the list of all addresses where B<Bu+1 where the this is true the next address (or after two addresses)
		;print,"wcount=",bcount
		;;if wcount gt 0 then begin
			;TBELW=BELW[WHERECONT]
			;print,"BELW=",BELW
			aaj=aj
			continuous=0
			startCount=-1
			numCount=0
			for el=1,bcount-1 do begin
				dif=(BELW[el] -BELW[el-1] lt 10) 
				;print,"[el,BELW[el],dif,continuous]=",[el,BELW[el],dif,continuous]
				if not dif and not continuous then continue

				if not dif and continuous then begin
					if numCount ge minWidth then aaj=BELW[el]
					numCount=0
					continuous=0
					continue
				endif

				if dif then begin
					if not continuous then begin
						startCount=el
						continuous=1
					endif
					numCount++
					if numCount ge minWidth then aaj=BELW[el];aj+el
				endif

			endfor
			print,aaj-aj
			print,"aj=",aj
			aj=aaj
			print,"aj=",aj
		;endif

	endif


	return, aj
end
