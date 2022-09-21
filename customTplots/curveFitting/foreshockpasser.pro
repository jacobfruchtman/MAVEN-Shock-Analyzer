function foreshockpasser,dim,aj,bi,extr,forecountdown,rhod,B2 , v5, newDim=newDim
	maxdisplace=20*60.
	maxdist=6*60.
	bhalf=mean([dim,aj,aj])+1
	newDim=dim

	N=numel(B2)
	print,"====foreshockpasser====="
	;;print,numel(extr)-bi
	;;print,numel(extr)-bhalf
	extrp=extr[bhalf:bi]
	GGp=where(extrp ne 0,gcount)

	if gcount le 0 then begin
		for cnt=bhalf,bi do forecountdown[cnt]+=1
		;;print,"====end foreshockpasser====="

		 return, aj
	endif	

	GG=GGp+bhalf
	exG=extrp[GGp]
	aj0=aj
	el0=-1
	for el=0, gcount-1 do begin
		if exG[el] lt 0 then continue
		aj0=GG[el]
		el0=el
		;;print,"first jump, bhalf,aj0+10=",bhalf,aj0+10
		for cnt=bhalf,aj0+10 do forecountdown[cnt]+=1
		break
	endfor
	if aj eq aj0 then begin
		;;print,"no jumps"
		;;print,"bhalf,aj0+10=",bhalf,aj0+10
		for cnt=bhalf,bi do forecountdown[cnt]+=1
		;forecountdown[bhalf:bi]+=1
		;;print,"====end foreshockpasser====="
		return,aj
	endif
	aj00=aj0
	aj0n=aj0
	naj0n=dim

	if gcount eq 1 then begin
		;print,"one extrema"
		shiftend=min([aj0+5*60,bi, N-1])
		;;print,aj0+11,shiftend,bi
		if bi gt aj0+11 then shiftend=min([aj0+5*60,bi, N-1]) else shiftend=min([aj0+60,N-1])
		for cnt=aj0+11,shiftend do forecountdown[cnt]+=1
	endif
	inout=0
	numfores=0
	lastoff=dim
	if gcount gt 1 then begin
		el22=el0
		for el2=el0+1,gcount-1 do begin


			if exG[el2] lt 0 then break


			if GG[el2]-aj00 gt maxdisplace then break


			if rhod[el2] gt 0.0 then begin
				forecountdown[aj0+11:GG[el2]+10]+=1
				;;print,""
				aj0=GG[el2]
				el22=el2
				;print,"first jump continues to GG[el2]=",GG[el2]
			endif
		endfor
		;print,"bhalf,aj0+10=",bhalf,aj0+10
		el2u=el22
		for el2=el22+1,gcount-1 do begin

			r=el2-el22
	
			sn=(-1)^r
			tsn=sign(exG[el2])

			if sn ne tsn then break
			if naj0n-aj0 gt maxdist then break
			if GG[el2]-aj00 gt maxdisplace then break
			inout= 1*(tsn eq 1)		
			if tsn eq -1 then begin
				naj0n=GG[el2]
				inout=0
			endif
			if tsn eq 1 then begin 
				lower=min([max([naj0n-10,0]),aj0+11])
				higher=max([max([naj0n-10,0]),aj0+11])
				if total(B2[naj0n:bi] lt B2[aj0]-1) le (bi-naj0n)/300.0 then begin
					;print,"not a real foreshock at ",GG[el2]
					;lower=min([max([naj0n-10,0]),aj0+11])
					for cnt=(naj0n+aj0+aj0)/3, bi do forecountdown[cnt]=0

					break
			
				endif

				inout=1

				lower=min([max([naj0n-10,0]),aj0+11])
				higher=max([max([naj0n-10,0]),aj0+11])
				if naj0n ne dim then begin
					;lastoff=naj0n
					for cnt=lower,higher do forecountdown[cnt]+=1
				endif
				;print,"jump #",r," at GG[el2]=",GG[el2]
				aj0=GG[el2]

				for cnt=bhalf,aj0+10 do forecountdown[cnt]+=1
			endif
			el2u=el2
		endfor

		;q=aj0
		;IF aj0 lt GG[-1] then begin
			;aj2=GG[where(GG eq aj0)+1]
			;q=(aj2+aj2+aj2+aj0)/4
		;;print,GG[-1],GG[el2],aj0,"(GG[el2]+aj0+aj0)/3=",q
			;shiftend=min([aj0+5*60,q, bi,N-1]) 

		;endif else shiftend=min([aj0+5*60, bi,N-1])
			;;print,shiftend
		shiftend=min([aj0+5*60, bi,N-1])
		;print,"aj0,aj0+5*60,shiftend,bi=",aj0,aj0+5*60,shiftend,bi
		for cnt=aj0+11, shiftend do forecountdown[cnt]+=1
		numfores=forecountdown[aj00]-1
		lasto=(where(forecountdown[bhalf:aj0+10] eq 1))[0]+bhalf
		if lasto ne bhalf then begin


			lmin=max([bhalf,aj00-60])
			;print,bhalf,aj00-60,lmin
			forecountdown[bhalf:lmin]+=1
			 newDim=lasto
		endif
		;if (lastoff-10 gt dim) then newDim=lastoff; and (lastoff lt aj0) then newDim=lastoff
	;print,aj0,forecountdown[shiftend],shiftend-aj0
	endif

	print,"====end foreshockpasser====="
	return, aj0
end
