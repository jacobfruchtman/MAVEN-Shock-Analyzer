function footbottomer, dim,aj,bi,B, SNR,iSNR,N_e,Ndiff,Nfdiff,snrThresh=snrThresh,isnrThresh=isnrThresh

	;aboveiSNR=1
	belowSNR=1
	gcount=0
	thresh=snrThresh+3
	;print,dim,aj
	n_p=N_e-Ndiff
	pererror=abs(Ndiff)/n_p

	;LLp=where(finite(N_e[dim:aj]) eq 1 and abs(Ndiff[dim:aj]) lt 10 and abs(Nfdiff[dim:aj]) lt .6  ,lcount)+dim
	;if lcount le 0 then begin
		;LLp=where(finite(N_e[dim:bi]) eq 1 and abs(Ndiff[dim:bi]) lt 10 and abs(Nfdiff[dim:bi]) lt .6  ,lcount)+dim
		;if lcount eq 0 then begin
		;	return,-1 ;;;IF no upstream SWE data, no point in measuring the shock 
		;endif else begin


		;endelse
	;endif

	GGp=where(SNR[dim:aj] gt snrThresh and finite(N_e[dim:aj]) eq 1 and abs(Ndiff[dim:aj]) lt 10 and abs(Nfdiff[dim:aj]) lt .6  ,gcount)

	 
	thresh=snrThresh+3

	if gcount ne 0 then begin
		
			ft=GGp[-1]+dim
			if ft-dim le 2*60 then return,ft 
			for j=1,gcount-1 do begin
				if ft-dim le 2*60 or total(finite(N_e[dim:ft-60],/NAN) eq 0) eq 0 then return,ft 
				ft=GGp[-j]+dim
				bft=max([dim,ft-60])
				
				Br=B[bft:ft]
				mxB=max(Br)
				mdB=median(Br)
				mnB=min(Br)
				stB=stddev(Br)
				if  total(abs(Br-mdB) lt stB/2.) gt min([45,.8*(ft-bft)]) and ((mxB-mdB gt 4 and mdB-mnB lt .5  ) or  ((mxB-mdB)/stB gt 3 and mxB-mdB gt 2 and (mdB-mnB)/stB lt 1)) then continue
				return,ft
			endfor
			return, GGp[-1]+dim

	endif
	aaj=mean([aj,dim,dim])
	while total(finite(N_e[dim:aaj]) eq 1) eq 0 and aaj lt mean([aj,aj,dim]) do aaj++ 
	while (gcount eq 0) and (thresh ge 9) do begin

		thresh-=3
		;print,"thresh=",thresh
		;
		GGp=where(SNR[dim:aj] ge thresh and finite(N_e[dim:aj]) eq 1 and abs(Ndiff[dim:aj]) lt 10 and Nfdiff[dim:aj] lt .6,gcount)

		if gcount gt 0 then begin
			GGp+=dim+10
			aaj=GGp[-1]
			break
		endif
		;print,"[dim,aj]=",[dim,aj]

		;while belowSNR  and aaj gt dim+10 do begin
		;aboveiSNR=1
		;	belowSNR=1
		;	if SNR[aaj] gt thresh then belowSNR=0
		;if iSNR[aaj] lt isnrThresh then aboveiSNR=0


		;	IF belowSNR THEN aaj--
		;endwhile
		;if aaj lt dim+10 then continue
		;break
	endwhile

	return,aaj

end
