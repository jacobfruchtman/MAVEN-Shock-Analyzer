function backendblock,dim,aj,bi,foreblock,Bmed,extr,rhod
	maxdisplace=20*60.
	maxdist=6*60.
	bhalf=mean([dim,aj,aj])+1
	newDim=dim

	N=size(Bmed,/n_el)
	;print,"====foreshockpasser====="
	;print,numel(extr)-bi
	;print,numel(extr)-bhalf
	extrp=extr[bhalf:bi]
	GGp=where(extrp ne 0 and rhod gt 0 ,gcount)
	;drops=where(extr[bhalf:bi] gt 0 and rhod gt 0 , dcount)+bhalf
	jumps=where(extr[bhalf:bi] gt 0 and rhod gt 0 , jcount)+bhalf
	if (gcount le 0) or (jcount le 0) then begin
		foreblock[bhalf:bi]+=1
		;print,"====end backendblock====="
		 return,bi
	endif	

	GG=GGp+bhalf
	exG=extrp[GGp]

	for el=0, jcount-1 do begin
		jj=jumps[el]

		backend=GG[-1]

		if total(Bmed[jj:backend] lt Bmed[jj]-1) le (backend-jj)/300.0 then begin
			;foreblock[bhalf:min([jj+2*60,N-1])]=1
			mn=min([jj+2*60,N-1])
			for k=bhalf,mn do if foreblock[k] eq 0 then foreblock[k]=1
			IF jj lt backend then shift=min([jj+60,(GG[el+1]+jj+jj)/3, N-1]) else shift=min([jj+60, N-1])
			return,shift
		endif

	endfor

	print,"somehow, we made it to the end"
	foreblock[bhalf:bi]+=1

	return,bi
end
