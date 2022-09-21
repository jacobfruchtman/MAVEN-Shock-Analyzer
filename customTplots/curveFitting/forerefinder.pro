pro forerefinder, dim,edim,aj,bi,fore,Bu,Bsmooth,updated=updated
	print,"~~~~forerefinder~~~~~"
	;if dim eq edim then return

	;foresp=fore[dim:bi]
;	print,"max(fore[dim:bi])=",max(fore[dim:bi])
 	if max(fore[dim:bi]) eq 1 then return

	Bsadj=Bsmooth-Bu


	
	;Bsp=Bsmooth[dim:bi]
	dhalf=mean([dim,aj])
	startfore=(where(fore[dhalf:bi] ne 0))[0]+dhalf
	endfore=(where(fore[dhalf:bi] ne 0))[-1]+dhalf
	lowfore=(where(fore[dhalf:bi] eq 1))[0]+dhalf
	threefore=(where(fore[dhalf:bi] eq 3.))[0]+dhalf
	;print,"numel(Bsmooth),dim,dhalf,startfore,threefore,lowfore,edim,aj,endfore,bi=",numel(Bsmooth),dim,dhalf,startfore,threefore,lowfore,edim,aj,endfore,bi
	;print,"total(Bsmooth[lowfore:endfore] - Bu lt 2.5)=",total(Bsmooth[lowfore:endfore] - Bu lt 2.5)
	;print,"(max(fore[dhalf:bi]) gt 1)=",(max(fore[dhalf:bi]) gt 1)
	;print,"min(Bsadj[lowfore:endfore],minloc),minloc+lowfore=",min(Bsadj[lowfore:endfore],minloc),minloc+lowfore
	while (total(Bsmooth[lowfore:endfore] - Bu lt 2.5) eq 0) and (max(fore[dhalf:bi]) gt 2) do begin
		print,"wrong location"
		for k=dhalf,bi do if fore[k] gt 0 then fore[k]-=1
		endfore=(where(fore[dhalf:bi] ne 0))[-1]+dhalf
		lowfore=(where(fore[dhalf:bi] eq 1))[0]+dhalf
		aj=endfore-11
		;edim=(where(fore[dhalf:bi] eq 1))[0]+dhalf
		updated=1
		print,"new location: numel(Bsmooth),dim,dhalf,startfore,lowfore,edim,aj,endfore,bi=",numel(Bsmooth),dim,dhalf,startfore,lowfore,edim,aj,endfore,bi
	endwhile
	if edim ne 0 then edim=lowfore
;	print,"updated=",updated

end
