;+
;function onofflength, flg
;INPUT:
;	flg: an array containing values of only 1s and 0s. We assume no NaNs in the array
;OUTPUT: 
;	An equal size array such that where there was a block of 1s, instead has the width of the block. For 0s, minus the width of the block  
;-

function onofflength, flg,onsonly=onsonly

	N=numel(flg)
	z=nanarr(N)
	if not keyword_set(onsonly) then flg=2*flg-1
	lastflip=0
	wherefin=where(finite(flg) eq 1)
	firstfin=(wherefin)[0]
	finalfin=wherefin[-1]
	if firstfin eq -1 then return, z

	z[firstfin]=sign(flg[firstfin])
	currsign=z[firstfin]
	lastsign=z[firstfin]
	monos=1
;	print,"[cs,ls,m,lf,flg[0]]=",[currsign,lastsign,monos,lastflip,flg[0]]
	;print,"z:"
	;print,z
	for i=1+firstfin,finalfin do begin

		currsign=flg[i]
		;print,"[cs,ls,m,lf,flg[",strtrim(i,2),"]]=",[currsign,lastsign,monos,lastflip,flg[i]]
		if lastsign eq currsign and finite(flg[i]) eq 1 then begin
			monos++
			if i eq finalfin then for j=lastflip,i do z[j]=lastsign*monos				
			continue
		endif
		if finite(flg[i-1]) eq 1 then for j=lastflip,i-1 do z[j]=lastsign*monos
		;print,"z:"
		;print,z
		monos=1
		lastsign=flg[i]
		lastflip=i
		if finite(flg[i]) eq 1 then if i eq finalfin then for j=lastflip,i do z[j]=lastsign*monos
	endfor
	

	return,z


end
