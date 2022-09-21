function findnextovershootmin, arr, loc,B,endunder,down,wid;,pt,endunder
	N=numel(arr)
	wh=1.5*wid
	B2=B
	B2[where(B2) eq 0]=9999
	print,"N=",N
	atPlateau=0
	atPlateau2=0
	startPlateau=loc+1
	startPlateau2=loc+1
	atbottom=0
	minloc=loc+1
	guessplat=-1
	;if 
	print,"loc=",loc
	if size(loc,/n_dim) ne 0 then loc=loc[0]
	;np=floor(N/pt)

	endplateau=N-1
	endunder=N-1
	if (N le 3) then return,min([loc+1,N-1])
	if (N le 31) then return,min([loc+1,N-29])

	for i=loc+1, N-31 do begin
		if max(B[0:i]) lt down  then continue
		if (arr[i] lt arr[i-1]) and (arr[i] lt arr[i+1]) and ~atbottom then begin
			;print,"(arr[i] lt arr[i-1]) and (arr[i] lt arr[i+1])"
			;help,i
			minloc=i
			atbottom=1
			endplateau=i
			;return, i
		endif
		if (arr[i] lt arr[i-1]) and (arr[i] gt arr[i+1]) and ~atbottom then begin

			atPlateau=0
			continue
		endif
		if (arr[i] lt arr[i-1]) and (arr[i] eq arr[i+1]) and ~atbottom then begin

			atPlateau=1
			startPlateau=i
			guessplat=i
			continue
		endif
		if atPlateau and (arr[i] lt arr[i+1]) and ~atbottom then begin
			endplateau=i
			atbottom=1
			;minloc=-1
			;Bmean=mean([B2[startPlateau],B2[endplateau]])
			;endunder=(where( abs(B2[startPlateau:endplateau]-Bmean) eq min(abs(B2[startPlateau:endplateau]-Bmean)))[0]+startplateau
			minloc= where (B2[startPlateau:endplateau] eq min(B2[startPlateau:endplateau]))+startplateau
			;return,minloc;1

		endif
		if atbottom and  (arr[i] ge arr[i+1]) then begin
			Bmean=mean([B2[minloc],B2[i]])
			endunder=(where( abs(B2[minloc:i]-Bmean) eq min(abs(B2[minloc:i]-Bmean))))[0]+minloc
			return,minloc
		endif
		
	endfor
	if atbottom then begin
		endunder=endplateau+2
		return,minloc
	endif
	if not atPlateau then begin
		endunder=N-30
		 return, N-31
	endif else begin
		s=where(B2[startPlateau: N-31] eq min(B2[startPlateau: N-31]))+startplateau;1
		endunder=s+1
		;print,"s=where(B[startPlateau: N-1] eq min(B))"
		;help,s

		return, s
	endelse
end




