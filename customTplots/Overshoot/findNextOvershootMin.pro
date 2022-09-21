function findNextOvershootMin0, arr, loc,B
	N=numel(arr)

	B2=B
	B2[where(B2) eq 0]=9999
	print,"N=",N
	atPlateau=0
	startPlateau=loc+1
	;if 
	print,"loc=",loc
	if size(loc,/n_dim) ne 0 then loc=loc[0]

	if (N le 3) then return,min([loc+1,N-1])


	for i=loc+1, N-3 do begin
		
		if (arr[i] lt arr[i-1]) and (arr[i] lt arr[i+1]) then begin
			;print,"(arr[i] lt arr[i-1]) and (arr[i] lt arr[i+1])"
			help,i
			return, i
		endif
		if (arr[i] lt arr[i-1]) and (arr[i] gt arr[i+1]) then begin

			atPlateau=0
			continue
		endif
		if (arr[i] lt arr[i-1]) and (arr[i] eq arr[i+1]) then begin

			atPlateau=1
			startPlateau=i
			continue
		endif
		if atPlateau and (arr[i] lt arr[i+1]) then begin
			endplateau=i

			;minloc=-1

			return, where (B2[startPlateau:endplateau] eq min(B2[startPlateau:endplateau]))+startplateau;1

		endif
	endfor

	if not atPlateau then begin
		 return, N-1 
	endif else begin
		s=where(B2[startPlateau: N-1] eq min(B2[startPlateau: N-1]))+startplateau;1
		;print,"s=where(B[startPlateau: N-1] eq min(B))"
		help,s

		return, s
	endelse
end




