function findNextOvershootMax, arr, B

	N=numel(arr)
	sp=0
	atP=1
	if arr[1] lt arr[0] then return, 0
	if arr[1] ne arr[0] then begin
		sp=1
		atP=0
	ENDIF
	for i=1, N-30 do begin;2 do begin
		if (arr[i] gt arr[i+1]) and (arr[i-1] gt arr[i]) then begin
			;print,"(arr[i] gt arr[i+1]) and (arr[i-1] gt arr[i])"
			help,i
			return, i
		endif
		if (arr[i] lt arr[i+1]) then begin
			atP=0
			sp=i+1
			continue
		endif

		if (arr[i] eq arr[i+1]) and (arr[i] gt arr[i-1]) then begin
			atP=1
			sp=i
		endif

		if (arr[i] gt arr[i+1]) and atP then begin
			;print,"(arr[i] gt arr[i+1]) and atP"
			s=where(B[sp:i] eq max(B[sp:i]))
			help,s
			return, s
		endif
	endfor
	s=uint(where(B eq max(B)))
	;print,"where(B eq max(B))"
	return,s[0]
end

