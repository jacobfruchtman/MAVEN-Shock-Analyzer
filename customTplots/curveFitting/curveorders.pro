pro curveorders,rollup,rollsec,rollthird,rollnone,orderCustom=orderCustom,doRoll=doRoll,doSecond=doSecond,doThird=doThird,doDefault=doDefault
		
	if KEYWORD_SET(orderCustom) then begin
		if Total(orderCustom eq -1) gt 0 then rollnone=1
		if Total(orderCustom eq 1) gt 0 then rollup=1
		if Total(orderCustom eq 2) gt 0 then rollsec=1
		if Total(orderCustom eq 3) gt 0 then rollthird=1
	endif else begin
		orderCustom=list()
		if KEYWORD_SET(doDefault) THEN begin
			orderCustom=[-1]
			rollnone=1
			return
		
		endif
		if KEYWORD_SET(doRoll) THEN begin
			rollup=1
			orderCustom.add,1
		endif
		if KEYWORD_SET(doSecond) THEN begin
			rollsec=1
			orderCustom.add,2
			
		endif
		orderCustom.add,0
		if KEYWORD_SET(doThird) THEN begin
			orderCustom.add,3
			;orderCustom.add,4
			rollthird=1                
		endif
		if numel(orderCustom) gt 3 then begin
			orderCustom.add,4
			orderCustom.swap,-1,-2
		endif
		if numel(orderCustom) gt 4 then begin
			orderCustom.add,5
			orderCustom.swap,-1,-2
		endif
		orderCustom=orderCustom.toArray()
	endelse

end
