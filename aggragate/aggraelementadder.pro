pro aggraelementadder,key,val,dosub=dosub,addondly=addonly

	for i=1,500 do begin
		ii=i
		get_data,ii,data=dat
		;help,dat
		if size(dat,/typ) eq 2 then break
		str_element,dat,'ytitle',success=s
		if ~s xor keyword_set(dosub) then continue
		;if numel(dat.y) eq numel(dat.x) then continue
		if keyword_set(addonly) then begin
			str_element,dat,key,success=s
			if s then continue
	
		endif
		str_element,data,key,val,/add
		store_data,ii,data=dat
	endfor
end
