pro tplot_element,plt,  tagname, value,SUCCESS=SUCCESS,ADD_REPLACE=ADD_REPLACE,DELETE=DELETE,CLOSEST=CLOSEST
	typ=size(plt,/typ)
	if (typ ne 2) and (typ ne 7) and (typ ne 8) then begin
		print,"invalid plt input"
		return
	endif
	stor=0
	if keyword_set(ADD_REPLACE) or keyword_set(DELETE) then stor=1
	n=size(plt,/n_el)
	for i=0,n-1 do begin 
		el=plt[i]
		if (typ eq 2) or (typ eq 7) then begin
			get_data,el,data=data
			str_element,data, tagname, value,SUCCESS=SUCCESS,ADD_REPLACE=ADD_REPLACE,DELETE=DELETE,CLOSEST=CLOSEST
			if stor then store_data,el,data=data
		endif else if typ eq 8 then str_element,el, tagname, value,SUCCESS=SUCCESS,ADD_REPLACE=ADD_REPLACE,DELETE=DELETE,CLOSEST=CLOSEST
	endfor
end
