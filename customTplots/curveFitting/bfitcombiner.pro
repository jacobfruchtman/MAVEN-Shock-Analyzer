pro bfitcombiner,fits,suffix=suffix,io=io
	newName='bf'
	notB=0
	suff=''
	if not keyword_set(io) then io = ''
	if not keyword_set(suffix) then begin
		fitstring='mvn_B_1sec_MAVEN_MSO_Mag'
		suffix=''
	endif else begin
		suff='_'+suffix
		if (suffix eq 'inbound') or (suffix eq 'outbound') then begin 
			fitstring='mvn_B_1sec_MAVEN_MSO_Mag' 
			
		endif else begin
			newName=suffix
			notB=1
			fitstring=''
		endelse
	endelse


	foreach el,fits do begin
		numstr=strtrim(string(el),2)
		fitstring+=' B_fitted'+numstr+suff

		if (io eq 'i') or (io eq 'inbound') or (io eq '_inbound') then fitstring+='_inbound'
		if (io eq 'o') or (io eq 'outbound') or (io eq '_outbound') then fitstring+='_outbound'
		newName+=numstr
		;print,fitstring
	endforeach
	;if not notB newName+=suffix
	if (io eq 'i') or (io eq 'inbound') or (io eq '_inbound') then newName+='i'
	if (io eq 'o') or (io eq 'outbound') or (io eq '_outbound') then fitstring+='o'
	store_data,newName,data=fitstring
	;TPLOT,newName,/add
end
