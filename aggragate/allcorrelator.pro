pro allcorrelator

	goodlist=list()
	corrs=list()
	for i=6, 112 do begin
		if total(i eq [7,9,17,28,31,34,47,48,71,77,78,88,89,102,103,104,105,106,107,108,109]) ne 0 then continue
		print,i
		for j=i+1,112 do begin
		
			if i eq j then continue
			if  total(j eq [7,9,17,28,31,34,47,48,71,77,78,88,89,102,103,104,105,106,107,108,109]) ne 0 then continue
			if total(i eq [38,39,85,86,87]) eq 1 and  total(j eq [38,85,39,86,87]) eq 1 then continue
			if total(i eq [36,37]) eq 1 and  total(j eq [36,37]) eq 1 then continue
			if total(i eq [43,44]) eq 1 and  total(j eq [43,44]) eq 1 then continue
			if total(i eq [20,69]) eq 1 and  total(j eq [20,69]) eq 1 then continue
			if total(i eq [74,69]) eq 1 and  total(j eq [74,69]) eq 1 then continue
			if total(i eq [70,75]) eq 1 and  total(j eq [70,75]) eq 1 then continue
			if total(i eq [45,46]) eq 1 and  total(j eq [45,46]) eq 1 then continue
			if total(i eq [95,96]) eq 1 and  total(j eq [95,96]) eq 1 then continue
			if total(i eq [98,99]) eq 1 and  total(j eq [98,99]) eq 1 then continue
			if total(i eq [76,79,80,81,17,16]) eq 1 and  total(j eq [76,79,80,81,17,16]) eq 1 then continue
			if total(i eq [12,13,14]) eq 1 and  total(j eq [12,13,14]) eq 1 then continue
			if total(i eq [6,8,15]) eq 1 and  total(j eq [6,8,15]) eq 1 then continue
			if total(i eq [100,101]) eq 1 and  total(j eq [100,101]) eq 1 then continue
			;print,j
			ii=i
			jj=j
			get_Data,ii,data=datx
			XX=datx.y
			get_data,jj,data=daty
			YY=daty.y
			rr=correlate(XX,YY)





			if abs(rr) ge 0.5 then begin
				goodlist.add,[datx.fn,daty.fn,strtrim(i,2),strtrim(j,2)]
				corrs.add,rr
			endif
			
		endfor
	endfor
	
	;b=reverse(sort(corrs))
	;corrs=corrs[b]
	;goodlist=goodlist[b]
	
	for i=0,numel(corrs)-1 do print, "r=",corrs[i],"at ",  goodlist[i]


end

	
