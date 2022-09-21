function vecsmoother, y, d, wid=wid
	nend=size(y,/n_el)
	x=findgen(nend)
	yy=y
	z=yy
	;nk=0
	print,size(yy)
	;print,yy
	if NOT KEYWORD_SET(wid) THEN wid=d+1
	print,"wid=",wid
	if wid gt 0 then begin


		for i=d, wid-1 do begin
			;print,'i=',i
			for k=2,nend-1 do if((yy[k-1] le yy[k]) and (yy[k-1] le yy[k-2])) then yy[k-1]=(yy[k]+yy[k-2])/2 ; if yy[k-2]>=yy[k-1]<=yy[k] then yy[k-1]=mean of surrounding two
			;WE do this to remove bumps
			for k=i+1,nend-1 do begin
				st=k-i-1
			;	print,"k=",k
				xi=x[k-i-1];this is equal to x[0] then x[1] then... (for i=3 this is x[0] then x[1] then ...)
				xf=x[k];this is equal to x[i+1] then x[i+2] then...(for i=3 this is x[4] then x[5] then ...)
				yi=yy[k-i-1]
				yf=yy[k]
				ymid=yy[k-i:k-1]
				if (( total(yy[k-i:k-1] le yy[k]) ne 0 ) and (total(yy[k-i:k-1] le yy[k-i-1]) ne 0 ) ) then begin ; if the y values between st and k are less than yy[st] and yy[k]
				; that is, if a perturbation exists that drops an intermediate y value to below either the start or end value, then we will set the intermediate points
				;to be on a line connecting the start and end points. while this does introduce a bias to increase y values and preserve peaks, this is still acceptable because it does not kill
				;overshoots 	 
					; if 

		;		
					m=1.0*(yf-yi)/(xf-xi)
					b=yf-m*xf
					for j=st,k do begin
		;		
						yy[j]=1.0*(m*x[j]+b)
					endfor  
				endif
			endfor
		endfor
	endif else begin

		;if NOT KEYWORD_SET(wid) THEN wid=d
		nwid=-wid
		for i=d, nwid-1 do begin
			;print,"i=",i
			ni=nwid-i
			;print,'ni=wid-i=',ni
			for k=2,nend-2 do if((yy[k-1] le yy[k]) and (yy[k-1] le yy[k-2])) then yy[k-1]=(yy[k]+yy[k-2])/2
			for k=ni,nend-1 do begin
				st=k-ni-1

				xi=x[k-ni-1]
				xf=x[k]
				yi=yy[k-ni-1]
				yf=yy[k]
				;print,"k=",k,", m=",ni
				;print,"k-m=",k-ni,": k-1=",k-1
				ymid=yy[k-ni:k-1]
				if (( total(yy[k-ni:k-1] le yy[k]) ne 0 ) and (total(yy[k-ni:k-1] le yy[k-ni-1]) ne 0 ) ) then begin
					

		;		
					m=1.0*(yf-yi)/(xf-xi)
					b=yf-m*xf
					for j=st,k do begin
		;		
						yy[j]=1.0*(m*x[j]+b)
					endfor  
				endif
			endfor
		endfor

	endelse

	;print,"before:",z
	;print,"after:",yy
	return,yy
end

;			for k=2,nend-1 do if((yy[k-1] le yy[k]) and (yy[k-1] le yy[k-2])) then yy[k-1]=(yy[k]+yy[k-2])/2
