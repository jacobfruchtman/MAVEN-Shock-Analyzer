
function m2Ffind, bta,theta
	mm=numel(theta)
	nn=numel(bta)

	
	

	ZZZ=findgen(mm,nn)
		MfmsTest=(findgen(10000))/1000+1.0

	for i=0,mm-1 do begin
		;print,i," out of ",mm-1
		for j=0,nn-1 do begin	
				m2test=fltarr(10000)
				for k=0,10000-1 do begin
				;print,"i=",j
					m2test[k]=m2find(MfmsTest[k],bta[j],theta[i])
				endfor
				m2loc=-1
				m2closest=min(ABS(m2test),m2loc)
				ZZZ[i,j]=Mfmstest[m2loc]
		endfor
	endfor

	;Contour(ZZZ,theta,beta)


	return, ZZZ
end

