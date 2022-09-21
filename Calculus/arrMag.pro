
function ArrMag_vecMag,ar,pseudoTensor=pseudoTensor

	mag=ar[*,0]
	dim=size(ar,/dim)
	;print,'dim='
	;print,dim
	dimdim=size(dim,/dim)
	;print,dimdim
	if(dimdim EQ 1) and (size(ar,/n_el) le 6) THEN BEGIN
		;N=2
		rnk=dim

		if keyword_set(pseudoTensor) and rnk eq 6 then rnk=4
		print,rnk
		CASE rnk of
			1: mag=abs(ar[*])
			2: mag=sqrt(TOTAL(ar[*]^2))
			3: mag=sqrt(TOTAL(ar[*]^2))
			4: mag=sqrt(TOTAL(ar[0:2]^2))
			6: mag=TOTAL(ar[0:2])/3
			ELSE: RETURN, "this is a bad plot"/0
		ENDCASE
	ENDIF ELSE BEGIN
		N=dim[0]
		if dimdim gt 1 then rnk=dim[1] else rnk =1
		if keyword_set(pseudoTensor) and rnk eq 6 then rnk=4
	;N=size(ar,/dim)[0]
	;rnk=size(ar,/dim)[1]
		print,rnk
		CASE rnk of
			1: mag=abs(ar);For i=0,N-1 do mag[i]=abs(ar[i,*])
			2: For i=0,N-1 do mag[i]=sqrt(TOTAL(ar[i,*]^2))
			3: For i=0,N-1 do mag[i]=sqrt(TOTAL(ar[i,*]^2))
			4: For i=0,N-1 do mag[i]=sqrt(TOTAL(ar[i,0:2]^2))
			6:For i=0,N-1 do mag[i]=TOTAL(ar[i,0:2])/3
			ELSE: RETURN, "this is a bad plot"/0
		ENDCASE
	ENDELSE
	return, mag
end

function ArrMag_vecCase,ar,outForm,newName,yt,pseudoTensor=pseudoTensor
				ys=ar
				mg=ArrMag_vecMag(ys,pseudoTensor=pseudoTensor)
				if (string(outForm) EQ string(-1)) then outForm=1

				if (string(outForm) EQ string(1)) OR (string(outForm) EQ 'array') THEN BEGIN
					return, mg 
				ENDIF ELSE IF (string(newName) ne string(-1)) AND (string(yt) ne string(-1)) AND ((string(outForm) EQ string(2)) OR (string(outForm) EQ 'tplot')) THEN BEGIN
					get_data,'mvn_swica_density',data=dat0,alim=limD
					store_data,newName,data={x:dat0.x,y:mg,ytitle:yt}
					return, 0
				ENDIF ELSE IF (string(yt) ne string(-1)) AND ((string(outForm) EQ string(3)) OR (string(outForm) EQ 'data')) THEN BEGIN
					get_data,'mvn_swica_density',data=dat0,alim=limD
					return, {x:dat0.x,y:mg,ytitle:yt}
				ENDIF ELSE BEGIN
					print, 'ArrMag_vecCase,'+string(outForm)
					print, "error: This is not a valid input"
					return, 0
				ENDELSE

end


function datMag,ar,outForm,newName,yt,pseudoTensor=pseudoTensor
				dat=ar
				ys=dat.y
				mg=ArrMag_vecMag(ys,pseudoTensor=pseudoTensor)
				if (string(outForm) EQ string(-1)) then outForm=3

				if (string(outForm) EQ string(1)) OR (string(outForm) EQ 'array') THEN BEGIN
					return, mg 
				ENDIF ELSE IF (string(newName) ne string(-1))  AND ((string(outForm) EQ string(2)) OR (string(outForm) EQ 'tplot')) THEN BEGIN
					if (string(yt) eq string(-1)) THEN yt=dat.YTITLE
					;get_data,'mvn_swica_density',data=dat0,alim=limD
					store_data,newName,data={x:dat.x,y:mg,ytitle:yt}
					return, 0
				ENDIF ELSE IF (string(outForm) EQ string(3)) OR (string(outForm) EQ 'data') THEN BEGIN
					if (string(yt) eq string(-1)) THEN yt=dat.YTITLE
					;get_data,'mvn_swica_density',data=dat0,alim=limD
					Return, {x:dat.x,y:mg,ytitle:yt}
				ENDIF ELSE BEGIN
					print, 'datMag,'+string(outForm)
					print, "error: This is not a valid input"
					return, 0
				ENDELSE
end

function arrMag,ar,outForm=outForm,newName=newName, yt=yt,pseudoTensor=pseudoTensor


	;outForms: Nx1 array=='arr' or 1, tplot=='tplot' or 2,data=='dat' or 3
	;default to array


	typarr=size(ar)
	print,'typarr[0]='+string(typarr[0])
	CASE typarr[0] of
		0: BEGIN
			get_data,ar,data=dat
			;print,dat.ytitle
			print,'size(dat.y)='
			print,size(dat.y)
			mg=ArrMag_vecMag(dat.y,pseudoTensor=pseudoTensor)
			if not keyword_set(outForm) then outForm=2
			if (string(outForm) EQ string(1)) OR (string(outForm) EQ 'array') THEN BEGIN
					RETURN, mg 
			ENDIF ELSE IF (string(outForm) EQ string(2)) OR (string(outForm) EQ 'tplot') THEN BEGIN
					if NOT KEYWORD_SET(newName) THEN newName=ar+'_Mag'
					print,newName
					if NOT KEYWORD_SET(yt) THEN yt=dat.YTITLE
					;get_data,'mvn_swica_density',data=dat0,alim=limD
					if newName EQ ar then return, 0
					store_data,newName,data={x:dat.x,y:mg,ytitle:yt}
					return,  0
				ENDIF ELSE IF ((string(outForm) EQ string(3)) OR (string(outForm) EQ 'data')) THEN BEGIN
					if NOT KEYWORD_SET(yt) THEN yt=dat.YTITLE
					;get_data,'mvn_swica_density',data=dat0,alim=limD
					return, {x:dat.x,y:mg,ytitle:yt}
				ENDIF ELSE BEGIN
					print,'typarr[0]='+string(typarr[0])
					print, "error: This is not a valid input"
					RETURN, 0
				ENDELSE
		END
		
		1: BEGIN
			if not keyword_set(outForm) then outForm=-1
			if not keyword_set(yt) then yt=-1
			if NOT KEYWORD_SET(newName) THEN newName=-1

			IF (size(ar,/type) EQ 8) AND Keyword_set(newName) THEN BEGIN

				
				return, datMag(ar,outForm,newName,yt,pseudoTensor=pseudoTensor)


				;dat=ar
				;ys=dat.y
				;mg=ArrMag_vecMag(ys)
				;if not keyword_set(outForm) then outForm=1

				;if (string(outForm) EQ string(1)) OR (string(outForm) EQ 'array') THEN BEGIN
				;	return, mg 
				;ENDIF ELSE IF KEYWORD_SET(newName)  AND ((string(outForm) EQ string(2)) OR (string(outForm) EQ 'tplot')) THEN BEGIN
				;	if NOT KEYWORD_SET(yt) THEN yt=dat.YTITLE
				;	;get_data,'mvn_swica_density',data=dat0,alim=limD
				;	store_data,newName,data={x:dat.x,y:mg,ytitle:yt}
				;	return, 0
				;ENDIF ELSE IF (string(outForm) EQ string(3)) OR (string(outForm) EQ 'data') THEN BEGIN
				;	if NOT KEYWORD_SET(yt) THEN yt=dat.YTITLE
				;	;get_data,'mvn_swica_density',data=dat0,alim=limD
				;	Return, {x:dat.x,y:mg,ytitle:yt}
				;ENDIF ELSE BEGIN
				;	print, "error: This is not a valid input"
				;	return, 0
				;ENDELSE
			ENDIF  ELSE IF total([2,3,4,5,12] EQ size(ar,/type)) eq 1 then begin
				return, ArrMag_vecCase(ar,outForm,newName,yt,pseudoTensor=pseudoTensor)
			endif ELSE BEGIN
				print,'typarr[0]='+string(typarr[0])
				print, "error: This is not a valid input"
				return, 0
			ENDELSE
		END
		2: BEGIN
				ys=ar
				mg=ArrMag_vecMag(ys,pseudoTensor=pseudoTensor)
				if not keyword_set(outForm) then outForm=1

				if (string(outForm) EQ string(1)) OR (string(outForm) EQ 'array') THEN BEGIN
					return, mg 
				ENDIF ELSE IF KEYWORD_SET(newName) AND KEYWORD_SET(yt) AND ((string(outForm) EQ string(2)) OR (string(outForm) EQ 'tplot')) THEN BEGIN
					get_data,'mvn_swica_density',data=dat0,alim=limD
					store_data,newName,data={x:dat0.x,y:mg,ytitle:yt}
					return, 0
				ENDIF ELSE IF KEYWORD_SET(yt) AND ((string(outForm) EQ string(3)) OR (string(outForm) EQ 'data')) THEN BEGIN
					get_data,'mvn_swica_density',data=dat0,alim=limD
					return, {x:dat0.x,y:mg,ytitle:yt}
				ENDIF ELSE BEGIN
					print, "error: This is not a valid input"
					return, 0
				ENDELSE
				
			END
		ENDCASE
end
