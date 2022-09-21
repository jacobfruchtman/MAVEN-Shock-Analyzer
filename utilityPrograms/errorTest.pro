pro errorTest

	AA=[0,1,1,2,3,5,8,14,22]

	BB=AA*(-1)^AA

	N=size(AA,/n_el)
	CC=AA	
	DD=fltarr(N-1)	

	EE=fltarr(N)
	;x=findgen(N-1)
	;y=findgen(N)
	catch,error_status0
	if error_status0 ne 0 then begin
			PRINT, 'Error index: ', error_status0
			PRINT, 'Error message: ', !ERROR_STATE.MSG
		DD=fltarr(N)
		error_status0=0
		catch,/cancel
	endif

	x=DD[N-1]

	for i=2, N-1 do begin
		catch,error_status1
		if error_status1 ne 0 then begin
			PRINT, 'Error index: ', error_status1
			PRINT, 'Error message: ', !ERROR_STATE.name
			PRINT, 'Error message: ', !ERROR_STATE.block
			PRINT, 'Error message: ', !ERROR_STATE.code
			PRINT, 'Error message: ', !ERROR_STATE.MSG
			catch,/cancel
		endif


		if error_status1 eq 0 then begin
			CC=AA	
			CC[i]*=-1
			DD[i]=1*(total(CC[i-2:i]) eq 0)


			EE[i+1]=CC[i]
			print,i,DD[i]
		endif
	endfor

	print,DD

end
