pro errorsaver,x0,errmsg,spectime=spectime
	dt=x2Greg(x0,/strformat)
	openU,1,"Documents/ERRORMESSAGES.txt",/append
	if not keyword_set(spectime) then begin
		 printf,1,"during time session "+dt 
	endif else printf,1,"at location "+dt
	foreach el, errmsg do printf,1,el
	printf,1,systime()
	close,1

	;tplot_element,"dateDataCurrent","finished",0,/add
end

	
