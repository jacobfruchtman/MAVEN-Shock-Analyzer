pro daySaver,cleanFile=cleanFile


get_data,"dateDataCurrent",data=dateData
startdate=dateData.startdate
numDates=dateData.numDates
dayLoaded=dateData.dayLoaded
date=(startdate.split('/'))[0];startdate.remove(-5)
ndays=dateData.ndays
fname=("dateData_"+date)[0]
store_data,fname,data=dateData
tplot_save,fname,filename="Documents/dateDatas/"+fname
tname=fname+".tplot"
if not keyword_set(cleanFile) then begin			
	print,fname
	openU,1,"dateFiles.txt",/append
	printf,1,fname+".tplot"
	close,1
endif else begin
	exsts=0
	openU,1,"dateFiles.txt"
	cols=1
	rows=2000    ;Default value for rows
	H=STRARR(cols,rows) ;A big array to hold the data
	S=STRARR(cols)      ;A small array to read a line
	ON_IOERROR,ers     ;Jump to statement ers when I/O error is detected
	n=0 ; Create a counter
	m=0
	WHILE n LT rows DO BEGIN
   		 READF,1,S    ;Read a line of data
    		if NOT S.startswith(';') then begin
			S=(S.split(';'))[0]
			if S eq tname then begin
			exsts=1
			break
			endif
		 H[*,m]=S     ;Store it in H
		m=m+1
    		endif
    		n=n+1        ;Increment the counter
	ENDWHILE          ;End of while loop
	ers: 	if cleanFile eq 1 then begin
	 	printf,1,fname+".tplot"
		CLOSE,1         ;Jump to this statement when an end of file is detected
		return
	endif else begin
		CLOSE,1         ;Jump to this statement when an end of file is detected
		H=H[*,0:m-1];n-1]
		IF not exsts then H=[H,tname]
		;H3=fltarr(N_ELEMENTS(H))
		;for i=0, numDates-1 do H3=julSplit((strsplit(H[i],";",/extract))[0])
		b = array[UNIQ(H, SORT(H))]
		H2=H[b]

		OPENW, outunit, tname, /GET_LUN, /MORE  
		foreach el,H2 do PRINTF,outunit,el     
		FREE_LUN, outunit 
	endelse

endelse

;tlimit

end
