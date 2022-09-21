PRO read_strDATA2, H, filename,ROWS=rows,numLines=numLines
OPENR,1,filename
;IF N_ELEMENTS(cols) LE 0 THEN cols=1 ;Default value for cols
cols=1
IF N_ELEMENTS(rows) LE 0 THEN rows=500000.    ;Default value for rows
H=STRARR(cols,rows) ;A big array to hold the data
S=STRARR(cols)      ;A small array to read a line
ON_IOERROR,ers     ;Jump to statement ers when I/O error is detected
n=0 ; Create a counter
m=0
WHILE n LT rows DO BEGIN
    READF,1,S    ;Read a line of data
    if NOT S.startswith(';') then begin
		;S=(S.split(';'))[0]
		 H[*,m]=S     ;Store it in H
		m=m+1
    endif
    n=n+1        ;Increment the counter
ENDWHILE          ;End of while loop
ers: CLOSE,1         ;Jump to this statement when an end of file is detected
H=H[*,0:m-1];n-1]
numLines=m
END
