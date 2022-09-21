function Cal2Unix, month,day,year,hour=hour,minute=minute,second=second, stringDate=stringDate,stringTime=stringTime,delim=delim
	if not keyword_set(delim) then delim ='/'
	if not keyword_set(hour) then hour=0
	if not keyword_set(minute) then minute=0
	if not keyword_set(second) then second=0
     if keyword_set(stringDate) then begin
                  sarr=strsplit(stringDate,'-',/EXTRACT)
               return,round((Julday(sarr[1],sarr[2],sarr[0]) -JulDay(1,1,1970,0,0,0)) *(24*60*60.0D),/L64)
      endif
     if keyword_set(stringTime) then begin
		  sarr=strsplit(stringTime,delim,/EXTRACT)
                  arrdat=strsplit(sarr[0],'-',/EXTRACT)
                  arrtime=strsplit(sarr[1],':',/EXTRACT)
			;print,sarr
			;print,arrdat
			;print,arrtime
			jd=Julday(arrdat[1],arrdat[2],arrdat[0],arrtime[0],arrtime[1],arrtime[2])
			j0=JulDay(1,1,1970,0,0,0)
			;print,"(Julday(arrdat[1],arrdat[2],arrdat[0],arrtime[0],arrtime[1],arrtime[2])=",jd
			;print,j0
               return,round((Julday(arrdat[1],arrdat[2],arrdat[0],arrtime[0],arrtime[1],arrtime[2]) -JulDay(1,1,1970,0,0,1)) *(24*60*60.0D),/L64)
      endif
      t=round((Julday(month,day,year,hour,minute,second) -JulDay(1,1,1970,0,0,0)) *(24*60*60.0D),/L64)
      RETURN,t
end
