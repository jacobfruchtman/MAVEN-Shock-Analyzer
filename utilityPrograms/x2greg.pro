


function x2greg,t,strformat=strformat,noSecond=noSecond,noMinute=noMinute,noHour=noHour,timeDelimiter=timeDelimiter,dosubsec=dosubsec,directed=directed

	delt=0
	strfrm=0
	;havesub=0
	isDirected=0
	if keyword_set(dosubsec) then delt=-1	
	if keyword_set(strformat) then strfrm=1
	if keyword_set(noSecond) then delt=1
	if keyword_set(noMinute) then delt=2
	if keyword_set(noHour) then delt=3
	if keyword_set(directed) then isDirected=1
	if not keyword_set(timeDelimiter) then timeDelimiter='T'
	x2cal, t, month, day, year, hour, minute, sec0
	; julTime = t / (24*60*60.0D) + JulDay(1,1,1970,0,0,0)
   ;CalDat, julTime, month, day, year, hour, minute, sec
   ;Print, year, month, day, hour, minute, sec

	subsec=0
	
	ss=strtrim(sec0,2)
	;print,ss
	sarr=ss.split('\.')
	;print,sarr
	sec=floor(sec0);float(sarr[0])
	sarr1=sec0-sec
	subsec=round(10*sarr1);round(sarr1/(10.0^(-1+sarr1.strlen() )));round(float(sarr[1])/(10.0^(-1+sarr[1].strlen() )))
	if delt ne -1 then sec=sec+0.1*subsec	

	tar=[year, month, day, hour, minute, sec,subsec]
	tar=round(tar[0:5-delt])
	;RETURN,tar

	;==    BRING THE FOLLOWING INTO ANOTHER FUNCTION =====
	;==    or not: TIMESTAMP already exists

	if (not strfrm) and (not isDirected)  then return, tar


	strtime=""

	SWITCH delt of

		-1: BEGIN
			strtime=TIMESTAMP(year=year,month=month,day=day,hour=hour,minute=minute,second=sec+0.1*subsec)
		    BREAK
		END
		0: BEGIN
			strtime=TIMESTAMP(year=year,month=month,day=day,hour=hour,minute=minute,second=sec)
			BREAK
		END
		2: minute=0
		1: BEGIN
			sec=0
			strtime=TIMESTAMP(year=year,month=month,day=day,hour=hour,minute=minute,second=sec)
			BREAK
		END 
		3: 	strtime=TIMESTAMP(year=year,month=month,day=day)
	ENDSWITCH
	strtime=strtime.replace('.',',')
	if (NOT isDirected) and (timeDelimiter eq 'T') then return,strtime

	dandt=strsplit(strtime,'T',/EXTRACT)
	std=dandt[0]

	if (timeDelimiter ne 'T') then strtime = dandt.join(timeDelimiter)

	if isDirected then strtime=std+'/'+strtime

	return,strtime
		

	

	;star=strtrim(round(tar),2)

	;sngls=star.Matches('^[0-9]?$')
	;N=size(star,/n_el)                                          
	;for i=0,N-1 do if sngls[i] eq 1 then star[i]='0'+star[i] 

	;stard=star[0:2]
	;std=strjoin(stard,'-')

	;st=std
	;if delt lt 3 then begin

	;	starrt=star[3:*]
	;	stt=starrt.join(':')
	;	if havesub then stt+'.'+subsec+'Z'
		
		

	;endif
END
