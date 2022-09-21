pro absPlot, plt,newT=newT,ytitle=ytitle
	
	get_data,plt,data=dat,limits=limits

	str_element,dat,'ytitle',var,success=s
	str_element,limits,'ytitle',var,success=t

	yt=''
	if keyword_set(ytitle) then begin
		if s or ~t then str_element,dat,'ytitle',ytitle,/add
		if t then str_element,limits,'ytitle',ytitle,/add
	endif else begin
		if s or ~t then dat.ytitle="abs("+ dat.YTITLE+")" 
		if t then limits.ytitle="abs("+ limits.YTITLE+")" 
		if ~t and ~s then begin
			if NOT KEYWORD_SET(newT) THEN newT=plt
			;options,plt,'ytitle',newT
			str_element,dat,'ytitle',"abs("+ limits.YTITLE+")",/add;;limits.ytitle="abs("+ limits.YTITLE+")"
			;str_element,limits,'ytitle',"abs("+ limits.YTITLE+")",/add;;limits.ytitle="abs("+ limits.YTITLE+")"
		endif
	endelse 
	dat.y=abs(dat.y)
	name=plt+"_abs"

	store_data,name,data=dat,limits=limits
END
