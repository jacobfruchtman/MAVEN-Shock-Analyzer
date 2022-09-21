pro pointstats1,load2=load2
biglistnum=""
		if keyword_set(load2) then biglistnum="2"

		manualList="Documents/savedPoints"+biglistnum+".txt"
		print,manualList
		read_strDATA2,shocklist,manualList,numLines=numLines2
	;	read_strDATA2,shocklist,"Documents/savedPoints_withoutPathologies.txt",numLines=numLines2
		shocklist2=strarr(numLines2)
		shockJulT=fltarr(numLines2)
		shockJulD=fltarr(numLines2)
		shockdates2=strarr(numLines2)
		dateOccurs=fltarr(numLines2)
		for i=0,numLines2-1 do begin
			var=(strsplit(shocklist[i],";",/extract))
			shocklist2[i]=var[0]
			shockJulT[i]=x2Jul(Cal2Unix(1,1,1,stringTime=var[0],delim="T"))
			shockJulD[i]=x2Jul(Cal2Unix(1,1,1,stringDate=var[0],delim="T"))
		endfor
		for i=0,numLines2-1 do begin
			dateOccurs[i] = total(shockJulD eq shockJulD[i])
		endfor

		minD=min(shockJulD)
		maxD=max(shockJulD+1)

		CalMonths=['2014-12-01','2015-01-01','2015-02-01','2015-03-01','2015-04-01','2015-05-01','2015-06-01','2015-07-01','2015-08-01','2015-09-01','2015-10-01','2015-11-01','2015-12-01',$
	'2016-01-01','2016-02-01','2016-03-01','2016-04-01','2016-05-01','2016-06-01','2016-07-01','2016-08-01','2016-09-01','2016-10-01','2016-11-01','2016-12-01',$
	'2017-01-01','2017-02-01','2017-03-01','2017-04-01','2017-05-01','2017-06-01','2017-07-01','2017-08-01','2017-09-01','2017-10-01','2017-11-01','2017-12-01',$
	'2018-01-01','2018-02-01','2018-03-01','2018-04-01','2018-05-01','2018-06-01','2018-07-01','2018-08-01','2018-09-01','2018-10-01','2018-11-01','2018-12-01',$	
	'2019-01-01','2019-02-01','2019-03-01','2019-04-01','2019-05-01','2019-06-01','2019-07-01','2019-08-01','2019-09-01','2019-10-01','2019-11-01','2019-12-01']
		numMon=numel(CalMonths)

		Julmonths=fltarr(numMon)


		for i=0, numMon-1 do julmonths[i]=timestamp2jul(CalMonths[i])



		p1=scatterplot(shockJulT,dateOccurs,XTICKUNITS="Time");,XTICKNAME=shockdates2)
		;alltimes=dindgen(maxD-minD)+minD
		;shocktimes=fltarr(maxD-minD)
		;shocktimes[shockJulT-minD]=1
		;for i=minD,maxD-1 do begin
			;j=i-minD
			;if 

		;endif
end
	
