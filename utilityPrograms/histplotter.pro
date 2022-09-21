pro histplotter,X,binsize=binsize,hist=hist,title=title,xtitle=xtitle,fname=fname
	if numel(X) eq 1 then begin
		tplot_element,X,'y',X
	endif
	pdf = HISTOGRAM(X, LOCATIONS=xbin,binsize=binsize)
	xx=xbin
	print,max(pdf)
	p1=plot(xx,pdf,xtitle=xtitle,ytitle='counts',title=Title,histogram=hist)
	if keyword_set(fname) then p1.save,fname
end
