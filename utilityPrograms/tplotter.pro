pro tplotter,plt1,plt2,p1=p1

	get_data,plt1,data=dat1
	get_data,plt2,data=dat2
	x=dat1.y
	y=dat2.y
	if numel(x) lt numel(y) then x=interpol(x,dat1.x,dat2.x)
	if numel(x) gt numel(y) then y=interpol(y,dat2.x,dat1.x)
	p1=scatterplot(x,y,xtitle=dat1.ytitle,ytitle=dat2.ytitle,sym_size=.1)
end
