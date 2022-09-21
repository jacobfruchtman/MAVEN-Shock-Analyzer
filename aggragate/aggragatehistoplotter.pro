pro aggragatehistoplotter,xplt,subset=subset,btype=btype,ct=ct,ar=ar,dims=dims,magrange=magrange,format=format,binsize=binsize,his=his,density=density,$
dustdist=dustdist,solsdist=solsdist,peridist=peridist,qsplit=qsplit,qconic=qconic,tbin=tbin
	if not keyword_set(format) then format='.eps'
	;corrText="_WithoutPathologies"
	dire="Documents/Plots/CombinedPlots/"
	get_data,xplt,data=datx

	xvars=datx.y
	t=datx.x
	if not keyword_set(btype) then btype=0
	if not keyword_set(binsize) then binsize=datx.binsize[0]
	
	if not keyword_set(ct) then begin 
	ct=75
 LOADCT, ct, FILE='Documents/mycolors1.tbl'
    TVLCT, red, green, blue, /GET

    rwb = [[red],[green],[blue]]
	endif else rwb = ct

	X=datx.y
	tplot_element,8,'y',Mfms
	XT=datx.YN[0]
	

	xttl=datx.ytitle[0]


	xfnm=datx.fn[0]

	subfn=''
	subT=''
	ww=where(Mfms gt 0)
	X=X[ww]
	N=numel(X)
	subflag=fltarr(N)+1
	sub=findgen(N)

	terrs=''


	Title=subT+XT+" Histogram"
	if keyword_set(subset) then begin
		get_data,subset,data=datsub
		subflag=datsub.y
		sub=where(subflag eq 1)		
		subT=datsub.YN
		subfn=datsub.fn
		

		;foreach el, sub do begin
			;print,time_string(t[el])+", ["+xfnm+' , '+yfnm+']=',[X[el],Y[el]]

		;endforeach
	endif

	plotFName=subfn+xfnm+"_histogram"+format
	
		pdf = HISTOGRAM(X[sub], LOCATIONS=xbin,binsize=binsize)
		xx=xbin
		print,max(pdf)
		ytitle='counts'
		if keyword_Set(density) then begin
				ytitle='counts/N'
				pdf/=1.*numel(X[sub])
		endif
		p1=plot(xx,pdf,xtitle=xttl,ytitle=ytitle,title=Title,histogram=his,name='unbinned')
		xr=p1.xrange
		xrb=min(xr)
		xrt=max(xr)
		xmx=max(X[sub])
		if xplt eq 'tjul' then begin
			tj0=max([x2jul(time_double('2014-11-15')),xrb])
			tjf=max([min([x2jul(time_double('2019-11-16')),xrt]),max(xx)+binsize])
			p1.xrange=[tj0,tjf]
			p1.xtickunits="time"
		endif
		if datx.radian[0] eq 1 then begin
			if xmx lt !pi/2 then mxx=!pi/2 else $
			if xmx lt !pi 	then mxx=!pi else mxx=2*!pi
			p1.xrange=[0,mxx]
		endif
		if datx.degree[0] eq 1 then begin
			if xmx lt 90 then mxx=90 else $
			if xmx lt 180 	then mxx=180 else mxx=360
			p1.xrange=[0,mxx]
		endif
	if btype eq 1 then begin
		plotlist=list(p1)
			subfn=''
		seasonColors=list("green","red","orange","blue")
		seasonColors2=list("spring_green","crimson","gold","deep_sky_blue")
		seasonColors3=list("forest_green","firebrick","goldenrod","steel_blue")

		seasonNames0=list("Spring","Summer","Autumn","Winter")
		seasonNames=list("Spring ($180^\circ\leq Ls<270^\circ$)","Summer ($270^\circ \leq Ls<360^\circ$)","Autumn ($0^\circ \leq Ls<90^\circ$)","Winter ($90^\circ \leq Ls<180^\circ$)")
		
		;get_data,'season',data=datseason
		;season=datseason.y
		;season=season[ww]
		;springIndices=where(season eq 0)
		;summerIndices=where(season eq 1)
		;autumnIndices=where(season eq 2)				
		;winterIndices=where(season eq 3)

		;seasonsubIndices=list(intersect(springIndices,sub),intersect(summerIndices,sub),$	
					;intersect(autumnIndices,sub),intersect(winterIndices,sub))
				if keyword_set(solsdist) then begin
						get_data,"solsbins",data=datseason
						seasonColors=list("red","orange","green","blue")
						seasonColors2=list("crimson","gold","spring_green","deep_sky_blue")
						seasonColors3=list("firebrick","goldenrod","forest_green","steel_blue")
						;if domedian eq 0 then seasonColors2=seasonColors
						seasonNames0=list("soldist $<45^\circ$","$45^\circ\leq$ soldist <$90^\circ$","$90^\circ\leq$ soldist $<135^\circ$","$135^\circ\leq$ soldist $<180^\circ$")
						seasonNames=seasonNames0
						season=(datseason.y);[b]
						springIndices=where(season eq 0)
						summerIndices=where(season eq 1)
						autumnIndices=where(season eq 2)				
						winterIndices=where(season eq 3)
						seasonsubIndices=list(intersect(springIndices,sub),intersect(summerIndices,sub),$	
						intersect(autumnIndices,sub),intersect(winterIndices,sub))
						subfn+='solsquart_'
					endif else if keyword_set(dustdist) then begin
						get_data,"dustbins",data=datseason
						seasonColors=list("red","orange","green","blue")
						seasonColors2=list("crimson","gold","spring_green","deep_sky_blue")
						seasonColors3=list("firebrick","goldenrod","forest_green","steel_blue")
						;if domedian eq 0 then seasonColors2=seasonColors
						seasonNames0=list("dustdist $<45^\circ$","$45^\circ\leq$ dustdist $<90^\circ$","$90^\circ\leq$ dustdist $<135^\circ$","$135^\circ\leq$ dustdist $<180^\circ$")
						seasonNames=seasonNames0
						season=(datseason.y);[b]
						springIndices=where(season eq 0)
						summerIndices=where(season eq 1)
						autumnIndices=where(season eq 2)				
						winterIndices=where(season eq 3)
						seasonsubIndices=list(intersect(springIndices,sub),intersect(summerIndices,sub),$	
						intersect(autumnIndices,sub),intersect(winterIndices,sub))
						subfn+='dustquart_'
					endif else if keyword_set(tbin) then begin
						;if tbin gt 1 then begin
						if tbin le 1 then begin
						get_data,"time_quartiles",data=datseason
						seasonColors=list("red","orange","green","blue")
						seasonColors2=list("crimson","gold","spring_green","deep_sky_blue")
						seasonColors3=list("firebrick","goldenrod","forest_green","steel_blue")

						seasonNames0=list('t<2016-02-14','2016-02-14<=t<2017-05-16','2017-05-16 <=t< 2018-08-15','t>=2018-08-15')
						seasonNames=seasonNames0
						season=(datseason.y);[b]
						springIndices=where(season eq 1)
						summerIndices=where(season eq 2)
						autumnIndices=where(season eq 3)				
						winterIndices=where(season eq 4)
						seasonsubIndices=list(intersect(springIndices,sub),intersect(summerIndices,sub),$	
						intersect(autumnIndices,sub),intersect(winterIndices,sub))
						
						endif else begin
							tbin=round(tbin)
							tinterval=time_double('2019-11-16')-time_double('2014-11-15')
							tquant=floor( (t-time_double('2014-11-15'))*tbin/tinterval)
							nparts=tbin
							seasonsubIndices=list()
							seasonColors=list()
							seasonColors2=list()
							seasonNames0=list()
							
							seasonColors3=list()
							tdt0=time_double('2014-11-15')
							for lll=0.,tbin-1 do begin
								print,"lll=",lll
								w=where(tquant eq lll)
								tdt=tinterval/tbin+tdt0
								;print,'tdt0=',tdt0
								;print,'time_string(tdt0)=',time_string(tdt0)
								;print,'tdt=',tdt
								;print,'time_string(tdt)=',time_string(tdt)
								tstr=time_string(tdt0)+'<=t<'+time_string(tdt)
								tstr=tstr.replace('/','T')
								tdt=tdt0
								seasonsubIndices.add,intersect(sub, w)
								clrRAW=transpose(rwb[255.99*(lll*1.0/(tbin)+.5/tbin  ),*])
								print,"clrRAW=",clrRAW
								;mxclr=max(clrRAW)
								COLOR_CONVERT, clrRAW[0], clrRAW[1], clrRAW[2], hh, ss, vv, /RGB_HSV
								COLOR_CONVERT, hh, 1, vv, rsat, gsat, bsat, /HSV_RGB
								COLOR_CONVERT, hh, ss, (vv+1.)/2, rlght, glght, blght, /HSV_RGB
								COLOR_CONVERT, hh, 1, (vv)/2, rdrk, gdrk, bdrk, /HSV_RGB
								clrSAT=[rsat,gsat,bsat]
								print,"clrSAT=",clrRAW
								clrLGHT=[rlght,glght,blght];clrRAW*255.99/mxclr
								print,"clrLIGHT=",clrLGHT
								clrDRK=[rdrk,gdrk,bdrk]
								print,"clrDARK=",clrDRK
								seasonColors.add,clrSAT
								seasonColors2.add,clrLGHT
								seasonColors3.add,clrDRK
								seasonNames0.add,tstr
							endfor
							
							print,seasonColors2[0]
							help,seasonColors2
							help,seasonColors
							help,seasonNames0
							seasonNames=seasonNames0
							;ppp=scatterplot(X,Y,sym_color=seasonColors2[0])
							;return
						endelse
						;if domedian eq 0 then seasonColors2=seasonColors
						subT+='time binned '
						subfn+='timequartiles_'
					endif else if keyword_set(peridist) then begin
						get_data,"peribins",data=datseason
						seasonColors=list("red","orange","green","blue")
						seasonColors2=list("crimson","gold","spring_green","deep_sky_blue")
						seasonColors3=list("firebrick","goldenrod","forest_green","steel_blue")
						;if domedian eq 0 then seasonColors2=seasonColors
						seasonNames0=list("perihelion dist $<45^\circ$","$45^\circ\leq$ peridist $<90^\circ$","$90^\circ\leq$ peridist $<135^\circ$","$135^\circ\leq$ peridist $<180^\circ$")
						season=(datseason.y);[b]
						seasonNames=seasonNames0
						springIndices=where(season eq 0)
						summerIndices=where(season eq 1)
						autumnIndices=where(season eq 2)				
						winterIndices=where(season eq 3)
						seasonsubIndices=list(intersect(springIndices,sub),intersect(summerIndices,sub),$	
						intersect(autumnIndices,sub),intersect(winterIndices,sub))
						subfn+='periquart_'
					endif else if keyword_set(qsplit) then begin
						get_data,'Quasiperp',data=datQperp
						get_data,'Quasipar',data=datQpar
						QparIndices=where(datQpar.y eq 1)
						QprpIndices=where(datQperp.y eq 1)
						seasonsubIndices=list(intersect(QprpIndices,sub),intersect(QparIndices,sub))
						seasonColors=list('lime green' ,"purple")
						seasonColors2=list('lime',"magenta")
						seasonColors3=list('yellow green',"dark magenta")
						
						
						seasonNames0=list("Quasiperp","Quasiparallel")
						seasonNames=seasonNames0
						nparts=2
						subfn+='Qsplit_'
						subT+='Q split '
					endif else if keyword_set(qconic) then begin
						get_data,'thetaBconic',data=datTHC

						QprpIndices=where(datTHC.y ge 45,nqperp,complement=QparIndices)
						seasonsubIndices=list(intersect(QprpIndices,sub),intersect(QparIndices,sub))
						seasonColors=list('lime green' ,"purple")
						seasonColors2=list('lime',"magenta")
						seasonColors3=list('yellow green',"dark magenta")
						
						
						seasonNames0=list("Quasiperp","Quasiparallel")
						seasonNames=seasonNames0
						nparts=2
						subfn+='QConicsplit_'
						subT+='Conic Q split '
					endif else begin
						subfn+="seasonal_"
						get_data,'season',data=datseason
						season=(datseason.y);[b]
						springIndices=where(season eq 0)
						summerIndices=where(season eq 1)
						autumnIndices=where(season eq 2)				
						winterIndices=where(season eq 3)
						seasonsubIndices=list(intersect(springIndices,sub),intersect(summerIndices,sub),$	
						intersect(autumnIndices,sub),intersect(winterIndices,sub))
						
					endelse	
					plotFName=subfn+xfnm+"_histogram"+format
					;if keyword_set(err) then fname+='_with_error'
					plotlist=list()
					
		for KK=0,n_elements(seasonsubIndices)-1 do begin
			pdf = HISTOGRAM(X[seasonsubIndices[KK]], LOCATIONS=xbin,binsize=binsize)
			if keyword_Set(density) then begin
				;ytitle='counts/N'
				pdf/=1.*numel(X[seasonsubIndices[KK]])
				plotFName="seasonal_"+subfn+xfnm+"_histogram_Density"+format
			endif
			
			plotlist.add,plot(xbin,pdf,histogram=his,color=seasonColors[KK],name=seasonNames0[KK]+' counts',/over)
		endfor		
		ccf = LEGEND(target=plotlist, font_size=5,transparency=80,/relative,VERTICAL_ALIGNMENT='bottom',pos=best)
		ccf.save,dire+plotFNAME
		plotlist[0].close
		return
	endif
	p1.save,dire+plotFNAME
	p1.close	
	return
end
