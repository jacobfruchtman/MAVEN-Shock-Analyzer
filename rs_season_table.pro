pro rs_season_table

	plt=43

	tplot_element,plt,'y',J
	tplot_element,8,'y',Mfms

	get_data,'season',data=datseason
	b=sort(J)
	J=J[b]
	ww=where(Mfms[b] gt 4)
	J2=J[ww]

	season=datseason.y
	season=season[b]
	season2=season[ww]
					springIndices=where(season eq 0,nsp)
					summerIndices=where(season eq 1,nsu)
					autumnIndices=where(season eq 2,nau)				
					winterIndices=where(season eq 3,nwi)
					seasonsubIndices=list(springIndices,summerIndices,$	
						autumnIndices,winterIndices)
						springIndices2=where(season2 eq 0,nsp)
					springsummerIndices2=where(season2 le 1,nsu,complement=autumnwinterIndices2)
					summerIndices2=where(season2 eq 1,nsu)
					autumnIndices2=where(season2 eq 2,nau)				
					winterIndices2=where(season2 eq 3,nwi)
					seasonsubIndices=list(springIndices,summerIndices,$	
						autumnIndices,winterIndices)
Jsp=J[springIndices]
Jsu=J[summerIndices]
Jau=J[autumnIndices]
Jwi=J[winterIndices]
Jsp2=J2[springIndices2]
Jsu2=J2[summerIndices2]
Jau2=J2[autumnIndices2]
Jwi2=J2[winterIndices2]			
Jauwi2=J2[autumnwinterIndices2]			
Jspsu2=J2[springsummerIndices2]	
Jauwi=J[autumnwinterIndices2]			
Jspsu=J[springsummerIndices2]			

	setlst=list(Jsu,Jau,Jwi,Jsp,Jauwi,Jspsu)
	setlst2=list(Jsu2,Jau2,Jwi2,Jsp2,Jauwi2,Jspsu2)
	strlst=list('Season 1 & Season 2 & $U_1$ & $U_2$ & z & p \\','\hline')
	setnames=list('Summer','Autumn','Winter','Spring','Au and Wi','Sp and Su')
	
	for i=0,numel(setlst)-1 do begin
		X1=setlst[i]
		sn1=setnames[i]
		for k=i+1,numel(setlst)-1 do begin
			;if ((i le 1) and k eq 5) or ( (i eq 2 or i eq 3) and k eq 4) then continue
			X2=setlst[k]
			sn2=setnames[k]
			rs=rs_test(X1,X2,UX=U1,UY=U2)
			u1s=string(u1,format='%0.3f')
			u2s=string(u2,format='%0.3f')
			zs=string(rs[0],format='%0.4g')
			ps=string(rs[1],format='%0.7g')
			strlst.add,sn1+'&'+sn2+'&'+u1s+'&'+u2s+'&'+zs+'&'+ps+'\\'
		endfor
	endfor
	strlst.add,'\hline'
	strlst.add,'For &$M_{fms}>4$: & & & & \\'
		for i=0,numel(setlst)-1 do begin
		X1=setlst2[i]
		sn1=setnames[i]
		for k=i+1,numel(setlst)-1 do begin
			;if ((i le 1) and k eq 5) or ( (i eq 2 or i eq 3) and k eq 4) then continue
			X2=setlst2[k]
			sn2=setnames[k]
			rs=rs_test(X1,X2,UX=U1,UY=U2)
			u1s=string(u1,format='%0.3f')
			u2s=string(u2,format='%0.3f')
			zs=string(rs[0],format='%0.4g')
			ps=string(rs[1],format='%0.7g')
			strlst.add,sn1+'&'+sn2+'&'+u1s+'&'+u2s+'&'+zs+'&'+ps+'\\'
		endfor
	endfor
	strlst.add,'\hline'
	strlst.add,'======================'
	strlst.add,''
	strlst.add,''
	strlst.add,''
	strlst.add,''
	strlst.add,''
	strlst.add,''
	strlst.add,''
	strlst.add,'======================'
	plt=45

	tplot_element,plt,'y',J
	tplot_element,8,'y',Mfms

	get_data,'season',data=datseason
	b=sort(J)
	J=J[b]
	ww=where(Mfms[b] gt 4)
	J2=J[ww]

	season=datseason.y
	season=season[b]
	season2=season[ww]
					springIndices=where(season eq 0,nsp)
					summerIndices=where(season eq 1,nsu)
					autumnIndices=where(season eq 2,nau)				
					winterIndices=where(season eq 3,nwi)
					seasonsubIndices=list(springIndices,summerIndices,$	
						autumnIndices,winterIndices)
						springIndices2=where(season2 eq 0,nsp)
					springsummerIndices2=where(season2 le 1,nsu,complement=autumnwinterIndices2)
					summerIndices2=where(season2 eq 1,nsu)
					autumnIndices2=where(season2 eq 2,nau)				
					winterIndices2=where(season2 eq 3,nwi)
					seasonsubIndices=list(springIndices,summerIndices,$	
						autumnIndices,winterIndices)
Jsp=J[springIndices]
Jsu=J[summerIndices]
Jau=J[autumnIndices]
Jwi=J[winterIndices]
Jsp2=J2[springIndices2]
Jsu2=J2[summerIndices2]
Jau2=J2[autumnIndices2]
Jwi2=J2[winterIndices2]			
Jauwi2=J2[autumnwinterIndices2]			
Jspsu2=J2[springsummerIndices2]	
Jauwi=J[autumnwinterIndices2]			
Jspsu=J[springsummerIndices2]			

	setlst=list(Jsu,Jau,Jwi,Jsp,Jauwi,Jspsu)
	setlst2=list(Jsu2,Jau2,Jwi2,Jsp2,Jauwi2,Jspsu2)
	strlst.add,'Season 1 & Season 2 & $U_1$ & $U_2$ & z & p \\'
	strlst.add,'\hline'
	setnames=list('Summer','Autumn','Winter','Spring','Au and Wi','Sp and Su')
	
	for i=0,numel(setlst)-1 do begin
		X1=setlst[i]
		sn1=setnames[i]
		for k=i+1,numel(setlst)-1 do begin
			;if ((i le 1) and k eq 5) or ( (i eq 2 or i eq 3) and k eq 4) then continue
			X2=setlst[k]
			sn2=setnames[k]
			rs=rs_test(X1,X2,UX=U1,UY=U2)
			u1s=string(u1,format='%0.3f')
			u2s=string(u2,format='%0.3f')
			zs=string(rs[0],format='%0.4g')
			ps=string(rs[1],format='%0.7g')
			strlst.add,sn1+'&'+sn2+'&'+u1s+'&'+u2s+'&'+zs+'&'+ps+'\\'
		endfor
	endfor
	strlst.add,'\hline'
	strlst.add,'For &$M_{fms}>4$: & & & & \\'
		for i=0,numel(setlst)-1 do begin
		X1=setlst2[i]
		sn1=setnames[i]
		for k=i+1,numel(setlst)-1 do begin
			;if ((i le 1) and k eq 5) or ( (i eq 2 or i eq 3) and k eq 4) then continue
			X2=setlst2[k]
			sn2=setnames[k]
			rs=rs_test(X1,X2,UX=U1,UY=U2)
			u1s=string(u1,format='%0.3f')
			u2s=string(u2,format='%0.3f')
			zs=string(rs[0],format='%0.4g')
			ps=string(rs[1],format='%0.7g')
			strlst.add,sn1+'&'+sn2+'&'+u1s+'&'+u2s+'&'+zs+'&'+ps+'\\'
		endfor
	endfor
	strlst.add,'\hline'

	openw,1,'Documents/rstable.txt'
	foreach el, strlst do printf,1,el
	close,1
end
