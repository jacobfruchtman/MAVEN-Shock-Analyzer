function plotcorrel2string,plt1,plt2,rank=rank,sub=sub
	r=plotcorrelate(plt1,plt2,rank=rank,sub=sub)
	return,string(r[0],format='%0.3g')
end
pro correlist

	paramlist1=list('$M_{fms}$','$\theta_{NB}$','$\beta$','proton $\beta$','electron $\beta$','Sol distance',$
	'standoff $\Delta$','SZA','alt','$\theta_{NV}$','$\mathcal{J}^{RH}$','$\Delta L_S$ from $L_S=270^\circ$','$M_{Alfven}$','$M_{ms1}$','$M_{fms}^{ion}$ (ion only)','$\mathcal{J}^{RH}$ (ion only)','$M_{fms}^{conic}$ ($\hat{n}=\hat{n}_{conic}$)*','$\theta_{BN_{conic}}$*','$M_A^{conic}$*','$\mathcal{J}^{conic RH}$*')
	paramtplot=list(8,10,12,13,14,35,32,'SZAd','alt',80,51,75,'Mflow',9,55,'Mfms_conic','thetaBconic','MachAlfvenConic','B2B1_RHconic');[8,10,12,13,14,35,32,98,80,51,135,75,84]
	linelist1=list()
	plt2=43
	
	for i=0,numel(paramtplot)-1 do begin
		param=paramlist1[i]
		plt1=paramtplot[i]
		r=plotcorrelate(plt1,plt2)
		rho=plotcorrelate(plt1,plt2,/rank)
		rau=plotcorrelate(plt1,plt2,sub='autumn')
		rwi=plotcorrelate(plt1,plt2,sub='winter')
		rsp=plotcorrelate(plt1,plt2,sub='spring')
		rsu=plotcorrelate(plt1,plt2,sub='summer')
		rperp=plotcorrelate(plt1,plt2,sub='Quasiperp')
		rpar=plotcorrelate(plt1,plt2,sub='Quasipar')
		sr=string(r,format='%0.3g')
		srho=string(rho[0],format='%0.3f')
		sau=string(rau,format='%0.3g')
		swi=string(rwi,format='%0.3g')
		ssp=string(rsp,format='%0.3g')
		ssu=string(rsu,format='%0.3g')
		sperp=string(rperp,format='%0.3g')
		spar=string(rpar,format='%0.3g')
		sss=param+'&'+sr+'&'+srho+'&'+sau+'&'+swi+'&'+ssp+'&'+ssu+'&'+sperp+'&'+spar+'\\'
		linelist1.add,sss
	endfor

	paramlist2=list('$M_{fms}/M_{crit}$','$M_{Alfven}$','$\beta$','proton $\beta$','electron $\beta$','Sol distance',$
	'standoff $\Delta$','SZA','alt','$\Delta L_S$ from $L_S=270^\circ$','$\Delta L_S$ from $L_S=285^\circ$','M_{ms1}','$M_{fms}^{ion}/M_{crit}^{ion}$ (ion only)','$M_{fms}^{conic}/M_{crit}^{conic}$ *','$M_{Alfven}^{conic}$')
	paramtplot=list(6,75,12,13,14,35,32,'SZAd','alt','solsticedist','dustdist','Mflow','FM_ion','FMconic','MachAlfvenConic')
		linelist2=list()
	plt2=45
	
	for i=0,numel(paramtplot)-1 do begin
		param=paramlist2[i]
		plt1=paramtplot[i]
		r=plotcorrelate(plt1,plt2)
		rho=plotcorrelate(plt1,plt2,/rank)
		rau=plotcorrelate(plt1,plt2,sub='autumn')
		rwi=plotcorrelate(plt1,plt2,sub='winter')
		rsp=plotcorrelate(plt1,plt2,sub='spring')
		rsu=plotcorrelate(plt1,plt2,sub='summer')
		rperp=plotcorrelate(plt1,plt2,sub='Quasiperp')
		rpar=plotcorrelate(plt1,plt2,sub='Quasipar')
		sr=string(r,format='%0.3g')
		srho=string(rho[0],format='%0.3f')
		sau=string(rau,format='%0.3g')
		swi=string(rwi,format='%0.3g')
		ssp=string(rsp,format='%0.3g')
		ssu=string(rsu,format='%0.3g')
		sperp=string(rperp,format='%0.3g')
		spar=string(rpar,format='%0.3g')
		sss=param+'&'+sr+'&'+srho+'&'+sau+'&'+swi+'&'+ssp+'&'+ssu+'&'+sperp+'&'+spar+'\\'
		linelist2.add,sss
	endfor


	paramlistx=list('$\hat{n}_{AVG}\cdot\langle\vec{V}^{ion}\rangle_U$','$M_{fms}$','$M_A$','$M_{ms1}$','\hspace{3mm}Peak dust season $L_S=285^\circ$','\hspace{3mm}Summer Solstice $L_S=270^\circ$','\hspace{3mm}Perihelion $L_S=250^\circ$', '\hspace{3mm}max r  $L_S=272^\circ$')
	paramplotx=list(79,8,75,84,'dustdist','solsticedist','periheliondist','standdegdist')
	paramlisty=list('SZA','SZA','$\Delta$','$\Delta$','$\Delta$','$\Delta$','$\Delta$','$\Delta$')
	paramploty=[98,98,32,32,32,32,32,32]
	linelist3=list()
	

		for i=0,numel(paramplotx)-1 do begin
		param1=paramlistx[i]
		param2=paramlisty[i]
		plt1=paramplotx[i]
		plt2=paramploty[i]
		r=plotcorrelate(plt1,plt2)
		rho=plotcorrelate(plt1,plt2,/rank)
		rau=plotcorrelate(plt1,plt2,sub='autumn')
		rwi=plotcorrelate(plt1,plt2,sub='winter')
		rsp=plotcorrelate(plt1,plt2,sub='spring')
		rsu=plotcorrelate(plt1,plt2,sub='summer')
		rperp=plotcorrelate(plt1,plt2,sub='Quasiperp')
		rpar=plotcorrelate(plt1,plt2,sub='Quasipar')
		sr=string(r,format='%0.3g')
		srho=string(rho,format='%0.3f')
		sau=string(rau,format='%0.3g')
		swi=string(rwi,format='%0.3g')
		ssp=string(rsp,format='%0.3g')
		ssu=string(rsu,format='%0.3g')
		sperp=string(rperp,format='%0.3g')
		spar=string(rpar,format='%0.3g')
		sss=param1+'&'+param2+'&'+sr+'&'+srho+'&'+sau+'&'+swi+'&'+ssp+'&'+ssu+'&'+sperp+'&'+spar+'\\'
		linelist3.add,sss
	endfor
	linelist3.add,correaggraline(67,'lat',param1='$\hat{n}_{AVG}\cdot\hat{n}_{conic}$',param2='lat',/short)	+'\\'
	linelist3.add,correaggraline(67,68,param1='$\hat{n}_{AVG}\cdot\hat{n}_{conic}$',param2='$\delta_{conic}$',/short)+'\\'
	linelist3.add,correaggraline(67,10,param1='$\hat{n}_{AVG}\cdot\hat{n}_{conic}$',param2='$\theta_{BN}$',/short)+'\\'
	linelist3.add,correaggraline(59,10,param1='$\% \mathsf{diff}(\mathcal{J}^{Fit},\mathcal{J}^{RH})$',param2='$\theta_{BN}$',/short)+'\\'
	linelist3.add,correaggraline(59,170,param1='$\% \mathsf{diff}(\mathcal{J}^{Fit},\mathcal{J}^{RH conic})$',param2='$\theta_{BN_{conic}}$',/short,/conic)+'\\'
	
	
	linelist3.add,correaggraline(79,'alt',param1='$\hat{n}_{AVG}\cdot\langle\vec{V}^{ion}\rangle_U$',param2='alt',/short)	+'\\'
	
	linelist3.add,correaggraline(75,'alt',param1='$M_{alfven}$',param2='alt',/short)	+'\\'
	linelist3.add,correaggraline(8,'alt',param1='$M_{fms}$',param2='alt',/short)	+'\\'
	linelist3.add,correaggraline(6,'alt',param1='$M_{fms}/M_{crit}$',param2='alt',/short)	+'\\'
	openU,1,'Documents/coefftablelines.txt',/append
	printf,1,'TABLE 1 LINES:'
	foreach el,linelist1 do printf,1,el
	printf,1,'======================='
	printf,1,'TABLE 2 LINES:'
	foreach el,linelist2 do printf,1,el
	printf,1,'======================='
	printf,1,'TABLE 3 LINES:'
	foreach el,linelist3 do printf,1,el
	close,1
end
