pro zerothtest
;catch,error_status
	
	get_data,"shocks",data=datShks

	if total(datShks.y) eq 0 then return
	get_data,'shock0_interpolated',data=datS0
	get_data,'shock0_normals',data=datS0n

	S0=datS0.y
	S0n=datS0n.y	

	R_m = 3389.50D  ; +/- 0.2
	get_data,"mvn_B_1sec_Mag",data=datB

	
	get_data,'shock0rho_interpolated',data=datS0rho
	get_data,'shock0x_interpolated',data=datS0x
	 x0  = 0.600*R_m
 	ecc = 1.026
  	L   = 2.081*R_m

	s0rho=datS0rho.y
	s0x=datS0x.y
	xs=datB.x
	s0phi=xs*0.0

	anyBackwards=0
	N=size(s0x,/n_el)
	sx0=s0x-x0
	srr=SQRT(sx0^2+s0rho^2)
	srt=Sqrt(s0rho^2+(sx0+ecc*srr)^2)
	xx=datS0rho.x
	catch,error_status
if error_status ne 0 then begin
	errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON ZerothTest,pos"]
	;print, 'Error index: ', error_status
			;print, 'Error message: ', !ERROR_STATE.MSG
			;;print,"FAILED ON "+nm
			errorsaver,xs[0],errmsg
			store_data,"shock0Acc",data={x:post,y:0,ytitle:"shockConicNormal !C dot ShockNormal"}
			;print,"setting to null result"
			wdelete
			return
endif

	snxx=(ecc+sx0/srr)*srr/srt   ;<---THIS IS THE TANGENT VECTOR, NOT THE NORMAL VECTOR. NEED TO FIX IT
	snrr=s0rho/srt
	SN=[[snrr],[fltarr(N)],[snxx]]

	;interpolator,"MAVEN_POS_(MARS-MSO)",newName="POS_interpolated_(MARS_MSO)"
	;plotToCyl,"POS_interpolated_(MARS_MSO)",newName="POS_MSO_CYL"

	get_data,'POS_interpolated_(MARS_MSO)',data=dpos
	;help,dpos
	if size(dpos,/typ) eq 2 then return
	
		
		xShock=xs[0]
		xsJ=x2Greg(xShock,/strformat)
		shockDate=(xsJ.split('T'))[0]
		YEAR=(shockDate.split('-'))[0]
		;print,shockDate
		dir='Documents/Plots/'+YEAR+'/'+shockDate+'/'
	


	post=dpos.x
	pos=dpos.y
	oposx=pos[*,0]
	oposy=pos[*,1]
	oposz=pos[*,2]



if error_status ne 0 then begin
	errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON ZerothTest,datN"]
	;print, 'Error index: ', error_status
			;print, 'Error message: ', !ERROR_STATE.MSG
			;;print,"FAILED ON "+nm
			errorsaver,xs[0],errmsg
			store_data,"shock0Acc",data={x:post,y:0,ytitle:"shockConicNormal !C dot ShockNormal"}
			;print,"setting to null result"
			wdelete
			return
endif
	
	dists=fltarr(N)
	accs=fltarr(N)
	;Pphi=atan(oposz,oposy)
	;Prho=SQRT(oposz^2+oposy^2)
	;Pxx=oposx

	R_mars=3389.5

	;Prho=Pcyl[*,0]
	;Pphi=Pcyl[*,1]
	;Pxx=Pcyl[*,2]

	Pphi=atan(oposz,oposy)
	Prho=SQRT(oposz^2+oposy^2)
	Pxx=oposx
	Pr=Sqrt(Pxx^2+Prho^2)

	Pcyl=carttocyl(pos,pos)
	;Nsw1=datN1.y
	;Nsw2=datN2.y

	;Repeat 
	get_data,"Shock_Normal",data=datN

	;get_data,"Shock_Normal_MX1",data=datN1
	;get_data,"Shock_Normal_MX2",data=datN2
;	get_data,"Shock_Normal_AVG",data=datN3
	get_data,"Shock_Normal_AVG",data=datN3;get_data,"Shock_Normal_best",data=datN3
	Nsw=datN.y

if error_status ne 0 then begin
	errmsg=[ 'Error index: '+strtrim(error_status,2),'Error message: '+ !ERROR_STATE.MSG,"FAILED ON ZerothTest,datN3"]
	;print, 'Error index: ', error_status
			;print, 'Error message: ', !ERROR_STATE.MSG
			;;print,"FAILED ON "+nm
			errorsaver,xs[0],errmsg
			store_data,"shock0Acc",data={x:post,y:0,ytitle:"shockConicNormal !C dot ShockNormal"}
			;print,"setting to null result"
			wdelete
			return
endif
	Nsw3=datN3.y


	Nzz3=Nsw3[*,2]
	Nyy3=Nsw3[*,1]
	Nxx3=Nsw3[*,0]
	Nphi3=atan(Nzz3,Nyy3)
	Nrho3=SQRT(Nyy3^2+Nzz3^2)

	Ncyl3=carttocyl(Nsw3,pos)


	Nrho3b=Ncyl3[*,0]
	Nphi3b=Ncyl3[*,1]
	Nxx3b=Ncyl3[*,2]



	GG=where(Nrho3^2+Nxx3^2 ne 0,gcount,Complement=nG)

	;print,"Nx^2+Ny^2+Nz^2=",Nxx3[GG]^2+Nyy3[GG]^2+Nzz3[GG]^2
	;print,"Nx3^2+Nrho3^2="
	;foreach el,GG do print,Nxx3[el]^2+Nrho3[el]^2,"=",Nxx3[el]^2+(Sqrt(Nyy3[el]^2+Nzz3[el]^2))^2
	;print,"Nx3b^2+Nrho3b^2="
	;foreach el,GG do print,Nxx3b[el]^2+Nrho3b[el]^2+Nphi3b[el]^2
	;print,"[Nxx3,Nxx3b,Nyy3,Nzz3,Nphi3,Nphi3b,Nrho3,Nrho3b,N^2,Nb^2]="
	;print,transpose([[Nxx3[GG]],[Nxx3b[GG]],[Nyy3[GG]],[Nzz3[GG]],[Nphi3[GG]],[Nphi3b[GG]],[Nrho3[GG]],[Nrho3b[GG]],[Nrho3[GG]^2+Nxx3[GG]^2],[Nrho3b[GG]^2+Nxx3b[GG]^2]])
	;print,"[Nxx3-Nxx3b,Nphi3-Nphi3b,Nrho3-Nrho3b,N^2-Nb^2]="
	;print,transpose([[Nxx3[GG]-Nxx3b[GG]],[Nphi3[GG]-Nphi3b[GG]],[Nrho3[GG]-Nrho3b[GG]], [Nrho3[GG]^2+Nxx3[GG]^2-Nrho3b[GG]^2-Nxx3b[GG]^2] ])
	;print,Nrho3[GG]^2+Nxx3[GG]^2
	;;FOR each point we pass the shock, find closest point in shock0 curve, determine distance, measure dot products
	minDists=GG*0.0
	s0locs=GG*0.0
	dotprods=GG*0.0
	backwardsLocs=GG*0.0

	s0NNr=GG*0.0
	s0NNx=GG*0.0
	closestConic=fltarr(N,3)
	closestNorm=fltarr(N,3)
	for i=0,gcount-1 do begin
		el=GG[i]
		ox=Pxx[el]
		ophi=Pphi[el]
		orho=Prho[el]
		;oy=oposy[el]
		;oz=oposz[el]

		D=Sqrt( (ox-s0x)^2+(orho-s0rho)^2);+ophi^2)
		minDists[i]=min(D,minloc)
		s0locs[i]=minloc
		;print,minDists[i]
		;print,"[s0x,s0rho][i]=",[s0x[i],s0rho[i]]
		;print,"[Pxx[el],Prho[el]]=",[Pxx[el],Prho[el]]
		s0NNr[i]=snrr[minloc]
		s0NNx[i]=snxx[minloc]
		dop=Nxx3b[el]*snxx[minloc]+Nrho3b[el]*snrr[minloc]
		;print,"dop=",dop
		dotprods[i]=dop

		if dop lt -.5 then begin
			Nsw[el]=Nsw[el]*(-1)
			anyBackwards=1
			backwardsLocs[i]=el
		endif
		;print,"dotprods[i]=dotprods[",i,"]=",dotprods[i]

		accs[el]=dop
		dists[el]=minDists[i]
		closestConic[el,*]=S0[minloc,*]
		
		closestNorm[el,0]=snrr[minloc];S0n[minloc,*]

		closestNorm[el,2]=snxx[minloc]
		

	endfor
	BIGST=where(minDists eq max(minDists))
	
	vecsx=snxx
	vecsrr=snrr
	vecsx[nG]=0
	vecsrr[nG]=0
	yrng=[-5*10^3,max(Prho)+1000]
	minAccept=min([-6*10^3,min(Pxx)*1.3])
	maxAccept=max([7*10^3,max(Pxx)*1.3,Nxx3[GG[BIGST]]*1.2])
	sInPlot=where((s0x gt minAccept) and (s0x lt maxAccept)) ; the conic section's x values have a minimum far beyond the region of interest, need to delete elements that 	aren't relevant

	shock0x=s0x[sInPlot]
	;print,dotprods
	shock0rho=s0rho[sInPlot]
	if 0 then begin
	ps0=plot(/buffer,shock0x,shock0rho,color='brown',xrange=[minAccept,maxAccept],yrange=yrng,POSITION=[0.10,0.22,0.9,0.9])
	p1=plot(Pxx,Prho,color='grey',/overplot, $


	XTITLE='X', YTITLE='$\sqrt{Y^2+Z^2}$',xrange=[minAccept,maxAccept],yrange=yrng)           
	;p2=vector(vecsx[sInPlot],vecsrr[sInPlot],shock0x,shock0rho,/overplot,color="brown")

	p2=vector(Nxx3b[GG],Nrho3b[GG],Pxx[GG],Prho[GG],RGB_table=13,vector_colors= BYTSCL(minDists),/overplot)
	c = COLORBAR(position=[.10,.09,.9,.17],RGB_table=13,TITLE='distance from conic', range=[min(dists),max(dists)])
	c.save,dir+"shock0vectors.png" 
	ps0.close

	ps0=plot(/buffer,shock0x,shock0rho,color='brown',xrange=[minAccept,maxAccept],yrange=yrng,POSITION=[0.10,0.22,0.9,0.9])
	p1=plot(Pxx,Prho,color='grey',/overplot, $


	XTITLE='X', YTITLE='$\sqrt{Y^2+Z^2}$',xrange=[minAccept,maxAccept],yrange=yrng)       
	p2=vector(s0NNx,s0NNr,Pxx[GG],Prho[GG],RGB_table=13,vector_colors= BYTSCL(dotprods),/overplot)
	c = COLORBAR(position=[.10,.09,.9,.17],RGB_table=13,TITLE='dot product from conic', range=[0,1])
	c.save,dir+"shock0normals.png" 
	ps0.close
	endif
	;tplot_element,"dateDataCurrent",'shock0Dist',minDists,/add
	tplot_element,"dateDataCurrent",'shock0Acc',dotprods,/add
	accsL=where(accs ne 0,acount)
	distsL=where(dists ne 0,dcount)
	;print,"accs[where(accs ne 0,acount)] = accs[where(accs ne 0, ",acount,")]=accs[",accsL,"]=",accs[accsL]

	store_data,"shock0distance",data={x:post,y:dists,ytitle:"distance from conic [km]"}
	store_data,"shock0Acc",data={x:post,y:accs,ytitle:"shockConicNormal !C dot ShockNormal"}
	;tplot_element,"shock0Acc",'ytitle',"$\bf{n}_{Shock0conic}$ (closest point)$\cdot \bf{n}_{calculatedShock}$"
	store_data,"closestConic",data={x:post,y:closestConic,ytitle:"Conic Position"}
	store_data,"closestConicNormal",data={x:post,y:closestNorm,ytitle:"Normal vector of closest conic"}
end
