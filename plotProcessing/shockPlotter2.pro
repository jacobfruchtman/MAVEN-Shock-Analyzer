


pro shockPlotter2

	get_data,'MAVEN_POS_(MARS-MSO)',data=dpos
	get_data,"Shock_Normal",data=datN

	get_data,"Shock_Normal_MX1_D",data=datN1
	get_data,"Shock_Normal_MX2",data=datN2
	get_data,"Shock_Normal_MX3",data=datN3

	oNsw=datN.y
	oNsw1=datN1.y
	oNsw2=datN2.y
	oNsw3=datN3.y
	ox=datN.x

	oNsw=datN.y

	oNx=oNsw[*,0]
	oNy=oNsw[*,1]
	oNz=oNsw[*,2]

	oNx1=oNsw1[*,0]
	oNy1=oNsw1[*,1]
	oNz1=oNsw1[*,2]


	oNx2=oNsw2[*,0]
	oNy2=oNsw2[*,1]
	oNz2=oNsw2[*,2]

	oNx3=oNsw3[*,0]
	oNy3=oNsw3[*,1]
	oNz3=oNsw3[*,2]


	post=dpos.x
	xp=dpos.y[*,0]
	yp=dpos.y[*,1]
	zp=dpos.y[*,2]

	Nx=interpol(oNx,ox,post)
	Ny=interpol(oNy,ox,post)
	Nz=interpol(oNz,ox,post)


	Nx1=interpol(oNx1,ox,post)
	Ny1=interpol(oNy1,ox,post)
	Nz1=interpol(oNz1,ox,post)


	Nx2=interpol(oNx2,ox,post)
	Ny2=interpol(oNy2,ox,post)
	Nz2=interpol(oNz2,ox,post)


	Nx3=interpol(oNx3,ox,post)
	Ny3=interpol(oNy3,ox,post)
	Nz3=interpol(oNz3,ox,post)

	Nsw=[[Nx],[Ny],[Nz]]
	Nsw1=[[Nx1],[Ny1],[Nz1]]
	Nsw2=[[Nx2],[Ny2],[Nz2]]
	Nsw3=[[Nx3],[Ny3],[Nz3]]	
	;xp=interpol(oposx,post,xx)
	;yp=interpol(oposy,post,xx)
	;zp=interpol(oposz,post,xx)

	

	pos=[[xp],[yp],[zp]]


	Ncyl=carttocyl(Nsw,pos)
	Ncyl1=carttocyl(Nsw1,pos)
	Ncyl2=carttocyl(Nsw2,pos)
	Ncyl3=carttocyl(Nsw3,pos)
	Pcyl=carttocyl(pos,pos)

	Nrho=Ncyl[*,0]
	Nphi=Ncyl[*,1]
	Nxx=Ncyl[*,2]

	Nrho1=Ncyl1[*,0]
	Nphi1=Ncyl1[*,1]
	Nxx1=Ncyl1[*,2]


	Nrho2=Ncyl2[*,0]
	Nphi2=Ncyl2[*,1]
	Nxx2=Ncyl2[*,2]

	Nrho3=Ncyl3[*,0]
	Nphi3=Ncyl3[*,1]
	Nxx3=Ncyl3[*,2]



	Prho=Pcyl[*,0]
	Pphi=Pcyl[*,1]
	Pxx=Pcyl[*,2]

	print,"max(Nphi3)=",max(Nphi3)

	print,"mean(Nphi3)=",mean(Nphi3)

	print,"min(Nphi3)=",min(Nphi3)

	
	p1=plot(Pxx,Prho,POSITION=[0.10,0.22,0.95,0.9], $

	XTITLE='X', YTITLE='SQRT(Y^2+Z^2)')

	p2=vector(Nxx,Nrho,Pxx,Prho,POSITION=[0.10,0.22,0.95,0.9],RGB_table=10,vector_colors=Nphi,Symbol='*',/overplot)            

	c = COLORBAR(TARGET=p2, $
	TITLE='Tangential component of unit vector (radians)')


;-------MX1


	p3=plot(Pxx,Prho,POSITION=[0.10,0.22,0.95,0.9], $

	XTITLE='X, MX1 normals', YTITLE='SQRT(Y^2+Z^2)')

	p4=vector(Nxx1,Nrho1,Pxx,Prho,POSITION=[0.10,0.22,0.95,0.9],RGB_table=10,vector_colors=Nphi1,Symbol='*',/overplot)            

	c = COLORBAR(TARGET=p4, $
	TITLE='Tangential component of MX1 unit vector (radians)')


;------Mx2

	p5=plot(Pxx,Prho,POSITION=[0.10,0.22,0.95,0.9], $

	XTITLE='X, MX2 normals', YTITLE='SQRT(Y^2+Z^2)')

	p6=vector(Nxx2,Nrho2,Pxx,Prho,POSITION=[0.10,0.22,0.95,0.9],RGB_table=10,vector_colors=Nphi2,Symbol='*',/overplot)            

	c = COLORBAR(TARGET=p6, $
	TITLE='Tangential component of MX2 unit vector (radians)')



;------Mx3
	p7=plot(Pxx,Prho,POSITION=[0.10,0.22,0.95,0.9], $

	XTITLE='X, MX3 normals', YTITLE='SQRT(Y^2+Z^2)')
	print,"p7 done"
	p8=vector(Nxx3,Nrho3,Pxx,Prho,POSITION=[0.10,0.22,0.95,0.9],RGB_table=10,vector_colors=Nphi3,Symbol='*',/overplot)            

	c = COLORBAR(TARGET=p8, $
	TITLE='Tangential component of MX3 unit vector (radians)',range=[-!pi,!pi])

end
