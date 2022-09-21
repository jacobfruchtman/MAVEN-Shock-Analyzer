


pro shockPlotter3

	get_data,'MAVEN_POS_(MARS-MSO)',data=dpos
	get_data,"Shock_Normal",data=datN

	get_data,"Shock_Normal_MX1",data=datN1
	get_data,"Shock_Normal_MX2",data=datN2
	get_data,"Shock_Normal_MX3",data=datN3

	Nsw=datN.y

	Nsw1=datN1.y
	Nsw2=datN2.y
	Nsw3=datN3.y

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



	xx=datN.x
	sn=size(xx,/n_el)

	post=dpos.x
	oposx=dpos.y[*,0]
	oposy=dpos.y[*,1]
	oposz=dpos.y[*,2]
	st=size(post,/n_el)

	nt=list()

	Ns=list()

	Ns1=list()

	Ns2=list()

	Ns3=list()

	print,post[0] gt xx[0]
	print,post[st-1] lt xx[sn-1] 
	print,post[1]-post[0]

	print,post[st-1]
	print,max(xx)
	print,xx[1]-xx[0]
	
	print,post[st-1]-xx[0]
	print,post[st-1]-post[0]
	print,max(xx)-xx[0]
	newt=findgen(st)+post[0]
	;for i=691200.50,size(xx,/n_el)-1 do begin

		;if xx[i] -post[0] lt 0 then continue
	;	if xx[i]-post[0] gt post[st-1]-post[0] then break
		
	;	nt.add,xx[i]
	;	Ns.add,Nsw[i,*]
	;	Ns1.add,Nsw1[i,*]
	;	Ns2.add,Nsw2[i,*]
	;	Ns3.add,Nsw3[i,*]
	;endfor	

	;Nsw=Ns.toarray()
	;Nsw1=Ns1.toarray()
	;Nsw2=Ns2.toarray()
	;Nsw3=Ns3.toarray()
	;xx=nt.toarray()
	;print,size(nt)
	;print,size(Ns)

	;print,Max(Nsw[*,0])	
	xp=interpol(oposx,post,newt)
	yp=interpol(oposy,post,newt)
	zp=interpol(oposz,post,newt)


	Nx=interpol(oNx,xx,newt)
	Ny=interpol(oNy,xx,newt)
	Nz=interpol(oNz,xx,newt)


	Nx1=interpol(oNx1,ox,newt)
	Ny1=interpol(oNy1,xx,newt)
	Nz1=interpol(oNz1,xx,newt)


	Nx2=interpol(oNx2,xx,newt)
	Ny2=interpol(oNy2,xx,newt)
	Nz2=interpol(oNz2,xx,newt)


	Nx3=interpol(oNx3,ox,newt)
	Ny3=interpol(oNy3,ox,newt)
	Nz3=interpol(oNz3,ox,newt)

	Nsw=[[Nx],[Ny],[Nz]]
	Nsw1=[[Nx1],[Ny1],[Nz1]]
	Nsw2=[[Nx2],[Ny2],[Nz2]]
	Nsw3=[[Nx3],[Ny3],[Nz3]]

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


get_data,175,data=datAMX1  
get_data,181,data=datAMX2
get_data,187,data=datAMX3
get_data,169,data=datAN

MX1=datAMX1.y
MX2=datAMX2.y
MX3=datAMX3.y
AN=datAN.y   
slocs=where(AN ne 0,nsh)

SNph=Nphi[slocs]*180/!pi
SNph1=Nphi1[slocs]*180/!pi
SNph2=Nphi2[slocs]*180/!pi
SNph3=Nphi3[slocs]*180/!pi

sA=AN[slocs]*180/!pi
sA1=MX1[slocs]*180/!pi
sA2=MX2[slocs]*180/!pi
sA3=MX3[slocs]*180/!pi

plot(SNph,sA,XTITLE='$\phi$ (degrees)', YTITLE='$\theta$ (degrees)',Symbol="*",NAME="Magnetic Coplanarity method",rgb_table=0,magnitude=slocs)
plot(SNph1,sA1,XTITLE='$\phi$ (degrees)', YTITLE='$\theta$ (degrees)',Symbol="s",NAME="Mixed Mode 1 method",rgb_table=0,magnitude=slocs,/overplot)
plot(SNph2,sA2,XTITLE='$\phi$ (degrees)', YTITLE='$\theta$ (degrees)',Symbol="D",NAME="Mixed Mode 2 method",rgb_table=0,magnitude=slocs,/overplot)
plot(SNph3,sA3,XTITLE='$\phi$ (degrees)', YTITLE='$\theta$ (degrees)',Symbol="X",NAME="Mixed Mode 3 method",rgb_table=0,magnitude=slocs,/overplot)
!null = LEGEND(target=[p_wspd, p_wdir])
end


