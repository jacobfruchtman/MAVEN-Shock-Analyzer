function sec,x



	return, 1/cos(x)
end



function m2find,M,bta,th,test

	g=5.0/3

	;k0=1+g*beta/2+sqrt(1+g^2*beta^2/4.0+g*beta*(1-2*(cos(th))^2))


	;a=beta/(M^2*k0)

	;b=2*cos(th)^2/(M^2 *k0)

	;c=2*sin(th)^2/(M^2 *k0)
	;d=g*(g-1)

	;p=( d*(2*a+c+4*b)-2*b+1 )/(1-2*d)

	;q=( 0-d*(4.0*a*b+2.0*b*c+2.0*b^2-c)+b*c-2.0*c+b^2-2.0*b)/(1-2*d)

	;r=b*(2*a*b*d+b+c)/(1-2*d)

	;QQ=(p^2-3*q)/9
	;RR=(2*p^3-9*p*q+27*r)/54

	d = g/(g - 1);
	k0 = 1 + g*bta/2 + Sqrt(1 + g^2*bta^2/4 + g*bta*(1 - 2*(Cos(th))^2));
	a = bta/(M^2*k0);
	b = 2.0 *Cos(th)^2/(M^2*k0);
	c = 2.0 *Sin(th)^2/(M^2*k0);
	p = (d *(2* a + c + 4 *b) - 2.0 *b + 1)/(1 - 2.0* d);
	q = (-d* (4* a*b + 2.0*b*c + 2.0* b^2 - c) + b*c - 2* c + b^2 - 2* b)/(1 - 2.0* d);
	r = b* (2*a*b*d + b + c)/(1.0 - 2.0 *d);
	QQ = (p^2 - 3* q)/9.0;
	RR = (2 *p^3 - 9 *p*q + 27.0* r)/54.0;
	

	if QQ^3-RR^2 lt 0 then begin
		;y=-(p/3.0) + QQ/(-RR + Sqrt(-QQ^3 + RR^2))^(1/3.0) + (-RR + Sqrt(-QQ^3 + RR^2))^(1/3.0)
		;y=RR/abs(RR) *((RR^2-QQ^3+abs(RR))^(1.0/3.0)+QQ*((RR^2-QQ^3)^(1.0/2)+abs(RR))^(-1.0/3))-(p/3.0)
		y=-(p/3.0) + QQ/(-RR + Sqrt(-QQ^3 + RR^2))^(1/3.0) + (-RR + Sqrt(-QQ^3 + RR^2))^(1/3.0)
	endif else begin
		n=2

		phi=acos(RR/QQ^(3.0/2))
		y=-2*sqrt(QQ)*cos((phi+n*!const.pi)/3)-p/3
		if test eq 1 then print,y
	endelse	

	dlt=(b-1)^2 /(b-y)^2

	return, g*(a+0.5*c*(1-dlt)+1-y)-y

end


function KcalcN,bta, theta,test
	MfmsTest=(findgen(1000))/100+1.1
   	m2test = MfmsTest*0;
  ; 	k = 1.0;Mathematica indexing starts at 1, not 0
   
   for k=0, size(m2test,/n_el)-1 do begin
    
    m2test[k] = m2find(MfmsTest[k], bta, theta,test);
    ;k++
   endfor
   loc = -1
   mn=min(abs(m2test),loc)
   Return, MfmsTest[loc]
end


function FFcalcN,theta, bta
	MfmsTest=(findgen(100))/10.0+1.0
   	LL = size(MfmsTest,/n_el);
	mm=size(theta,/n_el)
	nn=size(bta,/n_el)
	print,mm,nn
	
	

	ZZZ=findgen(mm,nn)*0
   ;i = 1;
   for i=0, mm-1 do begin;While i le numel(theta) do begin
    print,i," out of ",mm-1
    ;j = 1;
    for j=0 , nn-1 do begin;While j le numel(bta) do begin
     ;ZZZ[[i, j]] =; {theta[[i]]*180/!const.pi, beta[[j]], 
       ;KcalcN[beta[[j]], theta[[i]], ver, book]};
	ZZZ[i, j] =KcalcN(bta[j], theta[i]*!const.pi/180,0)
	;j++;
     endfor;endwhile;
    ;++i
    endfor;endwhile;
   Return, ZZZ;[Flatten[ZZZ, 1]]);
end
function m2Ffind, bta,theta
	mm=size(theta,/n_el)
	nn=size(bta,/n_el)
	

	ZZZ=findgen(mm,nn)
		MfmsTest=(findgen(10000))/1000+1.0

	for i=0,mm-1 do begin
		print,i," out of ",mm-1
		for j=0,nn-1 do begin	
				m2test=fltarr(10000)
				for k=0,10000-1 do begin
				;print,"i=",j
					m2test[k]=m2find(MfmsTest[k],bta[j],theta[i])
				endfor
				m2loc=-1
				m2closest=min(ABS(m2test),m2loc)
				ZZZ[i,j]=Mfmstest[m2loc]
		endfor
	endfor

	;Contour(ZZZ,theta,beta)


	return, ZZZ
end


Pro critMcalcTest,angle=angle

	

	pltpos=[0.15,0.20,0.95,0.8]
	cbpos=[0.30,0.05,0.70,0.10]


	print,KcalcN(1,!const.pi/6.0,1)


	betatest=findgen(500,start=0.0,increment=.01)
	thetatest=findgen(91,start=0,increment=1);*!const.pi/180



	;ZZZ=m2Ffind(betatest,thetatest)
	ZZZ=FFcalcN(thetatest,betatest)
	
	print,"[thetatest[30],betatest[110],ZZZ[30,110]]=",[thetatest[30],betatest[110],ZZZ[30,110]]
print,"[thetatest[30],betatest[110],ZZZ[30,110]]=",[thetatest[15],betatest[210],ZZZ[15,210]]
	p4=CONTOUR(ZZZ,thetatest,betatest, $
	XTITLE="theta", YTITLE='beta',rgb_table=33,POSITION=pltpos,C_VALUE=[1.0,1.1,1.2,1.3,1.4,1.5,1.7,1.8,2.0,2.2,2.4])

	


	;store_data,"critical_Mfms",data={x:datCs.x,y:crits,ytitle:"critical points"}

	print,KcalcN(1,!const.pi/6)

end

	
	




	
