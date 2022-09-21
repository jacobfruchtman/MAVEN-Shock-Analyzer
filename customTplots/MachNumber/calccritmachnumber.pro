
function m2find,M,bta,th;,btap=btap,btae=btae,g_e=g_e

	g=5.0/3
	gbta=g*bta
	;k0=1+g*bta/2+sqrt(1+g^2*bta^2/4.0+g*bta*(1-2*(cos(th))^2))
	;if not keyword_set(g_e) then g_e=1.
	;if keyword_set(btae) then begin
	;		 gbta=g_e*btae+g*btap
	;		g=gbta/(btap+btae)	 
	;endif
	;a=bta/(M^2*k0)

	;b=2*cos(th)^2/(M^2 *k0)

	;c=2*sin(th)^2/(M^2 *k0)
	;d=g*(g-1)

	;p=( d*(2*a+c+4*b)-2*b+1 )/(1-2*d)

	;q=( 0-d*(4.0*a*b+2.0*b*c+2.0*b^2-c)+b*c-2.0*c+b^2-2.0*b)/(1-2*d)

	;r=b*(2*a*b*d+b+c)/(1-2*d)

	;QQ=(p^2-3*q)/9
	;RR=(2*p^3-9*p*q+27*r)/54

	d = g/(g - 1);
	k0 = 1 + gbta/2 + Sqrt(1 + gbta^2/4 + gbta*(1 - 2*(Cos(th))^2));
	a = bta/(M^2*k0);
	b = 2.0 *Cos(th)^2/(M^2*k0);
	c = 2.0 *Sin(th)^2/(M^2*k0);
	p = (d *(2* a + c + 4 *b) - 2.0 *b + 1)/(1 - 2.0* d);
	q = (-d* (4* a*b + 2.0*b*c + 2.0* b^2 - c) + b*c - 2* c + b^2 - 2* b)/(1 - 2.0* d);
	r = b* (2*a*b*d + b + c)/(1.0 - 2.0 *d);
	QQ = (p^2 - 3* q)/9.0;
	RR = (2 *p^3 - 9 *p*q + 27.0* r)/54.0;
	

	if QQ^3-RR^2 lt 0 then begin
		y=-(p/3.0) + QQ/(-RR + Sqrt(-QQ^3 + RR^2))^(1/3.0) + (-RR + Sqrt(-QQ^3 + RR^2))^(1/3.0)   ;;What's that? this is nothing like the equation in the book shown below? 
													  ;;Yeah. They made a number of mistakes in their solution. No idea how they
													  ;;pulled out the absolute value of R
		;y=RR/abs(RR) *((RR^2-QQ^3+abs(RR))^(1.0/3.0)+QQ*((RR^2-QQ^3)^(1.0/2)+abs(RR))^(-1.0/3))-(p/3.0)

	endif else begin
		n=2

		phi=acos(RR/QQ^(3.0/2))
		y=-2*sqrt(QQ)*cos((phi+n*!const.pi)/3)-p/3
	endelse	

	dlt=(b-1)^2 /(b-y)^2

	return, g*(a+0.5*c*(1-dlt)+1-y)-y

end






function calccritmachnumber,th,beta;,btap=btap,btae=btae,g_e=g_e
			;tic
			th1=!pi/2-abs(!pi/2-th)
			numpoints=18000.
			inc=.0005	
			MfmsTest=findgen(numpoints,start=1+inc,increment=inc);findgen(90001,start=1.,increment=.0001)  ;; Going beyond 0.001 precession isn't really necessary
				
			m2test=fltarr(numpoints) 

			for k=0,numpoints-1 do begin
			;print,"i=",j
				m2test[k]=m2find(MfmsTest[k],beta,th1);,btap=btap,btae=btae,g_e=g_e)
			endfor

		

			m2loc=-1
			m2closest=min(ABS(m2test),m2loc)
		
			crit=Mfmstest[m2loc]
			;toc
			print,crit
			return,crit
end
