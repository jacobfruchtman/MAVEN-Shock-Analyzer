pro RH_parameters,Mfms,th,beta1,a,b,c,d,y,delta,beta_i=beta_i,beta_e=beta_e,g_e=g_e,MA=MA,Msound=Msound,U1n=Uin,Sound=Sound,Alfven=Alfven
	g=5.0/3
	gbta=g*beta1
	if not keyword_set(g_e) then g_e=1.
	
	if keyword_set(beta_i) then begin
			 gbta=g*beta_i+g_e*beta_e
			 beta1=beta_i+beta_e
			 g=gbta/beta1
	endif
	if numel(Mfms) gt 1 then begin
		N1=numel(Mfms)
	
		as=fltarr(N1)
		bs=fltarr(N1)
		cs=fltarr(N1)
		ds=fltarr(N1)
		deltas=fltarr(N1)
		ys=fltarr(N1)
		for i=0,N1-1 do begin
		
			d = g/(g - 1);
			if keyword_set(Sound) and keyword_set(Alfven) then k0=(2/Alfven[i]^2)*( Sound[i]^2+Alfven[i]^2+ sqrt( (Sound[i]^2+Alfven[i]^2)^2 - 4* Sound[i]^2 *Alfven[i]^2 *cos(th[i])^2)) else k0 = 1 + gbta[i]/2 + Sqrt(1 + gbta[i]^2/4 + gbta[i]*(1 - 2*(Cos(th[i]))^2));
			if keyword_set(Msound) then a=1/Msound[i]^2 else $
			if keyword_set(Uin) and keyword_set(Sound) then a=Sound[i]^2/Uin[i]^2 else a = beta1[i]/(Mfms[i]^2*k0);
			if keyword_set(MA) then begin
					b=Cos(th[i])^2 /MA[i]^2
					c=Sin(th[i])^2 /MA[i]^2
			endif else if keyword_set(Uin) and keyword_set(Alfven) then begin
					b=Alfven[i]^2 *cos(th[i])^2 /Uin[i]^2
					c=Alfven[i]^2 *sin(th[i])^2 /Uin[i]^2
					
			endif else begin
				  	b = 2.0 *Cos(th[i])^2/(Mfms[i]^2*k0);
				  	c = 2.0 *Sin(th[i])^2/(Mfms[i]^2*k0);
			endelse
			
			p = (d *(2* a + c + 4 *b) - 2.0 *b + 1)/(1 - 2.0* d);
			q = (-d* (4* a*b + 2.0*b*c + 2.0* b^2 - c) + b*c - 2* c + b^2 - 2* b)/(1 - 2.0* d);
			r = b* (2*a*b*d + b + c)/(1.0 - 2.0 *d);
			QQ = (p^2 - 3* q)/9.0;
			RR = (2 *p^3 - 9 *p*q + 27.0* r)/54.0;
	

			if QQ^3-RR^2 lt 0 then begin
				y=-(p/3.0) + QQ/(-RR + Sqrt(-QQ^3 + RR^2))^(1/3.0) + (-RR + Sqrt(-QQ^3 + RR^2))^(1/3.0)
		;y=RR/abs(RR) *((RR^2-QQ^3+abs(RR))^(1.0/3.0)+QQ*((RR^2-QQ^3)^(1.0/2)+abs(RR))^(-1.0/3))-(p/3.0)

			endif else begin
				n=2.0

				phi=acos(RR/QQ^(3.0/2))
				y=-2.0*sqrt(QQ)*cos((phi+n*!const.pi)/3)-p/3.0
			endelse	

			delta=(b-1)^2 /(b-y)^2
		
		
		
			as[i]=a
			bs[i]=b
			cs[i]=c
			ds[i]=d
			deltas[i]=delta
			ys[i]=y
		endfor
		a=as
		b=bs
		c=cs
		d=ds
		y=ys
		delta=deltas
		return
	endif
	;k0=1+g*beta1/2+sqrt(1+g^2*beta1^2/4.0+g*beta1*(1-2*(cos(th))^2))


	;a=beta1/(M^2*k0)

	;b=2*cos(th)^2/(M^2 *k0)

	;c=2*sin(th)^2/(M^2 *k0)
	;d=g*(g-1)

	;p=( d*(2*a+c+4*b)-2*b+1 )/(1-2*d)

	;q=( 0-d*(4.0*a*b+2.0*b*c+2.0*b^2-c)+b*c-2.0*c+b^2-2.0*b)/(1-2*d)

	;r=b*(2*a*b*d+b+c)/(1-2*d)

	;QQ=(p^2-3*q)/9
	;RR=(2*p^3-9*p*q+27*r)/54

	d = g/(g - 1);
	if keyword_set(Sound) and keyword_set(Alfven) then k0=(2/Alfven^2)*( Sound^2+Alfven^2+ sqrt( (Sound^2+Alfven^2)^2 - 4* Sound^2 *Alfven^2 *cos(th)^2)) else k0 = 1 + gbta/2 + Sqrt(1 + gbta^2/4 + gbta*(1 - 2*(Cos(th))^2));
	;a = beta1/(Mfms^2*k0);
	;b = 2.0 *Cos(th)^2/(Mfms^2*k0);
	;c = 2.0 *Sin(th)^2/(Mfms^2*k0);
	if keyword_set(Msound) then a=1/Msound^2 else $
	if keyword_set(Uin) and keyword_set(Sound) then a=Sound^2/Uin^2 else a = beta1/(Mfms^2*k0);
	if keyword_set(MA) then begin
			b=Cos(th)^2 /MA^2
			c=Sin(th)^2 /MA^2
	endif else if keyword_set(Uin) and keyword_set(Alfven) then begin
			b=Alfven^2 *cos(th)^2 /Uin^2
			c=Alfven^2 *sin(th)^2 /Uin^2
					
	endif else begin
			b = 2.0 *Cos(th)^2/(Mfms^2*k0);
			c = 2.0 *Sin(th)^2/(Mfms^2*k0);
	endelse
	p = (d *(2* a + c + 4 *b) - 2.0 *b + 1)/(1 - 2.0* d);
	q = (-d* (4* a*b + 2.0*b*c + 2.0* b^2 - c) + b*c - 2* c + b^2 - 2* b)/(1 - 2.0* d);
	r = b* (2*a*b*d + b + c)/(1.0 - 2.0 *d);
	QQ = (p^2 - 3* q)/9.0;
	RR = (2 *p^3 - 9 *p*q + 27.0* r)/54.0;
	

	if QQ^3-RR^2 lt 0 then begin
		y=-(p/3.0) + QQ/(-RR + Sqrt(-QQ^3 + RR^2))^(1/3.0) + (-RR + Sqrt(-QQ^3 + RR^2))^(1/3.0)
		;y=RR/abs(RR) *((RR^2-QQ^3+abs(RR))^(1.0/3.0)+QQ*((RR^2-QQ^3)^(1.0/2)+abs(RR))^(-1.0/3))-(p/3.0)

	endif else begin
		n=2.0

		phi=acos(RR/QQ^(3.0/2))
		y=-2.0*sqrt(QQ)*cos((phi+n*!const.pi)/3)-p/3.0
	endelse	

	delta=(b-1)^2 /(b-y)^2
end
