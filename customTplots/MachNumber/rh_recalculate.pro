pro rh_recalculate,M,th,bta,m1,th1,bta1;,btap=btap,btae=btae,g_e=g_e



	g=5.0/3
	gbta=g*bta
	;k0=1+g*bta/2+sqrt(1+g^2*bta^2/4.0+g*bta*(1-2*(cos(th))^2))
	if not keyword_set(g_e) then g_e=1.
	if keyword_set(btae) then gbta=g_e*btae+g*btap
	if numel(M) gt 1 then begin
			N=numel(M)
			M1=fltarr(N)
			th1=fltarr(N)
			bta1=fltarr(N)
			for i=0,N-1 do begin
				RH_parameters,M[i],th[i],bta[i],a,b,c,d,y,delta;,btap=btap,btae=btae,g_e=g_e
				bta1[i]=2*a/(b+c)
				th1[i]=acos(sqrt(b /(b+c)))
				M1[i]=1./sqrt(0.5*(b+c+a*d/(d-1)+sqrt((b+c+a*d/(d-1))^2-4*a*b*d/(d-1)) ))
			endfor
			return
	endif	
	
	RH_parameters,M,th,bta,a,b,c,d,y,delta;,btap=btap,btae=btae,g_e=g_e
	
	bta1=2*a/(b+c)
	th1=acos(sqrt(b /(b+c)))
	M1=1./sqrt(0.5*(b+c+a*d/(d-1)+sqrt((b+c+a*d/(d-1))^2-4*a*b*d/(d-1)) ))
	
end
