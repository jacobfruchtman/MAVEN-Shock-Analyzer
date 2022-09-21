function vecanglecalc,A,B,absolute=absolute,q1=q1,deg=deg,dot=dot


	Amag=sqrt(dotproduct(A,A))
	Bmag=sqrt(dotproduct(B,B))

	dot=dotproduct(A,B)

	if keyword_set(absolute) then dot=abs(dot)
	th= acos( dot /(Amag*Bmag))

	if keyword_set(q1) then th=!pi/2 -abs(!pi/2 -th)

	if keyword_set(deg) then th=th*180./!pi
	return,th
end
