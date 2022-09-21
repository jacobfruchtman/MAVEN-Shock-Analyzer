function calculatelocalcyclotronperiod,loc,SNR,B,dhalf

	loc0=loc
	N=numel(B)
	while loc gt dhalf or (loc ge dhalf-1 and dhalf-1 ge 0) do begin
		minloc=loc
		while minloc gt 0 do begin

			if SNR[minloc] lt 20 then break
			if loc-minloc eq 60 then break
			minloc--
		;if minloc eq 0 then break
		endwhile

		Bmean=mean(B[minloc:loc]) *10.0^(-9)

	

		t=(2*!pi*!const.mp)/(!const.e * Bmean)

		l2=max([0,loc-t])
		l3=max([0,loc-1.5*t])
		l4=max([0,loc-2*t])
		ll2=min([loc+t,N-1])
		ll3=min([loc+1.5*t,N-1])
		ll4=min([loc+2.0*t,N-1])
		;print,[l3:ll3]
		B2=mean(B[l2:ll2])
		B3=mean(B[l3:ll3])
		B4=mean(B[l2:ll4])
		if stddev([B2,B3,B4]) lt .15 then break


		loc--
	endwhile
	if loc ne loc0 then print,"bad foot: moved from ",loc0," to ", loc

	return,t
end
