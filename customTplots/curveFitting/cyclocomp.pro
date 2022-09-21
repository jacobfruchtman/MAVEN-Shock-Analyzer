pro cyclocomp


	get_data,"B_1stModalAverage",data=datB1
	get_data,"B_2ndModalAverage",data=datB2
	get_data,"B_3rdModalAverage",data=datB3

	get_data,"Franken_fitted",data=datFF

	FF=datFF.y
	B1=datB1.y
	B2=datB2.y
	B3=datB3.y

	N=numel(B1)
	Bms=fltarr(N)
	
	bigs=Bms

	for i=0, N-1 do begin

		if FF[i] lt max([B1[i],B2[i],B3[i]]) then bigs[i]=FF[i]
	endfor

	store_data,"Franken_big",data={x:datFF.x,y:bigs}

end
