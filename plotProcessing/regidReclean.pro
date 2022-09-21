pro regidReclean

	get_data,"regid_cleaned_plot_interpolated",data=datRi

	get_data,"B_stddev",data=datSTD

	Regi=datRi.y
	Regi=round(Regi)
	STD=datSTD.y

	stdx=datSTD.x
	
	Ni=size(STD,/n_el)

	for i=1,Ni-2 do begin

		j=Ni-1-i


		if (Regi[i-1] eq 2) and (Regi[i] lt 2) and (STD[i] ge 1) then Regi[i]=2
		if (Regi[j+1] eq 2) and (Regi[j] lt 2) and (STD[j] ge 1) then Regi[j]=2

	endfor

	datRi.y=Regi

	store_data,"regid_cleaned_plot_interpolated",data=datRi


	get_data,"regid_cleaned_plot",data=datR
	Reg=datR.y
	rx=datR.x
	N=size(rx,/n_el)
	smallSTD=interpol(STD,stdx,rx)
	for i=1,N-2 do begin

		j=N-1-i


		if (Reg[i-1] eq 2) and (Reg[i] lt 2) and (smallSTD[i] ge 1) then Reg[i]=2
		if (Reg[j+1] eq 2) and (Reg[j] lt 2) and (smallSTD[j] ge 1) then Reg[j]=2

	endfor
	
	datR.y=Reg

	store_data,"regid_cleaned_plot",data=datR
end
