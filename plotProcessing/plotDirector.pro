function plotDirector,Mdate=Mdate,currtime=currtime
	PRINT,"========================================="
	PRINT,"PLOT DIRECTOR PLOT DIRECTOR PLOT DIRECTOR"
	if not keyword_set(Mdate) then Mdate=''
	if not keyword_set(currtime) then currtime=''
	;if not keyword_set(currtime) then currtime=systime()
	if Mdate eq '' then date='' else date=Mdate+"/"
	ct=currtime
	print,1,ct

	if date ne '' then begin
		YEAR=(date.split('-'))[0]
		date=YEAR+'/'+date
	endif
	if ct ne '' then begin
		;ct=systime()
		secs=ct.extract(':[0-9]{2}') 
		print,2,secs
		ct=ct.REMOVE(0,3)
		print,3,ct
		ct=ct.replace(secs,'')
		print,4,ct
		ct=ct.replace('  ',' ')
		print,5,ct
		var=ct.split(' ') 
		print,6,var 
		var2=var[3]
		var3=var[2]
		var[2]=var2
		var[3]=var3
		print,7,var 
		ct=var.join('-')+"/"
		print,8,ct
	endif
	dire="Documents/Plots/"+date+ct
	print,9,dire
	FILE_MKDIR,dire
	PRINT,"======================================"
	return,dire
end
