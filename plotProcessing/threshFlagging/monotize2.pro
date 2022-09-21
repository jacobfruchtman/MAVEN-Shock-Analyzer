

function monotize2,y,t;returns an array with 1 where monotonically increasing and starts returning 0  when that stops being true until things start increasing again. ignores random spikes


;for example, 
;   the array y=[2,0,4,3,2,3,5,0,3,2,2,3] 
; will return m=[0,0,1,0,0,1,1,0,1,0,0,1], ignoring 0s


	;count=Size(B,/n_el)
	print,'entering mono2'
	N=Size(y,/n_el)
	print,N

	wherefinite=where(finite(y) eq 1,numfin)
	firstfin=wherefinite[0]
	finalfin=wherefinite[-1]
	y0f=y[firstfin]
	yff=y[finalfin]
	isinc=1*(y0f eq t);1*(y[0] eq t)


	;monoB=B*0.0

	mono=y*0.0
	
	flips=mono

	mono[firstfin]=isinc
	;print,firstfin,isinc,mono[firstfin]
	lasty=max([y0f*isinc,1])
	lasti=-1+isinc
	llasty=lasty
	llasti=-1+isinc
	lastvi=lasti;last vefified lasti
	lastvy=lasty;last  verified lasty
	prei=lasti
	prey=lasty


		
	print,'entering mono2 for  loop'
	;for i=1,N-1 do begin  ;may want to modify this so that it makes a distinction between if the previous element is zero or not
	for i=firstfin+1,finalfin do begin  ;may want to modify this so that it makes a distinction between if the previous element is zero or not
		

		if y[i] gt y[i-1] then begin

			isinc=1

			lasty=y[i]
			prey=y[i-1]
			lasti=i
			prei=i-1
			if(mono[i-1] eq 0) then begin 
				llasty=lasty
				llasti=i
				flips[i]=1
			endif else begin
				lastvy=llasty
				lastvi=llasti
			endelse
		endif else	if y[i]  lt y[i-1] then begin
			isinc=0
			if(llasti eq lasti) and y[i-1] eq t then begin
				for j=lasti,i do begin
					mono[j]=0
				endfor
				llasti=lastvi
				llasty=lastvy
			endif
		endif 
		;endelse		
		if (isinc and (y[i] ge y[i-1])) then mono[i]=1

	endfor
	

	return, mono
end  
