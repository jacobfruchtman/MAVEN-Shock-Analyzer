function KcalcN,bta, theta
	MfmsTest=(findgen(10000))/1000+1.0
   	m2test = MfmsTest*0.0;
  ; 	k = 1.0;Mathematica indexing starts at 1, not 0
   
   for k=0, numel(m2test)do begin
    
    m2test[[k]] = m2find(MfmsTest[[k]], bta, theta);
    ;k++
   endfor
   loc = -1
   mn=min(abs(m2test),loc)
   Return, MfmsTest[[loc]]
end
