function FFcalcN,theta, bta
	MfmsTest=(findgen(100))/10.0+1.0
   LL = numel(MfmsTest);
	mm=numel(theta)
	nn=numel(bta)

	
	

	ZZZ=findgen(mm,nn)
   ;i = 1;
   for i=0, mm do begin;While i le numel(theta) do begin
   ; print,i," out of ",mm
    ;j = 1;
    for j=0 , nn do begin;While j le numel(bta) do begin
     ;ZZZ[[i, j]] =; {theta[[i]]*180/!const.pi, beta[[j]], 
       ;KcalcN[beta[[j]], theta[[i]], ver, book]};
	ZZZ[[i, j]] =KcalcN(bta[[j]], theta[[i]]*!const.pi/180)
	;j++;
     endfor;endwhile;
    ;++i
    endfor;endwhile;
   Return, ZZZ;[Flatten[ZZZ, 1]]);
end
