function findshock,aj,m2,m1,yfit,imin, diff,foot
	diff=1
	;help,aj
	;help,m1
	NN=numel(yfit)
	mans=round(m2)+imin;round(m2/m1)+imin
	if m1 lt 0  then return,aj

	imax=imin+NN
	imax=imin+NN
	mans=round(m2)+imin;round(m2/m1)+imin
	if diff then begin

		derv=derivator(yfit,1,'mid')

		gxg=(where(derv eq max(derv)))[-1]+imin

		if gxg gt foot and gxg lt imax then return,gxg
		
	endif 
	return, round(m2)+imin;round(m2/m1)+imin;m2) +dim
	
end

