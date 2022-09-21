function thetanormalize,th,deg=deg
	if keyword_set(deg) then return, 90-abs(abs(th)-90) else  return, !pi/2-abs(abs(th)-!pi/2) 
end
