function normalize,v

	MAG=sqrt(total(1.0*v^2))
	for i=0,N_elements(v)-1 do v[i]/=1.0*MAG

return,v
end
