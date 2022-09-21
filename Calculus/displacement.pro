function displacement,V1,V2

	return, abs(sqrt((V1[0]-V2[0])^2+(V1[1]-V2[1])^2+(V1[2]-V2[2])^2))
end
