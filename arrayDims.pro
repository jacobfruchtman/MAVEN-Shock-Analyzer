
function arrayDims, ar

	num=0;number of dimensions

	if(size(ar,/n_dim) le 1) then num=size(ar,/n_dim) else num=size(ar[0,*],/n_el)
	
	return,num


end
