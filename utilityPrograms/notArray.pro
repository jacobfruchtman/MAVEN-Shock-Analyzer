pro notArray,plt,newName=newName

	if not keyword_set(newName) then newName=plt+"_inverted"

	get_data,plt,data=dat,alim=lim

	dat.y= NOT dat.y

	store_data,newName,data=dat,dlim=lim

end
