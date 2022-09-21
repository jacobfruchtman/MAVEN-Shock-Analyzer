function standarderror,array
	A=mean(array) - array
	;B=Total(A^2)

	C=Total(A^2)/(n_elements(array) -  1)

	return,Sqrt(Total(A^2)/(n_elements(array) -  1))/sqrt(n_elements(array))
end
