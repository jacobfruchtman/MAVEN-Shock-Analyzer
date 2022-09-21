function fracdiff,a,b

	NN=finite(a)*finite(b);+!VALUES.F_NAN*finite(a,/nan)*finite(b,/nan)
	return,2.*abs(a-b)/abs(a+b)*NN*(a/a)*(b/b)

end
