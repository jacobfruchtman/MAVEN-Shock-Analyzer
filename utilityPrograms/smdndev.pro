function smdndev,A

	mdn=median(A)
	return,sqrt(median ((A-mdn)^2))
end
