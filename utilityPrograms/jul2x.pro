function jul2x,t

	return,(t-JulDay(1,1,1970,0,0,0))*(24*60*60.0D);t / (24*60*60.0D) + JulDay(1,1,1970,0,0,0)

end
