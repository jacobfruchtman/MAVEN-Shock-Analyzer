

pro hallshockfinder
	interpolator2,'wind'
	finiteswitch,'wind_interpolated',newname='wind_shift' 
	hallwindfinder

end
