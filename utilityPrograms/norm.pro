function norm_vec,vec

	return, sqrt(total(vec^2))
end

function norm2, vec

	dim=size(size(vec,/dim),/dim)

	case dim of
		1:return, norm(vec)
		2: begin
			N=size(vec[*,0],/N_el)
			sclr=fltarr(N)
			for i=0,N-1 do sclr[i]=norm(vec[i,*])
			return, sclr
		    end
	endcase
end
