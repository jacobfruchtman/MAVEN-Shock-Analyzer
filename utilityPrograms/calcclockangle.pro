function calcclockangle,By,Bz
	if N_PARAMS() eq 1 and numel(By) eq 3 then begin
				BBy=By[1]
				BBz=By[2]
				den=SQRT(BBY^2+BBZ^2)

				clockangle=(-1)^(BBy lt 0) *acos( BBz/den)
				return,clockangle
	endif
	;print,N_PARAMS()
	BBy=By
	;if size(BBy,/N_dim) eq 2 and (size(BBy,/dim))[-1] eq 3 then begin
	IF N_PARAMS() eq 1 and (size(BBy,/dim))[-1] eq 3  THEN BEGIN
		B=BBy
		BBy=B[*,1]
		BBz=B[*,2]
	endif else BBz=Bz
	N=numel(BBZ)
	
	den=SQRT(BBY^2+BBZ^2)

	clockangle=(-1)^(BBy lt 0) *acos( BBz/den)
	w=where(BBy eq 0 and BBz eq 0,wcount)
	if wcount gt 0 then	clockangle[w]=!pi/2
	if size(clockangle,/n_dim) eq 2 then clockangle=transpose(clockangle)
	return,clockangle
end
