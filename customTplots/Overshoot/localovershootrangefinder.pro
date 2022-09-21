function localovershootrangefinder,yp,ymmp,down,wid,ishockp,Bmax
	N=numel(ymmp)
	fdmd=(ymmp-down)/down
	fdr1=shift(fdmd,1) ;shifts elements 1 to the right so that fdmd[i+1]=fdr1[i] ;; fdmd[i]=fdr1[i-1] ;;for increasing array fdmd=[1,2,3,4,5], fdmd[i] gt fdr1[i] when i~=0 
	fdl1=shift(fdmd,-1) ;shifts elements 1 to the left so that fdmd[i-1]=fdl1[i] ;; fdmd[i]=fdl1[i+1] ;;for increasing array fdmd=[1,2,3,4,5], fdmd[i] lt fdl1[i] when i~=-1 

	adjshock=ishockp-wid/5.
	thresh=.05
	numabove=0
	numbelow=0
	while numabove eq 0 or numbelow eq 0 do begin
	WABOVE=where(fdmd[adjshock:*] gt thresh and fdmd[adjshock:*] gt fdr1[adjshock:*] and fdmd[adjshock:*] lt fdl1[adjshock:*],numabove)+adjshock
	if numabove eq 0 then begin 
		thresh*=.75
		continue
	endif
	if numabove gt 0.8*(N-adjshock) then return,[-1,-1]
	wstart=WABOVE[0]
	WBELOW=where(fdmd[wstart+1:*] lt thresh and fdmd[wstart+1:*] lt fdr1[wstart+1:*] and fdmd[wstart+1:*] lt fdl1[wstart+1:*],numbelow)+wstart+1
	if numbelow eq 0 or WBELOW[0] ge N-2 then begin 
		thresh/=.75
		continue
	endif
	wend=WBELOW[0]
	endwhile
	Bmax=max(yp[wstart:wend])
	return,[wstart,wend]
end




