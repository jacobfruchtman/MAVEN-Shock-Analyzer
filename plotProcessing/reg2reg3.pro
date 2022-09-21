pro reg2reg3

	get_data,'regid',data=dat,alim=lim
	ys=dat.y
	xs=dat.x
	ys0=ys
	N=numel(xs)
	for i=0,N_elements(ys)-1 do if ys[i] gt 4 then ys[i]=4

	dat.y=ys
	store_data,'regid2',data=dat,dlim=lim

	yy=ys[*,0]
	w=where(yy ne 0,wcount)
	YNs=yy[w]
	yls=shift(YNs,1)
	yrs=shift(YNs,-1)
	yl2s=shift(YNs,2)
	yr2s=shift(YNs,-2)

	y0s=yns
	for i=1,numel(yns)-2 do if y0s[i-1] eq y0s[i+1] and y0s[i] ne y0s[i-1] then y0s[i]=y0s[i-1]
	for i=1,numel(yns)-2 do if y0s[i-1] eq y0s[i+1] and y0s[i] ne y0s[i-1] then y0s[i]=y0s[i-1]
	yls=shift(Y0s,1)
	yrs=shift(Y0s,-1)
	yl2s=shift(Y0s,2)
	yr2s=shift(Y0s,-2)
	w23s=where(y0s ge 3 and (yls eq 2 or yls eq 1) and yrs ne 2 and yr2s ne 2 and w ne 0,bgcount)
	w32s=where(y0s ge 3 and (yrs eq 2 or yrs eq 1) and yls ne 2 and yl2s ne 2 and w ne N-1,encount)
	
	xx=findgen(N)
	y3=yy
	y33=ys
	midflags=fltarr(N)
	print,w[w23s]
	for el=0,bgcount-1 do begin
		j=w23s[el]
		i=w[j]
		print,i,N-1
		im=w[j-1]
		yl=yls[j]
		ty=YNs[j]
		jm=(Where(yns[0:j] eq 1))[-1]
		if jm eq -1 then im=0 else im=w[jm+1]
		jt=(Where(yns[j:*] ne ty))[0]
		if jt eq -1 then it=N-1 else it=w[j+jt-1];it+i-1
		

		;PRINT,IM,IT,n-1
		yp=yy[im:it]
		mid=total(yp*xx[im:it])/total(yp)
		print,[im,mid,it]
		midflags[mid]=1
		y33[im:mid,*]=2
		y33[mid:it,*]=ty
	endfor
store_data,'midflags',data={x:xs,y:midflags,ytitle:'midflags'}
	
	dat.y=y33
	store_data,'regid3',data=dat,dlim=lim
	;return
	for el=0,encount-1 do begin
		j=w32s[el]
		i=w[j]
		im=w[j-1]
		yl=yls[j]
		ty=YNs[j]
		jm=(Where(yns[0:j] ne ty))[-1]
		if jm eq -1 then im=0 else im=w[jm+1]
		jt=(Where(yns[j:*] eq 1))[0]
		if jt eq -1 then it=N-1 else  it=w[j+jt-1];it+i-1

		print,[im,mid,it]
		yp=yy[im:it]
		mid=total(yp*xx[im:it])/total(yp)
		midflags[mid]=1
		y33[im:mid,*]=ty
		y33[mid:it,*]=2
	endfor

	store_data,'midflags',data={x:xs,y:midflags,ytitle:'midflags'}
	
	dat.y=y33
	store_data,'regid3',data=dat,dlim=lim
end
