function threshtypecheck,kindThresh
	errorBuilder=[0,1]
	typ=-1
	print,kindThresh
	;want to include multiple threshold inplementations (x>t, x==t, x[i]==t but x[i]!=t,t>x>mnt, etc
	;that's what typ is for. this procedure allows the following values for typ
	kindThresh=string(kindThresh)
	if (kindThresh eq string(0)) or  (kindThresh eq "greater") or  (kindThresh eq "grtr")or  (kindThresh eq "gt") then typ=0$
	;returns dat.y[i] gt t forall i
	else if (kindThresh eq string(1)) or  (kindThresh eq "greaterEqual") or  (kindThresh eq "grtrEq")or  (kindThresh eq "ge") then typ=1$
	;returns dat.y[i] ge t forall i
	else if (kindThresh eq string(2)) or  (kindThresh eq "less than") or (kindThresh eq "less") or  (kindThresh eq "lt") then typ=2$
	;returns dat.y[i] lt t forall i
	else if (kindThresh eq string(3)) or  (kindThresh eq "less or Equal") or (kindThresh eq "lessEq") or  (kindThresh eq "le") then typ=3$
	;returns dat.y[i] le t forall i
	else if (kindThresh eq string(4)) or  (kindThresh eq "Equal") or (kindThresh eq "eq") then typ=4$
	;returns dat.y[i] eq t forall i
	else if (kindThresh eq string(5)) or  (kindThresh eq "hit") then typ=5$
	;returns true if dat.y[i-1] ne t and dat.y[i] eq t

	else if (kindThresh eq string(6)) or  (kindThresh eq "leave") then typ=6$
	;returns true if dat.y[i+1] ne t and dat.y[i] eq t

	else if (kindThresh eq string(7)) or  (kindThresh eq "T_or") then typ=7$
	;returns true if (dat.y[i+1] eq t and dat.y[i] eq t) or  (dat.y[i-1] eq t and dat.y[i] eq t)
	else if (kindThresh eq string(8)) or  (kindThresh eq "F_or") then typ=8$
	;returns true if (dat.y[i+1] ne t and dat.y[i] eq t) or  (dat.y[i-1] ne t and dat.y[i] eq t)

	else if (kindThresh eq string(9)) or  (kindThresh eq "T_and") or (kindThresh eq "stable") then typ=9$
	;returns true if (dat.y[i+1] eq t) and  (dat.y[i-1] eq t) and (dat.y[i] eq t)


	else if (kindThresh eq string(10)) or  (kindThresh eq "F_and") or (kindThresh eq "unstable") then typ=10$
	;returns true if (dat.y[i+1] ne t) and  (dat.y[i-1] ne t) and (dat.y[i] eq t)

	else if (kindThresh eq string(11)) or  (kindThresh eq "xor") then typ=11$
	;returns true if (dat.y[i+1] ne t and dat.y[i] eq t) xor  (dat.y[i-1] ne t and dat.y[i] eq t)
	else if (kindThresh eq string(12)) or  (kindThresh eq "bound") then typ=12$
	;returns true if (dat.y[i] le t and dat.y[i] ge mnt)
	else if (kindThresh eq string(13)) or  (kindThresh eq "between") then typ=13$
	;returns true if (dat.y[i] lt t and dat.y[i] gt mnt) 
	else if (kindThresh eq string(14)) or  (kindThresh eq "unmoved") then typ=14$
	;returns true if (dat.y[i-1] eq t and dat.y[i] eq t)
	else if (kindThresh eq string(15)) or  (kindThresh eq "unmoving") then typ=15$
	;returns true if (dat.y[i+1] eq t and dat.y[i] eq t) (or creates an error)
	else if (kindThresh eq string(16)) or  (kindThresh eq "pass") then typ=16$
	; returns true if y goes passes  through t from above or below
	else if (kindThresh eq string(17)) or  (kindThresh eq "ascend") then typ=17$
	; returns true if y passes from below
	else if (kindThresh eq string(18)) or  (kindThresh eq "descend") then typ=18$
	; returns true if y passes from above
	else if (kindThresh eq string(19)) or  (kindThresh eq "drop")or  (kindThresh eq "fall") then typ=19$
	; returns true if y[i+1] lt t andd y[i] eq t
	else if (kindThresh eq string(20)) or  (kindThresh eq "rise") then typ=20$
	; returns true if y[i+1] gt t andd y[i] eq t
	else if (kindThresh eq string(21)) or  (kindThresh eq "fell") then typ=19$
	; returns true if y[i-1] gt t and y[i] eq t
	else if (kindThresh eq string(22)) or  (kindThresh eq "rose") then typ=20$
	; returns true if y[i-1] lt t and y[i] eq t
	else if (kindThresh eq string(23)) or  (kindThresh eq "delay") then typ=23$
	; special type which returns true to an array element mnt behind if passing from below and mnt ahead  if passing from above
	else if (kindThresh eq string(25)) or  (kindThresh eq "pause") then typ=25$
	else if (kindThresh eq string(26)) or  (kindThresh eq "max") then typ=26$
	; if daty[i] ge daty[i-1] and daty[i] gt daty[i+1]
	else if (kindThresh eq string(27)) or  (kindThresh eq "min") then typ=27$
	; if daty[i] le daty[i-1] and daty[i] lt daty[i+1]
	else if (kindThresh eq string(28)) or  (kindThresh eq "signswitchup") then typ=28$
	; if daty[i] le -abs(t) and daty[i+1] ge abs(t)
	else if (kindThresh eq string(29)) or  (kindThresh eq "signswitchdown") then typ=29$
	; if daty[i+1] le -abs(t) and daty[i] ge abs(t)
	 else  return, errorBuilder[3]

	;print,typ
	return, typ
end
