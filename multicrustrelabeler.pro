pro multicrustrelabeler


	fname="Documents/savedpntsU.txt"

	read_strDATA2,manualList,fname,numLines=numLines2

	for i=0,numLines2-1 do begin
		iscrust=0
		ismulti=0
		typ=1
		fullstr=manualList[i]
		if fullstr.EndsWith('/0:00') then continue
		if fullstr.EndsWith('#') then continue
		if fullstr.startswith(',') then typ*=2 else $       ;really, 2^1
		if fullstr.startswith('A') then typ*=4 else continue ;really, 2^2

		var=(strsplit(fullstr,"#",/extract))
		if numel(var) eq 1 then begin
			print,var
			return

		endif
		if var[1].startswith('multi') then typ *= 3 else $ ;really, 3^1
		if var[1].startswith('crust') then typ *= 9  else continue ;;really, 3^2

		case typ of

			2*3 : fullstr=fullstr.replace(',','R@')
			2*9 : fullstr=fullstr.replace(',','C@')
			4*3 : fullstr=fullstr.replace('A@','R@')
			4*9 : fullstr=fullstr.replace('A@','C@')
		endcase
		print,fullstr
		manualList[i]=fullstr
		print,manualList[i]
	endfor
	fname2="Documents/savedpntsU2.txt"
	print,fname2
	openW,1,fname2
	foreach el,manualList do begin

		printf,1,el
		if el.startswith('C') or el.startswith('R') then print,el
	endforeach
	close,1
	print,'%%%%%%%%%%%%%%%%%%%'
	read_strDATA2,manualList2,fname2,numLines=numLines2
	foreach el,manualList2 do begin

		
		if el.startswith('C') or el.startswith('R') then print,el
	endforeach

end
