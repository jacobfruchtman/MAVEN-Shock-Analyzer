pro textfilelinesorter,fname

			read_strDATA2,H,fname,numLines=numLines
			H2=strarr(numLines)
			for i=0,numLines-1 do H2[i]=(strsplit(H[i],";",/extract))[0] 

			
			H2=H2[UNIQ(H2, SORT(H2))]

			openW,1,fname;,/append
			foreach el,H2 do printf,1,el
			close,1
end
