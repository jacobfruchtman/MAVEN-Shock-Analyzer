pro listcleaner,filename



	read_strDATA2, H, filename,numLines=numLines
	
	H2=H[UNIQ(H, SORT(H))]

	OPENW, outunit, filename, /GET_LUN, /MORE  
	foreach el,H2 do PRINTF,outunit,el     
	FREE_LUN, outunit
end
