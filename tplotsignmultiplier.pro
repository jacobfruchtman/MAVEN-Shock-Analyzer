;+
; NAME:
;	tplotsignmultiplier
;
; PURPOSE:
;	This procedure returns the combined sign of the provided plots at each point in time for  
;	given tplots. This is useful if you want to check when normally correlating tplots stop doing so. 
;	(for example, when MAVEN passes over a crustal Magnetic field while dropping below the Bow Shock, the measured density will decrease to zero from it's bow shock maximum
;	(and thus the numerical derivative of a 1 minute average will be <=0)  while at the same time that the othrwise correlated Magnetic field will be increasing
;
; CATEGORY:
;	Time Series Analysis
;
; CALLING SEQUENCE:
;	tplotsignmultiplier,tplotname1,tplotname2[,plotlist=plotlist][,newName=newName]

; INPUTS:
;	tplotname1,2:  scalar strings containing the names of tplot variables. the tplots' y values must be 1D arrays of equal size 
;	plotlist: array of TPLOT variable names.  the tplots' y values must be 1D arrays of equal size
;
; OPTIONAL KEYWORDS:
;	plotlist: a named variable containing an array of strings containing TPLOT names. 
;		  If set, will instead calculate the combined sign of every tplot in the list/array. 
;		  The two standard inputs will be entirely ignored
;	newName: a scalar string or named variable of type string containing the name of the new TPLOT variable. 
;		By default,  the new TPLOT will be named "<tplotname1>_<tplotname2>_..._<tplotnameN>_combined_sign"
;
; PROCEDURE:
;	For each point, multiply the sign of the tplots' y values at that point.
;
; EXAMPLE:
;	Define two tplots
;	 store_data,'tplotexample1',data = {x:[0,1,2,3,4,5,6],y:[1,2,1,0,-1,-2,-1]}
;	 store_data,'tplotexample2',data = {x:[0,1,2,3,4,5,6],y:[1,2,3,2,1,0,-1]}
;	
;	create a tplot with the combined sign, and new name "EX12sign"
;	  tplotsignmultiplier,'tplotexample1','tplotexample2',newName="EX12sign"
;	This should create a new tplot called "EX12sign"
;	get the data structure to check the y data
;		get_data,"EX12sign",data=data
;		y=data.y
;		print,y
;	This should output [1,1,1,0,-1,-1,1]
;
;	OR
;	Define three tplots
;		store_data,'tplotexample1',data = {x:[0,1,2,3,4,5,6],y:[1,2,1,0,-1,-2,-1]}
;	 	store_data,'tplotexample2',data = {x:[0,1,2,3,4,5,6],y:[1,2,3,2,1,0,-1]}
;	 	store_data,'tplotexample3',data = {x:[0,1,2,3,4,5,6],y:[1,0,-1,0,1,0,-1]}
;	Define an array containing the names
;	 	nms=['tplotexample1','tplotexample2','tplotexample3']
;	create a tplot with the combined sign of the three
;		tplotsignmultiplier,1,1,plotlist=nms
;	This should create a new TPLOT named "tplotexample1_tplotexample2_tplotexample3_combined_sign"
;	Get the data structure to check the y data
;		get_data,"tplotexample1_tplotexample2_tplotexample3_combined_sign",data=data
;		y=data.y
;		print,y
;	This should output [1,0,-1,0,-1,0,-1]
;
; MODIFICATION HISTORY:
; 	Written by:	Jacob Fruchtman (jfruchtman@uiowa.edu), 2021-10-08.
;-

;***********************************************************************




pro tplotsignmultiplier,plt1,plt2,plotlist=plotlist



	if not keyword_set(plotlist) then begin

		get_data,plt1,data=dat1
		get_data,plt2,data=dat2
		x=dat1.x
		y1=dat1.y
		y2=dat2.y
		y1[where(abs(y1) lt .0001)]=0
		y2[where(abs(y2) lt .0001)]=0 
		;GG1=where(y1 eq 0,g1count)
		;GG2=where(y2 eq 
		z=sign(y1)*sign(y2)
		if not keyword_set(newName) then newName=plt1+"_"+plt2+"_combined_sign"
	
	endif else begin
		numDats=size(plotlist,/n_el)
		
		;nameList=list()
		ylist=list()
		foreach el, plotlists do begin
			get_data,el,data=dat
			y=dat.y
			y[where(abs(y) lt .0001)]=0
			ylist.add,y
		endforeach
		x=dat.x
		z=fltarr(size(x,/n_el))+1
		foreach el, ylist do z*=sign(el)
		if not keyword_set(newName) then newName=strjoin(plotlist,"_")+"_combined_sign"
	endelse

	store_data,newName,data={x:x,y:z,ytitle:"combined sign"}

end
