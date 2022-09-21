pro projectorplot2
	R_mars=3389.5
	get_data,'X_MSO',data=datx
	get_data,'RHO_MSO',data=daty
		dire="Documents/Plots/CombinedPlots/"
	X=datx.y
	Y=daty.y
	XT=datx.YN[0]
	YT=daty.YN[0]
	yttl='shock $\rho_{MSO} = \sqrt{Y^2+Z^2}$ [km]';daty.ytitle[0]
	xttl='shock $X_{MSO}$ [km]';datx.ytitle[0]

	yfnm=daty.fn[0]
	xfnm=datx.fn[0]
	binsize=datx.binsize[0]
	subfn=''
	subT=''
	t=datx.x
	N=numel(datx.x)
	get_data,'Mfms',data=datMfms
	get_data,'B2B1fit',data=datB2B1

	get_data,'overshootAmplitude',data=datA
	get_data,10,data=datTH

	get_data,'FM',data=datFM
	get_data,'MachAlfven',data=datMA

	get_data,'SolDistNorm',data=datDist
	get_data,'Velocity_N',data=datVN
	
	;;; [J   ,  A  ]
	;;; [Mfms,  M_A ]
	;;; [FM  ,  Dist]
	;;; [VN,   TH_BN]

	;cpos1=[0.084285714 ,0.75496875 ,0.45000000 ,0.77]

xl1=     0.092091667
xr1=      0.26524167
cxl1=     0.050666667
cxr1=      0.30666667
yt1=      0.93039063
yb1=      0.75002604
cyt1=      0.71952604
cyb1=      0.68752604
pJpos=[     0.092091667 ,     0.75002604   ,   0.26524167  ,    0.93039063]
cJpos=[     0.050666667  ,    0.68752604  ,    0.30666667  ,    0.71952604]





psize=.002
xl1=0.11;2901923
xr1=0.40790385
xl2=0.62901923
xr2=0.90790385
cxl1=0.071538462
cxr1=0.46538462
cxl2=0.57153846
cxr2= 0.96538462

xl1=     0.092725000
xr1=      0.45527500
cxl1=   .015  ;0.018000000
cxr1=     xr1; 0.53000000

xl2=      0.59272500
xr2=      0.95527500
cxl2=      0.51800000
cxr2=      xr2; 1.0300000


tx1=.08
tx2=.49




;yt1=.98;0.95603125
;cyt1=.8;0.78056875
;yb1=.83;0.80496875
;cyb1=.78;0.75496875 
yh=0.15106249;=.98-0.82893753
dypb=.02893753
dy=0.029999971+.015

yt1=   .992;   0.95603125
yb1=  yt1-yh ;  0.82893753 ; 0.80496875
cyt1=  yb1-dypb ;.8;   0.78056875
cyb1=  cyt1-.02; .78;   0.75496875


p1pos=[xl1,yb1,xr1,yt1]
c1pos=[cxl1,cyb1,cxr1,cyt1]
p2pos=[xl2,yb1,xr2,yt1]
c2pos=[cxl2,cyb1,cxr2,cyt1]
;yt2=.75;0.70603125
;cyt2=.57; 0.53056875
;yb2=.60;0.55496875
;cyb2=.55;0.50496875

yt2=  cyb1-dy;.74   ; 0.70603125
yb2=  yt2-yh; 0.58893751;   0.55496875
cyt2=  yb2-dypb ;.56;   0.53056875
cyb2=  cyt2-.02 ; .54;  0.50496875

p3pos=[xl1,yb2,xr1,yt2]
c3pos=[cxl1,cyb2,cxr1,cyt2]
p4pos=[xl2,yb2,xr2,yt2]
c4pos=[cxl2,cyb2,cxr2,cyt2]
yt3=cyb2-dy;0.30496875
yb3= yt3-yh;   0.45603125
cyt3= yb3-dypb  ;  0.28056875
cyb3=cyt3-.02;0.25496875
;yt3=      0.45603125
;yb3=      0.30496875
;cyt3=      0.28056875
;cyb3=      0.25496875
p5pos=[xl1,yb3,xr1,yt3]
c5pos=[cxl1,cyb3,cxr1,cyt3]
p6pos=[xl2,yb3,xr2,    yt3]
c6pos=[cxl2,cyb3,cxr2,cyt3]
yt4=cyb3-dy;-.01;0.30496875
yb4= yt4-yh;   0.45603125
cyt4= yb4-dypb  ;  0.28056875
cyb4=cyt4-.02;0.25496875
;yt4=      0.20603125
;yb4=     0.054968750
;cyt4=     0.055600000
;cyb4=     0.029999999
p7pos=[xl1,   yb4   ,xr1,    yt4]
c7pos=[cxl1,   cyb4  ,cxr1,   cyt4]
p8pos=[xl2, yb4  ,xr2,yt4]
c8pos=[cxl2,    cyb4  ,cxr2,cyt4]

poslist=list(p1pos,p2pos,p3pos,p4pos,p5pos,p6pos,p7pos,p8pos)
cposlist=list(c1pos,c2pos,c3pos,c4pos,c5pos,c6pos,c7pos,c8pos)

labes=['(a)','(b)','(c)','(d)','(e)','(f)','(g)','(h)','(i)']
;lpos=[-5000,10500]


lposxlist=list(tx1,tx2,tx1,tx2,tx1,tx2,tx1,tx2)
lposylist=list(yt1,yt1,yt2,yt2,yt3,yt3,yt4,yt4)
    	ww = WINDOW(DIMENSIONS=[900,1200])
	;;; Shock Jump

	lnum=2;1

	;cpos1=[0.084285714 ,0.75496875 ,0.45000000 ,0.77]

	;help,datB2B1
	zttl=datB2B1.ytitle
	ZT=datB2B1.YN[0]
	B2B1=datB2B1.y
	zmax=max(B2B1)
	zbyt=bytscl(B2B1,min=1,max=ceil(zmax))
	p10=scatterplot(x,y,magnitude=zbyt,xtitle=xttl,ytitle=yttl,$
				XTICKFONT_SIZE=7,YTICKFONT_SIZE=7,$
				SYMBOL='dot', /SYM_FILLED,sym_size=psize ,$
				/curr,layout=[3,4,lnum],RGB_TABLE=72)
	p10.aspect_ratio=1

cb1=colorbar(ORIENTATION=0,RGB_TABLE=72,range=[1,ceil(zmax)],title=zttl,target=p10)
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over)
	tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231])
	;print,t1.pos
	p1p=p10.pos
	c1p=cb1.pos
	print,'xl1=',p1p[0]
	print,'xr1=',p1p[2]
	print,'cxl1=',c1p[0]
	print,'cxr1=',c1p[2]

	print,'yt1=',p1p[3]
	print,'yb1=',p1p[1];
	print,'cyt1=',c1p[3];
	print,'cyb1=',c1p[1]
	PRINT,"pJpos=[",p10.pos
	PRINT,"cJpos=[",cb1.pos

	;;; Amplitude
	lnum=3;2
	;help,datB2B1
	zttl=datA.ytitle
	ZT=datA.YN[0]
	A=datA.y
	zmax=max(A)
	zbyt=bytscl(A,min=0,max=4)
	p20=scatterplot(x,y,magnitude=zbyt,xtitle=xttl,ytitle=yttl,$
				XTICKFONT_SIZE=7,YTICKFONT_SIZE=7,$
				SYMBOL='dot', /SYM_FILLED,sym_size=psize ,$
				/curr,layout=[3,4,lnum],RGB_TABLE=72)
	p20.aspect_ratio=1
	t2=text(/data,-5000,10500,labes[lnum-1],align=.5,target=p20)
	cb2=colorbar(target=p20,ORIENTATION=0,RGB_TABLE=72,range=[0,4],title=zttl)
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],target=p20)
	tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231],target=p20)
	PRINT,"pApos=p20.pos
	PRINT,"cApos=cb2.pos

	p2p=p20.pos
	c2p=cb2.pos
;	print,'xl2=',p2p[0]
;	print,'xr2=',p2p[2]
;	print,'cxl2=',c2p[0]
;	print,'cxr2=',c2p[2]

	;;;  Mfms
	lnum=5;3
	zttl=datMfms.ytitle
	ZT=datMfms.YN[0]
	Mfms=datMfms.y
	zmax=max(Mfms)
	zmin=min(Mfms)
	zbyt=bytscl(Mfms,min=floor(zmin),max=ceil(zmax))
	p30=scatterplot(x,y,magnitude=zbyt,xtitle=xttl,ytitle=yttl,$
				XTICKFONT_SIZE=7,YTICKFONT_SIZE=7,$
				SYMBOL='dot', /SYM_FILLED,sym_size=psize ,$
				/curr,layout=[3,4,lnum],RGB_TABLE=72)
	p30.aspect_ratio=1
	cb3=colorbar(target=p30,ORIENTATION=0,RGB_TABLE=72,range=[floor(zmin),ceil(zmax)],title=zttl)
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over,target=p30)
	tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231],target=p30)
	;PRINT,"pMpos=[",p30.pos
	;PRINT,"cMpos=[",cb3.pos
	p3p=p30.pos
	c3p=cb3.pos
;	print,'yt2=',p3p[3]
;	print,'yb2=',p3p[1]
;	print,'cyt2=',c3p[3]
;	print,'cyb2=',c3p[1]
;;;  M_A
	lnum=6;4
	zttl=datMA.ytitle
	ZT=datMA.YN[0]
	MA=datMA.y
	zmax=max(MA)
	zmin=min(MA)
	zbyt=bytscl(MA)
	p40=scatterplot(x,y,magnitude=zbyt,xtitle=xttl,ytitle=yttl,$
				XTICKFONT_SIZE=7,YTICKFONT_SIZE=7,$
				SYMBOL='dot', /SYM_FILLED,sym_size=psize ,$
				/curr,layout=[3,4,lnum],RGB_TABLE=72)
	p40.aspect_ratio=1
	cb4=colorbar(target=p40,ORIENTATION=0,RGB_TABLE=72,range=[zmin,zmax],title=zttl)
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over,target=p40)
	tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231],target=p40)
	;PRINT,cb4.pos
	;PRINT,"pMApos=[",p40.pos
	;PRINT,"cMApos=[",cb4.pos

;;; V_N
	lnum=7;5
	zttl=datVN.ytitle
	ZT=datVN.YN[0]
	VN=datVN.y
	zmax=max(VN)
	zmin=min(VN)
	zbyt=bytscl(VN)
	p50=scatterplot(x,y,magnitude=zbyt,xtitle=xttl,ytitle=yttl,$
				XTICKFONT_SIZE=7,YTICKFONT_SIZE=7,$
				SYMBOL='dot', /SYM_FILLED,sym_size=psize ,$
				/curr,layout=[3,4,lnum],RGB_TABLE=72)
	p50.aspect_ratio=1
	cb5=colorbar(target=p50,ORIENTATION=0,RGB_TABLE=72,range=[zmin,zmax],title=zttl)
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over,target=p50)
	tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231],target=p50)
	PRINT,"pVpos=[",p50.pos
	PRINT,"cVpos=[",cb5.pos
	p5p=p50.pos
	c5p=cb5.pos
;	print,'yt3=',p5p[3]
;	print,'yb3=',p5p[1]
;	print,'cyt3=',c5p[3]
;	print,'cyb3=',c5p[1]
;;; TH_BN
	lnum=4;6
	zttl=datTH.ytitle
	ZT=datTH.YN[0]
	TH=datTH.y
	zmax=max(TH)
	zmin=min(TH)
	zbyt=bytscl(TH)
	p60=scatterplot(x,y,magnitude=zbyt,xtitle=xttl,ytitle=yttl,$
				XTICKFONT_SIZE=7,YTICKFONT_SIZE=7,$
				SYMBOL='dot', /SYM_FILLED,sym_size=psize ,$
				/curr,layout=[3,4,lnum],RGB_TABLE=72)
	p60.aspect_ratio=1
	cb6=colorbar(target=p60,ORIENTATION=0,RGB_TABLE=72,range=[zmin,zmax],title=zttl)
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over,target=p60)
	tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231],target=p60)
	PRINT,"pTHpos=[",p60.pos
	PRINT,"cTHpos=[",cb6.pos


;;; FM
	lnum=8;5;7
	zttl=datFM.ytitle
	ZT=datTH.YN[0]
	FM=datFM.y
	zmax=max(FM)
	zmin=min(FM)
	zbyt=bytscl(FM)
	p70=scatterplot(x,y,magnitude=zbyt,xtitle=xttl,ytitle=yttl,$
				XTICKFONT_SIZE=7,YTICKFONT_SIZE=7,$
				SYMBOL='dot', /SYM_FILLED,sym_size=psize ,$
				/curr,layout=[3,4,lnum],RGB_TABLE=72)
	p70.aspect_ratio=1
	cb7=colorbar(target=p70,ORIENTATION=0,RGB_TABLE=72,range=[zmin,zmax],title=zttl)
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over,target=p70)
	tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231],target=p70)
	PRINT,"pFMpos=[",p70.pos
	PRINT,"cFMpos=[",cb7.pos
	p7p=p70.pos
	c7p=cb7.pos
;	print,'yt4=',p7p[3]
;	print,'yb4=',p7p[1]
;	print,'cyt4=',c7p[3]
;	print,'cyb4=',c7p[1]
;;; DIST
	;get_data,'SolDistNorm',data=datDist
	get_data,'solsticeDist',data=datDist
	lnum=1
	zttl=datDist.ytitle
	ZT=datDist.YN[0]
	Dist=datDist.y
	zmax=max(Dist)
	zmin=min(Dist)
	zbyt=bytscl(Dist)
	p80=scatterplot(x,y,magnitude=zbyt,xtitle=xttl,ytitle=yttl,$
				XTICKFONT_SIZE=7,YTICKFONT_SIZE=7,$
				SYMBOL='dot', /SYM_FILLED,sym_size=psize ,$
				/curr,layout=[3,4,lnum],RGB_TABLE=72)
	p80.aspect_ratio=1
	cb8=colorbar(target=p80,ORIENTATION=0,RGB_TABLE=72,range=[zmin,zmax],title=zttl)
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over,target=p80)
	
	tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231],target=p80)

;;; Mflow
	lnum=9
	get_data,'Mflow',data=datMflow
	zttl=datMflow.ytitle
	ZT=datMflow.YN[0]
	Mflow=datMflow.y
	zmax=max(Mflow)
	zmin=min(Mflow)
	zbyt=bytscl(Mflow)
	p80=scatterplot(x,y,magnitude=zbyt,xtitle=xttl,ytitle=yttl,$
				XTICKFONT_SIZE=7,YTICKFONT_SIZE=7,$
				SYMBOL='dot', /SYM_FILLED,sym_size=psize ,$
				/curr,layout=[3,4,lnum],RGB_TABLE=72)
	p80.aspect_ratio=1
	cb8=colorbar(target=p80,ORIENTATION=0,RGB_TABLE=72,range=[zmin,zmax],title=zttl)
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over,target=p80)
	
	tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231],target=p80)

;	PRINT,"pDpos=[",p80.pos
;	PRINT,"cDpos=[",cb8.pos
	;t1=text(/relative,cxl1+.01,yt1-.007,'(a)',align=.5,FONT_STYLE=1)
	;t2=text(/relative,cxl2,yt1-.007,'(b)',align=.5,FONT_STYLE=1)
	;t3=text(/relative,cxl1+.01,yt2-.007,'(c)',align=.5,FONT_STYLE=1)
	;t4=text(/relative,cxl2,yt2-.007,'(d)',align=.5,FONT_STYLE=1)
	;t5=text(/relative,cxl1+.01,yt3-.007,'(e)',align=.5,FONT_STYLE=1)
	;t6=text(/relative,cxl2,yt3-.007,'(f)',align=.5,FONT_STYLE=1)
	;t7=text(/relative,cxl1+.01,yt4-.007,'(g)',align=.5,FONT_STYLE=1)
	;t8=text(/relative,cxl2,yt4-.007,'(h)',align=.5,FONT_STYLE=1)


;; Beta
	lnum=10
	get_data,12,data=datBeta
	zttl=datBeta.ytitle
	ZT=datBeta.YN[0]
	Bta=alog10(datBeta.y)
	zmax=max(Bta)
	zmin=min(Bta)
	zbyt=bytscl(Bta)
	p80=scatterplot(x,y,magnitude=zbyt,xtitle=xttl,ytitle=yttl,$
				XTICKFONT_SIZE=7,YTICKFONT_SIZE=7,$
				SYMBOL='dot', /SYM_FILLED,sym_size=psize ,$
				/curr,layout=[3,4,lnum],RGB_TABLE=72)
	p80.aspect_ratio=1
	cb8=colorbar(target=p80,ORIENTATION=0,RGB_TABLE=72,range=[zmin,zmax],title=zttl)
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over,target=p80)
	
	tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231],target=p80)

	;;Beta_ion

	lnum=10
	get_data,13,data=datBeta
	zttl=datBeta.ytitle
	ZT=datBeta.YN[0]
	Bta=alog10(datBeta.y)
	zmax=max(Bta)
	zmin=min(Bta)
	zbyt=bytscl(Bta)
	p80=scatterplot(x,y,magnitude=zbyt,xtitle=xttl,ytitle=yttl,$
				XTICKFONT_SIZE=7,YTICKFONT_SIZE=7,$
				SYMBOL='dot', /SYM_FILLED,sym_size=psize ,$
				/curr,layout=[3,4,lnum],RGB_TABLE=72)
	p80.aspect_ratio=1
	cb8=colorbar(target=p80,ORIENTATION=0,RGB_TABLE=72,range=[zmin,zmax],title=zttl)
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over,target=p80)
	
	tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231],target=p80)

;; beta_e

	lnum=10
	get_data,14,data=datBeta
	zttl=datBeta.ytitle
	ZT=datBeta.YN[0]
	Bta=alog10(datBeta.y)
	zmax=max(Bta)
	zmin=min(Bta)
	zbyt=bytscl(Bta)
	p80=scatterplot(x,y,magnitude=zbyt,xtitle=xttl,ytitle=yttl,$
				XTICKFONT_SIZE=7,YTICKFONT_SIZE=7,$
				SYMBOL='dot', /SYM_FILLED,sym_size=psize ,$
				/curr,layout=[3,4,lnum],RGB_TABLE=72)
	p80.aspect_ratio=1
	cb8=colorbar(target=p80,ORIENTATION=0,RGB_TABLE=72,range=[zmin,zmax],title=zttl)
	marsP=ellipse(/data,0,0,ecc=0,major=R_mars,fill_color=[193,68,14],/over,target=p80)
	
	tmars=text(/data,0,500,"Mars",align=.5,color=[240,231,231],target=p80)

	tmars.save,dire+'parameter_projections3.png',res=600
	p10.close
end
