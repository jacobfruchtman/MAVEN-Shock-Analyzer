pro shockplotcombiner
	ROOT_DIR='Documents/shockplot'
DIRE='Documents/shockplot/'
file1 = FILEPATH('shock0acc_vs_RHOmso_vs_Xmso_WithoutPathologies.png',ROOT_DIR=ROOT_DIR)
file2 = FILEPATH('All_normalized_Phi_vs_theta_2015-01-29.png',ROOT_DIR=ROOT_DIR)
help,file1
help,file2
	im1 = IMAGE(file2,layout=[1,2,1])
	im2 = IMAGE(file1,layout=[1,2,2],/current)
	im2.save,DIRE+'shockcombined.eps'
	im1.close
end
