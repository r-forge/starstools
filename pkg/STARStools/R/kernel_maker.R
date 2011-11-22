# What to do with even sized 


kernel_maker=function(kernelx=3,kernely=3,kernelz=1,shape="circular",outformat="ENVI",
		outkernelfname="mykernel.ker")
{
	if(shape=="circular")
	{
		x_vector=(-(floor(kernelx/2))):((floor(kernelx/2)))
		kernel_matrix_x=matrix(data=x_vector,nrow=kernelx,ncol=kernely)
		kernel_matrix_y=t(kernel_matrix_x)
		kernel_matrix_distance=sqrt(kernel_matrix_x^2+kernel_matrix_y^2)
		kernel_weights=kernel_matrix_distance<=floor(kernelx/2)
		storage.mode(kernel_weights) <- "double"
	}
	# Other shapes here
	
	
	if(outformat=="ENVI")
	{
		write.table(kernel_weights,file=outkernelfname,row.names=FALSE,col.names=FALSE)
		
	}
	# Other formats here (e.g. raster)
	
	return(kernel_weights)	
}



