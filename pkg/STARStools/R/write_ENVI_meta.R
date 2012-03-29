

write_ENVI_meta=function(infiles,outname,sortby)
{
	require("raster")
	require("R.utils")
	# We eventually need to check to make sure the files are "legit" for an EMF.
	
	# Make sure the files are present.
	infiles_check=sapply(infiles,file.exists)
	if(prod(infiles_check)==0)
	{
		print("Some files are missing:")
		print(infiles[!infiles_check])
		print("Please check the path and the files.  Exiting...")
		return()
	}
	
	# Now make sure the full paths are there...
	infiles_full=sapply(infiles,getAbsolutePath,USE.NAMES=FALSE)
	
	# Get the info about the rasters...
	all_files=sapply(infiles,brick,simplify=FALSE)
	
	num_bands=sapply(all_files,nbands)
	xdims=sapply(all_files,ncol)
	ydims=sapply(all_files,nrow)
	
	# Now write the EMF.
	filecon=file(outname,"w")
	writeLines("ENVI META FILE",con=filecon)
	for(i in 1:length(infiles))
	{
		file_line=paste("File : ",infiles_full[i],sep="")
		bands_line=paste("Bands : ",num_bands[i],sep="")
		dims_line=paste("Dims : ","1","-",xdims[i],",","1","-",ydims[i],sep="")
		writeLines(file_line,con=filecon)
		writeLines(bands_line,con=filecon)
		writeLines(dims_line,con=filecon)
		writeLines("",con=filecon)
	}
	close(filecon)
	
}


