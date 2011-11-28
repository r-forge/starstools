write_raster_list=function(raster_list,prefix,outdirectory,...)
{
	if(missing(outdirectory))
	{
		outdirectory=getwd()
		olddir=getwd()
	} else
	{
		olddir=getwd()
		setwd(outdirectory)
	}
	
	if(missing(prefix))
	{
		print("Please supply a prefix")
		return()
	}
	
	if(class(raster_list) != "list")
	{
		print("raster_list must be a list")
		return()
		
	}
	
	raster_ids=1:length(raster_list)
	raster_ids=add_leading_zeroes(number=raster_ids)
	
	outprefixes=as.list(paste(prefix,raster_ids,sep=""))
	
	written_raster_list=mapply(writeRaster,x=raster_list,filename=outprefixes,MoreArgs=list(...))
	
	
	setwd(olddir)
	return(written_raster_list)
}
