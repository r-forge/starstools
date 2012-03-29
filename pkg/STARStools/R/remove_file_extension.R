#' remove_file_extension
#' @param filename TODO
#' @param extension_delimiter TODO
#' @export


remove_file_extension=function(filename,extension_delimiter=".")
{
	split_filename=unlist(strsplit(filename,extension_delimiter,fixed=TRUE))
	split_filename_length=length(split_filename)
	if(split_filename_length==1)
	{
		return(split_filename[1])
	} else
	{
		return(paste(as.character(split_filename)[1:(split_filename_length-1)],collapse=extension_delimiter))	
	}
}
