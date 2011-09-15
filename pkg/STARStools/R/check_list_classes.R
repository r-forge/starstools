#' check_list_classes
#' @param list TODO
#' @param class_to_check TODO
#' @param check_inheritance TODO
#' @export

check_list_classes=function(list,class_to_check,check_inheritance=FALSE)
{
	if(!check_inheritance)
	{
		list_classes=sapply(list,class)
		list_test_unique=unique(list_classes)
		if(length(list_test_unique)!=1 || list_test_unique[1]!=class_to_check)
		{
			return(FALSE)
		} else
		{
			return(TRUE)
		}
	} else
	{
		list_inheritance_check=mapply(inherits,list,MoreArgs=list(what=class_to_check))
		if(FALSE %in% list_inheritance_check)
		{
			return(FALSE)
		} else
		{
			return(TRUE)
		}
	}
}
