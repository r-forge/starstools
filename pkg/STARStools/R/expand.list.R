expand.list = function(list1,list2)
{
	list1_N=length(list1)
	list2_N=length(list2)
	
	list1_ids=1:list1_N
	list2_ids=1:list2_N
	
	listcombo_ids=as.data.frame(t(expand.grid(list1_ids,list2_ids)))
	
	listcombo_N=length(listcombo_ids)
	
	listcombo=mapply(function(list1,list2,list_ids) { list(list1[[list_ids[1]]],list2[[list_ids[2]]]) }
				,list_ids=listcombo_ids,MoreArgs=list(list1=list1,list2=list2),SIMPLIFY=FALSE)
		
	return(listcombo)
	
}
