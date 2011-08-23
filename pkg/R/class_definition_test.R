# TODO: Add comment
# 
# Author: jonathan
###############################################################################


require("zoo")

time.zooList=function(x,...)
{
	newlist=mapply(time,x@zooList,MoreArgs=list(...),simplify=FALSE)
	return(newlist)
}

setClass("zooList",representation(zooList="list"))
setMethod(f="time",
		signature="zooList",
		definition=time.zooList)

# Now test:
x.date <- as.Date(paste(2003, 2, c(1, 3, 7, 9, 14), sep = "-"))
x <- zoo(rnorm(5), x.date)

zooListData=list(x,x,x)

zooListNew=new("zooList",zooList=zooListData)


time(zooListNew)