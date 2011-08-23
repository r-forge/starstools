# TODO: Add comment
# 
# Author: jonathan
###############################################################################

require("sp")
require("zoo")
setClass("SpatialPointsDataFrameList",representation(list="list"),contains=c("SpatialPointsDataFrame"))
setClass("SpatialPointsDataFrameListZoo",contains=c("SpatialPointsDataFrameList"))

### Methods for SpatialPointsDataFrameList
setMethod(f="as.list",
		signature="SpatialPointsDataFrameList",
		definition=as.list.SpatialPointsDataFrameList)
setMethod(f="as.data.frame",
		signature="SpatialPointsDataFrameList",
		definition=as.data.frame.SpatialPointsDataFrameList)

### Methods for SpatialPointsDataFrameListZoo
setMethod(f="aggregate",
		signature="SpatialPointsDataFrameListZoo",
		definition=aggregate.SpatialPointsDataFrameListZoo)

setMethod(f="index",
		signature="SpatialPointsDataFrameListZoo",
		definition=index.SpatialPointsDataFrameListZoo)

setMethod(f="time",
		signature="SpatialPointsDataFrameListZoo",
		definition=time.SpatialPointsDataFrameListZoo)

setMethod(f="as.yearmon",
		signature="SpatialPointsDataFrameListZoo",
		definition=as.yearmon.SpatialPointsDataFrameListZoo)

setMethod(f="as.Date",
		signature="SpatialPointsDataFrameListZoo",
		definition=as.Date.SpatialPointsDataFrameListZoo)

setMethod(f="MATCH",
		signature="SpatialPointsDataFrameListZoo",
		definition=MATCH.SpatialPointsDataFrameListZoo)

setMethod(f="subset",
		signature="SpatialPointsDataFrameListZoo",
		definition=subset.zoo.SpatialPointsDataFrameListZoo)
