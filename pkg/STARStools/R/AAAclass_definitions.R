#' An S4 class that expands a SpatialPointsDataFrame to include a list.
# @slot list 
#' @export
setClass("SpatialPointsDataFrameList",representation(list="list"),contains=c("SpatialPointsDataFrame"))

#' An S4 class that is a specialized SpatialPointsDataFrameList where each list is a zoo object.
#' @export
setClass("SpatialPointsDataFrameListZoo",contains=c("SpatialPointsDataFrameList"))
