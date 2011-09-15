#' as.data.frame
setMethod("as.data.frame",
    signature(x = "SpatialPointsDataFrameList"),
	as.data.frame.SpatialPointsDataFrameList
)
