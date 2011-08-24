#' Extract the list from a SpatialPointsDataFrameList object.
#'
#' @name as.list
#' @aliases as.list,SpatialPointsDataFrameList-method
#' @docType methods
#' @rdname as.list.SpatialPointsDataFrameList-methods
#'
setMethod("as.list",
    signature(x = "SpatialPointsDataFrameList"),
	as.list.SpatialPointsDataFrameList
)
