
#' @title Function
#' @description Description
#' @param parameters description
#' @return returned arguments description
#' @examples examples
#' @export
loadRData <- function() {
    system('aws s3 ls s3://arn:aws:s3:ap-southeast-2:255329909679:accesspoint/nectar/stats/photo-transect/2021-01-14/process/Palau/XXXX/XXXX/reef/Kayangel/')
}
