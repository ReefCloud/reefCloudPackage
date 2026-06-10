##############################################################################################
## The following function is a wrapper for saving data to the aws bucket_exists             ##
##                                                                                          ##
## Arguments:                                                                               ##
##   - file:   a string representation of the name of the file to retrieve from the bucket. ##
##   - level:  a string representation of the name of the processing level                  ##
##             (primary, processed, modelled)                                               ##
##############################################################################################
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
write_aws <- function(file, level) {
    # Use safe_aws instead of system() (Suggestion 60)
    source_path <- paste0(DATA_PATH, level, file)
    dest_path <- paste0(AWS_PATH, level, file)

    result <- reefCloudPackage::safe_aws(
      args = c("s3", "cp", source_path, dest_path),
      profile = "stats"
    )

    return(result$status == 0)
}
