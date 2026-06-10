#' @title load_aws
#' @description The following function is a wrapper for loading data from the aws bucket_exists
#' @param file a string representation of the name of the file to retrieve from the bucket
#' @param level a string representation of the name of the processing level (primary, processed, modelled)
#' @return NULL
#' @export
load_aws <- function(file, level, col_types) {
    # Use safe_aws instead of system() (Suggestion 60)
    source_level <- ifelse(level == 'primary/', 'raw/', level)
    source_path <- paste0(AWS_PATH, source_level, file)
    dest_path <- paste0(DATA_PATH, level, file)

    result <- reefCloudPackage::safe_aws(
      args = c("s3", "cp", source_path, dest_path),
      profile = "stats"
    )

    return(result$status == 0)
}
