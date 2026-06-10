
#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
dev_data_cut <- function(data, tier4) {
    data %>% filter(Tier4 == tier4) %>%
        droplevels()
}
