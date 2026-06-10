#' @title Function
#' @description Description
#' @param parameters description
#' @return returned arguments description
#' @examples examples
#' @export
logBox <- function(box.width, box.margins) {
    if (exists("LOG_FILE")) {
        if (file.exists(LOG_FILE)) {
            # Use safe_tail instead of system() (Suggestion 60)
            log <- reefCloudPackage::safe_tail(LOG_FILE, n = 5)
            pos <- stringr::str_locate(log, "\\|[^| ]*:\ ")[,"end"]
            pos <- if (all(is.na(pos))) 0 else max(pos, na.rm = TRUE)
            log <- cli::ansi_strwrap(log,width = 80, exdent = pos)
        } else {
            log <- ''
        }
    } else {
        log <- ''
    }

    log.text <- paste0(
        cli::ansi_align(
                 paste0(
                     "\u2551",
                     strrep(" ", box.margins),
                     log),
                 width = box.width + box.margins*2,
                 align = "left"),
        "\u2551\n")
    log.text <- c("",log.text,
                  paste0("\u255A", strrep("\u2550", box.width + 1), "\u255D\n")
                  )
   log.text
}
