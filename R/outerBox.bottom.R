#' @title Function
#' @description Description
#' @param parameters description
#' @return returned arguments description
#' @examples examples
#' @export
outerBox.bottom <- function(outer.box.width, this.box.width) {
  bottom <- paste0("\u2560",
                   strrep("\u2550", this.box.width),
                   '\u2567',
                   strrep("\u2550", outer.box.width - this.box.width),
                   "\u2563",
                   "\n"
  )
  bottom
}
