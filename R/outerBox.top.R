#' @title Function
#' @description Description
#' @param parameters description
#' @return returned arguments description
#' @examples examples
#' @export
outerBox.top <- function(outer.box.width, this.box.width) {
  top <- paste0("\u2554",
                strrep("\u2550", this.box.width),
                "\u2564",
                strrep("\u2550", outer.box.width - this.box.width),
                "\u2557",
                "\n"
  )
  top
}
