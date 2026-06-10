#' @title Combine boxes
#' @description combines boxes
#' @param top top text
#' @param settings.box.text settings of box text
#' @param main.box.text main text
#' @param bottom bottom text
#' @param box.width box width
#' @param settings.box.width settings box width
#' @param box.margins box margins
#' @return a vector of combined texts
#' @export
combinedBoxes <- function(top, settings.box.text, main.box.text,
                          bottom, box.width, settings.box.width, box.margins) {
  combined.text <- NULL
  for (i in 1:max(length(settings.box.text), length(main.box.text))) {
    combined.text <- c(combined.text,
                       paste0(
                         ifelse(i>length(settings.box.text),
                                paste0("\u2551",
                                       cli::ansi_align("", width = settings.box.width, align = 'center'),
                                       "\u2502",
                                       strrep(" ", box.margins)),
                                settings.box.text[i]),
                         ifelse(i>length(main.box.text),
                                cli::ansi_align("", width = box.width - settings.box.width - 1, align = 'center'),
                                main.box.text[i]),
                         "\u2551",
                         "\n"))
  }
  combined.text <- c(top,combined.text,bottom)
}
