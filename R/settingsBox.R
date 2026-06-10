
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

#' @title Function 
#' @description Description 
#' @param parameters description
#' @return returned arguments description
#' @examples examples 
#' @export
settingsBox <- function(settings, box.width, box.nchar, box.margins, currentTime, max.settings.width) {
    box.text <- NULL
    keys <- settings$labels
    values <- sapply(settings$items, function(x) eval(parse(text = x)))
    ## ensure no values exceed the max width
    values <- suppressWarnings(sapply(1:length(values), function(i)
        stringr::str_trunc(values[i], width = max.settings.width - nchar(keys[i]) - 2, side = "center")))
    status <- settings$status
    for (i in 1:length(keys)) {
        box.text <- c(box.text,
                           paste0("\u2551",
                                  strrep(" ", box.margins),
                                  switch(status[i],
                                         'pending' = crayon::white(cli::symbol$line),
                                         'success' = crayon::green(cli::symbol$tick),
                                         'failure' = crayon::red(cli::symbol$cross)
                                         ),
                                  " ", crayon::blue(keys[i]), ": ",
                                  crayon::white(values[i]),
                                  strrep(" ", box.width - (box.nchar[i])-box.margins*2 -1),
                                  "\u2502",
                                  strrep(" ", box.margins)
                                  )
                           )
    }
    box.text
}
