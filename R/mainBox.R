#' @title Function
#' @description Description
#' @param parameters description
#' @return returned arguments description
#' @examples examples
#' @export
mainBox <- function(settings.box.text, box.width, settings.box.width, box.margins) {
    main.box.text <- c("ReefCloud Statistical Modelling", "")
    ## format the title to be centered
    for (i in 1:length(main.box.text))
        main.box.text[i] <- cli::ansi_align(main.box.text[i],
                                            width = box.width - settings.box.width - 1,
                                            align = 'center')
    if (all(runStage > 0)) {
        ## add the stages as left justified
        for (j in 1:length(runStage)) {
            main.box.text <- c(main.box.text,
                               cli::ansi_align(STATUS[[paste0("STAGE",runStage[j])]]$title,
                                               width = box.width - settings.box.width - 1,
                                               align = 'left')
                               )
            if (length(STATUS[[paste0("STAGE", runStage[j])]]$items) == 0) next
            for (i in 1:length(STATUS[[paste0("STAGE",runStage[j])]]$items)) {
                if (runStage[j] == CURRENT_STAGE |
                    STATUS[[paste0("STAGE",runStage[j])]]$status[i] == 'failure') {
                    main.box.text <- c(main.box.text,
                                       cli::ansi_align(
                                                paste0(strrep(" ", box.margins),
                                                       switch(STATUS[[paste0("STAGE",runStage[j])]]$status[i],
                                                              'pending' = crayon::white(cli::symbol$line),
                                                              'progress' = crayon::magenta("\u23F1"),
                                                              'success' = crayon::green(cli::symbol$tick),
                                                              'failure' = crayon::red(cli::symbol$cross)
                                                              ),
                                                       " ", crayon::blue(STATUS[[paste0("STAGE",runStage[j])]]$label[i])
                                                       ),
                                                width = box.width - settings.box.width - 1,
                                                align = 'left'
                                            )
                                       )
                }

            }
        }
    }
    main.box.nchar <- nchar(main.box.text)
    main.box.text
}
