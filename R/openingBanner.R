#' @title Opennig banner
#' @description Display an opening banner
#' @examples openingBanner()
#' @export
openingBanner <- function() {
    # Use ANSI escape codes instead of system('clear') (Suggestion 60)
    cat("\033[2J\033[H")
    cat(" ")
    currentTime <- format(Sys.time(),'%d/%m/%Y %H:%M:%S')

    STATUS$SETTINGS$items <- c(STATUS$SETTINGS$items, 'currentTime')
    STATUS$SETTINGS$labels <- c(STATUS$SETTINGS$labels, 'Date/Time')
    STATUS$SETTINGS$status <- c(STATUS$SETTINGS$status, 'success')

    box.style <- cli:::box_styles()
    box.width <- 80
    box.margins <- 1

    ## get the width of the path box
    max.settings.width <- 40
    settings.box.nchar <-nchar(
        paste0(STATUS$SETTINGS$labels, ': ',sapply(STATUS$SETTINGS$items, function(x) eval(parse(text = x))))
    )
    ## we want to ensure that the settings box has a max
    ## width of 40 characters. We will center truncate the strings
    settings.box.nchar <- pmin(settings.box.nchar, max.settings.width)
    settings.box.width <- max(settings.box.nchar) +
        2 +              # add one for the status character
        box.margins*2    # add the left and right margin

    ## Outer box (top)
    top <- reefCloudPackage::outerBox.top(box.width, settings.box.width)

    ## Settings box
    settings.box.text <- reefCloudPackage::settingsBox(settings = STATUS$SETTINGS,
                                     box.width = settings.box.width,
                                     box.nchar = settings.box.nchar,
                                     box.margins = box.margins,
                                     currentTime,
                                     max.settings.width)
    ## Main box
    main.box.text <- reefCloudPackage::mainBox(settings.box.text,
                                  box.width,
                                  settings.box.width,
                                  box.margins)
    ## Outer box (bottom)
    bottom <- reefCloudPackage::outerBox.bottom(box.width, settings.box.width)

    ## Combine boxes
    combined.boxes.text <- reefCloudPackage::combinedBoxes(
        top,
        settings.box.text,
        main.box.text,
        bottom,
        box.width,
        settings.box.width,
        box.margins)

    cat(combined.boxes.text)

    ## log box
    log.box <- reefCloudPackage::logBox(box.width, box.margins)
    cat(log.box)
}
