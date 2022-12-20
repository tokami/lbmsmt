
#' @name plotLBSPR.data
#' @title plot LBSPR data
#' @details something
#' @param lbspr_dat data
#' @param input input
#' @return something
#' @export
plotLBSPR.data <- function(lbspr_dat, input){
    dat <- lbspr_dat$dataExplo[['lfq']]
    ny <- length(dat$dates)
    if(ny >= 19){
        mfrow = c(4,ceiling(ny/4))
    }else if(ny >= 9){
        mfrow = c(3,ceiling(ny/3))
    }else if(ny < 9 & ny >= 4){
        mfrow = c(2,ceiling(ny/2))
    }else if(ny < 4){
        mfrow = c(1,ny)
    }
    par(mfrow = mfrow, mar = c(3,3,2,1), oma = c(3,3,1,1))
    x <- dat$midLengths
    bin.width <- diff(dat$midLengths)
    bin.lower <- dat$midLengths - c(bin.width[1], bin.width)/2
    bin.upper <- dat$midLengths + c(bin.width, bin.width[length(bin.width)])/2
    xlim <- range(0,dat$midLengths)
    ylim <- c(0,1.1) * range(dat$catch)
    for(i in 1:ny){
        if(ny == 1){
            y <- dat$catch
        }else{
            y <- dat$catch[,i]
        }
        plot(x, y, ty='n',
             xlim = xlim,
             ylim = ylim,
             yaxs="i", xaxs="i",
             xlab = "", ylab = "")
        for(j in seq(dat$midLengths)){
            polygon(
                x = c(bin.lower[j], bin.upper[j], bin.upper[j], bin.lower[j]),
                y = c(0, 0, y[j], y[j]),
                col = "grey80",
                border = "grey20", lwd = 1)
        }
        mtext(format(dat$dates[i], "%Y"), 3, 0.5, font = 2)
        mtext(paste0("Mid lengths [", input$LBSPR_lengthUnit,"]"), 1, 1, outer = TRUE)
        mtext("Frequency", 2, 1, outer = TRUE)
        box(lwd = 1.5)
    }
}




#' @name plotLBSPR.hist
#' @title plot LBSPR hist
#' @details something
#' @param lbspr_dat data
#' @param input input
#' @return something
#' @export
plotLBSPR.hist <- function(lbspr_dat, input){

    modfit <- lbspr_dat$results

    if(!is.null(modfit)){
        LBSPR::plotSize(modfit)
    }
}




#' @name plotLBSPR.sel
#' @title plot LBSPR sel
#' @details something
#' @param lbspr_dat data
#' @param input input
#' @return something
#' @export
plotLBSPR.sel <- function(lbspr_dat, input){

    modfit <- lbspr_dat$results

    if(!is.null(modfit)){
        LBSPR::plotMat(modfit, useSmooth = TRUE, Title = NULL)
    }
}




#' @name plotLBSPR.pie
#' @title plot LBSPR pie
#' @details something
#' @param lbspr_dat data
#' @param input input
#' @return something
#' @export
plotLBSPR.pie <- function(lbspr_dat, input){

    modfit <- lbspr_dat$results

    if(!is.null(modfit)){
        suppressMessages({
            LBSPR::plotSPRCirc(modfit,
                               SPRTarg = input$LBSPR_sprTarg,
                               SPRLim = input$LBSPR_sprLim,
                               useSmooth = TRUE,
                               Title = FALSE, Leg = FALSE,
                               limcol = "firebrick4",
                               targcol = "forestgreen",
                               abtgcol = "goldenrod2",
                               labcol = if(modfit@Ests[nrow(modfit@Ests),"SPR"] < input$LBSPR_sprLim){
                                            "firebrick4"} else if(modfit@Ests[nrow(modfit@Ests),"SPR"] < input$LBSPR_sprTarg){"forestgreen"}else{"goldenrod2"},
                               texcex = 1,
                               labcex = 1.2)
        })
    }
}



#' @name plotLBSPR.ts
#' @title plot LBSPR ts
#' @details something
#' @param lbspr_dat data
#' @param input input
#' @return something
#' @export
plotLBSPR.ts <- function(lbspr_dat, input){

    modfit <- lbspr_dat$results

    if(!is.null(modfit)){
        LBSPR::plotEsts(modfit, Lwd = 2, doSmooth = TRUE, incL50 = FALSE)
    }
}
