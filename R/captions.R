
#' @name captionLBSPR.tables
#' @title caption LBSPR tables
#' @details something
#' @param lbspr_dat data
#' @param input input
#' @param format format
#' @param type type
#' @return something
#' @export
captionLBSPR.tables <- function(lbspr_dat, input, format = "datatable", type){

    switch(type,
           "input" = {
               tab.num <- 0
               txt <- paste0("Input parameters.")
           },
           "res" = {
               tab.num <- 1
               if(format == "datatable"){
                   txt <- paste0("Results of the LBSPR method. Estimated Spawning potential ratio (SPR), length at 50% and 95% selectivity, SL50 and SL95, respectively, and estimated fishing mortality (FM) by year.") ## withMathJax("\\(L_\\infty\\)")
               }else if(format == "kable"){
                   txt <- paste0("Results of the LBSPR method. Estimated Spawning potential ratio (SPR), length at 50% and 95% selectivity, SL50 and SL95, respectively, and estimated fishing mortality (FM) by year.") ## "\\(L_\\infty\\)"
               }
           }
           )

    if(format == "datatable"){
        txt <- paste0("<p class=\"pheader_elefan\">Table ",tab.num,": ",txt, "</p>")
    }

    return(txt)
}



#' @name captionLBSPR.plots
#' @title caption LBSPR plots
#' @details something
#' @param lbspr_dat data
#' @param input input
#' @param format format
#' @param type type
#' @return something
#' @export
captionLBSPR.plots <- function(lbspr_dat, input, format = "withFig", type){

    switch(type,
           "data" = {
               plot.num <- 1
               txt <- paste0("Uploaded length-frequency distributions for each year.")
           },
           "pie" = {
               plot.num <- 2
               txt <- paste0("Estimated spawning potential and reference points. Note, if the analysis spans multiple years, only the estimates from the last year are shown.")
           },
           "sel" = {
               plot.num <- 3
               txt <- paste0("Estimated logistic selectivity curves.")
           },
           "ts" = {
               plot.num <- 4
               txt <- paste0("Estimated parameters over time with 95% confidence intervals.")
           }
           )

    if(format == "withFig"){
        txt <- paste0("<p class=\"pheader_elefan\">Figure ",plot.num,": ",txt, "</p>")
    }else if(format == "withFigLatex"){
        txt <- paste0("<p> Figure ",plot.num,": ",txt, "</p>")
    }

    return(txt)
}
