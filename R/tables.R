
#' @name tableLBSPR.data
#' @title table LBSPR data
#' @details something
#' @param lbspr_dat data
#' @param input input
#' @param format format
#' @return something
#' @export
tableLBSPR.data <- function(lbspr_dat, input, format = "dataframe"){

    tmp <- cbind(lbspr_dat$dataExplo$lfq$midLengths,
                 lbspr_dat$dataExplo$lfq$catch)

    labs <- c("Mid lengths", as.character(lbspr_dat$dataExplo$lfq$dates))

    ## Rounding
    tmp <- signif(tmp, digits = 3)

    ## Return
    colnames(tmp) <- labs
    rownames(tmp) <- NULL
    return(tmp)
}


#' @name tableLBSPR.inputPars
#' @title table LBSPR inputPars
#' @details something
#' @param lbspr_dat data
#' @param input input
#' @param format format
#' @return something
#' @export
tableLBSPR.inputPars <- function(lbspr_dat, input, format = "datatable"){

    if(is.null(input$LBSPR_MK) || is.na(input$LBSPR_MK)){
        mk <- input$LBSPR_M / input$LBSPR_K
    }else{
        mk <- input$LBSPR_MK
    }

    tab <- data.frame(binSize = input$LBSPR_binSize,
                      lengthUnit = input$LBSPR_lengthUnit,
                      MK = mk,
                      M = input$LBSPR_M,
                      K = input$LBSPR_K,
                      Linf = input$LBSPR_Linf,
                      Lm50 = input$LBSPR_Lm50,
                      Lm95 = input$LBSPR_Lm95,
                      a = input$LBSPR_LWa,
                      b = input$LBSPR_LWb)
    ind.remove <- which(is.na(tab[1,]) | tab[1,] == -999)
    if(length(ind.remove) > 0){
        tab <- tab[,-ind.remove]
    }

    if(format == "datatable"){
        labs <- c("Bin size",
                  "Length unit",
                  "M/K",
                  "M",
                  "K",
                  "L<sub>&#8734;</sub>",
                  "L<sub>m50%</sub>",
                  "L<sub>m95%</sub>",
                  "a",
                  "b")
    }else if(format == "kable"){
        labs <- c("Bin size",
                  "Length unit",
                  "M/K",
                  "M",
                  "K",
                  "L\\textsubscript{\\infty}",
                  "L\\textsubscript{m50\\%}",
                  "L\\textsubscript{m95\\%}",
                  "a",
                  "b")
    }else if(format == "dataframe"){
        labs <- c("Bin size",
                  "Length unit",
                  "M/K",
                  "M",
                  "K",
                  "Linf",
                  "Lm50",
                  "Lm95",
                  "a",
                  "b")
    }


    ## Selection
    tmp <- tab
    labs <- labs[-ind.remove]

    ## Rounding
    tmp[,which(labs != "Length unit")] <- signif(tmp[,which(labs != "Length unit")],
                                                digits = 3)


    if(format == "dataframe"){
        colnames(tmp) <- labs
        return(tab)
    }else if(format == "kable"){
        return(knitr::kable(tab, format = "latex", col.names = colnames(tab),
                            row.names = FALSE, escape = FALSE,
                            align=rep('c',ncol(tab)),
                            linesep = "",
                            caption = "Parameter values used.") %>%
               kable_styling(font_size = 11, latex_options = "HOLD_position") %>%
               column_spec(1:ncol(tab), width = "1.2cm"))
    }else if(format == "datatable"){
        return(DT::datatable(tab, colnames = colnames(tab),
                             escape = FALSE, rownames = FALSE,
                             options = list(dom = 't',
                                            columnDefs = list(list(
                                                className = 'dt-center',
                                                targets = 0:(ncol(tmp)-1))))))
    }
}



#' @name tableLBSPR.results
#' @title table LBSPR results
#' @details something
#' @param lbspr_dat data
#' @param input input
#' @param format format
#' @return something
#' @export
tableLBSPR.results <- function(lbspr_dat, input, format = "datatable"){

    modfit <- lbspr_dat$results

    if(!is.null(modfit)){
        respar <- matrix(c(modfit@SL50, modfit@SL95, modfit@FM, modfit@SPR),
                         ncol=4, byrow=FALSE)
        lo <- respar[,1:4] - 1.96 * sqrt(modfit@Vars)
        up <- respar[,1:4] + 1.96 * sqrt(modfit@Vars)
        lo[!is.finite(lo)] <- 0
        up[!is.finite(up)] <- 0
        lo[lo<0] <- 0
        up[up<0] <- 0
        ## from Barefoot Ecologist: correct bounded parameters
        lo[lo[,3]<0,3] <- 0
        up[up[,4]>1,4] <- 1
        lo[lo[,4]<0,4] <- 0
        lo <- round(lo,2)
        up <- round(up,2)

        res <- data.frame(year = modfit@Years,
                          spr = paste0(round(modfit@SPR, 2), " (", lo[,4], " - ", up[,4], ")"),
                          sl50 = paste0(round(modfit@SL50, 2), " (", lo[,1], " - ", up[,1], ")"),
                          sl95 = paste0(round(modfit@SL95, 2), " (", lo[,2], " - ", up[,2], ")"),
                          fm = paste0(round(modfit@FM, 2), " (", lo[,3], " - ", up[,3], ")"))
    }else{
        res <- NULL
    }

    refs <- c("year","spr","sl50","sl95","fm")
    if(format == "datatable"){
        labs <- c(
            "Year",
            "SPR [%]",
            "L<sub>s50%</sub>",
            "L<sub>s95%</sub>",
            "F/M")
    }else if(format == "kable"){
        labs <- c("Year",
                  "SPR [\\%]",
                  "L\\textsubscript{s50\\%}",
                  "L\\textsubscript{s95\\%}",
                  "F/M")
    }else if(format == "dataframe"){
        labs <- refs
    }

    rnames <- colnames(res)
    rnames <- labs[match(rnames, refs)]

    if(format == "dataframe"){
        tmp <- as.data.frame(res)
        colnames(tmp) <- rnames
        return(tmp)
    }else if(format == "kable"){
        tmp <- as.data.frame(res)
        return(knitr::kable(tmp, format = "latex", col.names = rnames,
                            row.names = FALSE, escape = FALSE,
                            align=rep('c',ncol(tmp)),
                            linesep = "",
                            caption = "Results of the LBSPR method.") %>%
               kable_styling(font_size = 11, latex_options = "HOLD_position") %>%
               column_spec(1:ncol(tmp), width = c("1.2cm","3cm","3.5cm","3.5cm","3cm")))
    }else if(format == "datatable"){
        return(DT::datatable(res, colnames = rnames, escape = FALSE, rownames = FALSE))
    }
}
