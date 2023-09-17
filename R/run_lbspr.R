
#' @name run_lbspr
#' @title Run lbspr
#' @details something
#' @param data data
#' @param bin.size bin.size
#' @param linf linf
#' @param lm50 lm50
#' @param lm95 lm95
#' @param mk mk
#' @param lwa lwa
#' @param lwb lwb
#' @param lunit lunit
#' @return something
#' @export
run_lbspr <- function(data, bin.size, linf, lm50, lm95, mk, lwa, lwb, lunit){

    returnResults <- list()
    out <- tryCatch( {

        ## Parameter checks
        missing.pars <- NULL
        neg.pars <- NULL
        if(is.na(bin.size)){
            missing.pars <- c(missing.pars, "binSize")
        }else if(bin.size < 0) neg.pars <- c(neg.pars, "binSize")
        if(is.na(linf)){
            missing.pars <- c(missing.pars, "Linf")
        }else if(linf < 0) neg.pars <- c(neg.pars, "Linf")
        if(is.na(lm50)){
            missing.pars <- c(missing.pars, "Lm50")
        }else if(lm50 < 0) neg.pars <- c(neg.pars, "Lm50")
        if(is.na(lm95)){
            missing.pars <- c(missing.pars, "Lm95")
        }else if(lm95 < 0) neg.pars <- c(neg.pars, "Lm95")
        if(is.na(mk)){
            missing.pars <- c(missing.pars, "MK")
        }else if(mk < 0) neg.pars <- c(neg.pars, "MK")
        if(!is.null(missing.pars)) stop(paste0(paste(missing.pars, collapse = ", ")," missing!"))
        if(!is.null(neg.pars)) stop(paste0(paste(neg.pars, collapse = ", ")," cannot be negative!"))

        catch <- data$catch
        midL <- data$midLengths

        ## More checks
        ## LBSPR returns error if Linf is larger than max length class
        ## if(linf > max(midL + bin.size/2, na.rm = TRUE)){
        ##     stop("Linf cannot be smaller than max length class in data!")
        ## }
        if(lm50 > lm95){
            stop("Lm50 is larger than Lm95! That is not possible")
        }

        years <- as.numeric(format(data$dates, "%Y"))
        ny <- length(years)
        if(any(duplicated(years))) warning("The length frequency data is not aggregated by year. This method should be used with yearly aggregated data!")

        ## Create LB pars object
        lbpars <- new("LB_pars")
        lbpars@Linf <- linf
        lbpars@L50 <- lm50
        lbpars@L95 <- lm95
        lbpars@MK <- mk
        lbpars@Species <- ""
        lbpars@L_units <- lunit
        lbpars@BinWidth <- bin.size
        walpha <- ifelse(is.null(lwa) || is.na(lwa), 1e-4, lwa)
        wbeta <- ifelse(is.null(lwb) || is.na(lwb), 3, lwb)
        lbpars@Walpha <- walpha
        lbpars@Wbeta <- wbeta
        ## Use Walpha_units?

        ## Create LB lengths object
        ## dataYearly <- TropFishR::lfqModify(data, aggregate = "year")
        ## HERE: TropFishR not online?
        dataYearly <- data
        if(!inherits(dataYearly$catch,"matrix")){
            dataYearly$catch = as.matrix(dataYearly$catch)
        }
        colnames(dataYearly$catch) = paste0("X",format(dataYearly$dates, "%Y"))
        print(dataYearly)
        ##
        dat <- cbind(dataYearly$midLengths,
                     dataYearly$catch)
        if(length(dataYearly$dates) == 1){
            years <- format(dataYearly$dates, "%Y")
        }else{
            if(length(grep("X",colnames(dataYearly$catch))) > 0){
                years <- as.numeric(sapply(strsplit(colnames(dataYearly$catch), "X"),"[[",2))
            }else{
                years <- as.numeric(colnames(dataYearly$catch))
            }
        }
        colnames(dat) <- c("length", years)
        print(dat)

        ## Necessary, because all(diff(LMids) == median(diff(LMids))) in LB_lengths initalisation can cause issue with decimal digits
        dat[,1] <- 1:nrow(dat) ## Later overwritten with original bins

        ## HERE: suppressWarnings leads to error on WPS anyways... hacking needed
        suppressWarnings({lblengths <- new("LB_lengths",
##                                           file = dat,
                                           LB_pars = lbpars,
                                           dataType = "freq")})
        lblengths@LMids <- as.matrix(dat[,1])
        lblengths@LData <- as.matrix(dat[,-1])
        lblengths@L_units <- lunit
        lblengths@Years <- as.character(years)
        lblengths@NYears <- length(years)
        lblengths@Years <- as.numeric(lblengths@Years)
        lblengths@LMids <- dataYearly$midLengths  ## overwrite dummy midlengths

        if(lbpars@Linf > max(dataYearly$midLengths)){

            lmids <- dataYearly$midLengths
            lmids.add <- seq(max(lmids), 2 * lbpars@Linf, lbpars@BinWidth)[-1]
            lmids.add <- lmids.add[1:which(lmids.add > lbpars@Linf)[1]]
            lblengths@LMids <- c(lmids,lmids.add)

            ldata <- lblengths@LData
            ldata <- rbind(ldata,matrix(0, length(lmids.add), ncol(ldata)))
            lblengths@LData <- ldata
        }

        print(lbpars)
        print(lblengths)

        ## Fit LBSPR
        modfit <- try(LBSPR::LBSPRfit(lbpars, lblengths, useCPP = TRUE))

        if(!inherits(modfit, "try-error")){
            res <- modfit
        }else{
            res <- NULL
        }

        returnResults[['res']] <- res

    },
    error = function(cond) {
        print("There was an error here")
        message(paste0("Error!!", cond))
        errorResult = list()
        errorResult[['error']] <- gettext(cond)
        return (errorResult)
        ## Choose a return value in case of error
    },
    finally={
        print("Done")
    })
    if ('error' %in% names(out)) {
        returnResults[['error']] <- out$error
    }
    return (returnResults)
}
