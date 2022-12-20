

#' @name dev.on
#' @title Device on
#' @details something
#' @param path path
#' @param format format
#' @param width width
#' @param height height
#' @return something
#' @export
dev.on <- function(path, format, width, height){
    if(format == "pdf"){
        grDevices::pdf(path, width = width, height = height)
    }else if(format == "png"){
        grDevices::png(path, width = width, height = height, units = "in", res = 120)
    }else if(format == "jpeg"){
        grDevices::jpeg(path, width = width, height = height, units = "in", res = 120)
    }else if(format == "tiff"){
        grDevices::tiff(path, width = width, height = height, units = "in", res = 120)
    }else if(format == "bmp"){
        grDevices::bmp(path, width = width, height = height, units = "in", res = 120)
    }else if(format == "ps"){
        grDevices::postscript(path, width = width, height = height)
    }
}


#' @name save.table
#' @title Save table
#' @details something
#' @param file file
#' @param path path
#' @param format format
#' @return something
#' @export
save.table <- function(file, path, format){
    if(format == "csv"){
        utils::write.csv(file, path, row.names = FALSE)
    }else if(format == "xls"){
        openxlsx::write.xlsx(file, path, row.names = FALSE)
    }else if(format == "xlsx"){
        openxlsx::write.xlsx(file, path, row.names = FALSE)
    }
}
