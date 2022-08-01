#' Read conductor file different formats txt o rds o xls xlsx o data_frame tibble
#' @param fitxer Character as a file name and path or data.frame
#'
#' @import data.table stringr readxl foreign
#'
#' @NoRd
read_conductor<-function(fitxer) {

  # Si el fitxer es un data_frame saltar
  if (any(class(fitxer) %in% c("tbl_df","tbl","data.frame")))

    dt <- tibble::as_tibble(fitxer)

  else {

    if (stringr::str_detect(fitxer,"\\.txt$")){

      dt<-data.table::fread(fitxer) %>% as_tibble()

    } else if (stringr::str_detect(fitxer,"\\.rds$")) {

      dt<-readRDS(fitxer) %>% as_tibble()

    } else if (stringr::str_detect(fitxer,"\\.xls$")) {

      dt<-readxl::read_excel(fitxer) %>% tidyr::as_tibble()

    } else if (stringr::str_detect(fitxer,"\\.xlsx$")) {

      dt<-readxl::read_excel(fitxer) %>% tidyr::as_tibble()

    } else if (stringr::str_detect(fitxer,"\\.sav$")) {

      dt<-foreign::read.spss(fitxer,use.value.labels = T,to.data.frame = T) %>% tidyr::as_tibble()
    }
    else {stop("Data format no reconegut ")}
  }

}
