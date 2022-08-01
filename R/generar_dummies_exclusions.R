#' Genera n variables dummis (exclusio1,exclusio2,....exclusion) segons criteris d'exclusio determinats


#' @param dt dataframe/tibble
#'
#' @param criteris vector string amb la definició del criteris: criteris=c("Sepal.Length < 5","Species=='setosa'")
#'
#' @return data.frame o tibble amb n columnes tipo dummie (0/1) afegides referides a les n exclusions

#' @import dplyr purrr

#' @examples
#'
#' generar_dummies_exclusions(dt=iris,criteris=c("Sepal.Length < 5","Species=='setosa'"))
#'
#'
#'
#' @export

generar_dummies_exclusions<-function(dt=iris, criteris=c("Sepal.Length < 5","Species=='setosa'")) {

  # dt=dt
  # criteris=exclusions
  # dt=dt, criteris=exclusions

  # Generar dummies segons criteris d'inclusió
  num_excl<-criteris %>% length()
  cols_dummies<-paste0("exclusio",c(1:num_excl))

  dt_dumies<-
    purrr::map2_dfc(criteris,cols_dummies,
                    ~dplyr::transmute(dt,!!sym(.y):=eval(parse(text=.x)))
    ) %>% dplyr::mutate_all(as.numeric)

  # Juntar-ho tot
  # Truc per eliminar dumiis existents en dt
  vars_dt<-names(dt)[!names(dt)%in% c(names(dt_dumies))]
  dt %>% select(vars_dt) %>% dplyr::bind_cols(dt_dumies)

}

