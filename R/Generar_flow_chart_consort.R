#' Dibuixa un flow_chart_ggconsort a partir d'una llista de n criteris d'exclusió que estan en un fitxer extern (Tipo excel)
#'
#' @param dt dataframe/tibble
#'
#' @param taulavariables string referent a path o tibble on hi ha una columna consten les exclusions: "conductor_cars.xls"
#'
#' @param camp string referent al nom de la columna de les variables implicades (Inecesari)
#'
#' @param criteris string referent a la columna on consten les exclusions
#'
#' @param grups string referent al grup de dues categories
#'
#' @param missings logic TRUE/FALSE  (De moment no operatiu)
#'
#' @param sequencial logic TRUE/FALSE . Exclusions sequencials o No
#'
#' @param labels  string nom del camp on hi ha les etiquetes de les exclusions (Optatiu)
#'
#' @param lab_start string referida a la etiqueta de l acaixa d'inici (default "Assessed for eligibility" )
#'
#' @param lab_random string referida a la etiqueta de caixa post exclusions (default "Analyzed sample" )
#'
#' @import dplyr purrr ggplot2 tidyr ggconsort

#' @export

#' @examples



Generar_flow_chart_consort<-function(dt=iris,
                                     taulavariables="conductor_cars.xls",
                                     camp="camp",criteris="exclusio",
                                     grups=NA,
                                     missings=F,
                                     sequencial=T,
                                     labels=NULL,
                                     lab_start="Assessed for eligibility",
                                     lab_random="Analyzed sample",
                                     ...)
{

  # # dt=dt_temp
  # # taulavariables=conductor_variables
  #  sheet="criteris_exclusio"
  #  criteris="exc_ggconsort"
  #  missings=F
  #  labels="descripcio"
  #  sequencial=F
  #  grups=NA
  #  camp="camp"
  #  lab_start="Assessed for eligibility"
  #  lab_random="Randomized"

  # dt=iris
  # taulavariables="conductor_cars.xls"
  # camp="camp"
  # criteris="exclusio"
  # missings=F
  # labels="descripcio"

  # taulavariables = here::here("Conductor.xls")
  # sheet="criteris_exclusio"
  # criteris="exclusio_nou"


  # 1. Obrir conductor d'exclusions del conductor

  ##  Llegeixo criteris de variables i selecciono variables amb filtres
  variables <- read_conductor(taulavariables,col_types = "text",...)  %>%
    # variables <- read_conductor(taulavariables,col_types = "text",sheet="criteris_exclusio")  %>%
    dplyr::select(camp=!!camp,criteris=!!criteris, labels=!!labels) %>%
    # Filtrar valors
    dplyr::filter(!is.na(criteris))

  # Parar si no hi ha criteris d'exclusió
  num_excl<-variables$criteris %>% length()
  if (num_excl==0) {
    print("No hi ha criteris jejejj")
    return("Error") }

  # 2. Generar dummies
  dt_nova<-generar_dummies_exclusions(dt=dt,criteris=variables$criteris)

  # Noves variables generades
  exclusions=paste0("exclusio",c(1:num_excl))

  # labels si no s'han passat
  if (is.null(labels)) variables<-variables %>% mutate(labels=criteris)

  # 3. Generar flow_chart
  Flow_chart_Consort(dt=dt_nova,
                     exclusions=exclusions,
                     lab_exclusions=variables$labels,
                     grups = grups,
                     sequencial=sequencial,
                     lab_start=lab_start,
                     lab_random=lab_random)

}

# Flow_chart_Consort(dt=trial_data,exclusions=c("prior_chemo","bone_mets"))
#
# Flow_chart_Consort(dt=trial_data,exclusions=c("declined","prior_chemo","bone_mets"),sequencial = F,grups="treatment")
# Flow_chart_Consort(dt=trial_data,exclusions=c("declined","prior_chemo","bone_mets"),sequencial = T,grups="treatment")
#
# Flow_chart_Consort(dt=trial_data,exclusions=c("declined","prior_chemo","bone_mets"),sequencial = T,grups=NA)
#
# Flow_chart_Consort(dt=trial_data,exclusions=c("prior_chemo","bone_mets"),sequencial = F,grups="treatment")
#
# Flow_chart_Consort(dt=trial_data,exclusions=c("prior_chemo","bone_mets"),sequencial = F,grups="treatment")
#
# Flow_chart_Consort(dt=trial_data,exclusions=c("bone_mets"))
#
#
# table(trial_data$prior_chemo)
#
# Flow_chart_Consort(exclusions=c("declined","prior_chemo","bone_mets"),sequencial = T)
# Flow_chart_Consort(exclusions=c("declined","prior_chemo","bone_mets"),sequencial = F)
#
# Flow_chart_Consort(exclusions=c("prior_chemo","bone_mets"),sequencial = T)
# Flow_chart_Consort(exclusions=c("prior_chemo","bone_mets"),sequencial = F)
#
# Flow_chart_Consort(exclusions=c("bone_mets","prior_chemo"),
#                    grups="treatment",sequencial=T)
#
#
# Flow_chart_Consort(exclusions=c("bone_mets","prior_chemo"),sequencial=T)
#
#
# Flow_chart_Consort(exclusions=c("prior_chemo"),
#                    grup="treatment",sequencial=T)
#
#
#




