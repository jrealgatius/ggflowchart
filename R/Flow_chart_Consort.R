#'  Dibuixa un flow_chart_ggconsort a partir d'una llista de n criteris d'exclusió
#'
#' @param dt dataframe/tibble
#'
#' @param exclusions vector de n strings amb la definició del criteris: exclusions=c("Sepal.Length < 5","Species=='setosa'") o dicotomics
#'
#' @param grups string referent al grup de dues categories (Opcional)
#'
#' @param sequencial logic TRUE/FALSE . Exclusions sequencials o No
#'
#' @param lab_start string referida a la etiqueta de l acaixa d'inici (default "Assessed for eligibility" )
#'
#' @param lab_random string referida a la etiqueta de caixa post exclusions (default "Analyzed sample" )
#'
#' @param lab_exclusions vector d'etiquetes referent a les N exclusions (Opcional)

#' @import dplyr purrr ggplot2 tidyr ggconsort stats glue ggtext

#' @export

#' @examples
#'
#'
#'
#' Flow_chart_Consort(dt=ggconsort::trial_data,
#'                   exclusions=c("declined","prior_chemo","bone_mets"),sequencial=TRUE,grups=NA,
#'                    lab_start="Assessed for eligibility",
#'                    lab_random="Analyzed sample",
#'                    lab_exclusions=NULL)

Flow_chart_Consort<-function(dt=ggconsort::trial_data,
                             exclusions=c("declined","prior_chemo","bone_mets"),
                             sequencial=TRUE,
                             grups=NA,
                             lab_start="Assessed for eligibility",
                             lab_random="Analyzed sample",
                             lab_exclusions=NULL)
{

  ## Testing
  # dt=dt_nova
  # exclusions= exclusions
  # lab_exclusions=variables$labels
  # grups = grups
  # sequencial=sequencial
  # lab_start="Assessed for eligibility"
  # lab_random="Randomized"
  # lab_exclusions=NULL

  # dt=trial_data
  # exclusions=c("declined","prior_chemo","bone_mets")
  # # # Parametres
  # lab_start="Assessed for eligibility"
  # lab_random="Randomized"
  # lab_exclusions=c("A","B","C")
  # dt=trial_data
  # exclusions<-c("exclusio1", "exclusio2","exclusio3")
  # exclusions=c("declined","prior_chemo","bone_mets")
  # exclusions=c("prior_chemo","bone_mets")
  # # exclusions=c("bone_mets")
  # grups="treatment"
  # sequencial=T
  # grups=NA
  # grups="treatment"
  # # #
  # # #
  # dt=iris
  # exclusions=c("Sepal.Length < 5","Species=='setosa'")

  # Proces per dicotomitzar criteris d'inclusió
  # vector_crit_excl<-c("DG.ANGOR","edat<30 | edat>=90","ANT.FA","ANT.event","sexe=='H'","DG.AIT")
  # subvector_a_canviar<-vector_crit_excl[vector_crit_excl %in% (dt_temp %>% names())] %>% paste0("==1")
  # vector_crit_excl[vector_crit_excl %in% (dt_temp %>% names())]<-subvector_a_canviar

  # Dicotomitzar....
  subvector_a_canviar<-exclusions[exclusions %in% (dt %>% names())] %>% paste0("==1")
  exclusions[exclusions %in% (dt %>% names())]<-subvector_a_canviar
  dt<-generar_dummies_exclusions(dt=dt, criteris=exclusions)

  # Num d'exclusions
  N_exc<-length(exclusions)
  noms_exclusions<-exclusions
  exclusions<-paste0("exclusio",c(1:N_exc))

  # Selecciono camps necessaris de dt (dades)
  if (is.na(grups))
  {dt<-dt %>%
    dplyr::select(exclusions) %>% dplyr::mutate(grup="Overall",idp = row_number())
  grup<-"Overall"} else
  {dt<-dt %>% dplyr::select(exclusions,grup=grups) %>% dplyr::mutate(idp = dplyr::row_number())}

  # capturar etiquetes
  if (is.null(lab_exclusions)) labels_exclusions<-noms_exclusions else labels_exclusions<-lab_exclusions

  label_grup<-grups
  levels_grup<-dt %>% dplyr::select(grup) %>% stats::na.omit() %>% dplyr::distinct() %>% dplyr::pull(grup)
  # Canvi de categories a grup numerats en character
  dt<-dt %>% dplyr::mutate(grup=as.factor(grup) %>% as.numeric(),
                           grup=dplyr::if_else(!is.na(grup),as.character(paste0("grup",grup)),NA_character_))

  # Generar dataframes de filtre

  # Inclusions sequencials
  # Genero fitxers inclusions sequencials
  dtlist_incl<-seq(1:N_exc) %>%
    purrr::map(~paste0("!",exclusions[1:.x],collapse = " & ")) %>%
    purrr::map(~dt %>% dplyr::filter(eval(parse(text = .x)))) %>%
    purrr::set_names(paste0("included",1:N_exc))

  # Inclusio final
  dt_inclusio_final<-dtlist_incl[N_exc] %>% purrr::set_names("Included_final")

  # Generar fitxers d'exclusions sequencials
  dt_excluded_totals = dplyr::anti_join(dt, dt_inclusio_final[[1]], by = "idp")
  dt_exc1 = dplyr::anti_join(dt, dtlist_incl[[1]], by = "idp")
  # si hi ha una unica exclusio dtlist_exclusions<-NULL

  if (N_exc>1) {
    if (sequencial) {
      dtlist_exclusions<-
        purrr::map2(dtlist_incl[1:(N_exc-1)],dtlist_incl[-1],~anti_join(.x,.y,by = "idp")) %>%
        purrr::set_names(paste0("Excluded",c(2:N_exc)))} else {

          dtlist_exclusions<-c(2:N_exc) %>%
            purrr::map(~paste0(exclusions[.x],"==1")) %>%
            purrr::map(~dt %>% dplyr::filter(eval(parse(text = .x)))) %>%
            purrr::set_names(paste0("Excluded",2:N_exc))
        }} else {dtlist_exclusions<-NULL}

  dtlist_exclusions<-append(list(Excluded1=dt_exc1),dtlist_exclusions)
  # Inclusions finals per grups
  # grup="FF.HTA"
  dt_inclusions_grups<-
    dtlist_incl[[N_exc]] %>% base::split(.[["grup"]])

  # # Elimino els NA's
  # dt_inclusions_grups$grupNA<-NULL

  # Fusionar llistats de de inclosos + exclosos
  llistat_arguments<-
    c(dt_inclusio_final,dtlist_incl,dt_inclusions_grups,
      list(Excluded_total=dt_excluded_totals),
      dtlist_exclusions)

  arglist = append(list(cohort_start(dt,lab_start)),
                   llistat_arguments)

  # Generar les cohorts via funció
  dades_cohorts<-
    do.call(cohort_define,
            arglist)

  # Provide text labels for cohorts ---------------------------
  llistat_noms<-dades_cohorts$data %>% names()

  llistat_labels<- c(lab_start,lab_random,
                     paste0("Included",c(1:N_exc)),
                     levels_grup,"Excluded",labels_exclusions)

  # Labels
  # llistat_noms<-dades_cohorts$data %>% names()
  # llistat_noms<-c(".full","Included_final", paste0("Included",c(1:N_exc)),
  #                 paste0("grup",1:length(levels_grup)),
  #                 "Excluded_total",  paste0("Excluded",c(1:N_exc)))

  for (i in 1:length(llistat_noms)) {
    dades_cohorts$labels[llistat_noms[i]]<-llistat_labels[i]}

  study_cohorts<-dades_cohorts

  #
  # study_cohorts<-
  #   dades_cohorts %>%
  #   cohort_label(
  #     Included_final = lab_random,
  #     grup1= "Allocated to arm A",
  #     grup2 = "Allocated to arm B",
  #     Excluded_total = "Excluded",
  #     Excluded1 = "Declined to participate",
  #     Excluded2 = "Prior chemotherapy",
  #     Excluded3 = "Bone metastasis")

  # Generar caixa d'exclusions
  noms_exc<-paste0("Excluded",1:N_exc)

  caixa_exc<-noms_exc %>%
    purrr::map_chr(~paste0('• {cohort_count_adorn(study_cohorts, ', .x,')}<br>')) %>%
    glue::glue_collapse()
  caixa_exclusions<-paste0(
    "{cohort_count_adorn(study_cohorts, Excluded_total)}<br>",
    caixa_exc)


  study_consort <- study_cohorts %>%
    ggconsort::consort_box_add(
      "full", 0, 50, ggconsort::cohort_count_adorn(study_cohorts, .full)
    ) %>%
    ggconsort::consort_box_add(
      lab_random, 0, 30, ggconsort::cohort_count_adorn(study_cohorts,Included_final)
    ) %>%

    # consort_box_add(
    #   "exclusions", 20, 40, glue::glue(caixa_exclusions)
    # ) %>%


    ggconsort::consort_box_add(
      "exclusions", 10, 40, glue::glue(caixa_exclusions)
    ) %>%


    ggconsort::consort_arrow_add(
      end = "exclusions", end_side = "left", start_x = 0, start_y = 40
    ) %>%
    ggconsort::consort_arrow_add(
      "full", "bottom", lab_random, "top"
    )

  # En cas de By grups
  if (!is.na(grups)) {

    # By grups
    study_consort <- study_consort %>%

      ggconsort::consort_box_add(
        "arm_a", -30, 10, ggconsort::cohort_count_adorn(study_cohorts, grup1)
      ) %>%

      ggconsort::consort_box_add(
        "arm_b", 30, 10, ggconsort::cohort_count_adorn(study_cohorts, grup2)
      )

    study_consort<- study_consort %>%
      ggconsort::consort_arrow_add(
        start_x = 0, start_y = 30, end_x = 0, end_y = 20,
      ) %>%
      ggconsort::consort_line_add(
        start_x = -30, start_y = 20, end_x = 30, end_y = 20,
      )  %>%
      ggconsort::consort_arrow_add(
        end = "arm_a", end_side = "top", start_x = -30, start_y = 20
      ) %>%
      ggconsort::consort_arrow_add(
        end = "arm_b", end_side = "top", start_x = 30, start_y = 20)

  }

  ## Fer-ho maco


  if (!is.na(grups)) {

    study_consort %>%
      ggplot2::ggplot() +
      ggconsort::geom_consort() +
      ggconsort::theme_consort(margin_h = 8, margin_v = 1) +
      # you can include other ggplot geoms, as needed -------------
    ggtext::geom_richtext(
      aes(x = 0, y = 10, label = "Allocation"),
      fill = "#9bc0fc") } else
      {
        study_consort %>%
          ggplot2::ggplot() +
          ggconsort::geom_consort() + xlim(-10,20) +
          ggconsort::theme_consort(margin_h = 10, margin_v = 1)
      }
}



# #############       Exemples        ################
# # Descarregar funcions github -
# link_source<-paste0("https://github.com/jrealgatius/Stat_codis/blob/master/funcions_propies.R","?raw=T")
# devtools::source_url(link_source)
# donat un vector exclusions, generar les cohorts d'exclusions sequencials
# dt<-dades

# exclusions<-c("exclusio1", "exclusio2","exclusio3", "exclusio4", "exclusio5")
# exclusions<-c("exclusio1", "exclusio2","exclusio3")
# exclusions<-c("exclusio1","exclusio3")

# devtools::install_github("tgerke/ggconsort")
# library(dplyr)
# library(purrr)
# library(ggplot2)
#
# Flow_chart_Consort(dt=trial_data,exclusions=c("declined","prior_chemo","bone_mets"),sequencial = F,grups = "treatment",
#                    lab_start = "Pob inicial", lab_random = "Aleatoritzats")
#
# # pp<-Flow_chart_Consort(dt=trial_data,exclusions=c("declined","prior_chemo","bone_mets"),sequencial = T)
#
# # ggsave("figura.jpg",plot = pp,dpi = 300,width=7,height = 5)
#
#
#
# #
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
