#' Déterminer les dates de début et de fin d'une série de données satisfaisant à une condition
#'     de proximité temporelle entre observations successives
#'
#' @param df Dataframe contenant les données
#' @param var_temp Variable temporelle
#' @param var_id_site Variable d'identification du site
#' @param max_nb_obs_manquantes Nombre maximum de données manquantes successives acceptées dans une série
#'
#' @return Dataframe avec une ligne par série et des colonnes indiquant les débuts, fins et nombre d'observations
#' @export
#' 
#' @importClassesFrom dplyr enquo group_by arrange mutate filter summarize ungroup select n
#'
#' @examples
#' \dontrun{
#' prov <- borner_series(df = annee_de_donnee,
#' var_id_site = pop_id,
#' var_temp = annee)
#' }
borner_series <- function(df,
                          var_temp,
                          var_id_site,
                          max_nb_obs_manquantes) {
  
  var_temp    <- rlang::enquo(var_temp)
  var_id_site <- rlang::enquo(var_id_site)
  
  test <- df %>%
    dplyr::group_by(!!var_id_site) %>%
    dplyr::arrange(!!var_temp, .by_group = TRUE) %>%
    dplyr::mutate(
      obs_precedente = dplyr::lag(!!var_temp),
      obs_manquantes = !!var_temp - obs_precedente - 1,
      obs_manquantes = ifelse(is.na(obs_manquantes), 0, obs_manquantes),
      boolean        = obs_manquantes <= max_nb_obs_manquantes,
      annee_mini     = min(!!var_temp, na.rm = TRUE)
    )
  
  # --- Ajout du comptage des indices ---
  test %>%
    dplyr::mutate(boolean = ifelse(is.na(boolean), 0, boolean)) %>%
    dplyr::group_by(
      !!var_id_site,
      annee_mini,
      group = cumsum(c(0, diff(boolean) != 0))
    ) %>%
    dplyr::filter(boolean == 1 & dplyr::n() > 1) %>%
    dplyr::summarize(
      debut  = suppressWarnings(min(as.character(obs_precedente))),
      fin    = max(as.character(!!var_temp)),
      n_annee = dplyr::n_distinct(!!var_temp),
      # Comptage des indices
      n_IBD = n_distinct( (!!var_temp)[code_indice == "5856"] ), 
      n_I2M2 = n_distinct( (!!var_temp)[code_indice == "7613"] ), 
      n_IBMR = n_distinct( (!!var_temp)[code_indice == "2928"] ), 
      n_IPR = n_distinct( (!!var_temp)[code_indice == "7036"] ),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-group) %>%
    dplyr::mutate(
      debut  = ifelse(is.na(debut), annee_mini, debut)
    )
}
