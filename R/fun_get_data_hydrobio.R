#' Télécharger les stations hydrobiologiques
#'
#' @param code_departement Un vecteur de codes départements (format : "01", "02", etc.)
#' @param station_suivie Un data.frame contenant les stations qui nous interessent
#'
#' @return Un objet sf contenant les stations hydrobiologiques avec leurs coordonnées et attributs
#' @export
#'
#' @details Cette fonction télécharge les stations hydrobiologiques depuis l'API Hub'Eau et
#' sélectionne seulement les stations d'intéret définies dans le paramètre "station_suivie"
#'
#' @examples
#' \dontrun{
#' stations <- telecharger_stations(c("75", "77", "78"), station_suivie)
#' }
#'
#' @importFrom dplyr select mutate
#' @importFrom hubeau get_hydrobio_stations_hydrobio
#' @importFrom sf st_as_sf
telecharger_stations <- function(code_departement, station_suivie) {
  hubeau::get_hydrobio_stations_hydrobio(
    code_departement = paste(code_departement, collapse = ",")
    ) %>%
    dplyr::select(
      code_station_hydrobio, libelle_station_hydrobio,
      uri_station_hydrobio, coordonnee_x, coordonnee_y,
      code_cours_eau, libelle_cours_eau, code_masse_eau,
      libelle_masse_eau, code_departement,
      date_premier_prelevement, date_dernier_prelevement
      ) %>%
    dplyr::mutate (libelle_station_hydrobio=str_to_sentence(str_to_lower(libelle_station_hydrobio)))%>%
    sf::st_as_sf(
      coords = c("coordonnee_x", "coordonnee_y"),
      crs = 2154,
      remove=FALSE
      ) %>%
  dplyr::filter(code_station_hydrobio %in% station_suivie$Code.Station.SANDRE)
}

#' Télécharger les indices biologiques
#'
#' @param code_departement Un vecteur de codes départements (format : "01", "02", etc.)
#' @param code_indice Un vecteur nommé de codes indices. 
#' @param station_suivie Un data.frame contenant les stations qui nous interessent
#' @param annee_depart 
#'
#' @return Un objet sf contenant les indices biologiques avec leurs coordonnées et attributs
#' @export
#'
#' @details Cette fonction télécharge les indices biologiques depuis l'API Hub'Eau pour les
#' départements, stations et indices spécifiés. Les indices sont géolocalisés et incluent les dates
#' de prélèvement.
#'
#' @examples
#' \dontrun{
#' indices <- telecharger_indices(c("75", "77", "78"))
#' }
#'
#' @importFrom dplyr distinct mutate filter
#' @importFrom hubeau get_hydrobio_indices
#' @importFrom lubridate as_date year
#' @importFrom sf st_as_sf
telecharger_indices <- function(code_departement, station_suivie, code_indice, annee_depart ) {
  hubeau::get_hydrobio_indices(
    list(
      code_departement = paste(code_departement, collapse = ","),
      code_indice = paste(code_indice, collapse = ",")
    )
  ) %>%
    sf::st_as_sf(
      coords = c("coordonnee_x", "coordonnee_y"),
      crs = 2154,
      remove = FALSE
    ) %>%
    dplyr::distinct(
      code_station_hydrobio, libelle_station_hydrobio, code_departement, code_support, libelle_support,
      code_prelevement, date_prelevement, code_indice, libelle_indice,
      resultat_indice, code_qualification, libelle_qualification
    ) %>%
    dplyr::mutate(
      date_prelevement = lubridate::as_date(date_prelevement)
    ) %>%
    dplyr::mutate(annee = lubridate::year(date_prelevement)) %>%
    dplyr::mutate (libelle_station_hydrobio=str_to_sentence(str_to_lower(libelle_station_hydrobio)))%>%
    dplyr::filter(!is.na(resultat_indice)) %>%
    dplyr::filter(code_station_hydrobio %in% station_suivie$Code.Station.SANDRE)%>%
    dplyr::filter(annee >= annee_depart)
}


#' Télécharger les listes faunistiques et floristiques
#'
#' @param code_eqb Un vecteur nommé de codes des éléments de qualité biologique. Par défaut :
#'    Diatomées (10), Macroinvertébrés (13), Macrophytes (27)
#' @param station_suivie Un data.frame contenant les stations qui nous interessent
#' @param annee_depart 
#' 
#' @return Un data.frame contenant les listes faunistiques et floristiques avec leurs coordonnées
#'   et attributs, incluant les abondances des taxons par prélèvement
#' @export
#'
#' @details Cette fonction télécharge les listes faunistiques et floristiques depuis l'API Hub'Eau
#'   pour les stations et éléments de qualité biologique spécifiés. En cas d'erreur lors du
#'   téléchargement global, la fonction tente de télécharger les données année par année.
#'
#' @examples
#' \dontrun{
#' listes <- telecharger_listes(c("75", "77", "78"))
#' }
#'
#' @importFrom dplyr group_by summarise pull
#' @importFrom hubeau get_hydrobio_taxons get_hydrobio_stations_hydrobio
#' @importFrom purrr map list_rbind
telecharger_listes <- function(station_suivie, code_eqb, annee_depart) {

  listes <- try(
    hubeau::get_hydrobio_taxons(
      code_station_hydrobio= paste(station_suivie$Code.Station.SANDRE, collapse = ","),
      code_support = paste(code_eqb, collapse = ",")
    ) %>%
      dplyr::group_by(
        code_station_hydrobio, libelle_station_hydrobio,
        code_prelevement, date_prelevement,
        code_support, libelle_support, code_appel_taxon, libelle_appel_taxon,
        coordonnee_x, coordonnee_y
      ) %>%
      dplyr::summarise(
        resultat_taxon = sum(resultat_taxon),
        .groups = "drop"
      )%>%
      dplyr::filter(code_station_hydrobio %in% station_suivie$Code.Station.SANDRE)%>%
      dplyr::mutate (libelle_station_hydrobio=str_to_sentence(str_to_lower(libelle_station_hydrobio)))%>%
      dplyr::mutate(annee = lubridate::year(date_prelevement))%>%
      dplyr::filter(annee >= annee_depart),
    silent=TRUE
  )

  if (inherits(listes, "try-error")) {
    
    message("Téléchargement global échoué → fallback année par année")
    
    annees <- annee_depart:lubridate::year(Sys.Date())
    
    listes_par_annee <- list()
    
    for (a in annees) {
      
      message("Téléchargement pour l’année : ", a)
      
      annual <- try(
        hubeau::get_hydrobio_taxons(
          code_station_hydrobio = paste(station_suivie$Code.Station.SANDRE, collapse = ","),
          code_support = paste(code_eqb, collapse = ","),
          date_debut_prelevement = paste0(a, "-01-01"),
          date_fin_prelevement   = paste0(a, "-12-31")
        ),
        silent = TRUE
      )
      
      if (!inherits(annual, "try-error") && nrow(annual) > 0) {
        listes_par_annee[[as.character(a)]] <- annual
      }
    }
    
    # Fusion des années
    listes <- dplyr::bind_rows(listes_par_annee)
    
    # Post-traitement
    listes <- listes %>%
      dplyr::mutate(
        annee = lubridate::year(date_prelevement),
        libelle_station_hydrobio = stringr::str_to_sentence(stringr::str_to_lower(libelle_station_hydrobio))
      ) %>%
      dplyr::filter(annee >= annee_depart)
  }
  
  return(listes)
}
