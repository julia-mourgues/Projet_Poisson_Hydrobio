#' Déterminer le cycle DCE auquel les prélèvements appartiennent
#'
#' @param annee Variable du dataframe contenant les années


cycle_DCE <- function (annee){
  ifelse(annee >= 2010 & annee <= 2015, "2010-2015",
         ifelse(annee >= 2016 & annee <= 2021, "2016-2021",
                ifelse(annee >= 2022 & annee <= 2027, "2022-2027",
                       NA)))
}