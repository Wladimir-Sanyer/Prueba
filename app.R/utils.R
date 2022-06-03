box::use(
  shiny[tagList, span, includeHTML, tags, isTruthy, addResourcePath],
  glue[glue],
  sass[sass, sass_file],
  bslib[bs_theme],
  dplyr[transmute, mutate, group_by, summarise, ungroup, collect, select_if, 
        rename_with],
  lubridate[month, year],

)



#' @export
titulo <- function(titulo, logo = file.path(getwd(), "img/logo.svg")) {
  shiny::tagList(
    shiny::span(class = "logo-lg", titulo),
    shiny::includeHTML(logo)
  )
}

#' @export
theme <- function() {
  bs_theme(
    version = 5,
    font_scale = 0.9,
    bootswatch = "simplex",
    secondary = "#09c",
    info = "#09c",
    primary = "#0071bc"
  )
}
