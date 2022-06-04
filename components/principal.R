box::use(
  shiny[...],
  bslib[bs_theme],
  shinycssloaders[withSpinner]
)



#' @export
freshUI<-function(id,label='fresh'){
  ns<-NS(id)

    fluidRow(tags$style("#fluidpage{margin-left:5%; margin-right:0%;},
                        h3 {font-family: 'Lobster', cursive;font-style:italic;  text-align: center;font-weight: 500;line-height: 1.1;color: #434a42;}
"),
      withSpinner(
        type = 7,
        color = "#2E1073",color.background = "white",
        htmlOutput(ns('inc')),      #   uiOutput(ns('inc')),
      ))
  
}
#' @export
freshServ<-function(input,output,session){

return(input)
}

