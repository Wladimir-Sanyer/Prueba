rm(list = ls(box:::loaded_mods), envir = box:::loaded_mods)
box::use(
  shiny[...],  bslib[bs_theme],  qs[qread],
  ./components/utils,
  ./components/principal
)

options(shiny.maxRequestSize = 1024*1024^2)
light=bs_theme()


ui <-fluidPage(id='fluidpage',
      theme = light,
      h2('Prueba Proyecto Ais'),
      fluidRow(utils$select_color("color_background", 
                           label="Seleccione el color de fondo",20),
               utils$select_color("color_font", 
                           label="Seleccione el color de letra",100)),
      principal$freshUI("fresh"))


server <- function(input, output, session) {
  observe(session$setCurrentTheme(
    dark <- bs_theme(bg = input$color_background, fg =input$color_font, primary = "purple")
  ))
  
  
  callModule(principal$freshServ,"fresh")
  

  
  
}
shinyApp(ui, server)
