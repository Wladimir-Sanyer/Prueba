rm(list = ls(box:::loaded_mods), envir = box:::loaded_mods)
box::use(
  shiny[...], 
  bslib[bs_theme],
  ./components/utils,  
  ./components/principal,  
  ./components/user, 
  ./components/register,
  ./components/relation_table,
  ./components/higcharts
)
options(shiny.maxRequestSize = 1024*1024^2)
light=bs_theme()


ui <-fluidPage(id='fluidpage',
      theme = light,
      h2('Prueba Proyecto Ais'),br(), 
      user$userUI('user'),br(),

      tabsetPanel(
        tabPanel('Home', 
                 principal$HomeUI("fresh")
                 ),
        tabPanel('Registro Información', 
                 register$registroUI("register")
        ),
        tabPanel('Relación tablas', 
                 relation_table$relation_tableUI("relation")
        ),
        tabPanel('Highcharts', 
                 higcharts$highchartUI("highcharter")
        )
          )
)
server <- function(input, output, session) {
  

  
  user=callModule(user$userServ,"user")
  color=callModule(principal$HomeServ,"fresh",user)
  push=callModule(register$registroServ,"register",user)
  relation=callModule(relation_table$relation_tableServ,"relation",push,user)
  callModule(higcharts$highchartServ,"highcharter")
  
  
  
  observe(session$setCurrentTheme(
    
   bs_theme(bg = color$color_background, fg =color$color_font, primary = "purple")
    
  ))

}
shinyApp(ui, server)
