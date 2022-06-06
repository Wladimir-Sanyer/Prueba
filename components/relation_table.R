box::use(
  shiny[...],./datasql
)
names_tbls=c('tbl_personal','tbl_job')
#' @export
relation_tableUI<-function(id,label='relation'){
ns<-NS(id)

tagList(
  
  
  
  div(
    class = "container",
    tags$hr(),
    div(
      class = "row",
      selectInput(
        inputId = ns("tabla_join"),
        label = "Tabla #1 a unir",
        choices = NULL,
        width = "100%"
      )
    ),
    div(
      class = "row",
      selectInput(
        inputId = ns("tabla_join1"),
        label = "Tabla #2 a unir",
        choices = NULL,
        width = "100%"
      )
    ),
    div(
      class = "row",
      div(
        class = "col-sm",
        selectInput(
          inputId = ns("cols_left"),
          label = "Columna izquierda",
          choices = NULL,
          width = "100%",
          multiple = F
        )
      ),
      div(
        class = "col-sm",
        selectInput(
          inputId = ns("cols_right"),
          label = "Columna derecha",
          choices = NULL,
          width = "100%",
          multiple = F
        )
      )
    ),
    div(
      class = "row",
      div(
        class = "col-sm",
        selectInput(
          inputId = ns("cols_join"),
          label = "Columnas a unir",
          choices = NULL,
          width = "100%",
          multiple = TRUE
        )
      )
    ),
    div(
      class = "row",
      div(
        class = "col-sm",
        actionButton(
          inputId = ns("left_join"),
          label = "Left join",
          width = "100%",
          class = "col-sm"
        )
      ),
      div(
        class = "col-sm",
        actionButton(
          inputId = ns("inner_join"),
          label = "Inner join",
          width = "100%",
          class = "col-sm"
        )
      ),
      div(
        class = "col-sm",
        actionButton(
          inputId = ns("anti_join"),
          label = "Anti join",
          width = "100%",
          class = "col-sm"
        )
      )
    )
  )
  
)

}


#' @export
relation_tableServ<-function(input,output,session){
  
  observe({
    updateSelectInput(
      inputId = "tabla_join",
      choices = names_tbls,
      selected = input$tabla_join
    )
  }) 
  
  observe({
    updateSelectInput(
      inputId = "tabla_join1",
      choices = names_tbls,
      selected = input$tabla_join1
    )
  }) 
  # 
  # %>%
  #   bindEvent(watch("sync"))
  
  
  # 
  observe({
    req(input$tabla_join)
    updateSelectInput(
      inputId = "cols_left",
      choices = colnames(datasql$tbl_sel(input$tabla_join,1)),
      selected = input$cols_left
    )
  })
  
  
  #%>%
  #   bindEvent(watch("cambio"))
  # 
  
  observe({
    req(input$tabla_join1)
    updateSelectInput(
      inputId = "cols_right",
      choices =  colnames(datasql$tbl_sel(input$tabla_join1,1)),
      selected = input$cols_right
    )
    updateSelectInput(
      inputId = "cols_join",
      choices =  c(colnames(datasql$tbl_sel(input$tabla_join,1)),colnames(datasql$tbl_sel(input$tabla_join1,1))),
      selected = input$cols_join
    )
  })
  
}