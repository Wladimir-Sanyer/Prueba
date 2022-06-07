box::use(
  shiny[...],
  gargoyle[init,trigger,watch,on],
  DT[...],
  R6[R6Class],
  DBI[dbGetQuery],
  ./utils,
  ./datasql
)
names_tbls=c('tbl_personal','tbl_job')

#' @description UI del Panel Relacional   
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
          inputId = ns("relacionar"),
          label = "Relacionar",
          width = "100%",
          class = "col-sm"
        )
      )
      # div(
      #   class = "col-sm",
      #   actionButton(
      #     inputId = ns("inner_join"),
      #     label = "Inner join",
      #     width = "100%",
      #     class = "col-sm"
      #   )
      # ),
      # div(
      #   class = "col-sm",
      #   actionButton(
      #     inputId = ns("anti_join"),
      #     label = "Anti join",
      #     width = "100%",
      #     class = "col-sm"
      #   )
      # )
    ),
    br(),
    fluidRow(
      utils$DT_table('Tabla Relacionada',ns('table_relation')))
  )
  
)

}

#' @description UI del Panel Relacional   
#' @export
relation_tableServ<-function(input,output,session,push,user){
  zoy <- new.env()
  observe({
    updateSelectInput(
      inputId = "tabla_join",
      choices = names_tbls[1],
      selected = input$tabla_join
    )
  }) 
  
  observe({
    updateSelectInput(
      inputId = "tabla_join1",
      choices = names_tbls[2],
      selected = input$tabla_join1
    )
  }) 

  #' @description R6 para limpiar data a mostrar como opcion
  data_to_choice <- R6Class("data_to_choice",
                    public = list(
                      name = NULL,
                      initialize = function(name = NA,user_id=NA) {
                        self$name <- name
                        self$greet()
                      },
                      greet = function() {
                        #cat(paste0("Hello, my name is ", self$name, ".\n"))
                        df=self$name$row_id
                        names(df)=self$name$name
                        return(df)
                      },
                      greet1 = function() {
                        #cat(paste0("Hello, my name is ", self$name, ".\n"))
                        df=self$name$row_id
                        names(df)=self$name$job
                        return(df)
                      }
                    )
  )
  
  #' @description Actualizr columna derecha y columna izquierda para unir
  observe({
    req(input$tabla_join)
    req(input$tabla_join1)
    push$submit_button_job
    push$submit_button
    user$id_user
    zoy$y=dbGetQuery(datasql$con,sprintf("select row_id from tbl_user where TRIM(id)= TRIM('%s')",user$id_user))
    
    if(user$id_user=='' | length(zoy$y$row_id)==0){
      
      updateSelectInput(
        inputId = "cols_left",
        choices = NULL,
        selected = NULL
      )
      
      updateSelectInput(
        inputId = "cols_right",
        choices =  NULL,
        selected = NULL
      )
      
    }else{

    data=data_to_choice$new(datasql$tbl_sel(input$tabla_join,zoy$y))
    updateSelectInput(
      inputId = "cols_left",
      choices = data$greet(),
      selected = input$cols_left
    )
    
    data1=data_to_choice$new(datasql$tbl_sel(input$tabla_join1,zoy$y))
    
    updateSelectInput(
      inputId = "cols_right",
      choices =  data1$greet1(),
      selected = input$cols_right
    )
    
    }
    
  })
  

  
  #' @description Nodo para relacionar Personal-Job
  observe({
    req(input$tabla_join1)
    req(input$tabla_join)

        updateSelectInput(
      inputId = "cols_join",
      choices =  c(colnames(datasql$tbl_sel_all(input$tabla_join)[,1:6]),colnames(datasql$tbl_sel_all(input$tabla_join1)[,1:2])),
      selected = input$cols_join
    )
  })
  
  init('princ','second')
  #' @description Insertar data de la relacion
  observeEvent(input$relacionar,{
    if(user$id_user=='' | length(zoy$y$row_id)==0 | is.null(input$cols_join)){
    
      utils$alert_register_two()

    }else{
  
    insert=dbGetQuery(datasql$con,sprintf("
    INSERT INTO tbl_relation (%s,  %s,tbl_user,row_id)
    VALUES (%s, %s,%s,(SELECT MAX(row_id)+1 from tbl_relation C));",
                                          input$tabla_join,input$tabla_join1,
                                          input$cols_left,input$cols_right,zoy$y$row_id)
    )
    
    
  
    zoy$v=dbGetQuery(datasql$con,sprintf("
          Select %s from tbl_relation relation
           left join tbl_personal personal
           on relation.tbl_personal = personal.row_id
           left join tbl_job job
           on relation.tbl_job = job.row_id
           where personal.name is not null 
           and tbl_user= %s order by relation.row_id",paste0(input$cols_join,collapse = ','),zoy$y$row_id))
    
    insert3=dbGetQuery(datasql$con,sprintf("
    INSERT INTO tbl_logs
    (logs, 
    timestamp,user_id,
    row_id)
    VALUES ('Relación Personal - Job','%s',%s,(SELECT MAX(row_id)+1 from tbl_logs C));",
                                           as.numeric(Sys.time()),zoy$y$row_id)
    )
    trigger('princ')
    }
  })
  

  #' @description Output de la tabla relacion
  output$table_relation<-renderDataTable({
    input$relacionar
   
    if(user$id_user=='' | length(zoy$y$row_id)==0 | is.null(input$cols_join)){
      validate(
        need(!is.null(user$user_id),HTML('Porfavor, Registrece para cargar su data. Luego continue ingresando data para que cargue su historial'))
      )
      
    }else{
      watch('princ')
      utils$datatable_output(zoy$v)
      
      
      
    }
  })
  
  
}