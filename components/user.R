box::use(
  shiny[...],
  ./forms,
  shinyjs[reset,toggleState],
  gargoyle[init,trigger,watch,on],
  ./utils,
  DBI[dbGetQuery],
  ./datasql,
  shinyalert[shinyalert]
  
)

#' @description UI Modulo User
#' @export
#' Se utilizara este modulo que reemplaza el shinyproxyname para saber que usuario esta
#' ingresado en la base de datos y poder recuperar sus avances previos
userUI<-function(id,label='user'){
  ns<-NS(id)
  tagList(
    tags$head(
              tags$link(rel = "stylesheet", 
                        type = "text/css",
                        href = "styles.css")
              ),
    fluidRow(
    htmlOutput(ns('name_user'))
    ),
    br(), 
    fluidRow(
      utils$action_button(
        ns('client'),
        label='Crear Usuario',
        icon='user'
      )
    ),
    br(),
  fluidRow(
    column(6,
      textInput(ns('id_user'),
                label = NULL,
                placeholder = 'Ingrese su id para recuperar cambios',
                value = '')
    ),
    utils$actt_bottom_prev(id=ns('eq'),
                           top=33,
                           left=25),
    utils$question_buttom(id=ns('eq'),
                          msg = 'Por favor registrece e ingrese un ID valido')
  ,column(4,
  actionButton(
    ns('search'),
    label = 'Buscar',
    icon=icon('check'))
  )
  ,column(4,
          actionButton(
            ns('close'),
            label = 'Reiniciar Sesión',
            icon=icon('sync'))
  )
 )
)
}

#' @description Server Modulo User
#' @export
userServ<-function(input,output,session){
  
  ns<-session$ns
  init('form','outputform') #reactive data personal)
  
  
# CREAR USUARIOS #######
#' @description  Entrar al form para agregar Nuevo Usuario #
observeEvent(input$client,{
 forms$form_user(session,"Cree su usuario",ns("submit_button"), "Guardar")
    trigger('form')
  })
  
  on("form", {
    # Triggering the flag
    trigger("outputform")
  })
  
  
  #' @description  Inserta en la base de datos el nuevo usuario, colores predeterminados,logs #
  observeEvent(input$submit_button, priority = 20,{
    exist=dbGetQuery(datasql$con,paste0("SELECT  CASE WHEN EXISTS (Select id FROM tbl_user where TRIM(id) =
                               TRIM('",input$id,"')) THEN 1 ELSE 0 END"))
    
    
    if(input$id=='' | exist==1){
      shinyalert(paste0("¡Porfavor!" ),
                 paste0("Ingrese un Id que no este en blanco y no exista"),
                 type = 'error')
    }else{

    insert=dbGetQuery(datasql$con,sprintf("
    INSERT INTO tbl_user
    (id, 
    user,
    row_id)
    VALUES ('%s', '%s',(SELECT MAX(row_id)+1 from tbl_user C));",
                                          input$id
                                          ,input$name)
    )
    
    insert2=dbGetQuery(datasql$con,paste0("
    INSERT INTO tbl_color
    (color1, 
    color2,user_id,
    row_id)
    VALUES ('#E6E6FA', '#B0171F',(SELECT MAX(row_id) from tbl_user),
                                           (SELECT MAX(row_id)+1 from tbl_color C));")
    )
    
    insert3=dbGetQuery(datasql$con,sprintf("
    INSERT INTO tbl_logs
    (logs, 
    timestamp,user_id,
    row_id)
    VALUES ('Creación Usuario','%s',(SELECT MAX(row_id) from tbl_user),
                                           (SELECT MAX(row_id)+1 from tbl_logs C));",
                                           as.numeric(Sys.time()))
    )
    
    
    
    
    shinyjs::reset("entry_form_user")
    removeModal()
    shinyalert(paste0("¡Bienvenido ",input$name, "!" ),
               paste0("Gracias por usar la presente app"),
               type = 'success')
    
    session$reload()
    
    }
    
  })
  
  

  #' @description Muestra el nombre del Usuario   
  y <- new.env()
  init('user','output_user')
  
    observeEvent(input$search,{
      y$user=dbGetQuery(datasql$con,sprintf("select user from tbl_user where TRIM(id)= TRIM('%s')",input$id_user))
      trigger("user") 
      
    })
    output$name_user<-renderText({
      input$search
      HTML(paste0("<b><i style=color:black;>Bienvenid@ ", y$user$user,"  </b>"))
    })
    
    #' @description Reiniciar Sesión
    observeEvent(input$close,{
      session$reload()
    }) 
      
  
return(input)
}