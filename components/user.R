box::use(
  shiny[...], ./forms, shinyjs[reset,toggleState],gargoyle[init,trigger,watch,on],
  ./utils,DBI[dbGetQuery],./datasql,shinyalert[shinyalert]
  
)

#' @export
#' Se utilizara este modulo que reemplaza el shinyproxyname para saber que usuario esta
#' ingresado en la base de datos y poder recuperar sus avances previos
userUI<-function(id,label='user'){
  ns<-NS(id)
  tagList(
    fluidRow(
    htmlOutput(ns('name_user'))
    ),br(),
    fluidRow(
      utils$action_button(
        ns('client'),
        label='Crear Usuario',
        icon='user'
      )
    ),br(),
  fluidRow(
    column(6,
      textInput(ns('id_user'),
                label = NULL,
                placeholder = 'Ingrese su id para recuperar cambios',
                value = '')
    )
  ,column(6,
  actionButton(
    ns('search'),
    label = 'Buscar',
    icon=icon('check'))
  )
  )
  )
}


#' @export
userServ<-function(input,output,session){
  
  ns<-session$ns
  init('form','outputform') #reactive data personal)
  
  
  # CREAR USUARIOS #######
  
  ##Reactive Form Nuevo Usuario#####
  # Entrar al agregar form para Nuevo Usuario #
  observeEvent(input$client,{
    forms$form_user(session,"Cree su usuario",ns("submit_button"), "Guardar")
    trigger('form')
  })
  
  on("form", {
    # Triggering the flag
    trigger("outputform")
  })
  
  
  
  observeEvent(input$submit_button, priority = 20,{
    exist=dbGetQuery(datasql$con,paste0("SELECT  CASE WHEN EXISTS (Select ID FROM tbl_user where TRIM(ID) =
                               TRIM('",input$ID,"')) THEN 1 ELSE 0 END"))
    
    
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
    
    shinyjs::reset("entry_form_user")
    removeModal()
    shinyalert(paste0("¡Bienvenido ",input$name, "!" ),
               paste0("Gracias por usar la presente app"),
               type = 'success')
    
    }
    
  })
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
    
    
      
  
return(input)
}