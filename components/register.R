box::use(
  ./utils,
  ./datasql, 
  ./forms,
  shiny[...], 
  gargoyle[init,trigger,watch,on],
  DBI[dbGetQuery],
  shinyalert[shinyalert],
  shinyjs[reset,toggleState],
  DT[...],
  data.table[data.table],
  R6[R6Class]
)

#' @description UI del Panel Registro   
#' @export
registroUI<-function(id,label='register'){
  ns<-NS(id)
  
  tagList(
    br(),
    
    utils$action_button(
      id =ns('personal'),
      label='Crear Personal',
      icon='user'
    ),
    utils$action_button(
     id= ns('job'),
      label='Crear job',
      icon='user'
    ),
    
    br(),
    br(),
    br(),

    fluidRow(
      utils$DT_table('Data Personal',
                     ns('table_personal'))),
    
    br(),
    br(),
    br(),
    
    fluidRow(
      utils$DT_table('Data Jobs',
                     ns('table_job')))
    
  )
  
}

#' @description Server del Panel Registro   
#' @export
registroServ<-function(input,output,session,user){
  
  ns<-session$ns
  init('personal','outputpersonal','form','outputform','delete','output_delete','personal_edit','outputpersonal_edit') #reactive data personal)
  z <- new.env()
  
  # PERSONAL #######
  #' @description Abre el form de ingreso al Personal  
    # Entrar al agregar form para Nuevo Personal #
  observeEvent(input$personal,{
    
    z$user_id_=dbGetQuery(datasql$con,sprintf("select row_id from tbl_user where TRIM(id)= TRIM('%s')",user$id_user))
    
    if (length(z$user_id_$row_id)==0){
      utils$alert_register()
    }else{
    
    forms$form_personal(session,"Nuevo Personal",ns("submit_button"), "Guardar")
      
    trigger('form')
    }
  })
 


  on("form", {
    

    # Triggering the flag
    trigger("outputform")
  })

  #' @description Inserta el nuevo personal  
# Insert Data Personal ####
  observeEvent(input$submit_button, priority = 20,{

    insert=dbGetQuery(datasql$con,sprintf("
    INSERT INTO tbl_personal 
    (id, 
    name,
    second_name, 
    age,
    email,
    adress,
    user_id,
    row_id)
    VALUES ('%s', '%s', '%s', %s,'%s','%s',%s,
                                  (SELECT MAX(row_id)+1 from tbl_personal C));",
                                  input$id
                                  ,input$name,
                                  input$second_name,
                                  input$age,
                                  input$email,
                                  input$adress,
                                  z$user_id_$row_id)
                      )
    
    
    insert3=dbGetQuery(datasql$con,sprintf("
    INSERT INTO tbl_logs
    (logs, 
    timestamp,user_id,
    row_id)
    VALUES ('Ingreso Personal','%s',%s,(SELECT MAX(row_id)+1 from tbl_logs C));",
                                           as.numeric(Sys.time()),z$user_id_$row_id)
    )
    
        shinyjs::reset("entry_form_personal")
        removeModal()
        shinyalert("¡Felicitaciones!",
                   paste0("Se ha creado el usuario ",input$name),
                   type = 'success')
        
    

  })
  
  #' @description Actualiza las tablas
  observe({
    user$id_user
    user$search
    input$yes_button_personal
    input$submit_button
    input$submit_edit_button
    input$yes_button_job
    input$submit_edit_button_job
    input$submit_button_job
    if(user$id_user ==''){}else{
    z$user_id_=dbGetQuery(datasql$con,sprintf("select row_id from tbl_user where TRIM(id)= TRIM('%s')",user$id_user))
    zi$personal=datasql$tbl_sel('tbl_job', z$user_id_$row_id)
    z$personal=datasql$tbl_sel('tbl_personal', z$user_id_$row_id)
    
    }
  })
  
  # Tabla Personal ####
  #' @description Output tabla de Personal
  output$table_personal<-renderDataTable({

    user$id_user
    user$search



    if(user$id_user=='' | is.null(z$personal) ){
      validate(
        need(!is.null(user$user_id) | !is.null(z$personal),HTML('Porfavor, Registrece para cargar su data. Luego continue ingresando data para que cargue su historial'))
      )
      
    }else{
      watch("personal")
      
      z$personal$profile=utils$button_table_two(id=ns('edit_butt_personal'),icon='fa fa-pencil',id2=ns('delete_button_personal'),icon2='fa-minus')
      z$personal_list=list(users=data.table(z$personal),user_df= z$personal)
 
      data_limp=z$personal_list$users[,.('Id'=id,'Name'=name,'Second Name'=second_name,Age=age,Adress=adress,
                                     Email=email,profile)]
      utils$datatable_output(data_limp)
      
    
}
    })
  
  
  # Delete Table Personal####
  #' @description Modal para confirmar eliminar personal
  observeEvent(input$delete_button_personal, priority = 20,{
    
    utils$modalito(session,ns("yes_button_personal"),"¿Esta seguro que desea eliminar este dato?")

    trigger("delete") 
  })
  
  on("delete", {
    # Triggering the flag
    SQL_df <-z$personal_list$users
    z$row_id <- SQL_df[input$table_personal_row_last_clicked, "row_id"]
 
    trigger("output_delete")
  })
 
  
  #' @description Confirmar eliminación de personal
  observeEvent(input$yes_button_personal, priority = 20,{

    delete=dbGetQuery(datasql$con,sprintf('delete from tbl_personal where row_id = %s',z$row_id))
    insert3=dbGetQuery(datasql$con,sprintf("
    INSERT INTO tbl_logs
    (logs, 
    timestamp,user_id,
    row_id)
    VALUES ('Eliminación Usuario','%s',%s,(SELECT MAX(row_id)+1 from tbl_logs C));",
                                           as.numeric(Sys.time()),z$user_id_$row_id)
    )
    removeModal()
  })
  
  observeEvent(input$yes_button_personal | input$submit_button | input$submit_edit_button,{

    trigger('personal')
  })
  
  
  on("personal", {

    trigger("outputpersonal")
  })
  

  # Editar personal ####
  #' @description Editar data del personal
  observeEvent(input$edit_butt_personal,{
    forms$form_personal(session,"Editar Usuario",ns("submit_edit_button"), "Guardar")
    SQL_df_2=z$personal_list$user_df
    updateTextInput(session, "id", value = SQL_df_2[input$table_personal_row_last_clicked, 'id'])
    updateNumericInput(session, "age", value = SQL_df_2[input$table_personal_row_last_clicked, 'age'])
    updateTextInput(session, "second_name", value = SQL_df_2[input$table_personal_row_last_clicked, 'second_name'])
    updateTextInput(session, "email", value = SQL_df_2[input$table_personal_row_last_clicked, 'email'])
    updateTextInput(session, "adress", value = SQL_df_2[input$table_personal_row_last_clicked, 'adress'])
    updateTextInput(session, "name", value = SQL_df_2[input$table_personal_row_last_clicked, 'name'])
    
    trigger('personal_edit')
  })
  
  on("personal_edit", {
    # Triggering the flag
    trigger("outputpersonal_edit")
  })
  

  
  

  

  #' @description Actualizar en la base de datos la data del personal seleccionado
  ## Envio a actualizar ####
  observeEvent(input$submit_edit_button, priority = 20,{
    
    SQL_df =z$personal_list$users
    row_selection <- SQL_df[input$table_personal_row_last_clicked, "row_id"]
   
    up_post_act= dbGetQuery(datasql$con,sprintf("UPDATE tbl_personal set id= '%s',name ='%s',second_name='%s',age=%s,email='%s',adress='%s'
                                    where row_id= %s",input$id,input$name,input$second_name,input$age,
                                                        input$email,input$adress,row_selection))
    insert3=dbGetQuery(datasql$con,sprintf("
    INSERT INTO tbl_logs
    (logs, 
    timestamp,user_id,
    row_id)
    VALUES ('Actualización Usuario','%s',%s,(SELECT MAX(row_id)+1 from tbl_logs C));",
                                           as.numeric(Sys.time()),z$user_id_$row_id)
    )
    
    shinyjs::reset("entry_form_personal")
    removeModal()
    
  })
  
  
  observeEvent(input$no_button, priority = 20,{
    
    removeModal()
    
  })
  
  
  ##Lista de valores a llenar antes de enviar  ####
  fieldsMandatory <- c('id','name')
  
  #Activar boton cuando este lleno todo  ####
  observe({
    #Hacer reativo los botoenes de agregar y actualizar

    input$submit_button
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    shinyjs::toggleState(id = "submit_button", condition = mandatoryFilled)
    
  })
  
  
  
  
  # JOBS ########
  
  init('personal1','outputpersonal1','form1','outputform1','delete1','output_delete1','personal_edit1','outputpersonal_edit1') #reactive data personal)

  #' @description Abre el form de ingreso al job  
  # Entrar al agregar form para Nuevo job #
  observeEvent(input$job,{

    if (length(z$user_id_$row_id)==0){
      utils$alert_register()
    }else{
    forms$form_job(session,"Nuevo Puesto",ns("submit_button_job"), "Guardar")
    trigger('form1')
    }
  })
  
  on("form1", {
    # Triggering the flag
    trigger("outputform1")
  })
  
  #' @description Insertar nuevo job
  # Insert Data Job ####
  observeEvent(input$submit_button_job, priority = 20,{
    userr=dbGetQuery(datasql$con,sprintf("select row_id from tbl_user where TRIM(id)= TRIM('%s')",user$id_user))
    
    if (length(userr$row_id)==0){
    utils$alert_register()
    }else{
    insert=dbGetQuery(datasql$con,sprintf("
    INSERT INTO tbl_job 
    (job, 
    description,
    user_id,
    row_id)
    VALUES ('%s', '%s',%s,(SELECT MAX(row_id)+1 from tbl_job C));",
                                          input$name
                                          ,input$description,
                                          z$user_id_$row_id)
    )
    insert3=dbGetQuery(datasql$con,sprintf("
    INSERT INTO tbl_logs
    (logs, 
    timestamp,user_id,
    row_id)
    VALUES ('Creación Job','%s',%s,(SELECT MAX(row_id)+1 from tbl_logs C));",
                                           as.numeric(Sys.time()),z$user_id_$row_id)
    )
    shinyjs::reset("entry_form_job")
    removeModal()
    shinyalert("¡Felicitaciones!",
               paste0("Se ha creado el Puesto ",input$name),
               type = 'success')
    
    }
    
  })

  # Tabla Job ####
  #' @description Output tabla personal
output$table_job<-renderDataTable({
  user$search
  watch("personal1")

  if(user$id_user=='' | is.null(zi$personal)){
    validate(
      need(!is.null(user$user_id) | !is.null(zi$personal) ,HTML('Porfavor, Registrece para cargar su data. Luego continue ingresando data para que cargue su historial'))
    )
    
  }else{

    zi$personal$profile=utils$button_table_two(id=ns('edit_butt_job'),icon='fa fa-pencil',id2=ns('delete_button_job'),icon2='fa-minus')
    zi$personal_list=list(users=data.table(zi$personal),user_df= zi$personal)
  data_limp=zi$personal_list$users[,.(Name=job,Description=description,profile)]
  utils$datatable_output(data_limp)
  
}
  })



# Delete Table Job####

zi <- new.env()


#' @description Abre el modulo para confirmar eliminar job
observeEvent(input$delete_button_job, priority = 20,{
  
  utils$modalito(session,ns("yes_button_job"),"¿Esta seguro que desea eliminar este dato?")
  
  trigger("delete1") 
})

on("delete1", {
  # Triggering the flag
  SQL_df <-zi$personal_list$users
  zi$row_id <- SQL_df[input$table_job_row_last_clicked, "row_id"]
  
  trigger("output_delete1")
})

#' @description Elimina el job seleccionado en la base de datos
observeEvent(input$yes_button_job, priority = 20,{
  
  delete=dbGetQuery(datasql$con,sprintf('delete from tbl_job where row_id = %s',zi$row_id))
  insert3=dbGetQuery(datasql$con,sprintf("
    INSERT INTO tbl_logs
    (logs, 
    timestamp,user_id,
    row_id)
    VALUES ('Eliminación Job','%s',%s,(SELECT MAX(row_id)+1 from tbl_logs C));",
                                         as.numeric(Sys.time()),z$user_id_$row_id)
  )
  removeModal()
})

observeEvent(input$yes_button_job | input$submit_button_job | input$submit_edit_button_job ,{
  

  trigger('personal1')
})


on("personal1", {
  
  trigger("outputpersonal1")
})



#' @description Carga data seleccionada en el form job para actualizar
# Editar Job ####
# Entrar al form para Editar Job #
observeEvent(input$edit_butt_job,{
  forms$form_job(session,"Editar Usuario",ns("submit_edit_button_job"), "Guardar")
  SQL_df_2=zi$personal_list$user_df
  updateTextAreaInput(session, "description", value = SQL_df_2[input$table_job_row_last_clicked, 'description'])
  updateTextInput(session, "name", value = SQL_df_2[input$table_job_row_last_clicked, 'job'])
  
  trigger('personal_edit1')
})

on("personal_edit1", {
  # Triggering the flag
  trigger("outputpersonal_edit1")
})

#' @description Actualizar data en la base de datos
## Envio a actualizar ####
observeEvent(input$submit_edit_button_job, priority = 20,{
  
  SQL_df =zi$personal_list$users
  row_selection <- SQL_df[input$table_job_row_last_clicked, "row_id"]
  
  up_post_act= dbGetQuery(datasql$con,sprintf("UPDATE tbl_job set job ='%s',description='%s'
                                    where row_id= %s",input$name,input$description,row_selection))
  insert3=dbGetQuery(datasql$con,sprintf("
    INSERT INTO tbl_logs
    (logs, 
    timestamp,user_id,
    row_id)
    VALUES ('Actualización Job',%s,%s,(SELECT MAX(row_id)+1 from tbl_logs C));",
                                         as.numeric(Sys.time()),z$user_id_$row_id)
  )
  shinyjs::reset("entry_form_job")
  removeModal()
  
})
return(input)

}