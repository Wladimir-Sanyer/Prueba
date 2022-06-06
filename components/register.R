box::use(
  shiny[...],  ./utils, ./datasql, 
  ./forms,gargoyle[init,trigger,watch,on],DBI[dbGetQuery],shinyalert[shinyalert],
  shinyjs[reset,toggleState],DT[...],data.table[data.table]
)


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
    
    br(),br(),br(),

    fluidRow(
      utils$DT_table('Data Personal',ns('table_personal'))),
    
    br(),br(),br(),
    
    fluidRow(
      utils$DT_table('Data Jobs',ns('table_job')))
    
  )
  
}


#' @export
registroServ<-function(input,output,session,user){
  
  ns<-session$ns
  init('form','outputform','delete','output_delete','personal_edit','outputpersonal_edit') #reactive data personal)
  z <- new.env()
  
  # PERSONAL #######
  
  ##Reactive Form Nuevo Usuario#####
  # Entrar al agregar form para Nuevo Usuario #
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
    
        shinyjs::reset("entry_form_personal")
        removeModal()
        shinyalert("¡Felicitaciones!",
                   paste0("Se ha creado el usuario ",input$name),
                   type = 'success')
        
    

  })

  
  output$table_personal<-renderDataTable({
    input$yes_button_personal
    input$submit_button
    input$submit_edit_button
    user$search
    
    if(user$id_user==''){
      validate(
        need(!is.null(user$user_id),HTML('Porfavor, Registrece para cargar su data. Luego continue ingresando data para que cargue su historial'))
      )

    }else{
  
      data_limp=z$personal_list$users[,.('Id'=id,'Name'=name,'Second Name'=second_name,Age=age,Adress=adress,
                                     Email=email,profile)]
    datatable(data_limp, filter = 'top',selection = 'single',style = "bootstrap4",escape = FALSE, plugins = "ellipsis",
              # caption = htmltools::tags$caption( style = 'caption-side: top;text-align: center; color:blue; font-size:100% ;','Data Suelos Irrismart'),
              rownames = F,  extensions = c('Scroller','Buttons'),
              list(deferRender = F, dom = 'Bfrt',autoWidth = TRUE,
                   columnDefs = list(list(className = 'dt-center',width = '220px', targets = "_all")),
                   scrollY = 220, scroller = TRUE, scrollX = T,
                   pageLength = 3,
                   buttons =c('excel','csv','copy','print'),
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#1e3a7b'});",
                     "}")))
    
}
    })
  
  
  # Delete Table Personal####
  


  

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
 
  
  
  observeEvent(input$yes_button_personal, priority = 20,{

    delete=dbGetQuery(datasql$con,sprintf('delete from tbl_personal where row_id = %s',z$row_id))
    
    removeModal()
  })
  
  init('personal','outputpersonal') #reactive data personal)
  observeEvent(input$yes_button_personal | input$submit_button | input$submit_edit_button | input$update,{
    
    z$personal=datasql$tbl_sel('tbl_personal', z$user_id_$row_id)
    z$personal$profile=utils$button_table_two(id=ns('edit_butt_personal'),icon='fa fa-pencil',id2=ns('delete_button_personal'),icon2='fa-minus')
    z$personal_list=list(users=data.table(z$personal),user_df= z$personal)
    trigger('personal')
  })
  
  
  on("personal", {

    trigger("outputpersonal")
  })
  

  # Editar Usuarios ####
  ##Reactive Form General Riego#####
  # Entrar al form para Editar Usuario #
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
  

  
  

  

  
  ## Envio a actualizar ####
  observeEvent(input$submit_edit_button, priority = 20,{
    
    SQL_df =z$personal_list$users
    row_selection <- SQL_df[input$table_personal_row_last_clicked, "row_id"]
   
    up_post_act= dbGetQuery(datasql$con,sprintf("UPDATE tbl_personal set id= '%s',name ='%s',second_name='%s',age=%s,email='%s',adress='%s'
                                    where row_id= %s",input$id,input$name,input$second_name,input$age,
                                                        input$email,input$adress,row_selection))
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
  
  init('form1','outputform1','delete1','output_delete1','personal_edit1','outputpersonal_edit1') #reactive data personal)
  ##Reactive Form Nuevo Job#####
  # Entrar al agregar form para Nuevo Usuario #
  observeEvent(input$job,{
    z$user_id_=dbGetQuery(datasql$con,sprintf("select row_id from tbl_user where TRIM(id)= TRIM('%s')",user$id_user))
    
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
  # Insert Data Job ####
  observeEvent(input$submit_button_job, priority = 20,{
    userr=dbGetQuery(datasql$con,sprintf("select row_id from tbl_user where TRIM(id)= TRIM('%s')",user$id_user))
    
    if (length(userr$row_id)==0){
    utils$alert_register()
    }else{
    insert=dbGetQuery(datasql$con,sprintf("
    INSERT INTO tbl_job 
    (name, 
    description,
    user_id,
    row_id)
    VALUES ('%s', '%s',%s,(SELECT MAX(row_id)+1 from tbl_job C));",
                                          input$name
                                          ,input$description,
                                          z$user_id_$row_id)
    )
    
    shinyjs::reset("entry_form_job")
    removeModal()
    shinyalert("¡Felicitaciones!",
               paste0("Se ha creado el Puesto ",input$name),
               type = 'success')
    
    }
    
  })

output$table_job<-renderDataTable({
  input$yes_button_job
  input$submit_button_job
  input$submit_edit_button_job
  user$search
  data_limp=zi$personal_list$users[,.(Name=name,Description=description,profile)]
  datatable(data_limp, filter = 'top',selection = 'single',style = "bootstrap4",escape = FALSE, plugins = "ellipsis",
            # caption = htmltools::tags$caption( style = 'caption-side: top;text-align: center; color:blue; font-size:100% ;','Data Suelos Irrismart'),
            rownames = F,  extensions = c('Scroller','Buttons'),
            list(deferRender = F, dom = 'Bfrt',autoWidth = TRUE,
                 columnDefs = list(list(className = 'dt-center',width = '220px', targets = "_all")),
                 scrollY = 220, scroller = TRUE, scrollX = T,
                 pageLength = 3,
                 buttons =c('excel','csv','copy','print'),
                 initComplete = JS(
                   "function(settings, json) {",
                   "$(this.api().table().header()).css({'background-color': '#fff', 'color': '#1e3a7b'});",
                   "}")))

  })



# Delete Table Job####

zi <- new.env()



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

observeEvent(input$yes_button_job, priority = 20,{
  
  delete=dbGetQuery(datasql$con,sprintf('delete from tbl_job where row_id = %s',zi$row_id))
  
  removeModal()
})

init('personal1','outputpersonal1') #reactive data personal)
observeEvent(input$yes_button_job | input$submit_button_job | input$submit_edit_button_job ,{
  
  zi$personal=datasql$tbl_sel('tbl_job', z$user_id_$row_id)
  zi$personal$profile=utils$button_table_two(id=ns('edit_butt_job'),icon='fa fa-pencil',id2=ns('delete_button_job'),icon2='fa-minus')
  zi$personal_list=list(users=data.table(zi$personal),user_df= zi$personal)
  trigger('personal1')
})


on("personal1", {
  
  trigger("outputpersonal1")
})




# Editar Usuarios ####
##Reactive Form General Riego#####
# Entrar al form para Editar Usuario #
observeEvent(input$edit_butt_job,{
  forms$form_job(session,"Editar Usuario",ns("submit_edit_button_job"), "Guardar")
  SQL_df_2=zi$personal_list$user_df
  updateTextAreaInput(session, "description", value = SQL_df_2[input$table_job_row_last_clicked, 'description'])
  updateTextInput(session, "name", value = SQL_df_2[input$table_job_row_last_clicked, 'name'])
  
  trigger('personal_edit1')
})

on("personal_edit1", {
  # Triggering the flag
  trigger("outputpersonal_edit1")
})

## Envio a actualizar ####
observeEvent(input$submit_edit_button_job, priority = 20,{
  
  SQL_df =zi$personal_list$users
  row_selection <- SQL_df[input$table_job_row_last_clicked, "row_id"]
  
  up_post_act= dbGetQuery(datasql$con,sprintf("UPDATE tbl_job set name ='%s',description='%s'
                                    where row_id= %s",input$name,input$description,row_selection))
  shinyjs::reset("entry_form_job")
  removeModal()
  
})

}