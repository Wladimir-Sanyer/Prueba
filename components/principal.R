box::use(
  shiny[...], 
  DBI[dbGetQuery],
  DT[...],
  ./utils, 
  ./datasql,
)


#' @description UI Modulo del Panel Home
#' @export
HomeUI<-function(id,label='fresh'){
  ns<-NS(id)
  tagList(
    br(),
      fluidRow(utils$select_color(ns("color_background"), 
                                  label="Seleccione el color de fondo",458),
               utils$select_color(ns("color_font"), 
                                  label="Seleccione el color de letra",100)),
    
    br(),
    utils$action_button(
      id= ns('save'),
      label='Guardar Cambios',
      icon='save'
    ),
    
    br(),br(),br(),
    utils$action_button(
      id= ns('reload'),
      label='Actualizar',
      icon='sync',
      style='color: #fff; background-color: #022461; border-color: #022461;position: relative;width: 15%;left: 80%;top:10%'
    ),
    
    br(),
    br(),
    br(),
    fluidRow(
      utils$DT_table('Logs de Cambios',ns('table_changes')))
    )
}

#' @description Server Modulo del Panel Home
#' @export
HomeServ<-function(input,output,session,user){
  z<- new.env()
  
  #' @description Tabla de Logs
  output$table_changes<-renderDataTable({
    user$search
    user$id_user
    input$save
    input$reload
    if(user$id_user==''){
      validate(
        need(!is.null(user$user_id),HTML('Porfavor, Registrece para cargar su data. Luego continue ingresando data para que cargue su historial'))
      )
      
    }else{
      z$personal_list=dbGetQuery(datasql$con,sprintf("select row_id from tbl_user where TRIM(id)= TRIM('%s')",user$id_user))
      z$personal_list_final=dbGetQuery(datasql$con,sprintf("select logs as Logs, datetime(timestamp,'unixepoch') as Timestamp from tbl_logs where user_id= %s",z$personal_list))
      
      #data_limp=z$personal_list_final
      utils$datatable_output(z$personal_list_final)

    }
  })
  

  
  #' @description Update background y font color
  observe({

    user$id_user
    input$save
    z$us=dbGetQuery(datasql$con,sprintf("select row_id from tbl_user where TRIM(id)= TRIM('%s')",user$id_user))

    if(user$id_user=='' | length(z$us$row_id)==0){
      updateSelectInput(
        inputId = "color_background",
        choices = utils$color_sel(),
        selected = utils$color_sel()[458]

      )

      updateSelectInput(
        inputId = "color_font",
        choices = utils$color_sel(),
        selected =utils$color_sel()[100]
      )


    }else{

    userr=dbGetQuery(datasql$con,sprintf("select color1,color2 from tbl_color color
                                         left join tbl_user user
                                         on color.user_id=user.row_id
                                         where TRIM(id)= TRIM('%s')",user$id_user))

    updateSelectInput(
      inputId = "color_background",
      choices = utils$color_sel(),
      selected = userr$color1
    )

      updateSelectInput(
        inputId = "color_font",
        choices = utils$color_sel(),
        selected = userr$color2
      )
      }

  })
  
  #' @description Insertar data y logs al guardar
  
  observeEvent(input$save,{
    
    
    if(user$id_user=='' | length(z$us$row_id)==0){
      utils$alert_register()
    }else{
     
      up_post_act= dbGetQuery(datasql$con,
                              sprintf("UPDATE tbl_color set color1 ='%s',color2='%s'where user_id= %s",
                                      input$color_background,input$color_font,z$us$row_id))

      insert3=dbGetQuery(datasql$con,sprintf("
    INSERT INTO tbl_logs
    (logs, 
    timestamp,user_id,
    row_id)
    VALUES ('Actualización color','%s',%s,(SELECT MAX(row_id)+1 from tbl_logs C));",
                                             as.numeric(Sys.time()),z$us$row_id)
      )
    }
  })
  
return(input)


  
}


