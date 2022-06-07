box::use(
  shiny[...], 
  ./utils, 
  ./datasql,
  DBI[dbGetQuery],
  DT[...]
)



#' @export
HomeUI<-function(id,label='fresh'){
  ns<-NS(id)
  tagList(
    fluidRow(
    tags$style("#fluidpage{margin-left:5%; margin-right:0%;},
                        h3 {font-family: 'Lobster', cursive;font-style:italic;  text-align: center;font-weight: 500;line-height: 1.1;color: #434a42;}"),
   
    ),br(),
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

#' @export
HomeServ<-function(input,output,session,user){
  z<- new.env()
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
      
      data_limp=z$personal_list_final
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


