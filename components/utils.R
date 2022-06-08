
box::use(
  shiny[...], 
  qs[qread],
  argonR[argonCard],
  shinyalert[shinyalert],
  DT[...],
  shinycssloaders[withSpinner],
  data.table[data.table]
)

#' @description Funcion para traer todos los colores a mostrar para background y font
#' @export
color_sel<-function(){
x=qread('data/color.qs')
color_cod=x$B
names(color_cod)=toupper(x$A)
return(color_cod)
}

#' @description Funcion para crear los selecinput de la seleccion de colores
#' @param id es el id del selectinput
#' @param label es el etiqueta que tendra dicho selectinput
#' @param num es el numero del color con el iniciara la aplicacion por default
#' @export
select_color<-function(id,label,num){
column(4,
       selectInput(
       id, 
       label=label,
       choices =color_sel(),
       selected=color_sel()[num]),
       style='margin-right: 15px;')
}

#' @description Funcion para crear un action button personalmente estilizado
#' @param id es el id del actionbutton
#' @param label es el etiqueta que tendra dicho actionbutton
#' @param icon es el icono que tendra dicho actionbutton
#' @param style es el estilo que tendra dicho actionbutton
#' @export
action_button<-function(id,label,icon,style='color: #fff; background-color: #022461; border-color: #022461,margin-right: 50px;'){
  actionButton(id,
               label=label ,
               icon = icon(icon),
               style=style
               )
}



# Two Button DT Table ####
#' @description Botones para editar y eliminar en el Datatable
#' @param id es el id del primer button de la tabla
#' @param icon es el icono del primer button de la tabla
#' @param id2 es el id del segundo button de la tabla
#' @param icon2 es el icono del segundo button de la tabla
#' @export
button_table_two<-function(id,icon,id2,icon2){
  paste0('<button id=','\"',id,'\"','type=\"button\" class=\"btn btn-link btn-sm\"
          onclick=\"Shiny.onInputChange(&quot;',id,'&quot;,  Math.random())\"><i class=\"fas ',icon,' fa-2x\"></i></button>',
         '<button id=','\"',id2,'\"','type=\"button\" class=\"btn btn-link btn-sm\"
          onclick=\"Shiny.onInputChange(&quot;',id2,'&quot;,  Math.random())\"><i class=\"fas ',icon2,' fa-2x\"></i></button>')
  
}


#' @description Divsion para Box con DT
#' @param title titlo del argon card (box card) 
#' @param es el id del datatable
#' @export
DT_table<-function(title,id){
  div(
    argonCard(title = HTML(paste0('<i> <b>',title,'</b></i>')),
              width=12,
              withSpinner(
                type = 6,
                color = "#244880", 
                DT::dataTableOutput(id)
                )
              ),
    style = "overflow-y: auto;overflow-x: auto;height:100%;width:100%")
}


## Modal para aceptar o negar una accion importante ####
#' @description Modal para aceptar o negar una accion importante
#' @param session la session en la que se encuentra
#' @param aceptar el button de aceptar el proceso
#' @param msj el mensaje a mostrar
#' @export
modalito<-function(session,aceptar,msj){
  ns<-session$ns
  showModal(
    modalDialog(id="delete_modal_total",
                title =  "Aviso",msj,
                br(),
                br(),
                actionButton(aceptar, "Si"),
                actionButton(ns("no_button"), "No"),
                easyClose = TRUE, footer = NULL))
  
}

#' @description Alerta para que el usuario inicie sesion
#' @export
alert_register<-function(){
shinyalert("¡Por favor!",
           paste0("Registrece como usuario para continuar"),
           type = 'error')
}

#' @description Alerta para que el usuario llene todos los campos
#' @export
alert_register_two<-function(){
  shinyalert("¡Por favor!",
             paste0("Registrece como usuario para continuar y llene todos los campos requeridos"),
             type = 'error')
}

#' @description Mensaje de Popover
#' @param id es el id del Popover
#' @param msg es el mensaje que muestra el popover
#' @export
question_buttom<-function(id,msg){

  tags$head(tags$script(HTML(paste0('$(document).ready(function(){
                      $("#',id,'")
                      .popover({html: true,
                      title: " ",
                      content: `',msg,'`,
                      trigger: "hover",delay: {show: 1, hide: 1}
                      });
                      });
                      ' ))))
}

#' @description Boton para el Popover
#' @param id es el id del acttion button del popover
#' @param icon es el icono del action button del popover
#' @param top es el la posicion top en la que estara
#' @param left es la posicion left en la que se encontrara el button popover
#' @param statu es el status del button (por default info)
#' @export
actt_bottom_prev<-function(id,icon='question',top,left=86,statu='info'){
  
  div(
    style=paste0("display:inline-block; vertical-align: middle;position: absolute;margin: auto;width: 5%;left: ",left,"%;top:",top,"%;"),
    actionButton(id,status = statu,
                 label='',
                 icon=icon(icon,lib='font-awesome'),
                 size='xs')
  )
}

#' @description Datatable personalizado
#' @param data es la data a output en el datatable
#' @export
datatable_output<-function(data){
  datatable(data, filter = 'top',selection = 'single',style = "bootstrap4",escape = FALSE, plugins = "ellipsis",
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