
box::use(
  shiny[...],   qs[qread],argonR[argonCard],shinyalert[shinyalert],
  DT[dataTableOutput],shinycssloaders[withSpinner]
)


#' @export
color_sel<-function(){
x=qread('/home/wladimir/Escritorio/Wladimir/Proyecto_Ais/color.qs')
color_cod=x$B
names(color_cod)=toupper(x$A)
return(color_cod)
}
#' @export
select_color<-function(id,label,num){
column(4,selectInput(id, 
            label=label,
            choices =color_sel(),selected=color_sel()[num]),style='margin-right: 15px;')
}

#' @export
action_button<-function(id,label,icon,style='color: #fff; background-color: #022461; border-color: #022461,margin-right: 50px;'){
  actionButton(id,label=label ,icon = icon(icon),
               style=style)
}


# Button DT Table ####
#' @export
button_table<-function(id,icon){
  paste0('<button id=','\"',id,'\"','type=\"button\" class=\"btn btn-link btn-sm\"
          onclick=\"Shiny.onInputChange(&quot;',id,'&quot;,  Math.random())\"><i class=\"fas ',icon,' fa-2x\"></i></button>')
  
}

# Two Button DT Table ####
#' @export
button_table_two<-function(id,icon,id2,icon2){
  paste0('<button id=','\"',id,'\"','type=\"button\" class=\"btn btn-link btn-sm\"
          onclick=\"Shiny.onInputChange(&quot;',id,'&quot;,  Math.random())\"><i class=\"fas ',icon,' fa-2x\"></i></button>',
         '<button id=','\"',id2,'\"','type=\"button\" class=\"btn btn-link btn-sm\"
          onclick=\"Shiny.onInputChange(&quot;',id2,'&quot;,  Math.random())\"><i class=\"fas ',icon2,' fa-2x\"></i></button>')
  
}



#' @export
DT_table<-function(title,id){
  div(
    argonCard(title = HTML(paste0('<i> <b>',title,'</b></i>')),width=12,withSpinner(type = 6, color = "#244880", 
                                                                                    DT::dataTableOutput(id))),
    style = "overflow-y: auto;overflow-x: auto;height:100%;width:100%")
}


## Modal para aceptar o negar una accion importante ####
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


#' @export
alert_register<-function(){
shinyalert("¡Por favor!",
           paste0("Registrece como usuario para continuar"),
           type = 'error')
}