
box::use(
  shiny[selectInput,column],   qs[qread]
)

x=qread('/home/wladimir/Escritorio/Wladimir/Proyecto_Ais/color.qs')
color_cod=x$B
names(color_cod)=toupper(x$A)

#' @export
select_color<-function(id,label,num){
column(4,selectInput(id, 
            label=label,
            choices =color_cod,selected=color_cod[num]),style='margin-right: 15px;')
}
