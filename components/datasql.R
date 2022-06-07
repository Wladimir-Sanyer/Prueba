box::use(
  DBI[dbConnect,dbGetQuery],  
  RSQLite[...],
  shinyalert[shinyalert]
)
#' @export
con<-dbConnect(SQLite(),"./data/Database.db")

#' @description Función para traer tabla del usuario que ingreso
#' @param tbl nombre de la tabla
#' @param user_id nombre de la tabla
#' @export
tbl_sel<-function(tbl,user_id){
  if(is.null(user_id)){
    tbl_sel=NULL
  }else{
  tbl_sel=dbGetQuery(con,paste0('Select * from ',tbl,' where user_id=',user_id))
  }
  return(tbl_sel)
}


#' @description Función para traer ttoda la tabla
#' @param tbl nombre de la tabla
#' @export
tbl_sel_all<-function(tbl){
    tbl_sel=dbGetQuery(con,paste0('Select * from ',tbl))
  return(tbl_sel)
}