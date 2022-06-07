box::use(
  DBI[dbConnect,dbGetQuery],   RSQLite[...],shinyalert[shinyalert]
)
#' @export
con<-dbConnect(SQLite(),"Database.db")

#' #' @export
#' tbl_user=dbGetQuery(con,'Select * from tbl_user')
#' tbl_personal=dbGetQuery(con,'Select * from tbl_personal')
#' tbl_job=dbGetQuery(con,'Select * from tbl_job')


#' @export
tbl_sel<-function(tbl,user_id){
    
  if(is.null(user_id)){
    tbl_sel=NULL
  }else{
  tbl_sel=dbGetQuery(con,paste0('Select * from ',tbl,' where user_id=',user_id))
  }
  return(tbl_sel)
}

#' @export
tbl_sel_all<-function(tbl){
    tbl_sel=dbGetQuery(con,paste0('Select * from ',tbl))
  return(tbl_sel)
}