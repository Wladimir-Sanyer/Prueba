library(DBI)
library(RSQLite)
con<-dbConnect(SQLite(),"Database.db")

insert_table<-function(name,dt){
  con<-dbConnect(SQLite(),"Database.db")
  dbWriteTable(con,name = name,value = dt,overwrite=TRUE)
}

# Data Usuario
user=c('Wladimir','Junior')
id=c('1234','3456') #Cedula
row_id=c(1:length(user))
df_0=data.frame(user,id,row_id)
insert_table('tbl_user',df_0)
dbGetQuery(con,'Select * from tbl_user')

# Data Personal
id=c('093554545','055544785','0822455')
name=c('Sheldon','Lonard','Penny')
second_name=c('Cooper','Wong','Philip')
age=c(20,24,30)
adress=c('San Felipe','San Carlos','Huancavilca')
email=c('she@gmail.com','wongcar@gmail.com','lopez@gmail.com')
user_id=c(1,1,1)
row_id=c(1:length(name))
df=data.frame(id,name,second_name,age,adress,email,user_id,row_id)
insert_table('tbl_personal',df)

# Data Puesto
job=c('Administrador','Gerente','Asistente','Transportista','Jefe','Operador','Estadistico','Informatico','Ingeniero','Fisico','Doctor')
description=c(rep('si '))
user_id=c(1,1,1,1,1,1,1,1,1,1,1)
row_id=c(1:length(job))
df1=data.frame(job,description,user_id,row_id)
insert_table('tbl_job',df1)


# Ubicacion puesto
adress=c('Norte Bogota','Sur Bogota','Bogota Central','Medallo')
id_adress=c(1:length(adress))
df2=data.frame(adress,id_adress)
insert_table('tbl_adress',df2)

# Relacion entre tablas (uno-muchos)
tbl_personal=c(1,2,3,4,5,6)
tbl_user=c(1,1,1,1,1,1)
tbl_job=c(2,3,4,5,2,1)
row_id=c(1:length(tbl_personal))
df3=data.frame(tbl_personal,tbl_user,tbl_job,row_id)
insert_table('tbl_relation',df3)



tbl_user=dbGetQuery(con,'Select * from tbl_user')
tbl_personal=dbGetQuery(con,'Select * from tbl_personal')
tbl_job=dbGetQuery(con,'Select * from tbl_job')
tbl_adress=dbGetQuery(con,'Select * from tbl_adress')
tbl_relation=dbGetQuery(con,'Select * from tbl_relation')

list_tables = function() unlist(DBI::dbGetQuery(conn = self$conn, queries$list_tables))

# Table Changes
id_user=c('0931128060')
color1=c('#b10000')
color2=c('#1b5f70')
table1=c('tbl_personal')
table2=c('tbl_job')
column_left=c('')
column_right=c('')
column_union=c('')
join=c(1)

row_id=c(1)

# Color
color1=c('#FFF0F5')
color2=c('#191970')
user_id=c(1)
row_id=1
df4=data.frame(color1,color2,user_id,row_id)
insert_table('tbl_color',df4)

#Logs
logs=c('Actualización colores')
timestamp=Sys.time()
user_id=1
row_id=1
df5=data.frame(logs,timestamp,user_id,row_id)
insert_table('tbl_logs',df5)


dbGetQuery(con,'Select * from tbl_relation relation
           left join tbl_personal personal
           on relation.tbl_personal = personal.row_id
           left join tbl_job job
           on relation.tbl_job = job.row_id
           where personal.name is not null 
           and tbl_user= 1')
insert_table('iris',iris)
