box::use(
  shiny[...]
)


# Form Personal ####
# Este form es el que se abre al presionar el boton de agregar o editar un usuario del Panel General#
#' @export
form_personal<-function(session,title,button_id,button_name){
  ns<-session$ns
  showModal(
    modalDialog(id = "entry_form_personal",
                title = title,
                footer=modalButton("Descartar"),
                div(
                  fluidPage(
                    htmlTemplate(
                      filename = "www/entry_form_personal.html",
                      id = textInput(ns("id"),label='Identificación', placeholder = NULL, width = "100%" ), 
                      name = textInput(ns("name"),label='Nombres', placeholder = NULL, width = "100%" ), 
                      second_name = textInput(ns("second_name"), label='Second Name', width = "100%"), 
                      age = numericInput(ns("age"), label='age', value=25,width = "100%"), 
                      email = textInput(ns("email"), label='Email', width = "100%"), 
                      
                      adress = textInput(ns("adress"), label='Adress', width = "100%"), 
                      
                      submit_button = actionButton(button_id,button_name,icon("save"))),
                    easyClose =TRUE))))
}

# Form Job ####
# Este form es el que se abre al presionar el boton de agregar o editar un usuario del Panel General#
#' @export
form_job<-function(session,title,button_id,button_name){
  ns<-session$ns

  showModal(
    modalDialog(id = "entry_form_job",
                title = title,
                footer=modalButton("Descartar"),
                div(
                  fluidPage(
                    htmlTemplate(
                      filename = "www/entry_form_job.html",
                      
                      name = textInput(ns("name"),label='Job', placeholder = NULL, width = "100%" ), 
                      description = textAreaInput(ns("description"),label='Description', placeholder = NULL, width = "100%" ), 
                      
                      submit_button = actionButton(button_id,button_name,icon("save"))),
                    easyClose =TRUE))))
}


# Form User ####
# Este form es el que se abre al presionar el boton de agregar o editar un usuario del Panel General#
#' @export
form_user<-function(session,title,button_id,button_name){
  ns<-session$ns
  
  showModal(
    modalDialog(id = "entry_form_user",
                title = title,
                footer=modalButton("Descartar"),
                div(
                  fluidPage(
                    htmlTemplate(
                      filename = "www/entry_form_user.html",
                      id = textInput(ns("id"),label='Identificación', placeholder = NULL, width = "100%" ), 
                      
                      name = textInput(ns("name"),label='Nombres', placeholder = NULL, width = "100%" ), 
                      submit_button = actionButton(button_id,button_name,icon("save"))),
                    easyClose =TRUE))))
}