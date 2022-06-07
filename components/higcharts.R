box::use(
  shiny[...], 
  highcharter[...],
  gargoyle[init,watch,on,trigger],
  magrittr[`%>%`],
  ./datasql
)

#' @description UI del Panel Higcharter   
#' @export
highchartUI<-function(id,label='highcharter'){
  ns<-NS(id)
  tagList(
    highchartOutput(ns('high'))
    
  )
  
}

#' @description Server del Panel Higcharter   
#' @export
highchartServ<-function(input,output,session){
  
  #' @description Output Higcharter
  output$high<-renderHighchart({
    iris=datasql$tbl_sel_all('iris')
    
    highchart() %>% 
      hc_yAxis_multiples(
        list(title=list(text="Sepal Length",style=list(fontSize= '14px')),labels=list(style=list(fontSize= '14px'))),
        list(title=list(text="Petal Length",style=list(fontSize= '14px')),labels=list(style=list(fontSize= '14px')), opposite = TRUE)
      ) %>%
      hc_add_series(iris,"line",name='Sepal Length',yAxis= 0,hcaes(x=iris$Species,y=iris$Sepal.Length,color=iris$Species),marker = list(radius = 3,enabled= T,symbol='circle'),showInLegend = T,
                    dataLabels = list(enabled =F,format = '{point.media}',style = list(fontSize = "20px",color='#e50000'))) %>%
      hc_add_series(iris,"scatter",name='Petal Length',yAxis= 1,hcaes(x=iris$Species,y=iris$Petal.Length,color=iris$Species),marker = list(radius = 3,enabled= T,symbol='circle'),showInLegend = T,
                    dataLabels = list(enabled =F,format = '{point.media}',style = list(fontSize = "20px",color='#e43000'))) %>%
      hc_title(text =paste0('<span style="display: block;">Iris Sepal Length vs Species </span>'), align = 'center', verticalAlign = 'top', style = list(fontWeight = 'bold', fontSize = '16px')) %>% 
      hc_chart(zoomType = "xy") %>%
      hc_mapNavigation(enabled = F) %>% 
      hc_exporting(enabled = TRUE, allowHTML= T,tableCaption = " ", csv = list(dateFormat = "%d/%m/%Y"), xls = list(dateFormat = "%d/%m/%Y"),
                   filename = "ETR promedios") %>% hc_legend(verticalAlign='bottom') %>%  hc_legend(itemStyle=list(fontSize='14px',font= '14pt Trebuchet MS, Verdana, sans-serif')) %>% 
      hc_navigator(enabled=F) %>% 
      
      hc_tooltip(crosshairs = F, shared = F,useHTML=T,headerFormat = "",xDateFormat='%A',
                 pointFormat='<span style=color:{point.color};>\u25CF</span><span style="font-size: 16px; font-weight: bold;">{point.series.name} <br>{point.Species}:<b> {point.y:.2f} </b></span>')
  })
  
  
 
  
}