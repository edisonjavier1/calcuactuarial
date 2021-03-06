#### install.packages('xts',dependencies = T) ####
#Correccion de Version en paquete 'xts' ===============
# install.packages('devtools', dependencies = T)
# require(devtools)
# install_version("xts", version = "0.9-7", repos = "http://cran.us.r-project.org")

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#---------------        ANALISIS CLUSTER IPC - PUCE      ------------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(shiny)
library(shinythemes)
# library(latex2exp)
# #Tablas
# library(readxl)
# library(readr)
# library(xtable)
# library(DT)
# library(tidyverse)
# #Series de Tiempo
# library(TSdist)
# library(ggplot2)
# library(gridExtra)
# # library(xts)
# library(forecast)
# library(hexbin)
# library(MASS)
#>> Carga de Datos -----------------------------------------------
# Sys.setlocale("LC_ALL", "ES_ES.UTF-8")

# periodos = c(2005, 2007, 2010, 2014, 2019)

#Tipo de Estandarizacion (Deflactar)

#-------------------------------------------
# sourse

#------------------------------------------------
library("readxl")
library("dplyr")

# Lectura tabla de mortalidad
tablaM<- read_excel("Data/Tabla_Mortalidad_Ecuador_macros.xlsm", sheet = "Mujeres_lxt")

tablaH <- read_excel("Data/Tabla_Mortalidad_Ecuador_macros.xlsm",sheet = "Hombres_lxt")

lx <- function(edad,salto,genero){
  if(genero==2){
    num <- as.numeric(tablaM[edad+salto+1,salto+2])
  }
  else{
    num <- as.numeric(tablaH[edad+salto+1,salto+2])
  }
  return(num)
}

# Probabilidad de supervivencia
tpx <- function(edad, salto, genero){
  return(lx(edad,salto, genero)/lx(edad,0,genero))
}

# Probabilidad de muerte
tqx <- function(edad, salto, genero){
  return(1-(lx(edad,salto, genero)/lx(edad,0,genero)))
}

# Probabilidad de muerte diferida
utqx <- function(edad, inicio, salto, genero){
  return(tqx(edad, inicio + salto, genero)- tqx(edad, inicio, genero))
}

# Factor financiero - actuarial
nEx <- function(edad, periodo, interes, genero){
  return(tpx(edad, periodo, genero)*(1+interes)^(-periodo))
}

# Seguro de vida temporal
A1xn <- function(edad, omega, interes, genero){
  suma=0
  for(i in 0:(omega-1)){
    suma = suma + utqx(edad,i,1,genero)*(1+interes)^(-(i+1))   
  }
  return(suma)
}
A1xn(45,5,0.03,1)*50000
# Seguro de supervivencia
Axn1 <- function(edad, periodo, interes, genero){
  return(nEx(edad, periodo, interes, genero))
}

# Seguro Mixto
Ax <- function(edad, periodo, interes, genero){
  return(A1xn(edad, periodo, interes, genero) + Axn1(edad, periodo, interes, genero))
}

#Seguro de vida entera
Axe<-function(edad,interes,genero){
  suma=0
  for(i in 0:31){
    suma = suma + utqx(edad,i,1,genero)*(1+interes)^(-(i+1))   
  }
  return(suma)
}

#seguro diferido de vida entera
nAx<-function(edad,periodo,interes,genero){
  return(nEx(edad,periodo,interes,genero)*Axe(edad+periodo,interes,genero))
}

#seguro diferido de vida temporal
uA1xn <- function(edad, u, omega, interes, genero){
  return(nEx(edad,u,interes,genero)*A1xn(edad+u,omega,interes,genero))
}
# Seguro de supervivencia edison
uAxn1 <- function(edad, u, periodo, interes, genero){
  return(nEx(edad,u,interes,genero)*Axn1(edad, periodo, interes, genero))
}
# Seguro Mixto diferido edisson
uAx <- function(edad,u, periodo, interes, genero){
  return(uA1xn(edad,u, periodo, interes, genero) + uAxn1(edad,u ,periodo, interes, genero))
}
#seguro continuo con hipotesis de uniformidad
Ax_c<-function(edad,interes,genero){
  delta<-log(1+interes)
  return((interes/delta)*Axe(edad,interes,genero))
}

#calculo de edad
edad<-function(AAMMDD){
  res<-Sys.Date()-as.Date(paste(AAMMDD), format("%Y%m%d"))
  return(as.numeric(round(res/365.25)))
}



# Renta de supervivencia

ax <- function(edad, periodo, interes, genero){
  suma=0
  for(k in 0:(periodo-1)){
    suma = suma + tpx(edad,i, genero)*(i+interes)^(-k)
  }
  retunr(suma)
}

# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!    USER INTERFACE   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================

ui <- navbarPage(title = "Calculadora Actuarial",
                 header = tags$h3("Header-Plataforma",tags$head(tags$link(rel='shortcut icon', 
                                                                          href='EPN.ico', 
                                                                          type='image/x-icon'))),
                 position = "fixed-top",
                 # theme = "estilo.css",
                 # theme=shinytheme('yeti'),
                 theme = shinytheme("united"),
                 footer = fluidRow(column(12,img(src='EPN_LOGO1.png',width='180px',align='center'),
                                          tags$b('Proyecto: '),' "Calculadora Actuarial".' ,
                                          # '-',tags$a('Instituto de Investigaciones EconCBCB3micas - PUCE (2018)',href='https://www.puce.edu.ec'),
                                          tags$b('  ||  '),tags$b('Desarrollado por: '),
                                          tags$a('Edison Quizhpi',
                                                  href='https://www.linkedin.com/edison-quizhpi')
                 )
                 ),
                 
                 #INTRODUCCION E INFORMACION DEL PROYECTO ---------------------------
                 tabPanel('Introducción',icon=icon('home'),
                          
                          fluidRow(
                            
                            sidebarPanel(img(src='EPN_LOGO1.png',width='100%',align='center' ),
                                         fluidRow(' '),
                                         hr(),
                                         
                                         fluidRow(
                                           column(3,tags$b('Integrantes:')),column(1),
                                           column(8,'Priscila Guayasamin')
                                         ),hr(),
                                         fluidRow(
                                           column(3),column(1),
                                           column(8,'Gissela Vargas')
                                         ),hr(),
                                         fluidRow(
                                           column(3),column(1),
                                           column(8,'Edison Quizhpi')
                                         ),hr(),
                                         fluidRow(
                                           column(3),column(1),
                                           column(8,'Willian Allauca')
                                         ),hr(),
                                         fluidRow(
                                           column(3),column(1),
                                           column(8,'Andres Suquillo')
                                         ),
                                         hr(),
                                         fluidRow(
                                           column(3),column(1),
                                           column(8,'Cristian Velastegui')
                                         )
                                         
                            ),
                            
                            mainPanel(
                              h3('Calculadora Actuarial'),
                              hr(),h4('Resume:'),
                              fluidRow(' '),
                              p('Calculadora Actuarial intro...........'),#,
                              # fluidRow(dataTableOutput("tabla_prod")) 
                              fluidRow(column(3,
                                textInput("nam", label = h3("Nombre"), value = "Pepito...")
                              ),column(3,numericInput("tasa", 
                                                       label = h3("Tasa de interes"), value =0.04,
                                                      step = 0.01, min = 0, max=1)
                                                        )
                                      ),
                              fluidRow(' '),
                              fluidRow(' '),
                              fluidRow(' '),
                                 # column(3, verbatimTextOutput("value"))),
                              fluidRow (
                               column(3, wellPanel(
                               selectInput('tipo', 
                                          label= 'Edad/Fecha de nacimiento',
                                          choices=c("Edad",
                                                    "Fecha de Nacimiento")
                                         
                                        ))),column(3,wellPanel(
                                        uiOutput("eda"))),column(3,
                               wellPanel( radioButtons("G", label = "Genero ",
                                          choices = list("Masculino" = 1, "Femenino" = 2), 
                                          selected = 1)))
                                       ),
                              fluidRow (column(3,radioButtons("seg", label = h3("Seleccion de Seguro"),
                                                     choices = list("Seguro de Vida Entera" ,
                                                                    "Seguro de Vida Temporal" , 
                                                                    "Seguro de Supervivencia" ,
                                                                    "Seguro Mixto","Seguro Diferido") 
                                                     )),column(3,uiOutput("tiposeguros")) ,column(3,uiOutput("tiposegurosdif"))
                                        ),
                              fluidRow (column(3,actionButton("action", label = "Calcular"))
                                        ),
                                        fluidRow (h3(textOutput('prima'))
                                
                                        )
                              
                            ),hr()
                          
                          
                          )
                 
                 #INFORMACICBCBN DE LA BASE DE DATOS ------------------------------
                 # tabPanel("Datos"
                 #            
                 #            ),
                 
                 # ANALISIS MULTIVARIANTE DE SERIES ============================
                 
                          ))


# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     SERVER      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
server <- function(input, output,session) {
  options(shiny.sanitize.errors = TRUE)
  output$eda <- renderUI({
    if (is.null(input$tipo))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$tipo,
           "Edad" = numericInput("edad", "Edad",
                                 value = 25),
           "Fecha de Nacimiento" = dateInput("date", label = "Fecha de Nacimiento",
                                                         value = "2014-01-01")
           
    )
    })
  output$tiposeguros <- renderUI({
    if (is.null(input$seg))
      return()
    
    switch(input$seg,
           "Seguro de Vida Entera" =  numericInput("ent", label = h3("Monto"), value =10000),
           
    
           
           "Seguro de Vida Temporal" = fluidRow(numericInput("ent1", label = h3("Monto"), value =10000),
                                     sliderInput("dur",
                                                   "Duracion:",
                                                   min = 1,
                                                   max = 40,
                                                   value = 10)),
           "Seguro de Supervivencia"=fluidRow(numericInput("ent", label = h3("Monto"), value = 10000),
           sliderInput("dur",
                       "Duracion:",
                       min = 1,
                       max = 40,
                       value = 10)),
           
            "Seguro Mixto"=fluidRow(numericInput("ent", label = h3("Monto"), value = 10000),
           sliderInput("dur",
                       "Duracion:",
                       min = 1,
                       max = 40,
                       value = 10)),
           
           "Seguro Diferido"=fluidRow(radioButtons("seg1", label = h3("Tipo de Seguro a Diferir" ),
                             choices = list("Vida Entera" ,
                                         "Vida Temporal" , 
                                         "Supervivencia" ,
                                         "Mixto") )
             )
    )
    
  })
 
  output$tiposegurosdif=renderUI({
    if (is.null(input$seg1))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$seg1,
           "Vida Entera" =  fluidRow(numericInput("ent", label = h3("Monto"), value =10000),
                                               sliderInput("dir",
                                                           "Tiempo a Diferir:",
                                                           min = 1,
                                                           max = 40,
                                                           value = 10)),
           
                    
           
           "Vida Temporal" = fluidRow(numericInput("ent", label = h3("Monto"), value =10000),
                                                sliderInput("dur1",
                                                            "Duracion:",
                                                            min = 1,
                                                            max = 40,
                                                            value = 10),sliderInput("dir",
                                                                                    "Tiempo a Diferir:",
                                                                                    min = 1,
                                                                                    max = 40,
                                                                                    value = 10)),
           "Supervivencia"=fluidRow(numericInput("ent", label = h3("Monto"), value = 10000),
                                              sliderInput("dur1",
                                                          "Duracion:",
                                                          min = 1,
                                                          max = 40,
                                                          value = 10),sliderInput("dir",
                                                                                 "Tiempo a Diferir:",
                                                                                 min = 1,
                                                                                 max = 40,
                                                                                 value = 10)),
           
           "Mixto"=fluidRow(numericInput("ent", label = h3("Monto"), value = 10000),
                                   sliderInput("dur1",
                                               "Duracion:",
                                               min = 1,
                                               max = 40,
                                               value = 10),sliderInput("dir",
                                                                       "Tiempo a Diferir:",
                                                                       min = 1,
                                                                       max = 40,
                                                                       value = 10))
           
      
             )
         
           
   
    
  })
  # 
  final = eventReactive(input$action,
                      {
    
    analisis1 = switch(input$seg,
                      "Seguro de Vida Entera"={
                        total0=round((input$ent)*(Axe(input$edad,input$tasa,as.numeric(input$G))),
                        2)
                       return( sprintf("Estimado %s el monto a pagar de su %s es $ %s, a la tasa de %s %s" ,
                                input$nam,input$seg, as.numeric(total0),input$tasa*100,"%"))
                        
                      
                        },
                        
                      "Seguro de Vida Temporal" ={
                        total1=round((input$ent1)*(A1xn(input$edad,as.numeric(input$dur),
                                                input$tasa,as.numeric(input$G))),2)
                        
                        return( sprintf("Estimado %s el monto a pagar de su %s es $ %s, 
                              a la tasa de %s %s" ,
                              input$nam,input$seg, as.numeric(total1),input$tasa*100,"%"))
                          
                      },
                      "Seguro Mixto"={
                        total2=round((input$ent)*(Ax(input$edad,as.numeric(input$dur),
                                                              input$tasa,as.numeric(input$G))),2)
                      return( sprintf("Estimado %s el monto a pagar de su %s es $ %s, 
                                      a la tasa de %s %s" ,
                                      input$nam,input$seg, as.numeric(total2),input$tasa*100,"%"))},
                      "Seguro de Supervivencia"={
                        total3=round((input$ent)*(Axn1(input$edad,as.numeric(input$dur),
                                              input$tasa,as.numeric(input$G))),2)
                        return( sprintf("Estimado %s el monto a pagar de su %s es $ %s,
                                        a la tasa de %s %s" ,
                                        input$nam,input$seg, as.numeric(total3),input$tasa*100,"%"))}
                    )
    
                 return(analisis1)
    
                      })
  
  final1 = eventReactive(input$action,
                        {
                          analisis = switch(input$seg1,
                                            "Vida Entera"={
                                              total4=round((input$ent)*(nAx(input$edad,input$dir,input$tasa,as.numeric(input$G))),2)
                                              return( sprintf("Estimado %s el monto a pagar de su %s  por %s aCB1os es $ %s, a la tasa de %s %s" ,
                                                              input$nam,input$seg,input$dir, as.numeric(total4),input$tasa*100,"%"))
                                              
                                              
                                            },
                                            
                                            "Vida Temporal" ={
                                              total5=round((input$ent)*(uA1xn(input$edad,as.numeric(input$dir),input$dur1,
                                                                      input$tasa,as.numeric(input$G))),2)
                                              return( sprintf("Estimado %s el monto a pagar de su %s  por %s aCB1os es $ %s, a la tasa de %s %s" ,
                                                              input$nam,input$seg,input$dir, as.numeric(total5),input$tasa*100,"%"))
                                              
                                            },
                                            "Mixto"={
                                              total6=round((input$ent)*(uAx(input$edad,as.numeric(input$dir),as.numeric(input$dur1),
                                                                    input$tasa,as.numeric(input$G))),2)
                                              return( sprintf("Estimado %s el monto a pagar de su %s  por %s aCB1os es $ %s, a la tasa de %s %s" ,
                                                              input$nam,input$seg,input$dir, as.numeric(total6),input$tasa*100,"%"))
                                                            },
                                            "Supervivencia"={
                                              total7=round((input$ent)*(uAxn1(input$edad,as.numeric(input$dir),input$dur1,
                                                                      input$tasa,as.numeric(input$G))),2)
                                              return( sprintf("Estimado %s el monto a pagar de su %s  por %s aCB1os es $ %s, a la tasa de %s %s" ,
                                                              input$nam,input$seg,input$dir, as.numeric(total7),input$tasa*100,"%"))
                                              }
                                              )
                          
                          return(analisis)
                          
                        })
  
  
  output$prima = renderText({
 if(input$seg=="Seguro Diferido" )
    {
      final1()
 }
  else
  {
    final()
  }
 
  })

}
# ========================================================================
# !!!!!!!!!!!!!!!!!!!!!!!!     RUN APP      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ========================================================================
shinyApp(ui = ui, server = server)
# name="edison"
# sprintf("estimado %s",name)
