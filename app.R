





library(shiny)
library(shinythemes)
  ui<-fluidPage(theme=shinytheme("superhero"),
    sidebarLayout(
     
      sidebarPanel(
        titlePanel("Select possible outcomes of a grade"),
  numericInput("num","A:grade range 70:100",value=1,min=0,max=100),
  numericInput("num1","B:grade range 60:69",value=1,min=0,max=100),
  numericInput("num2","C:grade range 50:59",value=1,min=0,max=100),
  numericInput("num3","D:grade range 40:49",value=1,min=0,max=100),
  numericInput("num4","SUPP select no of supps",value=0,min=0,max=100),
  numericInput("num5","select no of units you took",value=1,min=1,max=80),
  fileInput("fl","upload")
  
),mainPanel(
  titlePanel("Estimate your mean score"),
 
  navbarPage(
  tabPanel("dumm"),
  tabPanel("How",tags$div(HTML("<i>Use the app by selecting the number of subjects you had a particular grade<br>
                               :example if you scored A in five subjects select 5 in the range of 70:80</i>"))
           ),
  tabPanel("About",tags$div(HTML("<p>The estimator app was designed by Data Scientist Harron Wanga as a project,
                                to fill his portfolio.<br>
                                 contact:0746494596<br>
                                 "))),
  tabPanel("Rate me",
           radioButtons("r","how would you rate the app on a scale of 1: 10 ",choices=c("1:4POOR","5:7 AVERAGE","8:10 EXCELLENT"))
,textInput("txt","What features do you recommend improvement on"),
           actionButton("sub","submit") )
           
      
    
    
  ),
h1("final score"),
  verbatimTextOutput("n"),
 actionLink("link","find my github resipitory")
    )
               )
)
 
   
 
 
 server<-function(input,output,session){
   output$n<-renderPrint({
     a<-sample(70:80,input$num,replace=TRUE)
     b<-sample(60:69,input$num1,replace=TRUE)
     c<-sample(50:59,input$num2,replace=TRUE)
     d<- sample(40:49,input$num3,replace=TRUE)
     e<-40*input$num4
     f<-sum(a)+sum(b)+sum(c)+sum(d)+e
     w<-f/input$num5
   w
  if(w<=39){
    paste(w,"E; FAIL")
  } else if(w>=40&w<=49){
    paste(w,"D; PASS")
    
  }
   else if(w>=50&w<=59){    paste(w,"c: SECOND CLASS LOWER")
     
   }else if(w>=60&w<=69){    paste(w,"B: SECOND CLASS UPPER")
   }else if(w>=70&w<=100){    paste(w,"A: FIRST CLASS")
   }
   else{
     paste(w,"INVALID")
   }
   })

 
  
   
 }
shinyApp(ui=ui,server=server)