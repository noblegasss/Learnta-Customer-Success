#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)
library(grid)
library(shinydashboard)
library(lubridate)
library(DT)
library(rsconnect)
## Data

Hourly=read.csv("Hourlydat.csv")
org=unique(Hourly$orgId)
Hourly$time=as.POSIXct(Hourly$time)
Daily=read.csv("Dailydat.csv")
Daily$time=as.POSIXct(Daily$time)
Weekly=read.csv("Weeklydat.csv")
Monthly=read.csv("Monthlydat.csv")
Monthly$month=factor(Monthly$month,levels=c("Nov","Dec","Jan","Feb","Mar"))
Timespentweek=read.csv("table_alluesrs_time_spent_weekly.csv")
Timespentday=read.csv("table_alluesrs_time_spent_daily.csv")
profile=read.csv("profile.csv")
user=read.csv("users.csv")
week=read.csv("week.csv")
day=read.csv("day.csv")
class=read.csv("class.csv")
weekclass1=read.csv("weekclass.csv")
weekclass2=read.csv("weekclass2.csv")
monthclass1=read.csv("monthclass.csv")
monthclass2=read.csv("monthclass2.csv")
all=read.csv("correct_all.csv")
mission=read.csv("mission.csv")
missioncard=read.csv("missioncard.csv")
subject=read.csv("subject.csv")
totalclass=read.csv("totaltime_by_orgId_date.csv")
userclass=read.csv("totaltime_by_userId.csv")
log=read.csv("log.csv")
improve=read.csv("table_nquestion_improvement.csv")
Timespentweek[Timespentweek<0]=0
Timespentday[Timespentday<0]=0 

# Define UI for application that draws a histogram
ui <- dashboardPage(dashboardHeader(title="Learnta Customer Success"),skin = "yellow",
                    dashboardSidebar(
                        hr(),
                        sidebarMenu(id="tabs",
                                    menuItem("Introduction",tabName = "intro",icon=icon("mortar-board"),selected=TRUE,
                                             menuSubItem("Intro & Methods",tabName = "method",icon = icon("angle-right")),
                                             menuSubItem("Analysis Results",tabName = "results",icon =icon("angle-right")),
                                             menuSubItem("Summary",tabName = "suggest",icon = icon("angle-right"))),
                                    menuItem("Overall",tabName = "overall",icon=icon("line-chart"),
                                             menuSubItem("Statistics",tabName = "stat",icon = icon("angle-right")),
                                             menuSubItem("Score",tabName = "score",icon = icon("angle-right")),
                                             menuSubItem("Teaching Quality",tabName = "teach",icon = icon("angle-right"))),
                                    menuItem("Organization Profile",tabName = "profile",icon = icon("table"),
                                             menuSubItem("Organization",tabName = "all",icon = icon("angle-right")),
                                             menuSubItem("User",tabName = "use",icon = icon("angle-right"))
                                             
                                             ),
                                    hr(),
                                    conditionalPanel("input.tabs==profile",
                                                     fluidRow(column(1),column(10,
                                                                               selectInput(inputId="orgId",
                                                                                           label="Organization",
                                                                                           choices=org,
                                                                                           selected=100082),
                                                                               uiOutput("tab"))))
                        )
                        
                        
                    ),
                    
                    dashboardBody(tabItems(
                        tabItem(tabName = "method",
                                tabBox(tabPanel("Introduction",imageOutput("intro1")),
                                       tabPanel("Health Score",imageOutput("hscore")),
                                       tabPanel("Improvement",imageOutput("improve")),
                                       height=800,width = 10)),
                        tabItem(tabName = "results",
                                tabBox(tabPanel("Health Score",imageOutput("h1")),
                                       tabPanel("Correction Trend",imageOutput("ct")),
                                       tabPanel("Test Results",imageOutput("tr")),
                                       height=800,width = 10)),
                        tabItem(tabName = "suggest",
                                tabBox(tabPanel("Summary",imageOutput("h2")),
                                       
                                       tabPanel("Suggestion",imageOutput("sugg")),height=800,width = 10)),
                        tabItem(tabName = "stat",fluidRow(
                            box(title="Geographical Distribution",height=500,imageOutput("geo1",height=400),solidHeader = TRUE, status = "warning"),
                            box(height=500,imageOutput("geo2",height = 500),solidHeader = TRUE, status = "warning"),
                            box(title="Teacher and Student Numbers",height=500,imageOutput("num1"),solidHeader = TRUE, status = "warning"),
                            box(title="Teacher and Student Proportion",height=500,imageOutput("num2"),solidHeader = TRUE, status = "warning"))
                        ),
                        tabItem(tabName = "score",
                                box(title="Summary Statistics",width=100,dataTableOutput("table1"),solidHeader = TRUE, status = "warning")),
                        tabItem(tabName = "all",
                                box(title="Summary",height=150,tableOutput("table2"),solidHeader = TRUE, status = "warning"),
                                box(title="Ratio",height = 150,width=2,plotlyOutput("rate",height=90),solidHeader = TRUE, status = "warning"),
                                box(title="Scores",height=200,width=4,valueBoxOutput("health"),
                                
                                valueBoxOutput("degree"),solidHeader = TRUE, status = "warning"),
                                
                                box(height=300,width=4,solidHeader = TRUE, status = "warning",selectInput(inputId = "fre",
                                                           label = "Selection",
                                                           choices = c("Hourly","Daily","Weekly","Monthly"),
                                                           selected = "Weekly"),
                                    selectInput(inputId="type",
                                                label="User Type",
                                                choices = c(1,2),
                                                multiple=TRUE,
                                                selected=1),
                                    checkboxInput("avg","Average",TRUE),
                                    checkboxInput("total","Total",FALSE),
                                    checkboxInput("max","Maximum",FALSE),
                                    checkboxInput("min","Minimum",FALSE)),
                                
                                box(title="Time Spent",height=300,plotlyOutput("ts1",height = 200),solidHeader = TRUE, status = "warning"),
                                
                        
                                box(title="In-class/after-class Time Spent",height=300,plotlyOutput("class1",height = 200),solidHeader = TRUE, status = "warning"),
                                
                                tabBox(title="Frequency",height=300,
                                    tabPanel("Bar",plotlyOutput("frequ",height = 200)),
                                    tabPanel("Line",plotlyOutput("line",height = 200)))
                                ),
                                
                        
                        tabItem(tabName = "use",
                                
                                box(title="Time Spent",height=300,plotlyOutput("ts2",height = 200),solidHeader = TRUE, status = "warning"),
                                box(title="In-class/after-class Time Spent",height=300,plotlyOutput("class2",height = 200),solidHeader = TRUE, status = "warning"),
                                box(title="User Type",height=100,width=2,background = "yellow",textOutput("log"),solidHeader = TRUE, status = "warning"),
                                box(height=100,width=2,selectInput(inputId = "fre1",
                                                                   label = "Selection",
                                                                   choices = c("Daily","Weekly","Monthly"),
                                                                   selected = "Weekly")),
                                box(title="In-class/after-class Ratio",height=150,width=2,plotlyOutput("class3",height = 100),solidHeader = TRUE, status = "warning")
                                ),
                        
                        tabItem(tabName = "teach",
                               
                                
                                box(title="Correction Rate",height = 600,plotOutput("CR",height = 500)),
                                box(width=2,selectInput(inputId="CT",
                                                        label="Classification Type",
                                                        choices=c("All","Mission","MissionCards","Subjects"),
                                                        selected="All")),
                                box(title="Improvement",dataTableOutput("table3")))
                    )                 
                    )
)

server<-function(input,output){
    output$tab=renderUI({
        user2=user %>%
            filter(orgId %in% input$orgId) %>%
            select(userId)
        selectInput("userId","User",choices=user2$userId,selected=64267)
    })
    output$intro1=renderImage({
        return(list(src = "intro1.png",contentType = "image/png",width=1200,height=600))
    }, deleteFile = FALSE)
    output$hscore=renderImage({
        return(list(src = "health.png",contentType = "image/png",width=1200,height=600))
    }, deleteFile = FALSE)
    output$improve=renderImage({
        return(list(src = "improv.png",contentType = "image/png",width=1200,height=600))
    }, deleteFile = FALSE)
    output$h1=renderImage({
        return(list(src = "h2.png",contentType = "image/png",width=1200,height=600))
    }, deleteFile = FALSE)
    output$ct=renderImage({
        return(list(src = "cor1.png",contentType = "image/png",width=1200,height=700))
    }, deleteFile = FALSE)
    output$tr=renderImage({
        return(list(src = "c2.png",contentType = "image/png",width=1200,height=600))
    }, deleteFile = FALSE)
    output$h2=renderImage({
        return(list(src = "summary.png",contentType = "image/png",width=1200,height=600))
    }, deleteFile = FALSE)
    output$sugg=renderImage({
        return(list(src = "suggestion.png",contentType = "image/png",width=1200,height=600))
    }, deleteFile = FALSE)
    output$geo1=renderImage({
        return(list(src = "geo1.png",contentType = "image/png",width=600,height=400))
    }, deleteFile = FALSE)
    output$geo2=renderImage({
        return(list(src = "geo2.png",contentType = "image/png",width=550,height=400))
    }, deleteFile = FALSE)
    output$num1=renderImage({
        return(list(src = "num1.png",contentType = "image/png",width=550,height=400))
    }, deleteFile = FALSE)
    output$num2=renderImage({
        return(list(src = "num2.png",contentType = "image/png",width=550,height=400))
    }, deleteFile = FALSE)
    
    output$table1=renderDataTable({
        profile
    })
    r=reactive({
        t1=profile %>%
            filter(orgId==input$orgId) %>%
            select(orgId,province,teacher,student,ratio,health_score,customer_maintainance_score) 
    })
    output$table2=renderTable({
        r()
    })
    output$health=renderValueBox({
        r=r()
        v=round(r$health_score)
        valueBox(v,"Health Score",icon=icon("thumbs-up",lib="glyphicon"),color="green")
    })
    output$degree=renderValueBox({
        r=r()
        v=round(r$customer_maintainance_score)
        valueBox(v,"Active Degree",icon=icon("thumbs-up",lib="glyphicon"),color="blue")
    })
    output$rate=renderPlotly({
        r=r()
        student=r$student
        teacher=r$teacher
        number=data.frame(count=c(rep("Student",student),rep("Teacher",teacher)))
        plot_ly(data=number,labels=~count,type="pie",textinfo = 'label+percent',marker=list(colors=c('orange', 'yellow')))%>%
            layout(showlegend = FALSE)
    })
    output$ts1=renderPlotly({
        if(input$fre=="Daily"){
        d4=day %>%
            filter(organization %in% input$orgId) %>%
            arrange(date)
        d4$date=as.POSIXct(d4$date)
        p=plot_ly(data=d4,x=~date) %>%
            layout(title="Daily Time Spent",
                   yaxis=list(title="Minites"),
                   xaxis=list(title="Date"))
        if(input$avg==TRUE){
            p=p %>%
                add_lines(y=~avg,name="Average Time Spent") %>%
                add_lines(y=~stuavg,name="Student Average") %>%
                add_lines(y=~teaavg,name="Teacher Average")
            }
        else if(input$total==TRUE){
            p=p %>%
                add_lines(y=~total,name="Total Time Spent")
            }
        else if(input$min==TRUE){
            p=p %>%
                add_lines(y=~min,name="Minimum Time Spent")
        }
        else if(input$max==TRUE){
            p=p %>%
                add_lines(y=~max,name="Maximum Time Spent")
        }
        }
        else if(input$fre=="Weekly"){
        d2=week %>%
            filter(organization %in% input$orgId) %>%
            arrange(week_num)
        p=plot_ly(data=d2,x=~week_num) %>%
            layout(title="Weekly Time Spent",
                   yaxis=list(title="Minites"),
                   xaxis=list(title="Week"))
        if(input$avg==TRUE){
            p=p %>%
                add_lines(y=~avg,name="Average Time Spent") %>%
                add_lines(y=~stuavg,name="Student Average") %>%
                add_lines(y=~teaavg,name="Teacher Average")}
        else if(input$total==TRUE){
            p=p %>%
                add_lines(y=~total,name="Total Time Spent")}
        else if(input$min==TRUE){
            p=p %>%
                add_lines(y=~min,name="Minimum Time Spent")
        }
        else if(input$max==TRUE){
            p=p %>%
                add_lines(y=~max,name="Maximum Time Spent")
        }
        
    }
    })
    output$class1=renderPlotly({
        if(input$fre=="Daily"){
        c1=totalclass %>%
            filter(orgId %in% input$orgId) %>%
            arrange(date)
        c1$date=as.POSIXct(c1$date)
        time=data.frame(date=seq.POSIXt(floor_date(as.POSIXct("2018-11-30",'%m/%d/%y')), floor_date(as.POSIXct("2019-03-17",'%m/%d/%y')), by="day"))
        c1=merge(c1,time,by="date",all=TRUE)
        c1[is.na(c1)]=0
        c1$total_inclass=c1$total_inclass/60
        c1$total_afterclass=c1$total_afterclass/60
        plot_ly(data=c1,x=~date) %>%
            add_lines(y=~total_inclass,name="In Class Time Spent") %>%
            add_lines(y=~total_afterclass,name="After Class Time Spent",color="red",opacity=0.8) %>%
            layout(title="Daily In Class and After Class Time Spent",
                   yaxis=list(title="Minites"),
                   xaxis=list(title="Date"))
        }
        else if(input$fre=="Weekly"){
            c1=weekclass1 %>%
                filter(orgId %in% input$orgId) %>%
                arrange(week)
            c1$firstdayofweek=as.POSIXct(c1$firstdayofweek)
            time=data.frame(firstdayofweek=seq.POSIXt(floor_date(as.POSIXct("2018-11-30",'%m/%d/%y')), floor_date(as.POSIXct("2019-03-17",'%m/%d/%y')), by="week"))
            c1=merge(c1,time,by="firstdayofweek",all=TRUE)
            c1[is.na(c1)]=0
            c1$inclassbyweek=c1$inclassbyweek/60
            c1$afterclassbyweek=c1$afterclassbyweek/60
            plot_ly(data=c1,x=~firstdayofweek) %>%
                add_bars(y=~inclassbyweek,name="In Class") %>%
                add_bars(y=~afterclassbyweek,name="After Class",color="red",opacity=0.8) %>%
                layout(title="Weekly In Class and After Class Time Spent",
                       yaxis=list(title="Minites"),
                       xaxis=list(title="Date"))
        }
        else if(input$fre=="Monthly"){
            c1=monthclass1 %>%
                filter(orgId %in% input$orgId) %>%
                arrange(month)
            c1$firstdayofmonth=as.POSIXct(c1$firstdayofmonth)
            time=data.frame(firstdayofmonth=seq.POSIXt(floor_date(as.POSIXct("2018-11-30",'%m/%d/%y')), floor_date(as.POSIXct("2019-03-17",'%m/%d/%y')), by="month"))
            c1=merge(c1,time,by="firstdayofmonth",all=TRUE)
            c1[is.na(c1)]=0
            c1$inclassbymonth=c1$inclassbymonth/60
            c1$afterclassbymonth=c1$afterclassbymonth/60
            plot_ly(data=c1,x=~firstdayofmonth) %>%
                add_bars(y=~inclassbymonth,name="In Class") %>%
                add_bars(y=~afterclassbymonth,name="After Class",color="red",opacity=0.8) %>%
                layout(title="Monthly In Class and After Class Time Spent",
                       yaxis=list(title="Minites"),
                       xaxis=list(title="Date"))
        }
    })
    output$log=renderText({
        l=log %>%
            filter(orgId %in% input$orgId) %>%
            filter(userId==input$userId) %>%
            select(type) %>%
            unique()
        if(l$type==1){
            "Student"
        }
        else if(l$type==2){
            "Teacher"
        }
    })
    output$ts2=renderPlotly({
        if(input$fre1=="Daily"){
        d3=Timespentday %>%
            filter(user_id %in% input$userId) %>%
            select(user_id,date,time_spent_daily) %>%
            arrange(date)
        d3$date=as.POSIXct(d3$date)
        plot_ly(data=d3,x=~date) %>%
            add_bars(y=~time_spent_daily) %>%
            layout(title="User Daily Time Spent",
                   yaxis=list(title="Minites"),
                   xaxis=list(title="Date"))
        }
        else if(input$fre1=="Weekly"){
            d1=Timespentweek %>%
                filter(user_id %in% input$userId) %>%
                arrange(date)
            d1$date=as.POSIXct(d1$date)
            plot_ly(data=d1,x=~date) %>%
                add_bars(y=~weekly_time_spent) %>%
                layout(title="User Weekly Time Spent",
                       yaxis=list(title="Minites"),
                       xaxis=list(title="Date"))
        }
    })
    output$class2=renderPlotly({
        if(input$fre1=="Daily"){
            c2=class %>%
                filter(userId %in% input$userId) %>%
                arrange(date)
            c2$date=as.POSIXct(c2$date)
            time=data.frame(date=seq.POSIXt(floor_date(as.POSIXct("2018-11-30",'%m/%d/%y')), floor_date(as.POSIXct("2019-03-17",'%m/%d/%y')), by="day"))
            c2=merge(c2,time,by="date",all=TRUE)
            c2[is.na(c2)]=0
            plot_ly(data=c2,x=~date) %>%
                add_bars(y=~afterclasstime,name="After Class") %>%
                add_bars(y=~inclasstime,name="In Class") %>%
                layout(title="Daily In Class and After Class",
                       yaxis=list(title="Minites"),
                       xaxis=list(title="Date"))
        }
        else if(input$fre1=="Weekly"){
            c2=weekclass2 %>%
                filter(userId %in% input$userId) %>%
                arrange(firstdayofweek)
            c2$firstdayofweek=as.POSIXct(c2$firstdayofweek)
            time=data.frame(firstdayofweek=seq.POSIXt(floor_date(as.POSIXct("2018-11-30",'%m/%d/%y')), floor_date(as.POSIXct("2019-03-17",'%m/%d/%y')), by="day"))
            c2=merge(c2,time,by="firstdayofweek",all=TRUE)
            c2[is.na(c2)]=0
            plot_ly(data=c2,x=~firstdayofweek) %>%
                add_bars(y=~afterclassbyweek,name="After Class") %>%
                add_bars(y=~inclassbyweek,name="In Class") %>%
                layout(title="Weekly",
                       yaxis=list(title="Minites"),
                       xaxis=list(title="Date"))
        }
        else if(input$fre1=="Monthly"){
            c2=monthclass2 %>%
                filter(userId %in% input$userId) %>%
                arrange(firstdayofmonth)
            c2$firstdayofmonth=as.POSIXct(c2$firstdayofmonth)
            time=data.frame(firstdayofmonth=seq.POSIXt(floor_date(as.POSIXct("2018-11-30",'%m/%d/%y')), floor_date(as.POSIXct("2019-03-17",'%m/%d/%y')), by="day"))
            c2=merge(c2,time,by="firstdayofmonth",all=TRUE)
            c2[is.na(c2)]=0
            plot_ly(data=c2,x=~firstdayofmonth) %>%
                add_bars(y=~afterclassbymonth,name="After Class") %>%
                add_bars(y=~inclassbymonth,name="In Class") %>%
                layout(title="Monthly",
                       yaxis=list(title="Minites"),
                       xaxis=list(title="Date"))}
    })
    output$class3=renderPlotly({
        c3=userclass %>%
            filter(userId %in% input$userId)
        c3$total_inclass=c3$total_inclass/60
        c3$total_afterclass=c3$total_afterclass/60
        inclass=c3$total_inclass
        afterclass=c3$total_afterclass
        number=data.frame(count=c(rep("Inclass",inclass),rep("Afterclass",afterclass)))
        plot_ly(data=number,labels=~count,type="pie",textinfo = 'label+percent',marker=list(colors=c('orange', 'yellow')))%>%
            layout(showlegend = FALSE)
        
    })
    output$frequ <- renderPlotly({
        if(input$fre=="Hourly"){
        df1=Hourly %>%
            filter(orgId %in% input$orgId) %>%
            group_by(time) %>%
            mutate(total_users=sum(Users)) %>%
            mutate(total_logins=sum(Logins)) %>%
            unique()
        df2=Hourly %>%
            filter(orgId %in% input$orgId & type %in% input$type) %>%
            arrange(time)
        if(length(input$type)>1){
            plot_ly(data=df1,x=~time,y=~total_logins,type = "bar",name="Logins") %>%
                add_bars(y=~total_users,type="bar",name="Users") %>%
                
                layout(title = "Hourly Logins and Users Numbers",
                       yaxis = list(title="Counts"),
                       xaxis = list(
                           rangeselector = list(
                               buttons = list(
                                   list(
                                       step = "hour",
                                       stepmode = "backward"))),rangeslider = list(type = "date")))}
        else{
            plot_ly(data=df2,x=~time,y=~Logins,type = "bar",name="Logins") %>%
                add_bars(y=~Users,type="bar",name="Users") %>%
                layout(title = "Hourly Logins and Users Numbers",
                       yaxis = list(title="Counts"),
                       xaxis = list(
                           rangeselector = list(
                               buttons = list(
                                   list(
                                       step = "hour",
                                       stepmode = "backward"))),rangeslider = list(type = "date")))
        }
        }
        else if(input$fre=="Daily"){
            df3=Daily %>%
                filter(orgId %in% input$orgId) %>%
                group_by(time) %>%
                mutate(total_users=sum(Users)) %>%
                mutate(total_logins=sum(Logins)) %>%
                unique()
            df4=Daily %>%
                filter(orgId %in% input$orgId & type %in% input$type) %>%
                arrange(time)
            if(length(input$type)>1){
                plot_ly(data=df3,x=~time,y=~total_logins,type = "bar",name="Logins") %>%
                    add_bars(y=~total_users,type="bar",name="Users") %>%
                    layout(title = "Daily Logins and Users Numbers",
                           yaxis = list(title="Counts"),
                           xaxis = list(
                               rangeselector = list(
                                   buttons = list(
                                       list(
                                           step = "day",
                                           stepmode = "backward"))),rangeslider = list(type = "date")))
            }else{
                plot_ly(data=df4,x=~time,y=~Logins,type = "bar",name="Logins") %>%
                    add_bars(y=~Users,type="bar",name="Users") %>%
                    
                    layout(title = "Daily Logins and Users Numbers",
                           yaxis = list(title="Counts"),
                           xaxis = list(
                               rangeselector = list(
                                   buttons = list(
                                       list(
                                           step = "day",
                                           stepmode = "backward"))),rangeslider = list(type = "date")))
            }  
        }
        else if(input$fre=="Weekly"){
            df5=Weekly %>%
                filter(orgId %in% input$orgId) %>%
                group_by(weekNum) %>%
                mutate(total_users=sum(Users)) %>%
                mutate(total_logins=sum(Logins)) %>%
                unique()
            df6=Weekly %>%
                filter(orgId %in% input$orgId & type %in% input$type) %>%
                arrange(weekNum)
            if(length(input$type)>1){
                plot_ly(data=df5,x=~weekNum,y=~total_logins,type = "bar",name="Logins") %>%
                    add_bars(y=~total_users,type="bar",name="Users") %>%
                    layout(title = "Weekly Logins and Users Numbers",
                           yaxis = list(title="Counts"),
                           xaxis = list(
                               rangeselector = list(
                                   buttons = list(
                                       list(
                                           step = "week",
                                           stepmode = "backward"))),rangeslider = list(type = "date")))
            }else{
                plot_ly(data=df6,x=~weekNum,y=~Logins,type = "bar",name="Logins") %>%
                    add_bars(y=~Users,type="bar",name="Users") %>%
                    layout(title = "Weekly Logins and Users Numbers",
                           yaxis = list(title="Counts"),
                           xaxis = list(
                               rangeselector = list(
                                   buttons = list(
                                       list(
                                           step = "week",
                                           stepmode = "backward"))),rangeslider = list(type = "date")))
            }
        }
        else if(input$fre=="Monthly"){
            df7=Monthly %>%
                filter(orgId %in% input$orgId) %>%
                group_by(month) %>%
                mutate(total_users=sum(Users)) %>%
                mutate(total_logins=sum(Logins)) %>%
                unique()
            df8=Monthly %>%
                filter(orgId %in% input$orgId & type %in% input$type) %>%
                arrange(month)
            if(length(input$type)>1){
                plot_ly(data=df7,x=~month,y=~total_logins,type = "bar",name="Logins") %>%
                    add_bars(y=~total_users,type="bar",name="Users") %>%
                    layout(title="Montly Logins and Users Number",
                           yaxis=list(title="Counts"),
                           xaxis=list(title="Date"))
                
            }else{
                plot_ly(data=df8,x=~month,y=~Logins,type = "bar",name="Logins") %>%
                    add_bars(y=~Users,type="bar",name="Users") %>%
                    layout(title="Montly Logins and Users Number",
                           yaxis=list(title="Counts"),
                           xaxis=list(title="Date"))
            }
        }
    })
    
    
    output$CR=renderPlot({
        if (input$CT=="All"){
            t1=all %>%
                filter(org_id %in% input$orgId)
            t1$date=as.POSIXct(t1$date)
            ggplot(t1,aes(x=date,y=rate))+geom_bar(stat="identity")+scale_fill_brewer(palette="blues")+theme_bw()}
        else if(input$CT=="Mission"){
            t2=mission %>%
                filter(org_id %in% input$orgId) %>%
                mutate(rate1=ifelse(mission_type_id==1,rate,NA)) %>%
                mutate(rate2=ifelse(mission_type_id==2,rate,NA)) %>%
                mutate(rate3=ifelse(mission_type_id==3,rate,NA)) %>%
                mutate(rate4=ifelse(mission_type_id==4,rate,NA))
            t2$date=as.POSIXct(t2$date)
            ggplot(t2,aes(x=date,y=rate,order=mission_type_id))+geom_bar(stat="identity",aes(fill=factor(mission_type_id)))+scale_fill_brewer(palette="blues")+theme_bw()}
        else if(input$CT=="MissionCards"){
            t3=missioncard %>%
                filter(org_id %in% input$orgId)
            t3$date=as.POSIXct(t3$date)
            ggplot(t3,aes(x=date,y=rate,order=mission_card_type_id))+geom_bar(stat="identity",aes(fill=factor(mission_card_type_id)))+scale_fill_brewer(palette="blues")+theme_bw()}
        else{
            t4=subject %>%
                filter(org_id %in% input$orgId) 
            t4$date=as.POSIXct(t4$date)
            ggplot(t4,aes(x=date,y=rate,order=course))+geom_bar(stat="identity",aes(fill=factor(course)))+scale_fill_brewer(palette="blues")+theme_bw()}
    })
    output$table3=renderDataTable({
        k=improve %>%
            filter(organ_id %in% input$orgId) %>%
            select(student_id,number_questions,improvement)
        k
    })
    
    
    output$line <- renderPlotly({
        if(input$fre=="Hourly"){
            df1=Hourly %>%
                filter(orgId %in% input$orgId) %>%
                group_by(time) %>%
                mutate(total_users=sum(Users)) %>%
                mutate(total_logins=sum(Logins)) %>%
                unique()
            df2=Hourly %>%
                filter(orgId %in% input$orgId & type %in% input$type) %>%
                arrange(time)
            if(length(input$type)>1){
                plot_ly(data=df1,x=~time,name="Logins") %>%
                    add_lines(y=~total_logins,name="Users") %>%
                    add_lines(y=~total_users,name="Users") %>%
                    
                    layout(title = "Hourly Logins and Users Numbers",
                           yaxis = list(title="Counts"),
                           xaxis = list(
                               rangeselector = list(
                                   buttons = list(
                                       list(
                                           step = "hour",
                                           stepmode = "backward"))),rangeslider = list(type = "date")))}
            else{
                plot_ly(data=df2,x=~time,name="Logins") %>%
                    add_lines(y=~Logins,name="Users") %>%
                    add_lines(y=~Users,name="Users") %>%
                    layout(title = "Hourly Logins and Users Numbers",
                           yaxis = list(title="Counts"),
                           xaxis = list(
                               rangeselector = list(
                                   buttons = list(
                                       list(
                                           step = "hour",
                                           stepmode = "backward"))),rangeslider = list(type = "date")))
            }
        }
        else if(input$fre=="Daily"){
            df3=Daily %>%
                filter(orgId %in% input$orgId) %>%
                group_by(time) %>%
                mutate(total_users=sum(Users)) %>%
                mutate(total_logins=sum(Logins)) %>%
                unique()
            df4=Daily %>%
                filter(orgId %in% input$orgId & type %in% input$type) %>%
                arrange(time)
            if(length(input$type)>1){
                plot_ly(data=df3,x=~time,name="Logins") %>%
                    add_lines(y=~total_logins,name="Users") %>%
                    add_lines(y=~total_users,name="Users") %>%
                    layout(title = "Daily Logins and Users Numbers",
                           yaxis = list(title="Counts"),
                           xaxis = list(
                               rangeselector = list(
                                   buttons = list(
                                       list(
                                           step = "day",
                                           stepmode = "backward"))),rangeslider = list(type = "date")))
            }else{
                plot_ly(data=df4,x=~time,name="Logins") %>%
                    
                    add_lines(y=~Logins,name="Users",opacity=0.8) %>%
                    add_lines(y=~Users,name="Users",opacity=0.8) %>%
                    layout(title = "Daily Logins and Users Numbers",
                           yaxis = list(title="Counts"),
                           xaxis = list(
                               rangeselector = list(
                                   buttons = list(
                                       list(
                                           step = "day",
                                           stepmode = "backward"))),rangeslider = list(type = "date")))
            }  
        }
        else if(input$fre=="Weekly"){
            df5=Weekly %>%
                filter(orgId %in% input$orgId) %>%
                group_by(weekNum) %>%
                mutate(total_users=sum(Users)) %>%
                mutate(total_logins=sum(Logins)) %>%
                unique()
            df6=Weekly %>%
                filter(orgId %in% input$orgId & type %in% input$type) %>%
                arrange(weekNum)
            if(length(input$type)>1){
                plot_ly(data=df5,x=~weekNum,name="Logins") %>%
                    add_lines(y=~total_logins,name="Users") %>%
                    add_lines(y=~total_users,name="Users") %>%
                    layout(title = "Weekly Logins and Users Numbers",
                           yaxis = list(title="Counts"),
                           xaxis = list(
                               rangeselector = list(
                                   buttons = list(
                                       list(
                                           step = "week",
                                           stepmode = "backward"))),rangeslider = list(type = "date")))
            }else{
                plot_ly(data=df6,x=~weekNum,name="Logins") %>%
                    add_lines(y=~Logins,name="Users") %>%
                    add_lines(y=~Users,name="Users") %>%
                    layout(title = "Weekly Logins and Users Numbers",
                           yaxis = list(title="Counts"),
                           xaxis = list(
                               rangeselector = list(
                                   buttons = list(
                                       list(
                                           step = "week",
                                           stepmode = "backward"))),rangeslider = list(type = "date")))
            }
        }
        else if(input$fre=="Monthly"){
            df7=Monthly %>%
                filter(orgId %in% input$orgId) %>%
                group_by(month) %>%
                mutate(total_users=sum(Users)) %>%
                mutate(total_logins=sum(Logins)) %>%
                unique()
            df8=Monthly %>%
                filter(orgId %in% input$orgId & type %in% input$type) %>%
                arrange(month)
            if(length(input$type)>1){
                plot_ly(data=df7,x=~month,name="Logins") %>%
                    add_lines(y=~total_logins,name="Users") %>%
                    add_lines(y=~total_users,name="Users") %>%
                    layout(title="Montly Logins and Users Number",
                           yaxis=list(title="Counts"),
                           xaxis=list(title="Date"))
                
            }else{
                plot_ly(data=df8,x=~month,name="Logins") %>%
                    add_lines(y=~Logins,name="Logins") %>%
                    add_lines(y=~Users,name="Users") %>%
                    layout(title="Montly Logins and Users Number",
                           yaxis=list(title="Counts"),
                           xaxis=list(title="Date"))
            }
        }
    })
    
}


shinyApp(ui = ui, server = server)