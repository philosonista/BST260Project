library(shiny)
library(ggplot2)
library(lubridate)
library(tidyverse)

d <- setwd("/Users/owner/Desktop/260master/")

#Download datasets
for (id in c(19,58,59)){
    assign(paste0("Summary_UID",id), read.csv(paste0("Summary_UID",id,".csv")))
    assign(paste0("Happiness_UID",id), read.csv(paste0("Happiness_uid",id,".csv")))
    assign(paste0("AN_UID",id), read.csv(paste0("AN_UID",id,".csv")))
}


Happiness_UID59  = Happiness_UID59 %>% 
    group_by(date) %>%
    summarize(happy = round(mean(happy))) %>%
    ungroup() %>%
    mutate(uid = 59)

Happiness_UID19 = Happiness_UID19[-c(2)]
Happiness_UID58 = Happiness_UID58[-c(2)]
Happiness_UID59 = Happiness_UID59[c(3,1,2)]

Summary_UID19$UID = 19
Summary_UID58$UID = 58
Summary_UID59$UID = 59


AN_UID19$UID = 19
AN_UID58$UID = 58
AN_UID59$UID = 59


longDat = bind_rows(Summary_UID19, Summary_UID58, Summary_UID59)
longDatHappy = bind_rows(Happiness_UID19, Happiness_UID58, Happiness_UID59)
longDatAN = bind_rows(AN_UID19, AN_UID58, AN_UID59)

##Change Column Names
colnames(longDat)[4] = "Convo Duration"
colnames(longDat)[5] = "Total Convos"
colnames(longDat)[10] = "Avg Deadlines"
colnames(longDat)[11] = "Sleep"
colnames(longDat)[12] = "Inferred Sleep"

colnames(longDatAN)[3] = "Sociability"
colnames(longDatAN)[4] = "Mobility"
colnames(longDatAN)[5] = "Stress"
colnames(longDatAN)[6] = "Sleep"


ui <- fluidPage(
    titlePanel("Detecting Anomalous Behavioral Trends to Predict Mood Dips"),
    sidebarLayout(
        sidebarPanel(
            selectizeInput(inputId = "feature",
                           label = "Choose (up to 3) Features",
                           multiple = TRUE,
                           options = list(maxItems=3),
                           selected = "Sleep",
                           c("Stress","Deadlines","Sleep","Exercise","Activity","Total Convos","Convo Duration","Inferred Sleep")
                           
            ),
            selectInput(inputId = "UID",
                        label = "Choose a Student",
                        c("19","58","59")),
            
            dateRangeInput(
                inputId = "Date",
                label = "Choose Date Range",
                start = ymd("2013-04-25"),
                end = ymd("2013-06-01"),
                min = ymd("2013-04-25"),
                max = ymd("2013-06-01")
            ),
            
            tableOutput("summary")
        )
        
        ,
        mainPanel(
            verticalLayout(
                plotOutput("FeaturesPlot"),
                plotOutput("AnomalyPlot"),
                plotOutput("HappinessPlot")
            )
        )
    )
    
)
server <- function(input, output) {
    selected = reactive(longDat %>%
                            filter(UID == input$UID) %>%
                            filter(Date >= as.Date(input$Date[1], origin = "1970-01-01") & Date <= as.Date(input$Date[2], origin = "1970-01-01")) %>%
                            select(c("Date", as.vector(input$feature))) %>%
                            gather("feature","value",2:(1+length(as.vector(input$feature)))))
    
    selectedHappy = reactive(longDatHappy %>%
                                 filter(uid == input$UID) %>%
                                 filter(ymd(date) >= as.Date(input$Date[1], origin = "1970-01-01") & ymd(date) <= as.Date(input$Date[2], origin = "1970-01-01")) %>%
                                 select(c("date","happy")))

    
    selectedAN = reactive(longDatAN %>%
                              filter(UID == input$UID) %>%
                              filter(ymd(date) >= as.Date(input$Date[1], origin = "1970-01-01") & ymd(date) <= as.Date(input$Date[2], origin = "1970-01-01")) %>%
                              select(c("date","Sociability","Mobility","Stress","Sleep")) %>%
                              gather("feature","value",2:5))
    
    table = reactive(longDat %>%
                         filter(UID == input$UID) %>%
                         filter(Date >= as.Date(input$Date[1], origin = "1970-01-01") & Date <= as.Date(input$Date[2], origin = "1970-01-01")) %>%
                         select(c("Date", as.vector(input$feature))))
                        
    ##Create named vectors defining dates of depressive dips
    uids1 = c(19,58,59)
    uids2 = c(19,58,59)
    uids3 = c(19,58,59)
    uids4 = c(19,58,59)
    uids5 = c(19,58,59)
    uids6 = c(19,58,59)
    names(uids1) = c("2013-05-15","2013-05-20","2013-04-27")
    names(uids2) = c("2013-05-18","2013-05-22","2013-04-29")
    names(uids3) = c("","","2013-05-11")
    names(uids4) = c("","","2013-05-13")
 
    
    FeatureColors = c("Stress" = "purple","Deadlines" = "purple","Sleep" = "green", "Inferred Sleep" = "darkgreen","Exercise" = "red","Activity"="red","Total Convos"="blue","Convo Duration"="blue")
    ANColors = c("Sociability" = "blue","Mobility"="red","Stress"="purple","Sleep"="green")
    
    output$FeaturesPlot = renderPlot({
        selected() %>%
            ggplot(aes(Date,value, color = feature)) +
            geom_point(size=2.5) +
            scale_colour_manual(values =FeatureColors)+
            ggtitle(paste("Features by Day for Student", input$UID)) +
            xlab("")+ ylab("")+
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            facet_grid(feature ~., scales = "free_y") +
            
            #The following lines plot the shaded region
            annotate("rect", xmin = names(uids1)[uids1 == input$UID], xmax = names(uids2)[uids2 == input$UID], ymin = -Inf, ymax = Inf,
                 alpha = .2) +
            annotate("rect", xmin = names(uids3)[uids3 == input$UID], xmax = names(uids4)[uids4 == input$UID], ymin = -Inf, ymax = Inf,
                     alpha = .2) 
            
    })
    output$AnomalyPlot = renderPlot({
        selectedAN() %>%
            ggplot(aes(date, value,color = feature)) +
            ggtitle(paste("Anomaly Detection by Feature for Student", input$UID)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1))+
            xlab("")+ ylab("")+
            geom_point(aes(color = feature), size=2.5) +
            scale_colour_manual(values =ANColors) +
            
            #The following lines plot the shaded region
            annotate("rect", xmin = names(uids1)[uids1 == input$UID], xmax = names(uids2)[uids2 == input$UID], ymin = -Inf, ymax = Inf,
                     alpha = .2) +
            annotate("rect", xmin = names(uids3)[uids3 == input$UID], xmax = names(uids4)[uids4 == input$UID], ymin = -Inf, ymax = Inf,
                     alpha = .2)
        
    })
    
    output$HappinessPlot = renderPlot({
         selectedHappy() %>% na.omit() %>%
             ggplot(aes(as.Date(date),happy)) +
             ggtitle(paste("Happiness EMA Score for Student", input$UID)) +
             xlab("")+ ylab("")+
             theme(axis.text.x = element_text(angle = 90, hjust = 1))+
             geom_point(size=2.5)+
             geom_line() +
            
            #The following lines plot the shaded region
            annotate("rect", xmin = as.Date(names(uids1)[uids1 == input$UID]), xmax = as.Date(names(uids2)[uids2 == input$UID]), ymin = -Inf, ymax = Inf,
                     alpha = .2) +
            annotate("rect", xmin = as.Date(names(uids3)[uids3 == input$UID]), xmax = as.Date(names(uids4)[uids4 == input$UID]), ymin = -Inf, ymax = Inf,
                     alpha = .2)

    })
    
    output$summary = renderTable(
        
        table()
    )
}

# Run the application 
shinyApp(ui = ui, server = server)

