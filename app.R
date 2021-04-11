#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(quantmod)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(lubridate)



master_champ <- read.csv('spi_matches_latest.csv')
champ_list<- c('Chinese Super League','Argentina Primera Division','Major League Soccer','Mexican Primera Division Torneo Apertura',	
'French Ligue 1','Barclays Premier League','Spanish Primera Division','Dutch Eredivisie','Italy Serie A','German Bundesliga')

master_champ$X <- NULL
master_champ$date <- strptime(master_champ$date, format='%Y-%m-%d')
master_champ <- master_champ %>% drop_na()



# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Vitórias dos times mandantes nas principais ligas mundiais"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Análise",tabName = "analysis",icon=icon("chart-line")),
            menuItem("Comparaçao",tabName = "comparation",icon=icon("chart-bar"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = 'analysis',
                    fluidRow(
                        box(title = 'Selecione a liga a ser analisada', width=12, solidHeader = TRUE,
                            selectInput('league', 'Liga ', champ_list, multiple=FALSE),
                            uiOutput("timedate"),
                            actionButton('go', 'Analisar')
                        )
                    ),
                    fluidRow(
                        box(title = "Informações sobre o percentual de vitórias dos mandantes na liga escolhida", width = 12, solidHeader = TRUE,
                            DTOutput('info') )
                    ),
                    fluidRow(
                        box(title = "Grafico em linhas", width = 12, solidHeader = TRUE, plotOutput('lineGraph')
                        )
                    ),
                    fluidRow(
                        box(title = " Histograma", width = 12, solidHeader = TRUE,plotOutput('histogram')
                        )
                    ), fluidRow(
                        box(title = "BoxPlot", width = 12, solidHeader = TRUE,plotOutput('boxplot')
                        )
                    ) 
            ),
            
            tabItem(tabName = 'comparation',
                    fluidRow(
                        box(title = 'Selecione as ligas a serem comparadas', width=12, solidHeader = TRUE, status='warning',
                            selectInput('league_comp', 'Ligas', champ_list, multiple=TRUE),
                            uiOutput("timedate_comp"),
                            actionButton('go_comp', 'Comparar')
                        )
                    ),
                    fluidRow(
                        box(title = "Tabela Valor de correlaçao", width = 12, solidHeader = TRUE, plotOutput('corr_comp')
                        )
                    ),
                    fluidRow(
                        box(title = "Grafico em linhas comparado", width = 12, solidHeader = TRUE, plotOutput('lineGraph_comp')
                        )
                    ),
                    fluidRow(
                        box(title = "Grafico de barras 1", width = 12, solidHeader = TRUE, plotOutput('barPlot_comp')
                        )
                    ),
                    fluidRow(
                        box(title = "Grafico de barras 2", width = 12, solidHeader = TRUE, plotOutput('barPlot_comp2')
                        )
                    )
            )
        )
        
    )
)
    

# Define server logic required to draw a histogram
server <- function(input, output) {
    ####INPUT######
    select_liga <- eventReactive(input$go,{
        
        name_liga <-input$league
        
        twin<-input$true_date
        
        league_<- master_champ %>% filter(league==name_liga)
        return(league_)
    })
    
    select_liga_comp <- eventReactive(input$go_comp,{
        
        name_ligas <- input$league_comp

      # df1 <- master_champ %>% filter(league%in%name_ligas[1])
      #  df2 <- master_champ %>% filter(league%in%name_ligas[2])
      #  print(df1)
      #  print(df2)
        
        return(name_ligas)
    })
    
    #####OUTPUT#####
    output$timedate_comp <- renderUI({
        
        name_ligas <- input$league_comp
        
        df <- master_champ %>% filter(league %in% name_ligas)
        
        maxmin_time <- df %>% 
            group_by(league) %>% 
            summarise(MD = min(date)) %>% 
            .$MD %>% 
            max()
        
        minmax_time <- df %>% 
            group_by(league) %>% 
            summarise(MD = max(date)) %>% 
            .$MD %>% 
            min()
        
        min_time <- maxmin_time
        max_time <- minmax_time
        
        dateRangeInput("true_date_comp", "Período de análise",
                       end = max_time,
                       start = min_time,
                       min    = min_time,
                       max    = max_time,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language='pt-BR')
    })
    
    output$timedate <- renderUI({
        
        name_liga <- input$league

        df <- master_champ %>% filter(league == name_liga)
        
        min_time <- min(df$date)
        
        max_time <- max(df$date)
        
        dateRangeInput("true_date", "Período de análise",
                       end = max_time,
                       start = min_time,
                       min  = min_time,
                       max  = max_time,
                       format = "dd/mm/yy",
                       separator = " - ",
                       language='pt-BR')
    })
    
    
    output$corr_comp <-renderPlot({
        
        name_ligas <- select_liga_comp()
      
        name_ligas1 <- master_champ %>% filter(league %in% name_ligas[1])%>% select(7)
        name_ligas2 <- master_champ %>% filter(league %in% name_ligas[2])%>% select(7)
      
       dataF <- merge(name_ligas1, name_ligas2, by = "row.names", all = TRUE)
       
       dataFrame <-dataF %>% select(2,3)
       print(dataFrame)
       
        ggplot(dataFrame, aes(x=spi1.x, y=spi1.y)) + 
            geom_point()
        
    })
    #RENDER GRAFICO DE LINHA
    output $ lineGraph <- renderPlot({
        # All the inputs
        name_liga <- select_liga()
        
        aux <- name_liga$spi1 %>% na.omit() %>% as.numeric()
        aux1 <- min(aux)
        aux2 <- max(aux)
        
        name_liga$date <- ymd(name_liga$date)
        
        a <- name_liga %>% 
            ggplot(aes(date, spi1, group=1)) +
            geom_path() +
            ylab('Vitórias dos mandantes em percentual') +
            coord_cartesian(ylim = c(aux1, aux2)) +
            theme_bw() +
            scale_x_date(date_labels = "%Y-%m-%d")
        
        a
    })
    
    output$lineGraph_comp <- renderPlot({
        
        name_ligas <- select_liga_comp()
        
        name_ligas1 <- master_champ %>% filter(league %in% name_ligas)
        
        maxmin_spi1 <- name_ligas1 %>% 
            group_by(league) %>% 
            summarise(MD = min(spi1)) %>% 
            .$MD %>% 
            max()
 
        minmax_spi1 <- name_ligas1 %>% 
            group_by(league) %>% 
            summarise(MD = max(spi1)) %>% 
            .$MD %>% 
            min()
   
        min_spi1 <- maxmin_spi1
        max_spi1 <- minmax_spi1
        
        name_ligas1$date <- ymd(name_ligas1$date)
        
        a <- name_ligas1 %>% 
            ggplot(aes(date, spi1, group=1)) +
            geom_line(color="red")+
            geom_path() +
            ylab('Vitórias comparadas dos mandantes em percentual') +
            coord_cartesian(ylim = c(as.numeric(min_spi1), as.numeric(max_spi1))) +
            theme_bw() +
           scale_x_date(date_labels = "%Y-%m-%d")
        
        a
    
    })
    #RENDER HISTOGRAM
    output$histogram <-renderPlot({
        name_liga1 <- select_liga()
        
      #  timeLiga <- name_liga1$date %>% na.omit() %>% as.numeric()
      #  time1 <-min(timeLiga)
      #  time2 <-max(timeLiga)
        
        aux <- name_liga1$spi1 %>% na.omit() %>% as.numeric()
        aux1 <- min(aux)
        aux2 <- max(aux)
        
        hist(name_liga1$spi1,main="Vitórias dos times mandantes ao longo do período",xlab="Tempo(Início-Fim do Período)",ylab="Vitórias(percentual)",col="red",)
              
    })
    #GRAFICO DE BARRAS
    output$barPlot_comp <- renderPlot({
        
        name_ligas <- select_liga_comp()
        
        name_ligas1 <- master_champ %>% filter(league %in% name_ligas[1])%>% select(2,7)
        
        name_ligas1$date <- ymd(name_ligas1$date)
      
        ggplot(name_ligas1, aes(x=date, y=spi1)) +
            geom_bar(stat = "identity")+ scale_x_date(date_labels = "%Y-%m-%d")+ggtitle(paste("",name_ligas[1]))
        
     })
    
    output$barPlot_comp2 <- renderPlot({
        
        name_ligas <- select_liga_comp()
        name_ligas2 <- master_champ %>% filter(league %in% name_ligas[2])%>% select(2,7)
        
        name_ligas2$date <- ymd(name_ligas2$date)
        
        ggplot(name_ligas2, aes(x=date, y=spi1)) +
            geom_bar(stat = "identity")+ scale_x_date(date_labels = "%Y-%m-%d")+ggtitle(paste("",name_ligas[2]))
    })
    
    
    output$boxplot <-renderPlot({
        name_liga2 <- select_liga()
        
        ggplot(name_liga2, aes(x=as.factor(date), y=spi1)) + 
            geom_boxplot(fill="slateblue", alpha=0.2) + 
            xlab(name_liga2)
        
    })
    
    #MOSTRAR TABELA (MEDIA MODA MEDIANA DESVIO PADRAO MIN MAX)#
    ################ OUTPUT #####################
    Info_DataTable <- eventReactive(input$go,{
        name_liga <- select_liga()
        
        mean <- name_liga %>% select(spi1) %>% colMeans()
        Media <- mean[[1]]
        
        Liga <- input$league
       #apply(df1,2,median)
        
        median1<-apply(name_liga %>% select(spi1),2,median) 
        Mediana <- median1[[1]]
        
        mode1 <- names(table(name_liga %>% select(spi1)))[table(name_liga %>% select(spi1))==max(table(name_liga %>% select(spi1)))]
        Moda <- mode1[[1]]
       
        sd1 <- apply(name_liga %>% select(spi1),2,sd) 
        DesvioPadrao <- sd1[[1]]
        
        min1 <- min(name_liga %>% select(spi1))
        ValorMínimo <- min1[[1]]
        
        max1 <- max(name_liga %>% select(spi1))
        ValorMáximo <- max1[[1]]
        
        tipo_tb <-  data.frame(Liga,Media,Mediana,Moda,DesvioPadrao,ValorMínimo,ValorMáximo)
        
        tipo_tb <- as.data.frame(t(tipo_tb))
        
        return(tipo_tb)
    })
    
    
    
    output$info <- renderDT({
        Info_DataTable() %>%
            as.data.frame() %>% 
            DT::datatable(options=list(
                language=list(
                    url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json'
                )
            ))
    })
 
}


  


# Run the application 
shinyApp(ui = ui, server = server)

