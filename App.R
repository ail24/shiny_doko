library(shiny)
library(rsconnect)
library(tidyverse)
library(ggpubr)
library(ggsci)


##### FUNCTIONS #####

aggregate <- function(c){
  n <- c(c[1])
  for(i in 2:(length(c))){
    n[i] <- c[i] + n[i-1]
  }
  n
}


##### UI #####

ui <- fluidPage(
  
  titlePanel("DoKo shiny"),
  
      h4("Table"),
      tableOutput("table"),
      hr(),
      h4("Overall points"),
      plotOutput("plot1", width = "600px"),
      hr(),
      h4("Daily points"),
      plotOutput("plot2", width = "600px"),
      hr(),
      h4("Sum of points"),
      plotOutput("plot3", width = "600px"),
      hr(),
      h4("Best perfomance"),
      plotOutput("plot4", width = "600px"),
      hr(),
      h4("Worst performance"),
      plotOutput("plot5", width = "600px")

)





##### SERVER #####

server <- function(input, output){

  output$table <- renderTable({
    
    d <- read_csv("https://docs.google.com/spreadsheets/d/1doY9SSvwP2TPIigs6FqwRC4VRK23nnhedbAZzAWJe4o/gviz/tq?tqx=out:csv&sheet=Punkteliste", na = "-") %>%
      mutate(date = as.Date(Datum, "%d.%m.%y"))
    
    return(select(d, -date))
    })
  
  output$plot1 <- renderPlot({
    
    d <- read_csv("https://docs.google.com/spreadsheets/d/1doY9SSvwP2TPIigs6FqwRC4VRK23nnhedbAZzAWJe4o/gviz/tq?tqx=out:csv&sheet=Punkteliste", na = "-") %>%
      mutate(date = as.Date(Datum, "%d.%m.%y")) %>%
      select(-Datum) %>%
      gather(key = "player", value = "points", -date) %>%
      group_by(player) %>%
      summarize(sum = sum(points, na.rm = T)) %>%
      mutate(col = factor(sign(sum+0.1))) 

    p <- ggplot(d, aes(x = player, y = sum, fill = col))+
      geom_bar(stat = 'identity')+
      geom_text(aes(y = sum + 7.5*as.numeric(as.character(col)), label = round(sum,2)))+
      theme_pubclean()+
      geom_hline(yintercept = 0)+
      theme(axis.text = element_text(face = "bold"),
            legend.position = 'none')+
      labs(x = NULL,
           y = NULL)
    
    return(p)
    
  })
  
  
  output$plot2 <- renderPlot({
  
    d <- read_csv("https://docs.google.com/spreadsheets/d/1doY9SSvwP2TPIigs6FqwRC4VRK23nnhedbAZzAWJe4o/gviz/tq?tqx=out:csv&sheet=Punkteliste", na = "-") %>%
      mutate(date = as.Date(Datum, "%d.%m.%y")) %>%
      select(-Datum) %>%
      gather(key = "player", value = "points", -date) %>%
      group_by(player) %>%
      mutate(points = replace_na(points, 0))
    
    p <- ggplot(d, aes(x = date, y = points, color = player))+
      geom_line()+
      geom_point()+
      theme_pubclean()+
      geom_hline(yintercept = 0)+
      theme(axis.text = element_text(face = "bold"))+
      labs(x = NULL,
           y = NULL,
           color = NULL)+
      scale_color_jco()+
      theme(legend.key=element_blank())
    
    return(p)
    
  })
  
  output$plot3 <- renderPlot({
    
    d <- read_csv("https://docs.google.com/spreadsheets/d/1doY9SSvwP2TPIigs6FqwRC4VRK23nnhedbAZzAWJe4o/gviz/tq?tqx=out:csv&sheet=Punkteliste", na = "-") %>%
      mutate(date = as.Date(Datum, "%d.%m.%y")) %>%
      select(-Datum) %>%
      gather(key = "player", value = "points", -date) %>%
      group_by(player) %>%
      na.omit() %>%
      mutate(date2 = as.character(date)) %>%
      arrange(player, date2) %>%
      group_by(player) %>%
      mutate(points = aggregate(points))
    
    p <- ggplot(d, aes(x = date, y = points, color = player))+
      geom_line()+
      geom_point()+
      theme_pubclean()+
      geom_hline(yintercept = 0)+
      theme(axis.text = element_text(face = "bold"))+
      labs(x = NULL,
           y = NULL,
           color = NULL)+
      scale_color_jco()+
      theme(legend.key=element_blank())
    
    return(p)
    
  })
  
  
  
  
  output$plot4 <- renderPlot({
    
    d <- read_csv("https://docs.google.com/spreadsheets/d/1doY9SSvwP2TPIigs6FqwRC4VRK23nnhedbAZzAWJe4o/gviz/tq?tqx=out:csv&sheet=Punkteliste", na = "-") %>%
      mutate(date = as.Date(Datum, "%d.%m.%y")) %>%
      select(-Datum) %>%
      gather(key = "player", value = "points", -date) %>%
      group_by(player) %>%
      mutate(points = replace_na(points, 0)) %>%
      summarize(max = max(points),
                min = min(points))
    
    p <- ggplot(d, aes(x = player, y = max, fill = player))+
      geom_bar(stat = 'identity')+
      theme_pubclean()+
      theme(axis.text = element_text(face = "bold"))+
      labs(x = NULL,
           y = NULL,
           fill = NULL)+
      scale_fill_jco()+
      theme(legend.key=element_blank(),
            legend.position = 'none')+
      coord_flip()+
      scale_y_continuous(expand = c(0,0), limits = c(0,max(d$max)*1.05))
    
    return(p)
    
  })
  
  
  
  
  output$plot5 <- renderPlot({
    
    d <- read_csv("https://docs.google.com/spreadsheets/d/1doY9SSvwP2TPIigs6FqwRC4VRK23nnhedbAZzAWJe4o/gviz/tq?tqx=out:csv&sheet=Punkteliste", na = "-") %>%
      mutate(date = as.Date(Datum, "%d.%m.%y")) %>%
      select(-Datum) %>%
      gather(key = "player", value = "points", -date) %>%
      group_by(player) %>%
      mutate(points = replace_na(points, 0)) %>%
      summarize(max = max(points),
                min = min(points))
    
    p <- ggplot(d, aes(x = player, y = min, fill = player))+
      geom_bar(stat = 'identity')+
      theme_pubclean()+
      theme(axis.text = element_text(face = "bold"))+
      labs(x = NULL,
           y = NULL,
           fill = NULL)+
      scale_fill_jco()+
      theme(legend.key=element_blank(),
            legend.position = 'none')+
      coord_flip()+
      scale_y_continuous(expand = c(0,0), limits = c(min(d$min)*1.05,0))
    
    return(p)
    
  })
  
}



shinyApp(ui = ui, server = server)



#deployApp(appName = "doko_shiny")

