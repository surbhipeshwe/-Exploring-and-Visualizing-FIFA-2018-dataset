#########FIT5147 Data Exploration and Visualisation########

#Name : Surbhi Sanjay Peshwe
#Student ID: 30060567


# Loading required packages
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("shinyWidgets")
# install.packages("plotly")
# install.packages("DT")
# install.packages("dplyr")
# install.packages("tidyr")


#Load library
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)
library(ggthemes)

#load data
fifa <- read.csv("CompleteDataset.csv")
df <- fifa
df = tbl_df(df)
df <- select(df, ID, X, Name, Age, Nationality, Overall, Club, Value, Wage, Preferred.Positions)
#y<-top_n(fifa,17000,Overall)
y1<-fifa %>% select(Name,Club,Nationality,Value,Wage,Overall,Potential)
y2<-gather(y1,attribute,value,-Name)
Y3 <- top_n(fifa,10,Overall)
Y3 <- select(Y3,Name,Overall, Potential,Acceleration,Aggression,Agility,Balance,Ball.control,Composure,Crossing,Curve,Dribbling,Finishing,Free.kick.accuracy,GK.diving,GK.handling,GK.kicking,GK.positioning,GK.reflexes,Heading.accuracy,Interceptions,Jumping,Long.passing,Long.shots,Marking,Penalties,Positioning,Reactions,Short.passing,Shot.power,Sliding.tackle,Sprint.speed,Stamina,Standing.tackle,Strength,Vision,Volleys)



#function for converting wage and value into actual currency value
toNumberCurrency <- function(vector) {
  
  vector <- as.character(vector)
  vector <- gsub("(€|,)","", vector)
  result <- as.numeric(vector)
  k_positions <- grep("K", vector)
  result[k_positions] <- as.numeric(gsub("K","", vector[k_positions])) * 1000
  m_positions <- grep("M", vector)
  result[m_positions] <- as.numeric(gsub("M","", vector[m_positions])) * 1000000
  return(result)
}

df$Wage <- toNumberCurrency(df$Wage)
df$Value <- toNumberCurrency(df$Value)

# Select the first postion in the Preferred position column
df$Preferred.Positions <- gsub(" ", "", substr(df$Preferred.Positions, 1, 3))

#categorising players positions
x <- as.factor(df$Preferred.Positions)
levels(x) <- list(GK = c("GK"), DEF=c("LWB", "LB", "CB", "RB", "RWB"), MID = c("LW", "LM", "CDM", "CM", "CAM", "RM", "RW"), FWD = c("CF", "ST"))
df <- mutate(df, Position = x)



# Define UI for the application
ui <- fluidPage(theme = shinytheme("flatly"),
                
                # App title ----
                titlePanel(img(src = "http://pluspng.com/img-png/fifa-logo-png-fifa-logo-685.png", height = "100px"),"FIFA2018"),
                
                
                
                sidebarLayout(
                  sidebarPanel(width = 3,tags$style(".well {background-color: #bfcce6;}"),
                               helpText(h5("This application gives info and stats about players performance in FIFA 2018.A collection of football dataset including national teams, clubs, match schedules, players, stadiums, etc")),
                               
                               # Built with Shiny by RStudio
                               br(), br(),
                               h5( "Built with",
                                   img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                   "by",
                                   img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                                   "."
                               )
                  ),
                  #TAB INFORMATION
                  mainPanel(tabsetPanel(type = "tabs",
                                        tabPanel("INFORMATION",
                                                 img(src = "https://images.performgroup.com/di/library/GOAL/93/18/fifa-18-world-cup-update_177ld55qu1q0610zlg787d8hen.jpg", height = 350, width = 850),
                                                 
                                                 br(),br(),
                                                 h5("The 2018 FIFA World Cup was the 21st FIFA World Cup, an international football tournament contested by the men's national teams of the member associations of FIFA once every four years. It took place in Russia from 14 June to 15 July 2018. It was the first World Cup to be held in Eastern Europe, and the 11th time that it had been held in Europe. At an estimated cost of over $14.2 billion, it was the most expensive World Cup. in this application we will get deeper insight about the players performance and their wages for FIFA 18 and clubs valuation in FIFA 18"),
                                                 br(),br(),
                                                 h4("TOP PLAYERS IN FIFA2018"),
                                                 br(),
                                                 br(),
                                                 img(src = "https://www.futwiz.com/assets/img/fifa19/faces/100684097.png", height = 200, width = 200),
                                                 img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRriTx3ONYMlFz58S2LeAh2lfVnpJHhCMmbda2DgrI0IfYVKAJi", height = 200, width = 200),
                                                 img(src = "https://media.futview.com/players/fifa19/special/82931.png", height = 200, width = 200),
                                                 img(src = "https://futhead.cursecdn.com/static/img/19/players_alt/p67293805.png", height = 200, width = 200),
                                                 br(),
                                                 br(),
                                                 br(),
                                                 h4("TOP FOOTBALL CLUBS "),
                                                 br(),
                                                 br(),
                                                 img(src = "http://www.footballian.com/css/images/blog/champions_league_semi_finalists.png", height = 200, width = 700),
                                                 br(),
                                                 br(),
                                                 br()
                                                 
                                        ),
                                        
                                        #TAB ATRRIBUTE ANALYSIS  
                                        tabPanel("ATRRIBUTE ANALYSIS", 
                                                 img(src = "https://img.maximummedia.ie/joe_ie/eyJkYXRhIjoie1widXJsXCI6XCJodHRwOlxcXC9cXFwvbWVkaWEtam9lLm1heGltdW1tZWRpYS5pZS5zMy5hbWF6b25hd3MuY29tXFxcL3dwLWNvbnRlbnRcXFwvdXBsb2Fkc1xcXC8yMDE4XFxcLzA3XFxcLzI3MTA0MjAwXFxcL2ZpZmFfMTlfbWFpbi5qcGdcIixcIndpZHRoXCI6NzY3LFwiaGVpZ2h0XCI6NDMxLFwiZGVmYXVsdFwiOlwiaHR0cHM6XFxcL1xcXC93d3cuam9lLmllXFxcL2Fzc2V0c1xcXC9pbWFnZXNcXFwvam9lXFxcL25vLWltYWdlLnBuZz9pZD0yNjRhMmRiZTM3MGYyYzY3NWZjZFwiLFwib3B0aW9uc1wiOltdfSIsImhhc2giOiIwMDYzYmFjNTA3NDQ0MDA2NjdkYzA5NDg1YTg4MjEyOTRjMmQ1MTM5In0=/fifa-19-main.jpg", height = 350, width = 850),
                                                 br(),br(),br(),
                                                 
                                                 # Create a new Row in the UI for selectInputs
                                                 fluidRow(
                                                   column(12, numericInput("num", h4("PLAYERS DETAIL TABLE"),
                                                                           10, 1, nrow(y1))
                                                          
                                                   )
                                                 ),
                                                 
                                                 tableOutput("iris_table"),br(),br(),
                                                 h4("VARIATION IN TOP 10 PLAYERS ATTRIBUTE"),
                                                 
                                                 
                                                 fluidRow(
                                                   column(6,selectInput('xcol', "SELECT NAME", "",selected = 1)
                                                   ),
                                                   column(6,selectInput('ycol', "SELECT ATTRIBUTE (y axis)", "", selected = 5)
                                                   )
                                                   
                                                 ),
                                                 h4("By selecting the name in the X axis and selecting attributes like potential, acceleration, agility, etc in the y-axis gives the comparision of the Top 10 players in the world."),
                                                 plotlyOutput('MyPlot'),  br(),br()
                                                 
                                        ),
                                        
                                        #TAB PRICE ANALYSIS
                                        tabPanel(" PRICE ANALYSIS ",
                                                 
                                                 img(src = "https://media.playstation.com/is/image/SCEA/fifa-19-ronaldo-screen-01-ps4-us-08jun18?$native_md_nt$", height = 350, width = 850),
                                                 br(),br(),br(),
                                                 h4("Distribution between Age and Overall of players based on Wage bracket"),
                                                 h5("From the below plot, relationship between the overall potential of the individual player with their age keeping the color as wage bracket can be visualised. It gives an insight on how an overall attribute of the player affects his wage. In addition to that, a smoother has been plotted which shows the decrease of overall value with age, most of the players with age more than 40 has low overall. Practically it shows players efficiency get decrease as they crosses age 40.")
                                                 ,plotlyOutput("price1"),br(),br(),
                                                 h4("Top 10 Valuable Clubs"),
                                                 h5("Based on the players performance from the above plot, it is observed that top 10 clubs are of the player with highest wages. As wage and value of player are almost similar, players with highest value and wage contributes for increasing valuation of clubs. ")
                                                 ,plotlyOutput("price2"),br(),br(),br())
                  )
                  ),position = c("right")
                )
)

# Define the server logic
server <- function(input, output,session) {
  
  data <- reactive({ 
    df <- Y3
    
    updateSelectInput(session, inputId = 'xcol', label = 'SELECT NAME (x axis)',
                      choices = names(df), selected = names(df)[2])
    updateSelectInput(session, inputId = 'ycol', label = 'SELECT ATTRIBUTE (y axis)',
                      choices = names(df), selected = names(df)[2])
    
    return(df)
  })
  
  
  #players attribute comparision plot
  output$MyPlot <- renderPlotly({
    
    k <-ggplot(data(), aes_string(x = input$xcol, y = input$ycol)) +
      geom_point() + geom_smooth(color="darkblue") + theme_classic()
    gg <- ggplotly(k)
    gg <- style(gg, hoverinfo = paste("Name:",~Name), traces = 1)
    
  })
  
  
  
  #Show a table of the first n rows of the data
  output$iris_table <- renderTable({
    data1 <- y1[1:input$num, ]
    data1
  })
  
  
  #Player distribution plot
  output$price1 <- renderPlotly({
    wage_breaks <- c(0, 100000, 200000, 300000, 400000, 500000, Inf)
    wage_labels <- c("0-100k", "100k-200k", "200k-300k", "300k-400k", "400k-500k", "500k+")
    wage_brackets <- cut(x=df$Wage, breaks=wage_breaks, labels=wage_labels, include.lowest = TRUE)
    df <- mutate(df, wage_brackets)
    g_age_overall <- ggplot(df, aes(Age, Overall))
    b<-g_age_overall + geom_point(aes(color=wage_brackets, text=paste("Name:",Name,"<br>", "Club:",Club))) + geom_smooth(color="darkblue")
    ggplotly(b)
  })
  
  
  #top 10 valuable club plot
  output$price2 <- renderPlotly({
    group_clubs <- group_by(df, Club)
    club_value <- summarise(group_clubs, Total_Value = sum(Value))
    top_10_valuable_clubs <- top_n(club_value, 10, Total_Value)
    top_10_valuable_clubs$Club <- as.factor(top_10_valuable_clubs$Club)
    
    gtop <-ggplot(top_10_valuable_clubs, aes(x = Club, y = Total_Value)) + geom_bar(stat = "identity", aes(fill=Club)) + coord_flip() 
    d<- gtop
    ggplotly(d)
  })
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)