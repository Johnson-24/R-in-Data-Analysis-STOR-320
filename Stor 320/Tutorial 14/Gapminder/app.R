library(shiny)
library(tidyverse)
library(broom)
library(purrr)
library(gapminder)

gapminder2=gapminder
names(gapminder2)=c("Country","Continent","Year","Life Expectancy","Population","GDP")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    #Name of Shiny App
    titlePanel("Analysis of Gapminder Data"),
    
    #Specifies the Type of Layout
    sidebarLayout(
        
        #Part of "sidebarLayout"
        sidebarPanel(
            helpText("Instructions: Select Countries of Interest for 
                   Comparison and Analysis For Key Variables "),
            br(),
            #Input to Enter Name
            textInput(inputId="name",label="First Name"),
            
            #Input to Select Country
            selectizeInput(inputId='INcountry',
                           label="Available Countries",
                           choices=gapminder2$Country,
                           selected="United States",
                           multiple=TRUE,
                           options=list()),
            
            #Input to Select Variable
            radioButtons(inputId='INvariable',
                         label="Select Variable",
                         choices=names(gapminder2)[4:6]),
            
            #Part 3:Line Width for Trend Graphic
            sliderInput(inputId="width",
                        label="Width of Trend Lines",
                        min=1,max=3,value=1,step=1),
            
            #Submit Button For Updates
            submitButton("Stay Woke!")
        ),
        
        #Part of "sidebarLayout"
        mainPanel("",
                  
                  #Splits Main Panel Output Using Tabs         
                  tabsetPanel(
                      
                      #First Tab Gives a Preview of the Data Selected
                      tabPanel("Summary",
                               h2("Synopsis"),
                               br(),
                               h4(textOutput("OUTsynopsis1")),
                               h4(textOutput("OUTsynopsis2")),
                               h4(textOutput("OUTsynopsis3")),
                               br(),
                               
                               h2("Data Selected"),
                               br(),
                               #1: Print Data for Desired 
                               #   Countries and Variable
                               tableOutput("OUTpreview"),
                               br(),
                               
                               h2("Country Comparison"),
                               br(),
                               #2: Print Summary
                               tableOutput("OUTsummary"),
                               br()
                               
                      ),
                      tabPanel("Graphics",
                               
                               #3:Print Trend Graphic       
                               h2(textOutput("OUTtrendvar")),
                               br(),
                               plotOutput("OUTtrendplot"),
                               br()
                               
                      )
                  )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #Display the Options Selected By the User
    output$OUTsynopsis1<-renderText({
        expr=paste("User:",input$name)
    })
    output$OUTsynopsis2<-renderText({
        expr=paste("Countries Selected:",paste(input$INcountry,collapse=", "))
    })
    output$OUTsynopsis3<-renderText({
        expr=paste("Variable for Analysis:",input$INvariable)
    })
    
    
    #Part 1: Create a Table Previewing Data
    output$OUTpreview<-renderTable({
        gapminder2 %>% 
            select(Country,Continent,Year,input$INvariable)%>% 
            filter(Country %in% input$INcountry) %>%
            arrange(Year)
    })
    
    #Part 2: Create a Table Summarizing Data by Country
    output$OUTsummary<-renderTable({
        gapminder2 %>% 
            select(Country,Continent,Year,input$INvariable)%>% 
            filter(Country %in% input$INcountry) %>%
            arrange(Year)%>%
            group_by(Country) %>%
            summarize(N=n(),
                      MIN=min(get(input$INvariable)),
                      Q1=quantile(get(input$INvariable),0.25),
                      Q2=quantile(get(input$INvariable),0.5),
                      Q3=quantile(get(input$INvariable),0.75),
                      MAX=max(get(input$INvariable)),
                      CHANGE=MAX-MIN,
                      MEAN=mean(get(input$INvariable)),
                      SD=sd(get(input$INvariable))
            )
    })
    
    #Part 3: Create a Graphic Showing Trends
    output$OUTtrendvar<-renderText({
        expr=paste("Trend Comparison for",input$INvariable)
    })
    
    output$OUTtrendplot<-renderPlot({
        gapminder2 %>% 
            select(Country,Continent,Year,input$INvariable)%>% 
            filter(Country %in% input$INcountry) %>%
            arrange(Year)%>%
            ggplot(aes(x=Year,y=get(input$INvariable))) +
            geom_line(aes(color=Country),size=input$width)+
            ylab(input$INvariable) +
            theme_minimal()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
