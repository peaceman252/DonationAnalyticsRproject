#Adds packages to be used
#Shiny is an R package that makes it easy to build interactive web apps straight from R. 
library(shiny)
#Creates Dashboards with Shiny 
library(shinydashboard)
#Creates Compact Hash Digests of R Objects 
library(digest)
#Creates Interactive Web Maps with the JavaScript Leaflet Library 
library(leaflet)
#Bindings for the Geospatia' Data Abstraction Library
library(rgdal)
#Classes and Methods for Spatial Data
library(sp)
#Converts Data from and to GeoJSON or TopoJSON
library(geojsonio)
#R Database Interface
library(DBI)
#Database Interface and MariaDB Driver
library(RMariaDB)
#Creates Elegant Data Visualisations Using the Grammar of Graphics 
library(ggplot2)
#Reads Excel Files
library(readxl)

#Create header for shiny dashboard
header = dashboardHeader(
  title = "Donation Analytics",
  #outputs from the server
  menuItemOutput("header"),
  menuItemOutput("header2")
)
#Create sidebar for shiny dashboard
sidebar = dashboardSidebar(
  sidebarMenu(
    #outputs from the server
    menuItemOutput("menu")
  )
)
#Create body for shiny dashboard
body = dashboardBody(
  #Import CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "myStyle.css")
  ),
  #outputs from the server
  menuItemOutput("ui")
)


# Define dashboard's UI
ui <- dashboardPage(skin="red",header, sidebar, body)

# Define server logic
server <- function(input, output, session) {

  #create ui output for body
  output$ui <- renderUI({
    #If user is not logged in
    if (userInput$authenticated == FALSE) {
      #tabitems are represented on the sidebar
      tabItems(
        #Content for non-administrative user
        tabItem(tabName = "home",
          fluidRow(
            box(title = "Before Donation", width = 12,  collapsible = TRUE, status = "danger", solidHeader = TRUE,
              box(title = "Check your eligibility",  collapsible = TRUE, status = "danger", solidHeader = TRUE, collapsed = TRUE,
                p("An illness or injury, however minor, may mean it is not safe to give your blood to a sick patient.  Certain medications can also be harmful to patients. Therefore it is important that you are well and healthy when you come to donate blood.  Recent travel may also mean that you may not be eligible to donate. When you attend a clinic you will have an opportunity to speak in confidence with a nurse but if you have any queries in relation to your health or travel history you can contact the IBTS Donorline at 1850 731 137. Information is also available in the ‘Can I give blood’ section.")
              ),
              box(title = "Reduce risk of fainting", collapsible = TRUE, status = "danger", solidHeader = TRUE, collapsed = TRUE,
                p("Drinking plenty of cold, non-alcoholic fluids in the 24 hours prior to donating and eating savoury food and / or salty snacks the night before donation will greatly reduce the risk of fainting during or after donating. In addition, please ensure you eat something substantial in the 3 hours prior to your donation.")
              ),
              box(title = "Children in clinics", collapsible = TRUE, status = "danger", solidHeader = TRUE, collapsed = TRUE,
                p("Please note our new health and safety regulations in relation to children attending with a donating adult.  Due to the potential risk of a donor experiencing an adverse reaction during or after donating, a donating adult cannot be the sole supervisor of any child who is under 13 years of age. Please ensure that there is another adult designated to supervise any child who is less than 13 years of age during each part of the donation process.")
              ),
              box(title = "Allow enough time", collapsible = TRUE, status = "danger", solidHeader = TRUE, collapsed = TRUE,
                p("Although a donation only takes approximately 8 minutes, you will need to allow 60-90 minutes for the entire process, i.e. from the time you register to resting afterwards while enjoying refreshments.  Appointments are available in Dublin (D’Olier Street and Stillorgan clinics) and Cork (St Finbarr's Hospital).")
              )
            ),
            box(title = "During Donation", width = 6, collapsible = TRUE, status = "danger", solidHeader = TRUE,
              box(title = "Registration", collapsible = TRUE, status = "danger", solidHeader = TRUE, collapsed = TRUE,
                p("When you volunteer to give blood and attend a blood donation clinic, you will be asked to register with the Irish Blood Transfusion Service (IBTS). We ask for your name, address, date of birth and telephone numbers. This information is entered on the IBTS computerised donor database and is used by us to communicate with you, e.g. to send invitations to future blood donor clinics.  Information related to you and your blood donations is stored securely on the database.")
              ),
              box(title = "Donor Interview", collapsible = TRUE, status = "danger", solidHeader = TRUE, collapsed = TRUE,
                p("You will be given information to read and a health and lifestyle questionnaire to complete. You will then have an interview with a trained healthcare professional to determine if you are eligible to donate. The purpose of the interview is to ensure that it is safe for you to donate and safe for a patient to receive your blood. All information provided by you will be treated in the strictest confidence. There are many reasons why you may not be eligible to donate and if you are not eligible, the reason for this will be fully explained to you and you will be advised when you can return to donate. While we understand that it may be disappointing for those donors who are unable to donate on a given day, we would encourage all donors to return to give blood when we notify them that they are eligible again, to help us maintain the blood supply to the hospitals.")
              ),
              box(title = "Haemoglobin Testing", collapsible = TRUE, status = "danger", solidHeader = TRUE, collapsed = TRUE,
                p("If you are eligible to donate you will have your Haemoglobin tested. This test is carried out on a capillary sample, i.e. a small drop of blood taken from your fingertip. ")
              ),
              box(title = "Donation", collapsible = TRUE, status = "danger", solidHeader = TRUE, collapsed = TRUE,
                p("A needle attached to a blood pack is inserted into one of the veins in your forearm. Your donation is collected into this pack.  Blood samples are collected from this pack during the donation and these are tested in the laboratory post-donation. 470 mls of blood is collected during the donation and this takes approximately 8 minutes. You will be closely monitored by a staff member during this time.")
              ),
              box(title = "After Donating", collapsible = TRUE, status = "danger", solidHeader = TRUE, collapsed = TRUE,
                p("You are advised to remain in the donation clinic for at least 15 minutes after donating. This time is spent in the canteen area where you will be given refreshments. It is important to drink cold fluids post donation in order to rehydrate yourself. You will be given a card with post donation advice. It is important to keep this card for reference purposes.")
              )
            ),
            box(title = "After Donation", width = 6, collapsible = TRUE, status = "danger", solidHeader = TRUE,
              box(title = "If you feel unwell within 28 days of attending", collapsible = TRUE, status = "danger", solidHeader = TRUE, collapsed = TRUE,
                p("If you become unwell within 4 weeks of donating you must contact the IBTS as soon as you become unwell. The reason for this is that any future illness may have consequences for the patient that has received or will receive your blood donation. Contact details are printed on the post donation advice card.")
              ),
              box(title = "Possible complications of donation", collapsible = TRUE, status = "danger", solidHeader = TRUE, collapsed = TRUE,
                p("For the majority of people the process of giving blood is a simple and trouble-free experience. However, possible complications of giving blood include:"),
                p("Bleeding from the needle site"),
                p("Bruising"),
                p("Fainting / feeling weak or light-headed"),
                p("Uncommon risks of donating blood include:"),
                p("Nerve irritation / injury / pain"),
                p("Tendon injury / pain"),
                p("Arterial puncture"),
                p("If you suffer from any of these after you have left the clinic, please contact a doctor at the National Blood Centre on 01 4322800 or the Munster Region Transfusion Centre on 021 4807400. There is a doctor on call 24 hours a day who can advise you.")
              ),
              box(title = "What about my next attendance?", collapsible = TRUE, status = "danger", solidHeader = TRUE, collapsed = TRUE,
                p("You can donate every 90 days if you are eligible. The IBTS will contact you by text message with information about the next clinic in your area or you find a clinic on the map.")
              ),
              box(title = "Thank you", collapsible = TRUE, status = "danger", solidHeader = TRUE, collapsed = TRUE,
                p("150,000 donations a year or almost 3,000 donations a week are required for patients in Irish hospitals. The IBTS can only meet this requirement by the generosity of our donors.  Please donate regularly and encourage your friends and family to become donors too.")
              )
            )
          )
        ),
        #tab for the user to find their nearest clinic
        tabItem(tabName = "clinics",
          box(title = "Map of Clinic Locations in Dublin", width = 8, status = "danger", solidHeader = TRUE,
            #map created to show clinics
            leafletOutput('clinicMap', height = 480, width = 720)
          )
        ),
        #tab for the user to find their nearest clinic
        tabItem(tabName = "news",
          fluidRow(
            box(title = "Blood Donations Drop by a Quarter in Last Decade", width = 6, status = "danger", solidHeader = TRUE,
              h5("Sylvia Thompson"),
              img(src = "image.jpg", width = 480),
              p("If you are looking for a simple example of altruism, then donating blood is it. And if you’re looking for a place to witness acts of kindness, step inside a blood donor clinic."),
              p("On a visit to the blood donor clinic on D’Olier Street, Dublin recently, I was struck by the calm, organised atmosphere as people walk in off the street to give what one donor described as a renewable resource to someone who desperately needs it."),
              p("The problem is, not enough people are donating blood to keep supplies at the levels required. “The drop in blood donations is a major problem around the world,” says Paul McKinney from the Irish Blood Transfusion Service (IBTS)*."),
              p("A recent survey of blood donors in 18 countries found donations have dropped by over 25 per cent since 2005. “In Ireland, there is a particular need for new donors as 21 per cent fewer people came forward to donate for the first time in 2015 compared to 2010,” says Stephen Cousins, IBTS national donor services manager."),
              p("To draw attention to this fact, the IBTS has joined the international hashtag Missing Type (#missingtype) campaign. This social media campaign uses the letters, A, B and O disappearing at iconic locations around the world to highlight the need for blood. “It’s in everyone’s interest that we have a safe supply of blood. The typical age profile of blood donors in Ireland is over 40 and we are keen to attract young donors,” says McKinney."),
              p("In Ireland, about 3,000 blood donations are required every week to have enough for those receiving cancer treatment, recovering from surgery or childbirth and for all kinds of other medical emergencies. One in four of us will need a blood donation at some stage in our lifetime yet only about 3 per cent of those who can donate blood do so."),
              p("The IBTS is hoping to recruit 8,000 new donors by the end of 2016. Donating blood is a simple procedure. Upon arrival at the blood donor clinic, you fill in an eligibility form, answer questions about your health and have your haemoglobin levels checked to make sure you’re not anaemic. “There are a lot of factors that have to be taken into account before you give blood. I always say to people who can’t donate that if they were on the receiving end of a blood transfusion, they would be very pleased to know we are so careful,” says Graham Egan, manager at the clinic in D’Olier Street."),
              p("If everything is in order, you are taken to an enclosed area where you lie down while a needle is put into a vein in your arm and about 500ml of blood is taken. After about 15 minutes, you are brought to a canteen to have something to eat and drink before leaving."),
              p("Some 470ml of the blood donated is held in an overnight fridge while a 30ml sample is sent to the laboratory for testing. Once given the all clear, the blood is packaged and sent to the IBTS blood banks in Dublin and Cork from where it will be distributed to hospitals."),
              p("Marie Tighe (55), a nurse, is giving her 26th blood donation at the clinic. “I’ve worked in healthcare all my life and I’ve seen the need for blood. There is a culture around giving blood for some people but I’m aware that many young people have a fear of needles and many don’t give blood.”"),
              p("Tighe says greater awareness is needed in workplaces. “Blood donation should be promoted in the same way that quitting smoking and donating organs are promoted,” she says."),
              p("Sam Shaw (24) is also donating blood at the clinic when I visit. “I’ve been donating blood since I was 18 and my blood type is O-negative – which is rare but can be given to all blood types,” says Shaw."),
              p("Visibly proud of donating blood, Shaw explains how the supply of O-negative is critically low at the moment and his blood will be packaged separately for use for newborn babies. “I’m just finished my training to become a pilot in Spain so I will come back again in 90 days to give blood again,” he says."),
              p("Research scientist, Steven Fagan (28) wasn’t permitted to give blood on the day we visited but he will come back in three months. “I gave blood about eight years ago and then I got busy and didn’t think about giving blood. But now I think it’s a renewable resource: I don’t need all I have and other people need it urgently so I hope to give blood more regularly in the future.”")
            ),
            box(title = "Campaign to Encourage More Blood Donors Begins", width = 6, status = "danger", solidHeader = TRUE,
              h5("Ciarán D'Arcy"),
              img(src = "image2.jpg", width = 480),
              p("Organisations including Lidl, Glanbia and Irish Rail are dropping the letters A, B and O from their logos as part of a new publicity campaign to encourage more blood donors."),
              p("The Irish Blood Transfusion Service (IBTS) says the number of new donors attending clinics across the country fell by a fifth between 2010 and 2015, with a quarter fewer people aged 18 to 35 donating blood last year compared with 2010."),
              p("The #MissingType campaign will run through August, and the aim of the initiative is to make people aware of their own blood type and whether it is particularly in demand."),
              p("The IBTS is also asking people to share social media posts with the letters A, B and O missing in order to further raise awareness of the ongoing shortage."),
              p("The campaign is also supported by the Rose of Tralee Festival, which takes place this week."),
              p("“People rarely think about whether their blood type is A, B or O – many of us don’t even know our own blood type. But if that ‘type’ was to go missing in everyday life, people would start to pay closer attention to the need for blood,” said IBTS operations director Paul McKinney."),
              p("Anyone thinking of donating blood can find their nearest clinic at giveblood.ie.")
            ),
            box(title = "Appeal for 1,500 blood donations as stocks drop 21%", width = 6, status = "danger", solidHeader = TRUE,
              h5("Niall Sargent"),
              img(src = "image3.jpg", width = 480),
              p("The Irish Blood Transfusion Service (IBTS) has appealed for 1,500 additional donations to steady blood stocks eroded by a recent fall in donors."),
              p("While there is no immediate threat of blood shortages at hospitals, the service yesterday warned the wider issue of declining donor numbers and increasing donor age remains."),
              p("In the past decade, the number of donors fell 21 per cent, while the average age of donors rose from 38 to 41."),
              p("The decline in young donors is a particular worry for Paul McKinney, director of operations at IBTS."),
              p("As a unit of blood lasts just 35 days, he said a regular flow of donors is vital in the coming years."),
              p("“They are the future,” said Mr McKinney. “We need younger people to consider coming regularly as donors. Every donation does save a life, and you try to get them to appreciate this.”"),
              p("Currently, only three per cent of those eligible to donate blood are active donors, despite the fact that one in four people will require a transfusion at some point in their life."),
              p("“It’s an incredible feeling to see the gift you are giving people and I wish the younger people could see this,” Mr McKinney added."),
              p("He said the service is having difficulty in getting its message across to younger generations in the social media age. “We are up against so many other activities that we are competing for young people’s attention.”"),
              p("In addition, the increasing popularity of tattoos and the burgeoning interest in backpacking trips to exotic locations mean young adults often face deferral periods before they can donate."),
              p("“It’s difficult once you defer someone to get them back,” according to Mr McKinney, as many become discouraged from returning."),
              p("The move of many businesses from urban to suburban industrial estates has also affected the number of young donors."),
              p("This was a factor for Shane Bradshaw (32), a financial analyst who donated blood at the IBTS clinic on D’Olier Street in Dublin city, with colleagues from Symantec, yesterday."),
              p("Mr Bradshaw, who has donated blood since his early 20s, said he donated more often before his firm moved from Dublin city centre to the suburbs a few years ago."),
              p("But he is keen to start donating more regularly and has called on younger people to give blood."),
              p("“In the future, you might need blood. If you need it and you want it to be there, and people don’t go and donate, then you could end up having the issue where there’s nothing for you.”"),
              p("The IBTS will hold clinics on Sunday in Wexford town, Celbridge, Co Kildare and Mountbellew, Co Galway in a bid to increase donations. More details can be found at giveblood.ie.")
            )
          )
        ),
        #tab for login ui
        tabItem(tabName = "login",
          uiOutput("uiLogin"),
          uiOutput("uiRegister")
        )
      )
    }
    
    #If user has logged in
    else {
      tabItems(
        #Map showing different data
        tabItem(tabName = "map",
          fluidRow(
            box(title = "Blood Donations in Dublin", width = 8, status = "danger", solidHeader = TRUE,
              radioButtons("radio", p("Select an option:"),
                choices = list("Areas" = 1, "Clinics" = 2,
                "Population by Electoral Divisions"=5, 
                "Population of 18-65 year olds by Electoral Divisions"=6,
                "Population in Good Health by Electoral Divisions (could be donors)"=7,
                "Donors by Electoral Divisions"=8
                ), selected = 1),
              leafletOutput('myMap', height = 480, width = 720)
            )
          )
        ),
        #Plots of current blood stocks 
        tabItem(tabName = "stocks",
          fluidRow(
            box(title = "Blood Stocks", status = "danger", solidHeader = TRUE, width = 4,collapsible = TRUE,
              plotOutput("bloodStocks")
            ),
            box(title = "Irish Blood Group Frequency", status = "danger", solidHeader = TRUE, width = 4,collapsible = TRUE,
              plotOutput("bloodTypeFreq")
            ),
            box(title = "Point Plot of Blood Stocks", width = 4, solidHeader = TRUE, status = "danger",collapsible = TRUE,
                plotOutput("bloodStocks2")
            )
          ),
          fluidRow(
            box(title = "Bar Plot of Blood Stocks Over Time", width = 12, solidHeader = TRUE, status = "danger",collapsible = TRUE,
              plotOutput("bloodStocks3")
            )
          ),
          fluidRow(
            box(title = "Point Plot of Blood Stocks Over Time", width = 12, solidHeader = TRUE, status = "danger",collapsible = TRUE,
                plotOutput("bloodStocks4")
            )
          ),
          fluidRow(
            box(title = "Jitter Plot of Blood Stocks", width = 12, solidHeader = TRUE, status = "danger",collapsible = TRUE,
                plotOutput("bloodStocks5")
            )
          ),
          fluidRow(
            box(title = "Text Plot of Blood Stocks", width = 12, solidHeader = TRUE, status = "danger",collapsible = TRUE,
                plotOutput("bloodStocks6")
            )
          )
        ),
        #Graphics showing donations
        tabItem(tabName = "donation",
          fluidRow(
            box(title = "Donations by Bloodtype sorted by Gender and Day", status = "danger", solidHeader = TRUE, collapsible = TRUE, width = 12,
              plotOutput("donations")
            )
          ),
          fluidRow(
            box(title = "Donations by Day sorted by Gender and Bloodtype", status = "danger", solidHeader = TRUE, collapsible = TRUE, width = 12,
                plotOutput("donations2")
            )
          ),
          fluidRow(
            box(title = "Donations by Gender sorted by Day and Bloodtype", status = "danger", solidHeader = TRUE, collapsible = TRUE, width = 12,
              plotOutput("donations3")
            )
          ),
          fluidRow(
            box(title = "Donations by Gender", status = "danger", solidHeader = TRUE, collapsible = TRUE, width = 4,
              plotOutput("donations4")
            ),
            box(title = "Donations by Bloodtype", status = "danger", solidHeader = TRUE, collapsible = TRUE, width = 4,
              plotOutput("donations5")
            ),
            box(title = "Donations by Day", status = "danger", solidHeader = TRUE, collapsible = TRUE, width = 4,
              plotOutput("donations6")
            )
          ),
          fluidRow(
            box(title = "Donations by Gender sorted by Bloodtype", status = "danger", solidHeader = TRUE, collapsible = TRUE, width = 4,
                plotOutput("donations7")
            ),
            box(title = "Donations by Gender sorted by Day", status = "danger", solidHeader = TRUE, collapsible = TRUE, width = 4,
                plotOutput("donations8")
            ),
            box(title = "Donations by Day sorted by Bloodtype", status = "danger", solidHeader = TRUE, collapsible = TRUE, width = 4,
                plotOutput("donations9")
            )
          )
        ),
        #Logout tab
        tabItem(tabName = "logout",
          h3("Are you sure?"),
          actionButton("logout", "Logout")
        )
      )
    }
  })
  
  #Outputs sidebar menu
  output$menu <- renderMenu({
    #if user logged in
    if (userInput$authenticated == TRUE) {
      sidebarMenu(
        menuItem("Blood Stocks", tabName = "stocks", icon = icon("ambulance")),
        menuItem("Map", tabName = "map", icon = icon("map")),
        menuItem("Donation Data", tabName = "donation", icon = icon("medkit")),
        menuItem("Logout", tabName = "logout", icon = icon("lock"))
      )
    }
    
    #user not logged in
    else{
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("h-square")),
        menuItem("Clinics", tabName = "clinics", icon = icon("map")),
        menuItem("News Reports", tabName = "news", icon = icon("user")),
        menuItem("Login", tabName = "login", icon = icon("lock"))
      )
    }
  })
  
  #Outputs dashboard header
  output$header <- renderMenu({
    #If user logged in
    if (userInput$authenticated == TRUE) {
      dropdownMenu(type = "messages",
        messageItem(
          from = "Dun Laoghaire Clinic",
          message = "Blood stocks are steady this month."
        ),
        messageItem(
          from = "Technical Director",
          message = "How do you like the new system?",
          icon = icon("question"),
          time = "13:45"
        )
      )
    }
    #User not logged in
    else{
      
    }
  })
  #Outputs dashboard header
  output$header2 <- renderMenu({
    #If user logged in
    if (userInput$authenticated == TRUE) {
      dropdownMenu(type = "notifications",
        notificationItem(
          text = "5 new donors today",
          icon("users")
         ),
         notificationItem(
           text = "Donations have arrived at storage facility",
           icon("truck"),
           status = "success"
         ),
         notificationItem(
          text = "Stocks are low in Tallaght",
          icon = icon("exclamation-triangle"),
          status = "warning"
        )
      )
    }
    #If user not logged in
    else{
    }
  })

  # APP'S SERVER CODE
  #Reads in/Creates all data
  dubPoly <- geojsonio::geojson_read("data/dub.json", what = "sp")
  circles <- read.csv(textConnection("
   City,Lat,Long,Pop
   Stillorgan,53.291061,-6.20053,1200
   Lafayette Building, 53.3465209,-6.259002, 1000
  "))
  data = data.frame(BloodType=c("O+", "O-", "A+", "A-", "B+", "B-", "AB+", "AB-"), StockInDays=c(sample(45:50, 1),sample(8:12, 1),sample(23:28, 1),sample(4:7, 1),sample(6:10, 1),sample(1:5, 1),sample(1:5, 1),sample(1:5, 1)))
  data2 = data.frame(BloodType=c("O+", "O-", "A+", "A-", "B+", "B-", "AB+", "AB-"), Frequency=c(47, 8, 26, 5, 9, 2, 2, 1))
  Clinics <- read_excel("data/Clinics.xlsx")
  test <- read_excel("data/test.xlsx")
  Small_Area_Data <- read_excel("data/Small Area Data.xlsx")
  Donation <- read_excel("data/Donation.xlsx")
  Stocks <- read_excel("data/Stocks.xlsx")
  
  
  clinicMap = leaflet() %>% addTiles() %>% setView(-6.26, 53.3, 9) %>% addMarkers(data = Clinics, ~long, ~lat, label = ~name) %>%
    addEasyButton(easyButton(
      icon="fa-crosshairs", title="My Location",
      onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
  output$clinicMap = renderLeaflet(clinicMap)
  map = leaflet() %>% addTiles() %>% setView(-6.26, 53.3, 10)
  finalMap <- reactive ({
    if(input$radio == 1) return(map %>% addPolylines(data = dubPoly, stroke = TRUE, color = "#444444", fillColor = "red", fill=TRUE, label = ~name, highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                                                                                                                         bringToFront = TRUE)) %>%
                                  addEasyButton(easyButton(
                                    icon="fa-crosshairs", title="My Location",
                                    onClick=JS("function(btn, map){ map.locate({setView: true}); }"))))
    if(input$radio == 2) return (map %>% addMarkers(data = Clinics, ~long, ~lat, popup = ~donations, label = ~name) %>%
                                   addEasyButton(easyButton(
                                     icon="fa-crosshairs", title="My Location",
                                     onClick=JS("function(btn, map){ map.locate({setView: true}); }"))))
    if(input$radio == 5) return (leaflet(test) %>% addTiles() %>%
                                   addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1,
                                              radius = ~sqrt(Population) * 30, stroke = TRUE, color = "#444444", fillColor = "red", fill=TRUE, label = ~ElectoralDivision
                                   ) %>%
                                   addEasyButton(easyButton(
                                     icon="fa-crosshairs", title="My Location",
                                     onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
                                 )
    if(input$radio == 6) return (leaflet(test) %>% addTiles() %>%
                                   addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1,
                                              radius = ~sqrt(PopulationAged18to65) * 30, stroke = TRUE, color = "#444444", fillColor = "red", fill=TRUE, label = ~ElectoralDivision
                                   ) %>%
                                   addEasyButton(easyButton(
                                     icon="fa-crosshairs", title="My Location",
                                     onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    )
    if(input$radio == 7) return (leaflet(test) %>% addTiles() %>%
                                   addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1,
                                              radius = ~sqrt(NumberInGoodHealth) * 30, stroke = TRUE, color = "#444444", fillColor = "red", fill=TRUE, label = ~ElectoralDivision
                                   ) %>% 
                                   addEasyButton(easyButton(
                                     icon="fa-crosshairs", title="My Location",
                                     onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    )
    if(input$radio == 8) return (leaflet(test) %>% addTiles() %>%
                                   addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1,
                                              radius = ~sqrt(NumberInGoodHealth) * 5, stroke = TRUE, color = "#444444", fillColor = "red", fill=TRUE, label = ~ElectoralDivision
                                   ) %>% 
                                   addEasyButton(easyButton(
                                     icon="fa-crosshairs", title="My Location",
                                     onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    )
    else return (map)
  })
  
  output$bloodStocks <- renderPlot(
    ggplot(data, aes(x=BloodType, y=StockInDays)) + geom_bar(color="black", fill=rgb(0.9,0.4,0.3,0.7), stat = "identity")
  )
  output$bloodTypeFreq <- renderPlot(
    ggplot(data2, aes(x=BloodType, y=Frequency)) + geom_bar(color="black", fill=rgb(0.9,0.4,0.3,0.7), stat = "identity")
  )
  output$bloodStocks2<- renderPlot(
    ggplot(data, aes(x=BloodType, y=StockInDays)) + geom_point()
  )
  stocks <- ggplot(Stocks, aes(x=BloodType, y=Stock)) + geom_bar(color="black", fill=rgb(0.9,0.4,0.3,0.7), stat = "identity")
  output$bloodStocks3 <- renderPlot(
    stocks + facet_grid(. ~ Date)
  )
  stocks2 <- ggplot(Stocks, aes(x=BloodType, y=Stock)) + geom_point()
  output$bloodStocks4 <- renderPlot(
    stocks2 + facet_grid(. ~ Date)
  )
  stocks3 <- ggplot(Stocks, aes(x=BloodType, y=Stock)) + geom_jitter()
  output$bloodStocks5 <- renderPlot(
    stocks3
  )
  stocks4 <- ggplot(Stocks, aes(x=Date, y=Stock)) + geom_text(aes(label=BloodType))
  output$bloodStocks6 <- renderPlot(
    stocks4
  )
  donation <- ggplot(Donation, aes(x=BloodType)) + geom_bar(color="black", fill=rgb(0.9,0.4,0.3,0.7))
  output$donations <- renderPlot(
    donation + facet_grid(gender ~ day)
  )
  donation2 <- ggplot(Donation, aes(x=day)) + geom_bar(color="black", fill=rgb(0.9,0.4,0.3,0.7))
  output$donations2 <- renderPlot(
    donation2 + facet_grid(gender ~ BloodType)
  )
  donation3 <- ggplot(Donation, aes(x=gender)) + geom_bar(color="black", fill=rgb(0.9,0.4,0.3,0.7))
  output$donations3 <- renderPlot(
    donation3 + facet_grid(day ~ BloodType)
  )
  donation4 <- ggplot(Donation, aes(x=gender)) + geom_bar(color="black", fill=rgb(0.9,0.4,0.3,0.7))
  output$donations4 <- renderPlot(
    donation4
  )
  donation5 <- ggplot(Donation, aes(x=BloodType)) + geom_bar(color="black", fill=rgb(0.9,0.4,0.3,0.7))
  output$donations5 <- renderPlot(
    donation5
  )
  donation6 <- ggplot(Donation, aes(x=day)) + geom_bar(color="black", fill=rgb(0.9,0.4,0.3,0.7))
  output$donations6 <- renderPlot(
    donation6
  )
  donation7 <- ggplot(Donation, aes(BloodType))
  output$donations7 <- renderPlot(
    donation7 + geom_bar(aes(fill = gender))
  )
  donation8 <- ggplot(Donation, aes(day))
  output$donations8 <- renderPlot(
    donation8 + geom_bar(aes(fill = gender))
  )
  donation9 <- ggplot(Donation, aes(BloodType))
  output$donations9 <- renderPlot(
    donation9 + geom_bar(aes(fill = day))
  )
  output$myMap = renderLeaflet(finalMap())







  #Database Code
  con <- dbConnect(RMariaDB::MariaDB(),
                   host="localhost", user="root", password="",
                   dbname="shiny",  port=3306)
  #PASSWORDcode
  # reactive value containing user's authentication status
  userInput <- reactiveValues(authenticated = FALSE, validCredentials = FALSE, status = "")
  adminCode <- "4721"
  #When login button is pressed
  observeEvent(input$login_button, {
    #Send mariadb query
    rs <- dbSendQuery(con, "select * from users where 1;")
    #Fetch results
    credentials <- dbFetch(rs)
    #Assign value for username
    rowUsername <- which(credentials$username == input$username)
    #Assign value for password
    rowPassword <- which(credentials$password == digest(input$password)) # digest() makes md5 hash of password

    # if user name row and password name row are same, credentials are valid
    if (length(rowUsername) == 1 &&
        length(rowPassword) >= 1 &&  # more than one user may have same pw
        (rowUsername %in% rowPassword)) {
      userInput$validCredentials <- TRUE
      dbClearResult(rs)
    }


    # if a user has valid credentials he is authenticated
    if (userInput$validCredentials == TRUE) {
      userInput$authenticated <- TRUE
    } else {
      userInput$authenticated <- FALSE
    }

    # if user is not authenticated, set login status variable for error messages below
    if (userInput$authenticated == FALSE) {
      if (length(rowUsername) > 1) {#More than one username
        userInput$status <- "credentials_data_error"
      } else if (input$username == "" || length(rowUsername) == 0) {#username doesn't exist
        userInput$status <- "bad_user"
      } else if (input$password == "" || length(rowPassword) == 0) {#password doesn't exist
        userInput$status <- "bad_password"
      }
    }
  })
  #Register display, first register button clicked
  observeEvent(input$regButton, {
    output$uiRegister <-renderUI({
      box(title = "Register", status = "danger", solidHeader = TRUE,
        textInput("usernameR", label = h4("User Name", style = "color:black")),
        passwordInput("passwordR", label = h4("Password", style = "color:black")),
        passwordInput("cpassword", label = h4("Confirm Password", style = "color:black")),
        textInput("adminCode", label = h4("Admin Code", style = "color:black")),
        actionButton("regButton2", "Register"),
        uiOutput("pass2")
      )
    })
  })
  #Register code, second register button clicked
  observeEvent(input$regButton2, {
    #mariadb query
    rs4 <-dbSendQuery(con, "select username  from users where 1;")
    #Fetch results
    nameCheck <- dbFetch(rs4)
    #Assign value for existing usernames
    unameExist <- which(nameCheck$username == input$usernameR)
    #Check if username input is blank
    if (input$usernameR =="") {
      userInput$status <- "nameR"
    }
    #Check if username is taken
    else if (length(unameExist) >= 1){
      userInput$status <- "nameTaken"
    }
    #Check if password input is blank
    else if (input$passwordR =="") {
      userInput$status <- "passR"
    }
    #Check if confirm password input is blank
    else if (input$cpassword =="") {
      userInput$status <- "cpass"
    }
    #Check if Admin code input is blank
    else if (input$adminCode =="") {
      userInput$status <- "aCode"
    }
    #Check if Admin code input is blank
    else if (input$adminCode != adminCode) {
      userInput$status <- "aCode2"
    }
    #Check if passwords match
    else if (input$passwordR != input$cpassword){
      userInput$status <- "password"
    }
    else {
      userInput$status <- ""
      #X and y values set to be insered in mariadb
      x <- input$usernameR
      y <- digest(input$passwordR)
      #insert query
      rsReg <- dbSendQuery(con, sprintf("INSERT INTO users (username, password) VALUES ('%s', '%s');", x, y))
      dbClearResult(rsReg)
      dbDisconnect(con)
      userInput$authenticated <- TRUE
    }

  })
  #Logout
  observeEvent(input$logout,{
    userInput$authenticated <- FALSE
  })
  #Login display
  output$uiLogin <- renderUI({
    box(title = "Login", status = "danger", solidHeader = TRUE,
      textInput("username", label = h4("User Name", style = "color:black")),

      passwordInput("password", label = h4("Password", style = "color:black")),

      actionButton("login_button", "Login"),

      actionButton("regButton", "Register"),
      uiOutput("pass")
    )
  })

  #Red error message if bad credentials
  output$pass <- renderUI({
    if (userInput$status == "credentials_data_error") {
      h5(strong("Credentials Data Error - Contact Administrator!", style = "color:red"), align = "center")
    } else if (userInput$status == "bad_user") {
      h5(strong("Username Not Found!", style = "color:red"), align = "center")
    } else if (userInput$status == "bad_password") {
      h5(strong("Incorrect Password!", style = "color:red"), align = "center")
    } else {
      ""
    }
  })
  
  #red error message if bad credentials
  output$pass2 <- renderUI({
    if (userInput$status == "password") {
      h5(strong("Passwords Don't Match!", style = "color:red"), align = "center")
    }else if (userInput$status == "nameTaken") {
      h5(strong("Username Taken!", style = "color:red"), align = "center")
    }else if (userInput$status == "nameR") {
      h5(strong("Enter Username!", style = "color:red"), align = "center")
    }else if (userInput$status == "passR") {
      h5(strong("Enter Password!", style = "color:red"), align = "center")
    }else if (userInput$status == "cpass") {
      h5(strong("Confirm Password!", style = "color:red"), align = "center")
    }else if (userInput$status == "aCode") {
      h5(strong("Enter Admin Code!", style = "color:red"), align = "center")
    }else if (userInput$status == "aCode2") {
      h5(strong("Admin Code Incorrect!", style = "color:red"), align = "center")
    }else {
      ""
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

