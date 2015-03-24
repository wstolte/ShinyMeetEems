library(shiny)

# Load data file pre-processed with "voorbewerking MWTL.R"
# This is a selection of data from MWTL
# Only in the vicinity of Eems estuary
# Only surface samples (0 - 4 m), extinction and Secchi depth

load(file = "data/MWTL_Eems_bewerkt.Rdata")

# Define the overall UI

shinyUI(
  fluidPage(
    titlePanel("Metingen waterkwaliteit Eems-Dollard"),
    sidebarLayout(
      # Define the sidebar with one input
      sidebarPanel(width = 3,
                   img(src = "logo.png", width = "175px"),
                   selectInput("location", "Location:",
                               choices = levels(rws_dat$locoms), selected = "Groote Gat noord"),
                   #                    helpText("select location"),
                   selectInput("substance", "Substance:",
                               choices = levels(as.factor(rws_dat$variable)), selected = "gesuspendeerd materiaal"),
                   #                    helpText("select substance"),
                   sliderInput("interval", "Interval", min = 1970, max = 2014, c(1970, 2014), step = 1, sep = ""),
                   #                    helpText("select time interval"),
#                    radioButtons("fit", "Fit type",
#                                 c("Periodic" = "periodic",
#                                   "Summer" = "summer",
#                                   "Winter" = "winter")),
                   radioButtons("analysis", "Analysis",
                                c("Trend" = "trend",
                                  "90 percentile" = "per90")),
plotOutput("map", width = "300px", height = "250px")
      ),
      # Create a spot for the plots
      mainPanel(
        fluidRow(
          print("Onderstaande grafieken zijn samengesteld uit metingen gedaan door Rijkswaterstaat binnen het MWTL programma. De site is bedoeld als demonstratie, en conclusies aan de hand van de gepresenteerde data en analyse zijn voor rekening van de gebruiker. "),
          print("Ruwe data zijn te downloaden op http://live.waterbase.nl")
          ),
        fluidRow(
          column(width = 4,
                 plotOutput("timePlot", width = "450px", height = "350px")
#figuurtje invoegen modelled SPM vs fPP of Chla??
                 ),
          column(width = 4, offset = 2,
                 plotOutput("boxPlot", width = "450px", height = "350px")#,
#                  plotOutput("map", width = "400px", height = "250px")
          )
        ),
        fluidRow(
          column(width = 4,
                 print("Trend: Tijdserie met periodiciteit\n"),
                 print("y = a + b*x + c*cos(2*pi*x)+d*sin(2*pi*x)\n"),
                 print("90-percentile: 90 percentile of observations per summer/winter per jaar en voor de geselecteerde periode")
                 ),
          column(width = 4, offset = 2,
                 print("Boxplot van periodiciteit voor geselecteerde periode")
                 )
          )
      )
    )
  )
)
