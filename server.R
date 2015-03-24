## server app for shiny to create plots based on inputs from ui
## data are loaded from .Rdata file
## Load libraries

library(shiny)
library(ggplot2)
library(mapproj)
library(plyr)
library(scales)

## Load data, specify path when needed
load(file = "data/MWTL_Eems_bewerkt.Rdata")
load(file = "data/map.Rdata")
alllocs <- as.character(unique(rws_dat$locoms))

## Define a server for the Shiny app
shinyServer(function(input, output) {
  ## Select data that meet the criteria defined in the UI
  output$timePlot <- renderPlot({
    subdf = subset(rws_dat, rws_dat$variable == input$substance &  #"phosphate" &
                     rws_dat$locoms == input$location &   #  "Groote Gat noord" &
                     rws_dat$year >= input$interval[1] &
                     rws_dat$year <= input$interval[2])
    unit = unique(subdf$ehdcod)
    
    ## Create statistics information needed in the plot annotation
    if(input$analysis == "trend") {
      x = as.numeric(subdf$datetime); y = subdf$wrd    
      regression <- summary(glm(y ~ x + I(cos(2 * pi * x / 31556995)) + I(sin(2 * pi * x / 31556995))))
      slope <- regression$coefficients[2,1]  #slope of regression including season effect
      pvalue <- format(regression$coefficients[2,4], digits = 2)
      intercept <- regression$coefficients[[1]]
    }
    
    ## make parameters for positioning of plot annotation
    minscale <- min(subdf$datetime)
    maxscale <- max(subdf$datetime)
    yposition <- quantile(subdf$wrd, 0.99)
    #     q90 <- function(x) {quantile(x,probs=0.9)}
    
    subdf_year <- ddply(subdf, ~ year + season, summarize, perc = quantile(wrd, probs = 0.9), na.rm = T)
    subdf_year$year <- as.POSIXct(paste(as.character(subdf_year$year), "-07-01 00:00:00", sep = ""))
    subdf_season <- ddply(subdf, ~ season, summarize, perc = quantile(wrd, probs = 0.9))
    
    summer90 <- quantile(subset(subdf, subdf$season == "summer")$wrd,  probs = 0.9)
    winter90 <- quantile(subset(subdf, subdf$season == "winter")$wrd,  probs = 0.9)
    
    ## create plot 
    pl <- ggplot(subdf, aes(datetime, wrd))
    pl <- pl + facet_grid(variable ~ locoms, scales = "free")
    #            pl <- pl + geom_smooth(aes(color = season) , method="lm", size = 2)
    if(input$analysis == "trend") {   # in case trend is choice of analysis
      pl <- pl + geom_point(aes(), color = "seagreen4", alpha = 0.5)
      pl <- pl + geom_smooth(aes(), method='lm', 
                             formula = y ~ x+I(cos(2*pi*x/31622400))+I(sin(2*pi*x/31622400)), 
                             size = 0.7, alpha = 0.3, n = 1000)
      pl <- pl + geom_abline(intercept = intercept, slope = slope , color = "blue", size = 1)
      pl <- pl + annotate("text", label = paste("slope is: ", format(slope*31622400, digits = 2), unit, "per year", "p=", pvalue),
                          x = maxscale - 0.5 * (maxscale - minscale), y = yposition)
    }
    if(input$analysis == "per90") {  # in case 90 percentile is choice of analysis
      pl <- pl + geom_point(aes(color = season), size = 1, alpha = 0.4)
      pl <- pl + geom_crossbar(data = subdf_year, aes(year, perc, color = season), size = 0.75, ymin =F, ymax = F)
      pl <- pl + geom_hline(yintercept = summer90, color = "orange", size = 1, alpha = 0.5)
      pl <- pl + geom_hline(yintercept = winter90, color = "turquoise4", size = 1, alpha = 0.5)
    }
    pl <- pl + theme(text = element_text(size = 16), legend.position = "bottom")
    pl <- pl + xlab("date") + ylab(unit)
    print(pl)
    
  })
  output$boxPlot <- renderPlot({
    
    ## Select data that meet the criteria defined in the UI
    subdf = subset(rws_dat, rws_dat$variable == input$substance &
                     rws_dat$locoms == input$location &
                     rws_dat$wrd < 500 &
                     rws_dat$year >= input$interval[1] &
                     rws_dat$year <= input$interval[2])
    unit = unique(subdf$ehdcod)
    
    ## Create plot
    q <- ggplot(subdf,aes(month,wrd))
    q + geom_violin(outlier.colour = "orange", alpha = 0.5, color = "orange", notch = F, notchwidth = 0.5) +
      geom_smooth(aes(group = 1), method='lm', formula = y ~ I(cos(2*pi*x/12))+I(sin(2*pi*x/12)), size = 0.7, alpha = 0.2) +
      #       geom_point(aes(), position = "jitter", colour = "seagreen4", alpha = 0.3) +
      geom_jitter(position = position_jitter(width = .5), colour = "seagreen4", alpha = 0.3) +
      facet_grid(variable ~ locoms, scales = "free") +
      theme(text = element_text(size = 16)) +
      xlab("month") + ylab(unit)
    
  })
  
  output$map <- renderPlot({
    ## Select data that meet the criteria defined in the UI
    maploc   <-   input$location
    locmapsub  <- subset(locmap, locmap$locomsch == maploc)
    
    alllocmap <- subset(locmap, locmap$locomsch %in% alllocs)
    
    ## Create map
    map <- ggplot() +
      geom_polygon(data = NL_df, aes(x=long, y=lat, group=group),
                   color = "lightgrey", fill = "darkgrey") +
      geom_polygon(data = GER_df, aes(x=long, y=lat, group=group),
                   color = "lightgrey", fill = "darkgrey")
    
    xxlim <- c(5,7.35)   ## selected range suitable for Ems estuary
    yylim <- c(53,54)
    
    map +
      coord_map("ortho", xlim = xxlim, ylim = yylim, orientation=c(55, 10, 0)) +
      geom_point(aes(X_WGS, Y_WGS), data = alllocmap, alpha = 0.4, size = 3, color = "blue") +
      geom_point(aes(X_WGS, Y_WGS), data = locmapsub, size = 15, shape = "*", color = "red") +
      geom_text(aes(X_WGS, Y_WGS, label = locomsch),hjust = 1, vjust = 0, data = locmapsub, size = 4) +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank())
    
  })
  
})
