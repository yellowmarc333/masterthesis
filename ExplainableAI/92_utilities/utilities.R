


####---------------------Aux-----------------------####

# define plot theme and export params
setPlotParams <- function(){
  create_theme <- function() {
    wdl_theme <- theme_minimal() +
      theme(panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey"),
            legend.position = "bottom") +
      theme(plot.title = element_text(hjust = 0.5, colour= "#4f4f4f", size = 16),
            plot.subtitle = element_text(hjust = 0.5, colour= "#4f4f4f", size = 14),
            axis.title.x = element_text(colour = "#4f4f4f", size = 14),
            axis.title.y = element_text(colour = "#4f4f4f", size = 14)
            # , legend.text = element_text(size = )
      ) +
      theme(text = element_text(family="Executive", size = 14))
    return(wdl_theme)
  }
  wdl_theme <<- create_theme()
  
  
  assign(x = "plotWidthWide" , 30, envir = .GlobalEnv)
  assign("plotWidthMedium",  25, envir = .GlobalEnv)
  assign("plotWidthNarrow",  15, envir = .GlobalEnv)
  assign("plotHeight" , 16, envir = .GlobalEnv)
  assign("diagColors" ,c("FALSE" = "deepskyblue",
                       "TRUE" = "#ED6C42"), envir = .GlobalEnv)
  textSizeMain <- 16

  textSizeSub <- 14
  
  assign("customTheme",  
    theme_minimal() +
    theme(
      # panel.grid.major = element_blank(), 
      # panel.grid.minor = element_blank()
      # ,panel.background = element_rect(fill = bgColor,colour = NA)
      # # ,legend.background = element_rect(fill = bgColor,colour = NA)
      # ,legend.key = element_rect(fill = bgColor,colour = NA)
      # , legend.text = element_text(colour = "white")
      # # ,plot.background = element_rect(fill = bgColor,colour = NA)
      # ,strip.background = element_rect(fill = bgContrast, colour = NA)
      # , 
      title = element_text(colour = "black", size = textSizeMain)
      # , plot.title = element_text(hjust = 0.5)
      # , plot.subtitle = element_text(hjust = 0.5, size = textSizeSub)
      
      # , axis.text = element_text(colour = textColorMain)
      # , strip.text = element_text(colour = textColorMain)
      # ,plot.margin = plotMargin
    ), envir = .GlobalEnv )
  
  assign("outputPath",  "03_computedData/06_evaluatedData/", envir = .GlobalEnv)
}

