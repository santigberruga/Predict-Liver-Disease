###################################
#    Liver Disease Prediction     #
#      Web aplication Plots       #
#                                 #
#  By Santiago Gonzalez Berruga   #  
###################################

# Load packages:
# library(ggplot2)
# library(plotly)
# library(dplyr)
# library(rlang)


# Load the training data used to train the models.
# We use this data as a base in the different plots.
load("data/data_train.RData")


# We define the functions to make the different 
# interactive plots with packages ggplot2 and plotly:

## Pie plots for categorical variables

### Pie plot Gender
plot_gender <- function(dataset) {
  
  # For the categorical variable Gender we use pie charts to observe
  # the number of samples from each case in the data_train set and 
  # we overlap the user's data so that he can relate them.
  
  p <- plot_ly(data_train, labels = ~ Gender, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+value+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste('</br> <i><b>Your samples</b></i>',
                             '</br> Female:', table(dataset$Gender)[1],   # Female
                             '</br> Male:',   table(dataset$Gender)[2]),  # Male
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               showlegend = FALSE)
  p <- p %>% 
    layout(title = "Samples by Gender") %>% 
    config(displaylogo = FALSE, 
           toImageButtonOptions = list(filename= 'Gender Pie chart'))

  return(p)
}

### Pie plot Class
plot_class <- function(dataset) {
  
  # For the categorical variable Class we use pie charts to observe
  # the number of samples from each case in the data_train set and 
  # we overlap the user's data so that he can relate them.
  
  p <- plot_ly(data_train, labels = ~ Class, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+value+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               text = ~paste('</br> <i><b>Your samples</b></i>',
                             '</br> LD:', table(dataset$Class)[1],   # LD
                             '</br> H:',  table(dataset$Class)[2]),  # H
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               showlegend = FALSE)
  p <- p %>% 
    layout(title = "Samples by Class") %>% 
    config(displaylogo = FALSE, 
           toImageButtonOptions = list(filename= 'Class Pie chart'))
  
  return(p)
}



## Distplot (Histogram + Rug)
plot_distplot <- function(dataset, var, var_name) {
  
  # For continuous variables we use density diagrams of the 
  # training data (data_train) versus the distribution of the user samples:
  
  # In the previous study of the data, we saw that some variables 
  # present an asymmetric distribution, so in those cases we use their
  # logarithm to facilitate their visualization.
  
  # First we create the plots with ggplot2
  
  if (var_name %in% c("TB", "DB", "Alkphos", "Sgpt", "Sgot")) { 
    
    p <- ggplot(data = data_train, aes(x = log({{var}}), fill = Class)) +
      geom_density(alpha = 0.5) +
      scale_fill_manual(values = c("gray50", "orangered2"),
                        name="Legend") +
      geom_rug(data = dataset,
               alpha = 0.5,
               show.legend = FALSE,
               aes(color = Class,
                   text = paste0('</br> <i><b>Your data</b></i>',
                                 '</br> Class: ', Class,
                                 '</br> ', var_name, ": ", {{var}},
                                 '</br> ', "log(", var_name,")", ": ", round(log({{var}}),3),
                                 '</br> ID: ', Id))) +
      scale_color_manual(values = c("gray50", "orangered2")) +
      ylab("Density") +
      ggtitle(paste0("Density plots log(", var_name,") by class")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
    
  } else {
    
    p <- ggplot(data = data_train, aes(x = {{var}}, fill = Class)) +
      geom_density(alpha = 0.5) +
      scale_fill_manual(values = c("gray50", "orangered2"),
                        name="Legend") +
      geom_rug(data = dataset,
               alpha = 0.5,
               show.legend = FALSE,
               aes(color = Class,
                   text = paste0('</br> <i><b>Your data</b></i>',
                                 '</br> Class: ', Class,
                                 '</br> ', var_name, ": ", {{var}},
                                 '</br> ID: ', Id))) +  
      scale_color_manual(values = c("gray50", "orangered2")) + # Color plot
      ylab("Density") +
      ggtitle(paste0("Density plots ", var_name," by class")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) 
    
  }
  
  

  # We create the interactive graph with ggplotly from the graph of ggplot2. 
  # In this case we create two graphs with different annotations to obtain 
  # information about the hover in the following steps:
  w <- ggplotly(p)
  k <- ggplotly(p, tooltip=c("text"))
  
  # We edit the hover texts of the graph so that they show the information we want and 
  # configure the legend:
  
  if (table(dataset$Class)[1] != 0 & table(dataset$Class)[2] != 0) {
    
    z <- w %>%
      style(text = k[["x"]][["data"]][[3]][["text"]], traces = 3) %>%
      style(text = k[["x"]][["data"]][[4]][["text"]], traces = 4)
    
    # Legend title
    z[["x"]][["layout"]][["legend"]][["title"]][["text"]] <- "Legend"
    
    # Legend labels
    z[["x"]][["data"]][[1]][["legendgroup"]] <- "LD"
    z[["x"]][["data"]][[1]][["name"]] <- "LD"
    
    z[["x"]][["data"]][[2]][["legendgroup"]] <- "H"
    z[["x"]][["data"]][[2]][["name"]] <- "H"
    
    z[["x"]][["data"]][[3]][["legendgroup"]] <- "LD"
    z[["x"]][["data"]][[3]][["name"]] <- "LD"
    
    z[["x"]][["data"]][[4]][["legendgroup"]] <- "H"
    z[["x"]][["data"]][[4]][["name"]] <- "H"
    
    z[["x"]][["data"]][[3]][["showlegend"]] <- FALSE
    z[["x"]][["data"]][[4]][["showlegend"]] <- FALSE
    
  } else {
    z <- w %>%
      style(text = k[["x"]][["data"]][[3]][["text"]], traces = 3)
    
    # Legend title
    z[["x"]][["layout"]][["legend"]][["title"]][["text"]] <- "Legend"
    
    # Legend labels
    z[["x"]][["data"]][[1]][["legendgroup"]] <- "LD"
    z[["x"]][["data"]][[1]][["name"]] <- "LD"
    
    z[["x"]][["data"]][[2]][["legendgroup"]] <- "H"
    z[["x"]][["data"]][[2]][["name"]] <- "H"
    
    z[["x"]][["data"]][[3]][["legendgroup"]] <- dataset$Class
    z[["x"]][["data"]][[3]][["name"]] <- dataset$Class
    
    z[["x"]][["data"]][[3]][["showlegend"]] <- FALSE
  }
  
  
  # Control modebar
  z <- z %>% config(displaylogo = FALSE, 
                    scrollZoom = FALSE,
                    toImageButtonOptions = list(filename= paste(var_name, "Distplot")),
                    modeBarButtonsToRemove = c("zoomIn2d", 
                                               "zoomOut2d",
                                               "autoScale2d"))
 
  return(z)
  
}




## Boxplot 
plot_boxplot <- function(dataset, var, var_name) {
  
  # For continuous variables we use boxplots of the 
  # training data (data_train) versus the distribution of the user samples:
  
  # In the previous study of the data, we saw that some variables 
  # present an asymmetric distribution, so in those cases we use their
  # logarithm to facilitate their visualization.
  
  # First we create the plots with ggplot2
  
  if (var_name %in% c("TB", "DB", "Alkphos", "Sgpt", "Sgot")) {
    
    p <- ggplot(data = data_train, aes(x = Class, y = log({{var}}), color = Class)) +
      geom_boxplot() +
      geom_jitter(alpha = 0.7, width = 0.15,
                  data = dataset,
                  show.legend = FALSE,
                  aes(fill = Class,
                      text = paste0('</br> <i><b>Your data</b></i>',
                                    '</br> Class: ', Class,
                                    '</br> ', var_name, ": ", {{var}},
                                    '</br> ', "log(", var_name,")", ": ", round(log({{var}}),3),
                                    '</br> ID: ', Id))) +
      scale_fill_manual(values = c("red", "#218c08")) +         # Samples color
      scale_color_manual(values = c("#808080", "#e39932")) +    # Boxplots color
      ggtitle(paste0("Boxplots log(", var_name,") by class")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
    
  } else {
    
    p <- ggplot(data = data_train, aes(x = Class, y = {{var}}, color = Class)) +
      geom_boxplot() +
      geom_jitter(alpha = 0.7, width = 0.15,
                  data = dataset,
                  show.legend = FALSE,
                  aes(fill = Class,
                      text = paste0('</br> <i><b>Your data</b></i>',
                                    '</br> Class: ', Class,
                                    '</br> ', var_name, ": ", {{var}},
                                    '</br> ID: ', Id))) +
      scale_fill_manual(values = c("red", "#218c08")) +      # Samples color
      scale_color_manual(values = c("#808080", "#e39932")) + # Boxplots color
      ggtitle(paste0("Boxplots ", var_name," by class")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
    
  }
  
  # We create the interactive graph with ggplotly from the graph of ggplot2.
  # We edit the hover texts of the graph so that they show the information we want:
  p <- ggplotly(p, tooltip="text")
  
  # Plot colors
  z <- plotly_build(p)
  
  z$x$data <- lapply(z$x$data, FUN = function(x){
    x$marker$outliercolor = x$line$color 
    x$marker$line = x$line$color
    return(x)
  })
  
  # Configure the legend:
  
  if (table(dataset$Class)[1] != 0 & table(dataset$Class)[2] != 0) {
    # Legend title
    z[["x"]][["layout"]][["legend"]][["title"]][["text"]] <- "Legend"
    
    # Legend labels
    z[["x"]][["data"]][[1]][["legendgroup"]] <- "LD"
    z[["x"]][["data"]][[1]][["name"]] <- "LD"
    
    z[["x"]][["data"]][[2]][["legendgroup"]] <- "H"
    z[["x"]][["data"]][[2]][["name"]] <- "H"
    
    z[["x"]][["data"]][[3]][["legendgroup"]] <- "LD"
    z[["x"]][["data"]][[3]][["name"]] <- "LD"
    
    z[["x"]][["data"]][[4]][["legendgroup"]] <- "H"
    z[["x"]][["data"]][[4]][["name"]] <- "H"
    
    z[["x"]][["data"]][[3]][["showlegend"]] <- FALSE
    z[["x"]][["data"]][[4]][["showlegend"]] <- FALSE
    
  } else {
    # Legend title
    z[["x"]][["layout"]][["legend"]][["title"]][["text"]] <- "Legend"
    
    # Legend labels
    z[["x"]][["data"]][[1]][["legendgroup"]] <- "LD"
    z[["x"]][["data"]][[1]][["name"]] <- "LD"
    
    z[["x"]][["data"]][[2]][["legendgroup"]] <- "H"
    z[["x"]][["data"]][[2]][["name"]] <- "H"
    
    z[["x"]][["data"]][[3]][["legendgroup"]] <- dataset$Class
    z[["x"]][["data"]][[3]][["name"]] <- dataset$Class
    
    z[["x"]][["data"]][[3]][["showlegend"]] <- FALSE
    
  }
  
  # Control modebar 
  z <- z %>% config(displaylogo = FALSE, 
                    scrollZoom = FALSE,
                    toImageButtonOptions = list(filename= paste(var_name, "Boxplot")),
                    modeBarButtonsToRemove = c("select2d",
                                               "lasso2d",
                                               "zoomIn2d", 
                                               "zoomOut2d",
                                               "autoScale2d"))
  
  return(z)
  
}



