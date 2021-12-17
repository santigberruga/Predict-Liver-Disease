###################################
#    Liver Disease Prediction     #
#          Report Plots           #
#                                 #
#  By Santiago Gonzalez Berruga   #  
###################################

# Load packages:
# library(ggplot2)
# library(dplyr)
# library(rlang)


# Load the training data used to train the models.
# We use this data as a base in the different plots.
load("data_train.RData")


# We define the functions to make the different 
# plots with package ggplot2

## Pie plots for categorical variables

### Pie plot Gender
plot_gender <- function(dataset) {
  
  # For the categorical variable Gender we use pie charts to observe
  # the number of samples from each case in the data_train set and 
  # we overlap the user's data so that he can relate them.
   
  # Model train data
  data_model <- as.data.frame(table(data_train$Gender))
  colnames(data_model) <- c("Gender", "Samples")
  
  data_model <- 
    data_model %>% 
    arrange(desc(Gender)) %>%
    mutate(prop = round(Samples / sum(Samples),4) *100) %>%
    mutate(ypos = Samples/2 + c(0, cumsum(Samples)[-length(Samples)])) # Compute the position of labels
  
  # Prediction data
  data <- as.data.frame(table(dataset$Gender))
  colnames(data) <- c("Gender", "Samples")
  
  data <- 
    data %>% 
    arrange(desc(Gender)) %>%
    mutate(prop = round(Samples / sum(Samples),4) *100) %>%
    mutate(ypos = Samples/2 + c(0, cumsum(Samples)[-length(Samples)])) # Compute the position of labels
  
  
  p <- ggplot(data_model, aes(x="", y=Samples, fill=Gender)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    scale_fill_manual(values = c("#FF7F0E", "#1F77B4"))+
    theme_void() + # remove background, grid, numeric labels
    theme(legend.position="none") +
    geom_text(aes(y = ypos,
                  label = paste0(Gender,"\n",
                                 "Train: ",Samples," (",prop,"%)","\n",
                                 "Data: ",data$Samples," (",data$prop,"%)")), 
              color = "white", size=3) + 
    ggtitle("Gender Pie plot") +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}

### Pie plot Class
plot_class <- function(dataset) {
  
  ## For the categorical variable Class we use pie charts to observe
  # the number of samples from each case in the data_train set and 
  # we overlap the user's data so that he can relate them.
  
  # Model train data
  data_model <- as.data.frame(table(data_train$Class))
  colnames(data_model) <- c("Class", "Samples")
  
  data_model <- 
    data_model %>% 
    arrange(desc(Class)) %>%
    mutate(prop = round(Samples / sum(Samples),4) *100) %>%
    mutate(ypos = Samples/2 + c(0, cumsum(Samples)[-length(Samples)])) # Compute the position of labels
  
  # Prediction data
  data <- as.data.frame(table(dataset$Class))
  colnames(data) <- c("Class", "Samples")
  
  data <- 
    data %>% 
    arrange(desc(Class)) %>%
    mutate(prop = round(Samples / sum(Samples),4) *100) %>%
    mutate(ypos = Samples/2 + c(0, cumsum(Samples)[-length(Samples)])) # Compute the position of labels
  
  
  p <- ggplot(data_model, aes(x="", y=Samples, fill=Class)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    scale_fill_manual(values = c("#FF7F0E", "#1F77B4"))+
    theme_void() + # remove background, grid, numeric labels
    theme(legend.position="none") +
    geom_text(aes(y = ypos,
                  label = paste0(Class,"\n",
                                 "Train: ",Samples," (",prop,"%)","\n",
                                 "Data: ",data$Samples," (",data$prop,"%)")), 
              color = "white", size=3) + 
    ggtitle("Class Pie plot") +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
  
}



## Distplot (Histogram + Rug)
plot_distplot <- function(dataset, var, var_name) {
  
  # For continuous variables we use density diagrams of the 
  # training data (data_train) versus the distribution of the user samples:
  
  # In the previous study of the data, we saw that some variables 
  # present an asymmetric distribution, so in those cases we use their
  # logarithm to facilitate their visualization
  
  if (var_name %in% c("TB", "DB", "Alkphos", "Sgpt", "Sgot")) { 
    
    p <- ggplot(data = data_train, aes(x = log({{var}}), fill = Class)) +
      geom_density(alpha = 0.5) +
      scale_fill_manual(values = c("gray50", "orangered2"),
                        name="Legend") +
      geom_rug(data = dataset,
               alpha = 0.5,
               show.legend = FALSE,
               aes(color = Class)) +
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
               aes(color = Class)) +  
      scale_color_manual(values = c("gray50", "orangered2")) + # Color plot
      ylab("Density") +
      ggtitle(paste0("Density plots ", var_name," by class")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5)) 
    
  }
  
  return(p)
  
}




## Boxplot
plot_boxplot <- function(dataset, var, var_name) {
  
  # For continuous variables we use boxplots of the 
  # training data (data_train) versus the distribution of the user samples:
  
  # In the previous study of the data, we saw that some variables 
  # present an asymmetric distribution, so in those cases we use their
  # logarithm to facilitate their visualization.

  if (var_name %in% c("TB", "DB", "Alkphos", "Sgpt", "Sgot")) {
    
    p <- ggplot(data = data_train, aes(x = Class, y = log({{var}}), color = Class)) +
      geom_boxplot() +
      geom_jitter(alpha = 0.7, width = 0.15,
                  data = dataset,
                  show.legend = TRUE,
                  shape=21,
                  aes(fill = Class)) +
      scale_fill_manual(values = c("blue", "#218c08")) +        # Samples color
      scale_color_manual(values = c("gray50", "orangered2")) +  # Boxplots color
      ggtitle(paste0("Boxplots log(", var_name,") by class")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
    
  } else {
    
    p <- ggplot(data = data_train, aes(x = Class, y = {{var}}, color = Class)) +
      geom_boxplot() +
      geom_jitter(alpha = 0.7, width = 0.15,
                  data = dataset,
                  show.legend = TRUE,
                  shape=21,
                  aes(fill = Class)) +
      scale_fill_manual(values = c("blue", "#218c08")) +        # Samples color
      scale_color_manual(values = c("gray50", "orangered2")) +  # Boxplots color
      ggtitle(paste0("Boxplots ", var_name," by class")) +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5))
    
  }
  
  return(p)
  
}


