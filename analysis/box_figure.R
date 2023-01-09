# box_figure.R
## code to create a figure for the different types of phi box
## three-panel figure with (a) phi psii, (b) phi etr, and (c) phi co2 temperature responses
## under three conditions: (1) no phi psii temperature response, 
## (2) yes phi psii temperature response,
## (3) differential electron partitioning for the phi co2 response

### functions
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

### libraries
library(ggplot2)



### define some parameters
lowEP <- 4 # low value of electron partitioning (e needed to fix mol CO2)
highEP <- 8 # high value of electron partitioning (e needed to fix mol CO2)
light <- 800 # par (umol m-2 s-1)
abs <- 0.85 # light absorbance
PP <- 0.5 # photon partitioning between PSII and PSI

### make a data frame
t_sequence <- seq(0, 50, 1)

b_tresp <- 0.0474
c_tresp <- 0.000859

### get some y-axis scaling values
topt_posch <- b_tresp / (2 * c_tresp)
model_avg_phi_psii <- 0.75

a_tresp <- model_avg_phi_psii - ((b_tresp * topt_posch) - (c_tresp * topt_posch^2)) # a, given the assumption that model average phi psii is at the temperature optimum estimated from the data

phi_psii_noT <- a_tresp + 
  (b_tresp * 25) - 
  (c_tresp * 25 * 25) # change based on Brad's results
phi_etr_noT <- phi_psii_noT * abs * PP
phi_co2_noT_lowEP <- phi_etr_noT / lowEP
phi_co2_noT_highEP <- phi_etr_noT / highEP

phi_psii_yesT <- a_tresp + 
  (b_tresp * t_sequence) - 
  (c_tresp * t_sequence * t_sequence) # change based on Brad's results
phi_etr_yesT <- phi_psii_yesT * abs * PP
phi_co2_yesT_lowEP <- phi_etr_yesT / lowEP
phi_co2_yesT_highEP <- phi_etr_yesT / highEP

phi_df <- as.data.frame(cbind(t_sequence, 
                              phi_psii_noT, phi_etr_noT, phi_co2_noT_lowEP, phi_co2_noT_highEP,
                              phi_psii_yesT, phi_etr_yesT, phi_co2_yesT_lowEP, phi_co2_yesT_highEP))

### make some figures

phi_psii_plot <- ggplot(data = phi_df, aes(y = phi_psii_noT, x = t_sequence)) +
  theme(legend.position = 'none',
        legend.text = element_text(size = rel(2)),
        axis.title.y=element_text(size=rel(2.5), colour = 'black'),
        axis.title.x=element_text(size=rel(2.5), colour = 'black'),
        axis.text.x=element_text(size=rel(2.5), colour = 'black'),
        axis.text.y=element_text(size=rel(2.5), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "white"),
        plot.tag = element_text(size = 30)) +
  geom_line(linewidth = 2, color = 'black') +
  geom_line(linewidth = 2, color = 'red', aes(y = phi_psii_yesT)) +
  ylim(c(0, 0.8)) + 
  ylab(expression('Φ'['PSII'] * ' (mol/mol)')) +
  xlab('Temperature (°C)') +
  labs(tag = "(a)")

phi_etr_plot <- ggplot(data = phi_df, aes(y = phi_etr_noT, x = t_sequence)) +
  theme(legend.position = 'none',
        legend.text = element_text(size = rel(2)),
        axis.title.y=element_text(size=rel(2.5), colour = 'black'),
        axis.title.x=element_text(size=rel(2.5), colour = 'black'),
        axis.text.x=element_text(size=rel(2.5), colour = 'black'),
        axis.text.y=element_text(size=rel(2.5), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "white"),
        plot.tag = element_text(size = 30)) +
  geom_line(linewidth = 2, color = 'black') +
  geom_line(linewidth = 2, color = 'red', aes(y = phi_etr_yesT)) +
  ylim(c(0, 0.4)) + 
  ylab(expression('Φ'['ETR'] * ' (mol/mol)')) +
  xlab('Temperature (°C)') +
  labs(tag = "(b)")

phi_co2_plot <- ggplot(data = phi_df, aes(y = phi_co2_noT_lowEP, x = t_sequence)) +
  theme(legend.position = 'none',
        legend.text = element_text(size = rel(2)),
        axis.title.y=element_text(size=rel(2.5), colour = 'black'),
        axis.title.x=element_text(size=rel(2.5), colour = 'black'),
        axis.text.x=element_text(size=rel(2.5), colour = 'black'),
        axis.text.y=element_text(size=rel(2.5), colour = 'black'),
        panel.background = element_rect(fill = 'white', colour = 'black'),
        panel.grid.major = element_line(colour = "white"),
        plot.tag = element_text(size = 30)) +
  geom_line(linewidth = 2, color = 'black') +
  geom_line(linewidth = 2, color = 'red', aes(y = phi_co2_yesT_lowEP)) +
  geom_line(linewidth = 2, color = 'black', lty = 2, aes(y = phi_co2_noT_highEP)) +
  geom_line(linewidth = 2, color = 'red', lty = 2, aes(y = phi_co2_yesT_highEP)) +
  ylim(c(0, 0.2)) + 
  ylab(expression('Φ'['CO2'] * ' (mol/mol)')) +
  xlab('Temperature (°C)') +
  labs(tag = "(c)")

jpeg(filename = "plots/phi_plot.jpeg", 
     width = 21, height = 7, units = 'in', res = 600)
multiplot(phi_psii_plot, phi_etr_plot, phi_co2_plot, cols = 3)
dev.off()

