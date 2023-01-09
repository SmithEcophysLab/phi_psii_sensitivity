# model_figure.R
## code to run the photosynthesis model comparison and make a figure

## multiplot code
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


## libraries
# install.packages('R.utils')
library(R.utils)
library(ggplot2)
library(ggplot2)
library(colorRamps)
library(RColorBrewer)

## load up model and functions
source('../model_code/photosynthesis_model.R')
sourceDirectory('../model_code/functions')

## run model
posch_data <- photosynthesis_model(phi_psii_tresp = "yes", 
                                   # a_tresp = 0.1,
                                   par = 400,
                                   temperature_c = seq(0, 50, 1))
head(posch_data)
plot(phi_psii ~ temperature_c, data = posch_data)
plot(a ~ temperature_c, data = posch_data)
lines(aj ~ temperature_c, data = posch_data, col = 'blue')
lines(ac ~ temperature_c, data = posch_data, col = 'red')

bethy_data <- photosynthesis_model(phi_psii_tresp = "no", 
                                   phi_psii = 0.66,
                                   par = 400,
                                   temperature_c = seq(0, 50, 1))
head(bethy_data)
plot(phi_psii ~ temperature_c, data = bethy_data)
plot(a ~ temperature_c, data = bethy_data)
lines(aj ~ temperature_c, data = bethy_data, col = 'blue')
lines(ac ~ temperature_c, data = bethy_data, col = 'red')

jules_data <- photosynthesis_model(phi_psii_tresp = "no", 
                                   phi_psii = 0.75,
                                   par = 400,
                                   temperature_c = seq(0, 50, 1))
head(jules_data)
plot(phi_psii ~ temperature_c, data = jules_data)
plot(a ~ temperature_c, data = jules_data)
lines(aj ~ temperature_c, data = jules_data, col = 'blue')
lines(ac ~ temperature_c, data = jules_data, col = 'red')

clm45_data <- photosynthesis_model(phi_psii_tresp = "no", 
                                   phi_psii = 0.85,
                                   par = 400,
                                   temperature_c = seq(0, 50, 1))
head(clm45_data)
plot(phi_psii ~ temperature_c, data = clm45_data)
plot(a ~ temperature_c, data = clm45_data)
lines(aj ~ temperature_c, data = clm45_data, col = 'blue')
lines(ac ~ temperature_c, data = clm45_data, col = 'red')

jsbach_data <- photosynthesis_model(phi_psii_tresp = "no", 
                                   phi_psii = 0.66,
                                   par = 400,
                                   temperature_c = seq(0, 50, 1))
head(jsbach_data)
plot(phi_psii ~ temperature_c, data = jsbach_data)
plot(a ~ temperature_c, data = jsbach_data)
lines(aj ~ temperature_c, data = jsbach_data, col = 'blue')
lines(ac ~ temperature_c, data = jsbach_data, col = 'red')

ed2_data <- photosynthesis_model(phi_psii_tresp = "no", 
                                    phi_psii = 0.75,
                                    par = 400,
                                    temperature_c = seq(0, 50, 1))
head(ed2_data)
plot(phi_psii ~ temperature_c, data = ed2_data)
plot(a ~ temperature_c, data = ed2_data)
lines(aj ~ temperature_c, data = ed2_data, col = 'blue')
lines(ac ~ temperature_c, data = ed2_data, col = 'red')

gday_data <- photosynthesis_model(phi_psii_tresp = "no", 
                                 phi_psii = 0.61,
                                 par = 400,
                                 temperature_c = seq(0, 50, 1))
head(gday_data)
plot(phi_psii ~ temperature_c, data = gday_data)
plot(a ~ temperature_c, data = gday_data)
lines(aj ~ temperature_c, data = gday_data, col = 'blue')
lines(ac ~ temperature_c, data = gday_data, col = 'red')

ocn_data <- photosynthesis_model(phi_psii_tresp = "no", 
                                  phi_psii = 0.75,
                                  par = 400,
                                  temperature_c = seq(0, 50, 1))
head(ocn_data)
plot(phi_psii ~ temperature_c, data = ocn_data)
plot(a ~ temperature_c, data = ocn_data)
lines(aj ~ temperature_c, data = ocn_data, col = 'blue')
lines(ac ~ temperature_c, data = ocn_data, col = 'red')

## make figures
blue_palette <- brewer.pal(10, 'Blues')

# phi_psii_plot <- ggplot(data = posch_data, aes(x = temperature_c, y = phi_psii)) +
#   theme(legend.position = 'none',
#         legend.text = element_text(linewidth = rel(2)),
#         axis.title.y=element_text(linewidth=rel(2.5), colour = 'black'),
#         axis.title.x=element_text(linewidth=rel(2.5), colour = 'black'),
#         axis.text.x=element_text(linewidth=rel(2.5), colour = 'black'),
#         axis.text.y=element_text(linewidth=rel(2.5), colour = 'black'),
#         panel.background = element_rect(fill = 'white', colour = 'black'),
#         panel.grid.major = element_line(colour = "grey")) +
#   geom_line(linewidth = 2, color = 'black') +
#   geom_line(linewidth = 1, color = blue_palette[4], data = bethy_data, lty = 3) +
#   geom_line(linewidth = 1, color = blue_palette[5], data = jules_data, lty = 3) +
#   geom_line(linewidth = 1, color = blue_palette[6], data = clm45_data, lty = 3) +
#   geom_line(linewidth = 1, color = blue_palette[7], data = jsbach_data, lty = 3) +
#   geom_line(linewidth = 1, color = blue_palette[8], data = ed2_data, lty = 3) +
#   geom_line(linewidth = 1, color = blue_palette[9], data = gday_data, lty = 3) +
#   geom_line(linewidth = 1, color = blue_palette[10], data = ocn_data, lty = 3) +
#   ylim(c(0, 1)) + 
#   ylab(expression('Φ'['PSII'] * ' (mol/mol)')) +
#   xlab('Temperature (°C)')

phi_psii_plot <- ggplot(data = posch_data, aes(x = temperature_c, y = phi_psii)) +
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
  geom_line(linewidth = 1.5, color = 'blue', data = bethy_data, lty = 1, alpha = 0.5) +
  geom_line(linewidth = 1.5, color = 'purple', data = jules_data, lty = 1, alpha = 0.5) +
  geom_line(linewidth = 1.5, color = 'red', data = clm45_data, lty = 1, alpha = 0.5) +
  geom_line(linewidth = 1.5, color = 'brown', data = gday_data, lty = 1, alpha = 0.5) +
  ylim(c(0, 1)) + 
  ylab(expression('Φ'['PSII'] * ' (mol/mol)')) +
  xlab('Temperature (°C)') +
  labs(tag = "(a)")

aj_plot <- ggplot(data = posch_data, aes(x = temperature_c, y = aj)) +
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
  geom_line(linewidth = 1.5, color = 'blue', data = bethy_data, lty = 1, alpha = 0.5) +
  geom_line(linewidth = 1.5, color = 'purple', data = jules_data, lty = 1, alpha = 0.5) +
  geom_line(linewidth = 1.5, color = 'red', data = clm45_data, lty = 1, alpha = 0.5) +
  geom_line(linewidth = 1.5, color = 'brown', data = gday_data, lty = 1, alpha = 0.5) +
  ylim(c(0, 20)) + 
  ylab(expression('A'['j'] * ' (µmol CO'[2] * ' m' ^ '-2' * ' s' ^ '-1' * ')')) +
  xlab('Temperature (°C)')+
  labs(tag = "(b)")

jpeg(filename = "plots/model_plot_v2.jpeg", 
     width = 13, height = 7, units = 'in', res = 600)
multiplot(phi_psii_plot, aj_plot, cols = 2)
dev.off()



