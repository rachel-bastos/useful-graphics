# =======================================================================================
# A compilation of basic and useful graphics
# ======================================================================================

# packages ---------------------------------------------------------------------
if (!require("install.load")) {
  install.packages("install.load")
  library(install.load)
}
install_load("dplyr", "stringr", "reshape", "DescTools", "readxl", "rgdal", "openxlsx", 
             "RColorBrewer", "ggplot2", "gridExtra", "cowplot", "ggrepel")

# graphics design configuration ------------------------------------------------
source('config.R')

# time series
ggplot(data, aes(y = y, x = x))  + 
  geom_line(size = 1.5, color = '#7AE7C7') + 
  geom_point(size = 2.5, color = '#37323E') + 
  geom_text(aes(label=values_formatter(y)), 
            vjust=-.8, hjust=1, size = 6, fontface='bold', check_overlap = T) + 
  scale_x_date(labels = date_formatter_1) +
  scale_y_continuous(labels = values_formatter)+
  themePlot
ggsave('time_series.png', dpi = 300, width = 50, height = 25, units = 'cm')

# lines
ggplot(data, aes(y = y, x = x)) + 
  geom_point(aes(group=group, color=group)) + 
  geom_line(aes(group=group, color=group)) +
  theme_bw() + theme(panel.grid.major = element_blank())
ggsave('lines.png', dpi = 300, width = 50, height = 25, units = 'cm')

# horizontal bars
ggplot(data, aes(y = y, x = x)) +
  geom_col(fill = "#37323E") + 
  coord_flip() + 
  geom_text(aes(label=values_formatter(y)), 
            position = position_stack(vjust = .5), color = 'white', size = 6, fontface='bold') + 
  scale_y_continuous(labels = values_formatter)+
  themePlot
ggsave('hbars.png', dpi = 300, width = 50, height = 25, units = 'cm')

# grouped horizontal bars
ggplot(data, aes(y = y, x = reorder(str_wrap(x, width = 30), y), fill = group)) + 
  facet_grid(~group)+
  geom_col()+
  coord_flip() + 
  scale_fill_manual(values=c('#EC0B43','#7AE7C7'))+
  geom_text(aes(label=percent_formatter(y),group=group), 
           position = position_dodge(width = 1), hjust=-.05,size = 6,fontface='bold') + 
  themePlot + theme(axis.line.y = element_blank())
ggsave('grouped_hbars.png', dpi = 300, width = 50, height = 25, units = 'cm')

# vertical bars
ggplot(data, aes(y = y, x = x)) +
  geom_col(fill = "#37323E") + 
  geom_text(aes(label=values_formatter(y)), 
            position = position_stack(vjust = .5), color = 'white', size = 6, fontface='bold') + 
  scale_y_continuous(labels = values_formatter)+
  themePlot
ggsave('bars.png', dpi = 300, width = 50, height = 25, units = 'cm')

# heatmap
ggplot(data = data, aes(x=x, y=y, fill=value))+
  geom_tile()+ 
  scale_fill_gradientn(colours  = rev(c('#001828','#00334B','#0D4869','#325989','#1576AC','#3688D2')))+
  geom_text(aes(label=values_formatter(value)), size = 6, color = 'white', fontface='bold') + 
  themePlot
ggsave('heatmap.png', dpi = 300, width = 45, height = 40, units = 'cm')

# pyramid
ggplot(data, aes(x = age, y = percent, fill = gender)) + 
  geom_bar(data = subset(data, gender == "M"),
           aes(y = -percent), stat="identity",width=.95) +
  geom_bar(data = subset(data, gender == "F"), 
           aes(y = percent), position="stack", stat="identity",width=.95)+ 
  coord_flip() + 
  geom_text(aes(label=percent_formatter(percent),group=gender), data = subset(pyramid, gender == "M" & percent > 0.02),
            position = position_stack(vjust = -.5), size = 6, fontface='bold') + 
  
  geom_text(aes(label=percent_formatter(percent),group=gender,hjust=.9), data = subset(pyramid, gender == "M" & percent <= 0.02),
            position = position_stack(vjust=-1.1),size = 6, fontface='bold') + 
  
  geom_text(aes(label=percent_formatter(percent),group=gender), data = subset(pyramid, gender == "F" & percent > 0.02),
            color = 'white', position = position_stack(vjust = .5), size = 6, fontface='bold') + 
  
  geom_text(aes(label=percent_formatter(percent),group=gender), data = subset(pyramid, gender == "F" & percent <= 0.02),
            position = position_dodge(width = 1),hjust=-.01, size = 6, fontface='bold') + 
  
  scale_fill_manual(values=c('#58355E','#7AE7C7')) + 
    scale_y_continuous(labels = percent_formatter) +
  theme_bw()+ themePlot +theme(legend.position = 'right')
ggsave('pyramid.png', dpi = 300, width = 50, height = 25, units = 'cm')

# geographic map
ggplot() + 
  geom_polygon(data=shape, aes(x = long, y = lat, group = group), fill = '#37323E', color='#f8f8f8') +
  geom_point(data = bubbles, aes(x=longitude, y=latitude, size = value1, fill = value2), 
             shape = 21, colour = 'white') +
  scale_fill_gradient(low = "#45003C", high = "#F03850") +
  scale_size_area(max_size=15)+
  geom_label_repel(data = labels, 
            aes(longitude, latitude,
                label = paste(local, ": ", 
                              percent_formatter(value1))), 
            color = 'black', size = 4, fontface='bold', alpha = .7) + 
  labs(x='', y='') +
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     legend.position = "none",
                     axis.line = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     strip.background = element_rect(colour="white", fill="#ffffff"))
  
ggsave('map.png', dpi = 300, width = 20, height = 20, units = 'cm')

# scatter plot
ggplot(data, aes(y = y, x = x)) +
  scale_y_continuous(labels = values_formatter)+
  scale_x_continuous(labels = values_formatter)+
  geom_point(color = '#EC0B43')  + 
  themePlot
ggsave('scatterplor.png', dpi = 300, width = 50, height = 25, units = 'cm')

# multiple scatter plot
ggplot(data, aes(y = y, x = x)) + geom_point() + 
  facet_wrap(~group) +
  theme_bw() + theme(panel.grid.major = element_blank())
ggsave('multiple_scatterplot.png', dpi = 300, width = 50, height = 25, units = 'cm')

# pie
ggplot(data, aes(x="", ymax=ymax, ymin=ymin, xmax = 3, xmin = 6, fill=category))+
  geom_rect(colour = "white")+
  coord_polar("y", start=0)+
  scale_fill_manual(values=pal)+ 
  blank_theme +
geom_text(aes(label = percent_formatter(y), x = 5, y = (ymin + ymax)/2,color = color, group=category),
            inherit.aes = F, show.legend = F, size = 6, fontface='bold') +
  scale_colour_manual(values = c('white','black'))
ggsave('pie.png', dpi = 300, width = 50, height = 25, units = 'cm')

# boxplot + histogram
grid.arrange(qplot(data= data, x = value, ylab="", xlab="") +
               geom_histogram(colour = "black", fill = "slategray1") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             qplot(data= data, y = value, x = factor(""), ylab="", xlab="") + 
               geom_boxplot(colour = "black", fill = "grey88") +
               theme_bw() + theme(panel.grid.major = element_blank()),
             ncol = 2)
ggsave('boxplot_histogram.png', dpi = 300, width = 50, height = 25, units = 'cm')

# correlation matrix
png('correlation_matrix.png')
levelplot(abs(correl),
          main = list(label = "Matriz de correlação", cex = 2),
          xlab = "", ylab="",
          col.regions = colorRampPalette(c("blue","yellow","red","darkred")))
dev.off()

# screeplot
qplot(y=as.numeric((sdev)^2),x=factor(x),main="Scree plot",
      xlab = "Componentes",ylab="Variância explicada (%)") + 
  geom_bar(stat="identity", fill = "slategray1") + 
  geom_point() + geom_line(aes(x=x), size=1)+
  theme_bw() + theme(panel.grid.major = element_blank())
ggsave('screeplot_pca.png', dpi = 300, width = 50, height = 25, units = 'cm')
