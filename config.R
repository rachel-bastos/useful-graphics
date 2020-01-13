# --------------- functions
date_formatter_1 <- function(x) format(x, '%d/%m')
date_formatter_2 <- function(x) format(x, '%m/%Y')
values_formatter <- function(x) format(round(x,2), big.mark=".",decimal.mark = ',', scientific = FALSE)
percent_formatter <- function(x) paste0(format(round(100*x,2), decimal.mark = ','), '%')
th_formatter <- function(x) paste0(x,'ª')
firstup <- function(x) {substr(x, 1, 1) <- toupper(substr(x, 1, 1)); return(x)}
def_week <- function(x) {
  breaks <- seq(min(x, na.rm=T),max(x, na.rm=T),by='week')
  d <- ifelse(x < breaks[2], 1,
              ifelse(x < breaks [3], 2,
                     ifelse(x < breaks[4], 3,
                            ifelse(x < breaks[5], 4, 5))))
  return(d)
}
def_weekday <- function(x) ifelse(x %in% c('Domingo','Sábado'), x, paste0(x,'-feira'))

# --------------- inputs
weekDays <- c('Domingo','Segunda-feira','Terça-feira','Quarta-feira','Quinta-feira','Sexta-feira','Sábado')
themePlot <- theme_bw() + theme(panel.grid.major = element_blank(), 
                                panel.grid.minor.x=element_blank(),
                                panel.border = element_blank(),
                                legend.position="none",
                                legend.text = element_text(colour="#000000", size=25),
                                legend.title = element_text(colour="#000000", size=25),
                                axis.line = element_line(colour = "black"),
                                axis.text = element_text(colour="#000000", size=25),
                                axis.title = element_text(colour="#000000", size=25),
                                axis.ticks = element_blank(),
                                strip.text = element_text(colour="#000000", size=25),
                                strip.background = element_rect(colour="white", fill="#ffffff"))

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    axis.text.x=element_blank(),
    legend.text = element_text(colour="#000000", size=25),
    legend.title = element_text(colour="#000000", size=25),
    plot.title=element_text(size=14, face="bold")
  )
pal <- c('#EC0B43','#58355E','#7AE7C7','#37323E','#D6FFB7','#FFF689')
