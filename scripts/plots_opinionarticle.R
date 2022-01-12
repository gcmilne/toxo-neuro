##########################################################################
## Association between T. gondii infection & neuropsychiatric disorders ##
##########################################################################
library(ggrepel)
library(ggpubr)
library(RColorBrewer)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(plotly)
library(akima)

#############
# Set fonts #
#############
if(.Platform$OS.type == "windows") { # set Times New Roman font on Windows
  # library(extrafont)
  # font_import()
  # loadfonts(device = "win")
  windowsFonts(Times=windowsFont("TT Times New Roman")) 
}

###################################################################
## Dot & whisker plot of systematic review odds ratios + 95% CIs ##
###################################################################
# Load data
df <- read.csv("data/neurological.csv")

# Position dodge
dodge <- position_dodge(width=0.6)  

# How many systematic reviews?
length(unique(df$disorder_ref))

# Create custom color scale
df$disorder_level <- df$disorder  #create levels for plot colours
ind <- grep("epilepsy", df$disorder_level, fixed=T)  #find indices of all epilepsy disorders
df$disorder_level[ind] <- "Epilepsy"  #rename

myColors        <- brewer.pal(11, "RdYlBu")
names(myColors) <- sort(unique(df$disorder_level))
colScale        <- scale_colour_manual(name = "plotcols",values = myColors)

# Plot
p <- ggplot(data = df, aes(y = OR, x = disorder, line = group, col = disorder_level)) + 
  coord_flip() +
  geom_point(position=dodge, size=1.2) +
  geom_errorbar(aes(ymax=OR_upper,ymin=OR_lower), width = 0, size=0.5, position = dodge) + 
  geom_hline(yintercept = 1, linetype = "dotted") + 
  scale_y_continuous(breaks = seq(0, 8, 1), limits = c(0, 8), expand = c(0, 0)) + xlab("") + 
  xlab("") + ylab("Odds ratio (95% CI)") + 
  theme_light(base_size = 12, base_line_size = 0, base_family = "Times") + 
  theme(legend.position = "none", panel.grid = element_blank(), axis.ticks = element_blank())

p + colScale  #add colour scale

# Save as PDF
ggsave(filename = "plots/fig2.pdf", width = 6, height = 4, 
  units = "in", family = "Times")

# Save as PNG
ggsave(filename = "plots/fig2.png", width = 6, height = 4, 
       units = "in", family = "Times", dpi=600)


######################################################
## Examining trends in individual studies over time ##
######################################################

# Load data
df2 <- data.frame(read.csv("data/neuro_individual.csv"))

# Range of years studies published in
range(df2$year_published)

# Number of countries in the database
df2$country %>%
  unique %>%
  length()

# Number of WHO regions (broad regions)
df2$who_region %>%
  unique %>%
  length()

# Number of WHO regions (fine scale regions)
df2$who_region_fine %>%
  unique %>%
  length()

Disorder <- df2$disorder  #capitalise for plot legend title

# Create data for arrows showing when meta-analyses published (must be same length as dataset, n=192)
meta <- df$year_published %>% na.omit

arrow_x    <- c(meta[1:15], rep(0, 187))
arrow_y    <- rep(c(1.2, 0), times=c(15, 187))
arrow_yend <- rep(c(0.2, 0), times=c(15, 187))

# Plot trends in publishing (coloured by disorder)
p1 <- ggplot(data=df2, aes(year_published, fill=Disorder, color=Disorder)) + 
  geom_histogram(alpha=1, position = position_stack(reverse=F), binwidth = 1, size=0.02) + 
  xlab("") + ylab("No. published datasets") +
  scale_x_continuous(breaks= seq(1950, 2020, 10)) +
  scale_y_continuous(breaks=seq(0, 30, 5), limits=c(0, 30), expand=c(0, 0)) + 
  geom_segment(x = arrow_x, y = arrow_y,
               xend = arrow_x, yend = arrow_yend,
               lineend = "round", # See available arrow types in example above
               linejoin = "round",
               size = .2, 
               arrow = arrow(length = unit(0.03, "inches")),
               colour = "black") + # Also accepts "red", "blue' etc
  theme_light(base_family="Times", base_size = 12, base_line_size = 0) + 
  theme(legend.key.size = unit(.5, "cm")) + 
  scale_fill_brewer(palette="RdYlBu") + 
  theme(panel.grid = element_blank(), axis.ticks = element_blank(),  legend.text = element_text(color = "grey30"))

p1

# Save as PDF
ggsave(filename = "plots/PubHistDisorders.pdf", width = 6, height = 4, 
       units = "in", family = "Times")

# Save as PNG
ggsave(filename = "plots/PubHistDisorders.png", width = 6, height = 4, 
       units = "in", family = "Times", dpi=600)

# Plot trends in publishing (coloured by WHO Region)
p2 <- ggplot(data=df2, aes(year_published, fill=who_region, color=who_region)) + 
  geom_histogram(alpha=1, position = position_stack(reverse=F), binwidth = 1, size=0.02) + 
  xlab("Year of publication") + ylab("No. published datasets") +
  labs(fill = "WHO Region", colour = "WHO Region") +
  scale_x_continuous(breaks= seq(1950, 2020, 10)) +
  scale_y_continuous(breaks=seq(0, 30, 5), limits=c(0, 30), expand=c(0, 0)) +
  theme_light(base_family="Times", base_size = 12, base_line_size = 0) + 
  theme(legend.key.size = unit(.5, "cm")) + 
  scale_fill_brewer(palette="BrBG") + 
  theme(panel.grid = element_blank(), axis.ticks = element_blank(), legend.text = element_text(color = "grey30"))

p2

# Save as PDF
ggsave(filename = "plots/PubHistWHO.pdf", width = 6, height = 4, 
       units = "in", family = "Times")

# Save as PNG
ggsave(filename = "plots/PubHistWHO.png", width = 6, height = 4, 
       units = "in", family = "Times", dpi=600)

## Multi-panel plot
ggarrange(p1, p2, ncol=1, nrow=2, labels=c("(A)", "(B)"), font.label=list(size=12, family="Times"), hjust = .02)

# Save as PDF
ggsave(filename = "plots/PubHistMultipanel.pdf", width = 6, height = 6, 
       units = "in", family = "Times")

# Save as PNG
ggsave(filename = "plots/PubHistMultipanel.png", width = 6, height = 6, 
       units = "in", family = "Times", dpi=600)


##################
## Box 2 figure ##
##################

# Load data
data <- read.csv("data/or_rr.csv", header=T)

# Example disorders for plotting points
disorder <- c("Schizophrenia", "Suicide attempts", "Traffic accidents", "Addiction disorder", 
              rep("0",length(data$OR)-4))
inc <- c(15.2/100000, 0.5, 0.14, ((60099.6*1000)/7200000000)*100, rep(-1,length(data$OR)-4)) #incidences
OR  <- c(2.71, 1.39, 1.69, 1.91, rep(0,length(data$OR)-4)) #odds ratios
df  <- data.frame(disorder, inc, OR)

# Coordinates for plotting points
x      <- (rep(5, 5))
y      <- c(1.1, 1.62, 2.2, 2.8, 3.45)
coords <- data.frame(x, y)

# Change incidence in unexposed to be per 100,000
data[,1] <- data[,1]*100000

# Select relevant rows for plotting
data <- subset(data, RR==1 | RR==1.5 | RR==2 | RR==2.5 | RR==3| RR==3.5 | RR==3.75 | RR==4)

# Organise data for plotly
x <- data[,1]
y <- data[,"OR"]
z <- data[,"RR"]

a <- interp(x, y, z)

f <- list(
  family = "Times New Roman",
  size = 14,
  color = "black"
)

x1 <- list(
  title = "Incidence (cases per 100,000)",
  titlefont = f, color = "grey", 
  tick0 = 0, dtick = 0,
  ticklen = 0, tickwidth = 0,
  showgrid = FALSE, 
  linecolor = "lightgrey",
  showline=T,
  linewidth = 1,  tickformat = "digit",
  range = c(0,10000)
  )

y1 <- list(
  title = "Odds ratio",
  titlefont = f, color = "grey", 
  tick0 = 0, dtick = 0,
  ticklen = 0, tickwidth = 0,
  showgrid = FALSE, 
  linecolor = "lightgrey",
  showline=T,
  linewidth = 1, 
  range = c(1,3.4))

# Create label position coordinates
coords.x <- c(1450, 1750, 2450, 3700)
coords.y <- c(2.71, 1.39, 1.69, 1.91)
labels   <- head(unique(disorder), -1)
labs     <- data.frame(labels, coords.x, coords.y)

# Create point position coordinates (incidence per 100,000)
point.x <- c(15.2, 9.94*20, 888, 2000)
point.y <- c(2.71, 1.39, 1.69, 1.91)

# Set colours 
cols <- rev(brewer.pal(n = 11, name = "RdYlBu"))

# Plot
p <- plot_ly(x = a$x, y = a$y, z = matrix(a$z, nrow = length(a$y), byrow = TRUE),
        type = "contour", contours = list(
          start = 1,
          end = 3.5,
          size = 0.25),
        colors  = cols[1:8], opacity = 0.8) %>%
  add_trace(x=point.x, y=point.y, z=NULL, size=1, type = 'scatter', mode = 'markers', 
            marker = list(color = cols[11], size = 8, opacity=1)) %>% 
  layout(xaxis = x1, yaxis = y1, font=f) %>% 
  add_annotations(x = labs$coords.x,
                y =labs$coords.y,
                text = labs$labels,
                showarrow = F, 
                arrowhead = 0,
                arrowsize = 0, ax = 5,
                ay = -15, arrowcolor='', 
                font = list(family = 'Times New Roman',
                                      size = 14)) %>% 
  colorbar(title='Risk ratio', thickness=40, outlinecolor="lightgrey", ticks="", showticklabels=T, tick0 = 1,
           dtick = 1, tickfont=list(family = "Times New Roman", color = "grey", opacity=0.2))
p

## Save plotly image as SVG
# Save current viewer settings (e.g. RStudio viewer pane)
op <- options()

# Set viewer to web browser
options(viewer = NULL)

# Use web browser to save image
p %>% htmlwidgets::onRender(
  "function(el, x) {
  var gd = document.getElementById(el.id); 
  Plotly.downloadImage(gd, {format: 'svg', width: 550, height: 450, filename: 'plot'});
  }"
)

# Restore viewer to old setting (i.e.. RStudio)
options(viewer = op$viewer)
