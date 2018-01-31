library(plotly)
library(ggplot2)
library(rjson)

# some examples, to see more visit https://plot.ly/r/
# full reference https://plot.ly/r/reference/

# pie chart

USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[,c('Categorie', 'X1960')]

p <- plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie') %>%
  layout(title = 'United States Personal Expenditures by Categories in 1960',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p

# scatterplot

d <- diamonds[sample(nrow(diamonds), 1000), ]

p <- plot_ly(
  d, x = ~carat, y = ~price,
  color = ~carat, size = ~carat
)
p

# styled bubble chart

data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/gapminderDataFiveYear.csv")

data_2007 <- data[which(data$year == 2007),]
data_2007 <- data_2007[order(data_2007$continent, data_2007$country),]
slope <- 2.666051223553066e-05
data_2007$size <- sqrt(data_2007$pop * slope)
colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')

p <- plot_ly(data_2007, x = ~gdpPercap, y = ~lifeExp, color = ~continent, size = ~size, colors = colors,
             type = 'scatter', mode = 'markers', sizes = c(min(data_2007$size), max(data_2007$size)),
             marker = list(symbol = 'circle', sizemode = 'diameter',
                           line = list(width = 2, color = '#FFFFFF')),
             text = ~paste('Country:', country, '<br>Life Expectancy:', lifeExp, '<br>GDP:', gdpPercap,
                           '<br>Pop.:', pop)) %>%
  layout(title = 'Life Expectancy v. Per Capita GDP, 2007',
         xaxis = list(title = 'GDP per capita (2000 dollars)',
                      gridcolor = 'rgb(255, 255, 255)',
                      range = c(2.003297660701705, 5.191505530708712),
                      type = 'log',
                      zerolinewidth = 1,
                      ticklen = 5,
                      gridwidth = 2),
         yaxis = list(title = 'Life Expectancy (years)',
                      gridcolor = 'rgb(255, 255, 255)',
                      range = c(36.12621671352166, 91.72921793264332),
                      zerolinewidth = 1,
                      ticklen = 5,
                      gridwith = 2),
         paper_bgcolor = 'rgb(243, 243, 243)',
         plot_bgcolor = 'rgb(243, 243, 243)')

p



# horizontal bar

y <- c('Japan', 'United Kingdom', 'Canada', 'Netherlands', 'United States', 'Belgium', 'Sweden', 'Switzerland')
x_saving <- c(1.3586, 2.2623000000000002, 4.9821999999999997, 6.5096999999999996,
              7.4812000000000003, 7.5133000000000001, 15.2148, 17.520499999999998)
x_net_worth <- c(93453.919999999998, 81666.570000000007, 69889.619999999995, 78381.529999999999,
                 141395.29999999999, 92969.020000000004, 66090.179999999993, 122379.3)
data <- data.frame(y, x_saving, x_net_worth)

p1 <- plot_ly(x = ~x_saving, y = ~reorder(y, x_saving), name = 'Household savings, percentage of household disposable income',
              type = 'bar', orientation = 'h',
              marker = list(color = 'rgba(50, 171, 96, 0.6)',
                            line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
  layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
         xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) %>%
  add_annotations(xref = 'x1', yref = 'y',
                  x = x_saving * 2.1 + 3,  y = y,
                  text = paste(round(x_saving, 2), '%'),
                  font = list(family = 'Arial', size = 12, color = 'rgb(50, 171, 96)'),
                  showarrow = FALSE)

p2 <- plot_ly(x = ~x_net_worth, y = ~reorder(y, x_saving), name = 'Household net worth, Million USD/capita',
              type = 'scatter', mode = 'lines+markers',
              line = list(color = 'rgb(128, 0, 128)')) %>%
  layout(yaxis = list(showgrid = FALSE, showline = TRUE, showticklabels = FALSE,
                      linecolor = 'rgba(102, 102, 102, 0.8)', linewidth = 2,
                      domain = c(0, 0.85)),
         xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,
                      side = 'top', dtick = 25000)) %>%
  add_annotations(xref = 'x2', yref = 'y',
                  x = x_net_worth, y = y,
                  text = paste(x_net_worth, 'M'),
                  font = list(family = 'Arial', size = 12, color = 'rgb(128, 0, 128)'),
                  showarrow = FALSE)

p <- subplot(p1, p2) %>%
  layout(title = 'Household savings & net worth for eight OECD countries',
         legend = list(x = 0.029, y = 1.038,
                       font = list(size = 10)),
         margin = list(l = 100, r = 20, t = 70, b = 70),
         paper_bgcolor = 'rgb(248, 248, 255)',
         plot_bgcolor = 'rgb(248, 248, 255)') %>%
  add_annotations(xref = 'paper', yref = 'paper',
                  x = -0.14, y = -0.15,
                  text = paste('OECD (2015), Household savings (indicator), Household net worth (indicator). doi: 10.1787/cfc6f499-en (Accessed on 05 June 2015)'),
                  font = list(family = 'Arial', size = 10, color = 'rgb(150,150,150)'),
                  showarrow = FALSE)
p

# sankey diagram

json_file <- "https://raw.githubusercontent.com/plotly/plotly.js/master/test/image/mocks/sankey_energy.json"
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

p <- plot_ly(
  type = "sankey",
  domain = c(
    x =  c(0,1),
    y =  c(0,1)
  ),
  orientation = "h",
  valueformat = ".0f",
  valuesuffix = "TWh",
  
  node = list(
    label = json_data$data[[1]]$node$label,
    color = json_data$data[[1]]$node$color,
    pad = 15,
    thickness = 15,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = json_data$data[[1]]$link$source,
    target = json_data$data[[1]]$link$target,
    value =  json_data$data[[1]]$link$value,
    label =  json_data$data[[1]]$link$label
  )
) %>% 
  layout(
    title = "Energy forecast for 2050<br>Source: Department of Energy & Climate Change, Tom Counsell via <a href='https://bost.ocks.org/mike/sankey/'>Mike Bostock</a>",
    font = list(
      size = 10
    ),
    xaxis = list(showgrid = F, zeroline = F),
    yaxis = list(showgrid = F, zeroline = F)
  )

p

# 2d density contour plot

x <- rnorm(200)
y <- rnorm(200)
s <- subplot(
  plot_ly(x = x, type = "histogram"),
  plotly_empty(),
  plot_ly(x = x, y = y, type = "histogram2dcontour"),
  plot_ly(y = y, type = "histogram"),
  nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), margin = 0,
  shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE
)
p <- layout(s, showlegend = FALSE)
p

# scientific

# heatmap

p <- plot_ly(z = volcano, type = "heatmap")
p

# polar area chart

p <- plot_ly(plotly::wind, r = ~r, t = ~t) %>% add_area(color = ~nms)
layout(p, radialaxis = list(ticksuffix = "%"), orientation = 270)
p

# polar scatter plot

p <- plot_ly(
  plotly::mic, r = ~r, t = ~t, color = ~nms, alpha = 0.5, type = "scatter"
)
layout(p, title = "Mic Patterns", orientation = -90)
p

# paralel coordinates plot

df <- read.csv("https://raw.githubusercontent.com/bcdunbar/datasets/master/parcoords_data.csv")

p <- df %>%
  plot_ly(width = 1000, height = 600) %>%
  add_trace(type = 'parcoords',
            line = list(color = ~colorVal,
                        colorscale = 'Jet',
                        showscale = TRUE,
                        reversescale = TRUE,
                        cmin = -4000,
                        cmax = -100),
            dimensions = list(
              list(range = c(~min(blockHeight),~max(blockHeight)),
                   constraintrange = c(100000,150000),
                   label = 'Block Height', values = ~blockHeight),
              list(range = c(~min(blockWidth),~max(blockWidth)),
                   label = 'Block Width', values = ~blockWidth),
              list(tickvals = c(0,0.5,1,2,3),
                   ticktext = c('A','AB','B','Y','Z'),
                   label = 'Cyclinder Material', values = ~cycMaterial),
              list(range = c(-1,4),
                   tickvals = c(0,1,2,3),
                   label = 'Block Material', values = ~blockMaterial),
              list(range = c(~min(totalWeight),~max(totalWeight)),
                   visible = TRUE,
                   label = 'Total Weight', values = ~totalWeight),
              list(range = c(~min(assemblyPW),~max(assemblyPW)),
                   label = 'Assembly Penalty Weight', values = ~assemblyPW),
              list(range = c(~min(HstW),~max(HstW)),
                   label = 'Height st Width', values = ~HstW),
              list(range = c(~min(minHW),~max(minHW)),
                   label = 'Min Height Width', values = ~minHW),
              list(range = c(~min(minWD),~max(minWD)),
                   label = 'Min Width Diameter', values = ~minWD),
              list(range = c(~min(rfBlock),~max(rfBlock)),
                   label = 'RF Block', values = ~rfBlock)
            )
  )

p

# 3d bubble plot

data <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/gapminderDataFiveYear.csv")

data_2007 <- data[which(data$year == 2007),]
data_2007 <- data_2007[order(data_2007$continent, data_2007$country),]
data_2007$size <- data_2007$pop
colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')

p <- plot_ly(data_2007, x = ~gdpPercap, y = ~lifeExp, z = ~pop, color = ~continent, size = ~size, colors = colors,
             marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(5, 150),
             text = ~paste('Country:', country, '<br>Life Expectancy:', lifeExp, '<br>GDP:', gdpPercap,
                           '<br>Pop.:', pop)) %>%
  layout(title = 'Life Expectancy v. Per Capita GDP, 2007',
         scene = list(xaxis = list(title = 'GDP per capita (2000 dollars)',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(2.003297660701705, 5.191505530708712),
                                   type = 'log',
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwidth = 2),
                      yaxis = list(title = 'Life Expectancy (years)',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   range = c(36.12621671352166, 91.72921793264332),
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwith = 2),
                      zaxis = list(title = 'Population',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   type = 'log',
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwith = 2)),
         paper_bgcolor = 'rgb(243, 243, 243)',
         plot_bgcolor = 'rgb(243, 243, 243)')
p

# 3d line plot

count <- 3000

x <- c()
y <- c()
z <- c()
c <- c()

for (i in 1:count) {
  r <- i * (count - i)
  x <- c(x, r * cos(i / 30))
  y <- c(y, r * sin(i / 30))
  z <- c(z, i)
  c <- c(c, i)
}

data <- data.frame(x, y, z, c)

p <- plot_ly(data, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines',
             line = list(width = 4, color = ~c, colorscale = list(c(0,'#BA52ED'), c(1,'#FCB040'))))

p

# 3d surface plot

kd <- with(MASS::geyser, MASS::kde2d(duration, waiting, n = 50))
p <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()
p

# controls

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/globe_contours.csv')
df$id <- seq_len(nrow(df))

library(tidyr)
d <- df %>%
  gather(key, value, -id) %>%
  separate(key, c("l", "line"), "\\.") %>%
  spread(l, value)

geo <- list(
  showland = TRUE,
  showlakes = TRUE,
  showcountries = TRUE,
  showocean = TRUE,
  countrywidth = 0.5,
  landcolor = 'rgb(230, 145, 56)',
  lakecolor = 'rgb(0, 255, 255)',
  oceancolor = 'rgb(0, 255, 255)',
  projection = list(
    type = 'orthographic',
    rotation = list(
      lon = -100,
      lat = 40,
      roll = 0
    )
  ),
  lonaxis = list(
    showgrid = TRUE,
    gridcolor = toRGB("gray40"),
    gridwidth = 0.5
  ),
  lataxis = list(
    showgrid = TRUE,
    gridcolor = toRGB("gray40"),
    gridwidth = 0.5
  )
)

## add custom events

# dropdown
projections = data.frame(type = c("equirectangular", "mercator", "orthographic", "natural earth","kavrayskiy7", 
                                  "miller", "robinson", "eckert4", "azimuthal equal area","azimuthal equidistant", 
                                  "conic equal area", "conic conformal", "conic equidistant", "gnomonic", "stereographic", 
                                  "mollweide", "hammer", "transverse mercator", "albers usa", "winkel tripel"))

all_buttons <- list()
for (i in 1:length(projections[,])) { 
  all_buttons[[i]] <- list(method = "relayout",
                           args = list(list(geo.projection.type = projections$type[i])),
                           label = projections$type[i])
}

# sliders
lon_range = data.frame(seq(-180, 180, 10))
lat_range = data.frame(seq(-90, 90, 10))
colnames(lon_range) <- "x"
colnames(lat_range) <- "x"

all_lat <- list()
for (i in 1:length(lat_range[,])) { 
  all_lat[[i]] <- list(method = "relayout",
                       args = list(list(geo.projection.rotation.lat = lat_range$x[i])),
                       label = lat_range$x[i])
}

all_lon <- list()
for (i in 1:length(lon_range[,])) {  
  all_lon[[i]] <- list(method = "relayout", 
                       args = list(list(geo.projection.rotation.lon = lon_range$x[i])),
                       label = lon_range$x[i]) 
} 

# annotations
annot <- list(x = 0, y=0.8, text = "Projection", yanchor = 'bottom', 
              xref = 'paper', xanchor = 'right',
              showarrow = FALSE)


# original d3-globe with contours
p <- plot_geo(d) %>%
  group_by(line) %>%
  add_lines(x = ~lon, y = ~lat, color = ~line, colors = 'Reds') %>%
  layout(
    showlegend = FALSE, geo = geo
  )

# plot with custom events
p <- p %>%
  layout(annotations = annot,
         updatemenus = list(list(active = 2, x = 0, y = 0.8, 
                                 buttons=all_buttons)),
         sliders = list(
           
           list(
             active = (length(lon_range[,])-1)/2, 
             currentvalue = list(prefix = "Longitude: "), 
             pad = list(t = 20), 
             
             steps = all_lon),
           
           list(
             active = (length(lat_range[,])-1)/2, 
             currentvalue = list(prefix = "Latitude: "), 
             pad = list(t = 100), 
             
             steps = all_lat)))
p
