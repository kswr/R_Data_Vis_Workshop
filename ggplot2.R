# ggplot2

library(ggplot2)
# 

# Data
pl <- ggplot(data = mtcars)
pl
# Aesthetics
pl <- ggplot(data = mtcars, aes(x = mpg,y = hp))
pl
# Geometry (use help to access all geometries)
pl <- pl + geom_point()
pl
# Facets
pl <- pl + facet_grid(cyl~.)
pl
# Statistics
pl2 <- pl + stat_smooth()
pl2
# Coordinates
pl2 <- pl2 + coord_cartesian(xlim = c(15,25))
pl2
# Theme - can be downloaded or created
pl2 <- pl2 + theme_bw()
pl2

# histograms

library(ggplot2movies)

# data and aesthetics
pl <- ggplot(movies,aes(x = rating))
pl
# geometry
pl2 <- pl + geom_histogram(binwidth = 0.1, color = 'red', fill = 'pink',alpha=0.4)
pl2

pl3 <- pl2 + xlab('Movie Rating') + ylab('Count')
pl3

pl3 <- pl3 + ggtitle('Movie ratings count')
pl3

pl10 <- pl + geom_histogram(binwidth = 0.1, aes(fill=..count..)) + xlab('Movie Rating') + ylab('Count') + ggtitle('Movie ratings count')
pl10

# scatterplot

df <- mtcars
# data & aesthetics
pl <- ggplot(df,aes(x=wt,y=mpg))
# geometry
pl + geom_point()
# opacity
pl + geom_point(alpha = 0.5, size = 5)
# size based on continous variable
pl4 <- pl + geom_point(alpha = 0.5, aes(size = hp))
pl4
# use factor for discrete variables
pl5 <- pl + geom_point(alpha = 0.5, aes(size = factor(cyl)))
pl5
# use factor for discrete variables with shape and color
pl6 <- pl + geom_point(alpha = 1, size = 4, aes(shape = factor(cyl), color = factor(cyl)))
pl6
# more about color
pl7 <- pl + geom_point(size=5, color = '#00ffcc')
pl7
pl8 <- pl + geom_point(size=5, aes(color=hp)) + scale_color_gradient(low = 'blue', high = 'red')
pl8 

# regression
pl8 + geom_smooth(method = lm)
pl8 + geom_smooth(method = lm, se = F)
# loess
pl8 + geom_smooth()

# bar plots

df <- mpg
pl <- ggplot(df, aes(x=class)) # class is variable of df 
pl + geom_bar()
# use colors based on other column
pl + geom_bar(color='blue', fill = 'blue')
# to do so use aes() function
pl + geom_bar(aes(fill=drv))
# use position parameter to change displaying of bars (for easier comparing)
pl + geom_bar(aes(fill=drv), position = 'dodge') + ggtitle('dodge')
pl + geom_bar(aes(fill=drv), position = 'fill') + ggtitle('fill')

# boxplots

df <- mtcars
pl <- ggplot(df, aes(x=factor(cyl), y=mpg))
# boxplot
pl + geom_boxplot()
# reversed coordinates
pl + geom_boxplot() + coord_flip()
# separate colors for each cylinder
pl + geom_boxplot(aes(fill = factor(cyl)))
# id + theme
pl + geom_boxplot(aes(fill = factor(cyl))) + theme_dark()

# variable plotting

library(hexbin)
pl <- ggplot(movies, aes(x = year, y = rating))
pl + geom_bin2d(binwidth = c(3,1)) + scale_fill_gradient(high = 'red', low = 'blue')
pl + geom_hex() + scale_fill_gradient(high = 'red', low = 'blue')
pl + geom_density_2d() + scale_fill_gradient(high = 'red', low = 'blue')

# coordinates and faceting

pl <- ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()
pl
pl + coord_cartesian(xlim = c(1,4), ylim = c(15,30))
pl + coord_fixed(ratio = 1/3)

pl

# separate to 4 facets based on number of cylinders each of those cars has

# syntax: facet_grid(y_axis_separate ~ x_axis_separate)

# check documentation for `facet_grid`

pl
pl + facet_grid(. ~ cyl)
pl + facet_grid(drv ~ .)
pl + facet_grid(drv ~ cyl)

# themes

# set theme globally

# theme_set(theme_classic())
# theme_set(theme_minimal())

pl <- ggplot(mtcars,aes(x=wt,y=mpg)) + geom_point()
pl

# set theme manually for one plot

pl + theme_dark()

pl + theme_minimal()

# use ggthemes

library(ggthemes)

pl + theme_excel()

pl + theme_economist()

pl + theme_wsj()

pl + theme_stata()

# plotly

library(ggplot2)
library(data.table)
df <- fread("Economist_Assignment_Data.csv", drop = 1)
head(df)
pl2 <- ggplot(df, aes(x = CPI, y = HDI, color = Region)) + geom_point(shape = 1, size = 5) + geom_smooth(aes(group=1), method = 'lm', formula = y ~log(x), se = F, color = 'red')
pl2
pl <- ggplot(df, aes(x = CPI, y = HDI, color = Region)) + geom_point(shape = 1, size = 5) + geom_smooth(aes(group=1), method = 'lm', formula = y ~log(x), se = F, color = 'red') + geom_text(aes(label=Country))
pl
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")

pl3 <- pl2 + geom_text(aes(label = Country), color = "gray20", 
                       data = subset(df, Country %in% pointsToLabel),check_overlap = TRUE)
pl3
pl4 <- pl3 + theme_bw() + scale_x_continuous(name = "Corruption perception index, 2011 (10 = least corrupt)", limits = c(1,10), breaks = 1:10) + scale_y_continuous(name = "Human Development Index, 2011 (1=best)", limits = c(0.2,1.0), breaks = c(0.2,0.4,0.6,0.8,1.0)) + ggtitle("Corruption and Human Development")
pl4
# theme
pl5 <- pl4 + ggthemes::theme_economist_white()
pl5

library(plotly)
library(shiny)

ggplotly(pl5)
