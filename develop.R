library(xts)
library(dygraphs)
library(lubridate)
library(rCharts)
library(dplyr)
library(ggplot2)


# graph_data <- xts(x = c(1,2,3,4), order.by = lubridate::ymd(c("2015-01-01", "2015-02-01", "2015-03-01", "2015-04-01")))
# names(graph_data) <- "value"
# 
# 
# lungDeaths <- cbind(mdeaths, fdeaths)
# class(lungDeaths)
# dygraph(lungDeaths)
# 
# 
# dygraph(lungDeaths) %>% dyRangeSelector()

###############################################################################


RANKING_NEED_ENERGY <- read.csv("C:/RICARDO-AEA/dygraph/Ranked_NEED_cleaned_2012.csv")
RANKING_NEED_ENERGY <- RANKING_NEED_ENERGY %>% 
  select(energy,
         energy_floor_area,
         energy_IMD_ENG,
         energy_EE_BAND,
         octile_energy,
         octile_area)


########################################################################

library(rCharts)
library(ggplot2)
library(reshape2)
library(devtools)
#pkgs <- c("slidify", "slidifyLibraries", "rCharts")
#devtools::install_github(pkgs, "ramnathv", ref = "dev")


## Simulate some data
# 
# ## 3 Factor Variables
# FacVar1 = as.factor(rep(c("level1", "level2"), 25))
# FacVar2 = as.factor(rep(c("levelA", "levelB", "levelC"), 17)[-51])
# FacVar3 = as.factor(rep(c("levelI", "levelII", "levelIII", "levelIV"), 13)[-c(51:52)])
# 
# ## 4 Numeric Vars
# set.seed(123)
# NumVar1 = round(rnorm(n = 50, mean = 1000, sd = 50), digits = 2)  ## Normal distribution
# set.seed(123)
# NumVar2 = round(runif(n = 50, min = 500, max = 1500), digits = 2)  ## Uniform distribution
# set.seed(123)
# NumVar3 = round(rexp(n = 50, rate = 0.001))  ## Exponential distribution
# NumVar4 = 2001:2050
# 
# simData = data.frame(FacVar1, FacVar2, FacVar3, NumVar1, NumVar2, NumVar3, NumVar4)
# 
# 
# plot(simData$NumVar1, type = "o")  ## Index plot
# hist(simData$NumVar1)  ## histogram
# plot(density(simData$NumVar1))  ## Kernel density plot
# 
# ggplot(simData, aes(y = NumVar1, x = 1:nrow(simData), group = "NumVar1")) + 
#   geom_point() + geom_line() + xlab("")  ## Index plot 
# 
# ggplot(simData, aes(x = NumVar1)) + geom_histogram()  ## histogram
# 
# ggplot(simData, aes(x = NumVar1)) + geom_density()  ## Kernel density plot
# 
# simData$index = 1:nrow(simData)
# hPlot(x = "index", y = "NumVar1", data = simData, type = "line")
# # h1$publish('h1',host='gist') h1$save('h1.html',cdn=TRUE)
# 
# rPlot(x = "bin(NumVar1,10)", y = "count(NumVar1)", data = simData, type = "bar")
# # rp1$publish('rp1',host='gist') rp1$save('rp1.html',cdn=TRUE)
# 
# dense = density(simData$NumVar1)
# dense = data.frame(dense$x, dense$y)
# nPlot(x = "dense.x", y = "dense.y", data = dense, type = "lineChart")
# # n1$save('n1.html',cdn=TRUE) n1$publish('n1',host='gist')
# dygraph(dense) %>% dyRangeSelector()

##############################################################

m <- ggplot(RANKING_NEED_ENERGY, aes(x = energy))
 # +  geom_density(alpha = 0.5, fill = "red")


# p1 <- nPlot(
#   density ~ x
#   ,data = ggplot_build(m + geom_density())$data[[1]][c("x","density")]
#   ,type = "lineChart"
# )
# print(p1)


dygraph(density ~ x,
        data= ggplot_build(m + geom_density())$data[[1]][c("x","density")]) %>% dyRangeSelector(c(10000,50000))

#################################################################

sub <- subset(RANKING_NEED_ENERGY[1],
              energy == 2000)
sub <- sub$energy
sub <- as.numeric(sub)
class(sub)
dense_sub = density(sub)

k = row(RANKING_NEED_ENERGY[1])
N = max(k)
dense_ENE = density(RANKING_NEED_ENERGY$energy)
# Convert to counts

dense_ENE$y = N/sum(dense_ENE$y) * dense_ENE$y

dense_ENE = data.frame(dense_ENE$x, dense_ENE$y)
nPlot(x = "dense_ENE.x", y = "dense_ENE.y", data = dense_ENE, type = "lineChart")

dygraph(dense_ENE) %>% dyRangeSelector()
