# Library needed
library(igraph)
library(tidyverse)

# Read the data
data <- read.csv("R/fake_ntw.csv", header = TRUE, sep = ";")

topo <- data[,c(1,2)]

# Topological graph
g_topo <- graph_from_data_frame(topo)
V(g_topo)$color <- c("chocolate1","dodgerblue","dodgerblue","darkolivegreen2","darkolivegreen2","darkolivegreen2")
get.vertex.attribute(g_topo)

tkid <- tkplot(g_topo)
l <- tkplot.getcoords(tkid)
tk_close(tkid, window.close = T)

png("presentation/images/network_topo.png", width = 1000, height = 1400, bg = "transparent")
plot(g_topo, layout = l, vertex.label = "", edge.color = "black")
dev.off()

# Quantitative graph
g <- graph_from_data_frame(data)
V(g)$color <- c("chocolate1","dodgerblue","dodgerblue","darkolivegreen2","darkolivegreen2","darkolivegreen2")
E(g)$width <- data$flux

tkid <- tkplot(g)
l <- tkplot.getcoords(tkid)
tk_close(tkid, window.close = T)

png("presentation/images/network_flux.png", width = 1000, height = 1400, bg = "transparent")
plot(g, layout = l, vertex.label = "", edge.color = "black")
dev.off()
