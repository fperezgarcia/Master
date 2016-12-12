setwd("/Users/fernando_perez/Documents/05 Fernando/Mis Docs/Master Utad/R")
relationships <- read.csv("./stormofswords.csv", header=TRUE)
characters <- as.data.frame(unique(union(dat[,1], dat[,2])))
colnames(characters) <- c("Name")

g <- graph.data.frame(relationships, directed=FALSE, vertices=characters)
V(g)$degree <- degree(g)

communities=leading.eigenvector.community(g)
V(g)$membership <- communities$membership
E(g)$weight <- c(2000, 100)[crossing(communities, g) + 1]

layout <- layout.fruchterman.reingold(g)

plot(g,
     layout = layout,
     vertex.color = V(g)$membership,
     vertex.frame.color = V(g)$membership,
     vertex.size = V(g)$degree,
     edge.color = "black",
     vertex.label.family = "sans", 
     vertex.label.color = "black",
     vertex.label.cex = 0.5)
