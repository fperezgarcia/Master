##################################
# PART1 Simple Grpah Represntation
##################################

library(igraph)
library(ggplot2)
## Vertex & Edges
cities <- data.frame(name = c("Almería", "Granada", "Jaén", "Madrid", "Málaga", "Murcia"),
                     population = c(192070, 237540, 115837, 3165000, 566913, 439712))

relations <- data.frame(from = c("Almería", "Almería", "Granada", "Jaén",   "Jaén",   "Madrid"),
                        to   = c("Granada", "Murcia",  "Jaén",    "Madrid", "Málaga", "Murcia"),
                        distance = c(92070, 237540, 115837, 3165000, 566913, 439712))

g <- graph.data.frame(relations, directed=FALSE, vertices=cities)
print(g)

plot(g)
plot(g, layout = layout.fruchterman.reingold, 
     vertex.label = V(g)$name,
     vertex.size = V(g)$population/100000,
     vertex.label.family = "sans",
     edge.color="black")
write.graph(graph = g,file = "./graph.gml", format = "gml")

## Adjacency Matrix
setwd("/Users/fernando_perez/Documents/05 Fernando/Mis Docs/Master IBM/R")
dat <- read.csv("./cities.csv", header=TRUE, row.names=1)
mat <- as.matrix(dat)

g <- graph.adjacency(mat, mode="undirected", weighted = NULL)
print(g)
plot(g)
g

## La operacion inversa
mVert <- get.data.frame(g, what="vertices")
mEdges <- get.data.frame(g, what="edges")


## Caminos de longitud n entre 2 nodos
mat2 <- mat %*% mat
mat3 <- mat2 %*% mat
mat4 <- mat3 %*% mat
mat5 <- mat4 %*% mat

## Árboles, Estrellas y Grafos completos

g <- graph.tree(n = 20, children = 2, mode = "undirected")
plot(g)

g <- graph.star(n = 20, mode = "undirected")
plot(g)

g <- graph.full (n = 20, directed = FALSE, loops = FALSE)
plot(g)


## Eigen Centrality
#Creamos 2 estrellas
g1 <- graph.star(n = 6, mode = "undirected")
V(g1)$name <- (letters[1:6])

g2 <- graph.star(n = 8, mode = "undirected")
V(g2)$name <- (letters[7:14])

#Las unimos por sus nodos centrales a través de un nuevo nodo
g3 <- g1 %u% g2
g3 <- g3 + vertices("z")
g3 <- add.edges(g3,
        c(grep(V(g3)$name, pattern = "a"), grep(V(g3)$name, pattern = "z"),
          grep(V(g3)$name, pattern = "g"), grep(V(g3)$name, pattern = "z")))

plot(g3)

par(mfrow = c(1, 2))
#Plotear por grado
V(g3)$degree <- degree(g3)

l <- layout.kamada.kawai(g3)
plot(g3, layout = l, 
     vertex.label = V(g3)$degree,
     vertex.label.family = "sans",
     vertex.label.dist=1.5,
     edge.color="black")

#Plotear por EigenVector
V(g3)$evcent <- round(evcent(g3)$vector,3)

plot(g3, layout = l, 
     vertex.label = V(g3)$evcent,
     vertex.label.family = "sans",
     vertex.label.dist=1.5,
     edge.color="black")

par(mfrow = c(1, 1))


##Eigenvectors
setwd("/Users/fernando_perez/Documents/05 Fernando/Mis Docs/Master IBM/R")
dat <- read.csv("./cities.csv", header=TRUE, row.names=1)
mat <- as.matrix(dat)

maxEigenVal <- which.max(eigen(mat)$values)
cent1 <- eigen(mat)$vectors[,maxEigenVal]

g <- graph.adjacency(mat, mode="undirected", weighted = NULL)
V(g)$evcent <- round(evcent(g)$vector,3)

norm_vec <- function(x) sqrt(sum(x^2))
normEv <- norm_vec(V(g)$evcent)
V(g)$evcent / normEv

plot(g, layout = layout.kamada.kawai, 
     vertex.label = paste0(V(g)$name, ", ", V(g)$evcent),
     vertex.label.family = "sans",
     vertex.label.dist=1,
     edge.color="black")


## Katz
g <- graph( c(1,2,
              1,3,
              2,3,
              3,4,
              4,5,
              5,6,
              5,7,
              6,7), directed = TRUE )

V(g)$name <- letters[1:7]

plot(g)

V(g)$alphac1 <- round(alpha.centrality(g,alpha=1,loops = FALSE),3)
V(g)$alphac2 <- round(alpha.centrality(g,alpha=1/2),3)
V(g)$alphac3 <- round(alpha.centrality(g,alpha=1/3),3)
V(g)$alphac4 <- round(alpha.centrality(g,alpha=1/4),3)

V(g)$label.cex = 1.5
V(g)$label.color = "black"
V(g)$color = "red"
V(g)$edge.color="gray"

par(mfrow = c(2, 2))

l <- layout.fruchterman.reingold(g, niter=10000) #layout.kamada.kawai
plot(g, layout = l, 
     xlab = "Alpha = 1",
     vertex.label = paste(V(g)$name,V(g)$alphac1, sep = ","),
     edge.arrow.size=0.2,
     vertex.label.dist=1.5)

plot(g, layout = l, 
     xlab = "Alpha = 1/2",
     vertex.label = paste(V(g)$name,V(g)$alphac2, sep = ","),
     edge.arrow.size=0.2,
     vertex.label.dist=2)

plot(g, layout = l, 
     xlab = "Alpha = 1/3",
     vertex.label = paste(V(g)$name,V(g)$alphac3, sep = ","),
     edge.arrow.size=0.2,
     vertex.label.dist=2)

plot(g, layout = l, 
     xlab = "Alpha = 1/4",
     vertex.label = paste(V(g)$name,V(g)$alphac4, sep = ","),
     edge.arrow.size=0.2,
     vertex.label.dist=2)

par(mfrow = c(1, 1))


##PageRank
g <- graph(n = 5, 
           c(1,2,
             1,3,
             2,3,
             4,5), directed = TRUE )

V(g)$name <- letters[1:5]
V(g)$pr <- round(page.rank(g, damping = 0.85)$vector, 4)

plot(g, layout = layout.circle, 
     xlab = "PageRank",
     vertex.label = paste(V(g)$name,V(g)$pr, sep = ","),
     edge.arrow.size=0.2,
     vertex.label.dist=1)

##Closeness Centrality

#Creamos 2 estrellas
g1 <- graph.star(n = 6, mode = "undirected")
V(g1)$name <- (letters[1:6])

g2 <- graph.star(n = 8, mode = "undirected")
V(g2)$name <- (letters[7:14])

#Las unimos por sus nodos centrales a través de un nuevo nodo
g <- g1 %u% g2
g <- g + vertices("z")
g <- add.edges(g,
                c(grep(V(g)$name, pattern = "a"), grep(V(g)$name, pattern = "z"),
                  grep(V(g)$name, pattern = "g"), grep(V(g)$name, pattern = "z")))


V(g)$closeness <- closeness(g)

l <- layout.fruchterman.reingold(g)
plot(g, layout=l, vertex.label = round(V(g)$closeness,3),
     vertex.label.dist=1)


##Betweenness

V(g)$betweennses <- betweenness(g)

l <- layout.fruchterman.reingold(g)
plot(g, layout=l, vertex.label = round(V(g)$betweennses,3),
     vertex.label.dist=2)

## Degree Distribution
n = 100
g1 <- graph.ring(n)
g2 <- erdos.renyi.game(n, 0.10)
g3 <- barabasi.game(n)    

deg1 <- degree(g1)
deg2 <- degree(g2)
deg3 <- degree(g3)

par(mfrow = c(2, 3))

plot(g1, vertex.label= NA, edge.arrow.size=0.002, vertex.size = 0.5, xlab = "Ring Network")
plot(g2, vertex.label= NA, edge.arrow.size=0.002, vertex.size = 0.5, xlab = "Random Network p=0.10")
plot(g3, vertex.label= NA, edge.arrow.size=0.002, vertex.size = 0.5, xlab = "Scale-free Network")
hist(deg1, col=rgb(0,0,1,.4), xlim=c(0,20), xlab="degree", ylab="freq")
hist(deg2, col=rgb(1,0,0,.4), xlim=c(0,20), xlab="degree", ylab="freq")
hist(deg3, col=rgb(0,1,0,.4), xlim=c(0,20), xlab="degree", ylab="freq")

par(mfrow = c(1, 1))

## Power Law

library("poweRlaw")

n = 5000

g1 <- barabasi.game(n)
#g1 <- erdos.renyi.game(n, 0.10)
deg1 <- degree(g1)

par(mfrow = c(1, 2))

#plot del histograma
hist(deg1, main = "Histogram")

#plot red aleatoria
m = displ$new(deg1)
plot(m, main = "Power Law", xlab = "degree", ylab = "freq")
est = estimate_pars(m)
m$setPars(est)
lines(m, col=2, lwd=1)

par(mfrow = c(1, 1))

#Cálculo de los momentos de la distribución
degVal <- seq(1, max(deg1), by = 1)
freqDeg <- cbind("k" = degVal,
                 "freq" = unlist(lapply(degVal, function(x){ 
                                                    length(which(deg1 == x)) / n
                                                })),
                 "count" = unlist(lapply(degVal, function(x){ 
                                                    length(which(deg1 == x))
                                                }))
                 )

m_ord1 <- sum(freqDeg[,1] * freqDeg[,2])
m_ord1
weighted.mean(x = freqDeg[,1], w = freqDeg[,3])
m_ord2 <- sum(freqDeg[,1]^2 * freqDeg[,2])
sqrt(m_ord2)
weighted.mean(x = freqDeg[,1]^2, w = freqDeg[,3])
m_ord3 <- sum(freqDeg[,1]^3 * freqDeg[,2])
m_ord3


## Erdos Renyi

n=500

p1 = 1/n^2 
p2 = (1/n^(3/2))
p3 = 1/n
p4 = log(n)/n

#p1 <- p1 * 1.10
#p2 <- p2 * 1.10
#p3 <- p3 * 1.10
#p4 <- p4 * 1.10

er1 <- erdos.renyi.game(n, p1, type="gnp")
er2 <- erdos.renyi.game(n, p2, type="gnp")
er3 <- erdos.renyi.game(n, p3, type="gnp")
er4 <- erdos.renyi.game(n, p4, type="gnp")

deg1 <- degree(er1)
deg2 <- degree(er2)
deg3 <- degree(er3)
deg4 <- degree(er4)

par(mfrow = c(2, 4))

plot(er1, main=paste0("p=",round(p1,digits = 4)), vertex.size = 1, vertex.label = NA)
plot(er2, main=paste0("p=",round(p2,digits = 4)), vertex.size = 1, vertex.label = NA)
plot(er3, main=paste0("p=",round(p3,digits = 4)), vertex.size = 1, vertex.label = NA)
plot(er4, main=paste0("p=",round(p4,digits = 4)), vertex.size = 1, vertex.label = NA)
hist(deg1, col=rgb(0,0,1,.4), xlim=c(0,20), xlab="degree", ylab="freq", 
     main=paste0("T. AvgDeg=",round((1/n),2), " R. AvgDeg=", round(mean(deg1),2)))
hist(deg2, col=rgb(1,0,0,.4), xlim=c(0,20), xlab="degree", ylab="freq", 
     main=paste0("T. AvgDeg=",round(1/(n^.5),2), " R. AvgDeg=", round(mean(deg2),2)))
hist(deg3, col=rgb(0,1,0,.4), xlim=c(0,20), xlab="degree", ylab="freq", 
     main=paste0("T. AvgDeg=",(1), " R. AvgDeg=", round(mean(deg3),2)))
hist(deg4, col=rgb(1,1,0,.4), xlim=c(0,20), xlab="degree", ylab="freq", 
     main=paste0("T. AvgDeg=",round(log(n),2), " R. AvgDeg=", round(mean(deg4),2)))

par(mfrow = c(1, 1))

# Clustering Coefficient = c / (n-1)
er1 <- erdos.renyi.game(n, 0.05, type="gnp")
deg1 <- degree(er1)
#teórico
mean(deg1) / (n-1)
#real
transitivity(er1)


## Watts - Strogatz

n = 100
neighborhood = 2

par(mfrow = c(1, 2))

g <- graph.ring(n, circular = TRUE)
g <- connect.neighborhood(g, neighborhood)
plot(g, layout = layout.circle(g), vertex.size = 5, vertex.label = NA,
        main = paste0("Lattice ", n, " nodes\n",
                      "Connect neighborhood: ", neighborhood),
        sub  = paste0("Shortest Path: ", mean(shortest.paths(g)), "\n",
                     "Diameter: ", diameter(g), "\n",
                     "C. Coefficient:", transitivity(g)))

rewProb <- 0.05
g <- rewire.edges(g, prob = rewProb)
plot(g, layout = layout.circle(g), vertex.size = 4, vertex.label = NA,
     main = paste0("Rewire Prob: ", rewProb),
     sub  = paste0("Shortest Path: ", mean(shortest.paths(g)), "\n",
                   "Diameter: ", diameter(g), "\n",
                   "C. Coefficient:", transitivity(g)))

par(mfrow = c(1, 1))

#Directamente con la función de R watts.strogatz.game
g <- watts.strogatz.game(dim = 1, size = 100, nei = 2, p=0.05)
plot(g, layout = layout.circle(g), vertex.size = 4, vertex.label = NA,
     main = paste0("Rewire Prob: ", rewProb),
     sub  = paste0("Shortest Path: ", mean(shortest.paths(g)), "\n",
                   "Diameter: ", diameter(g), "\n",
                   "C. Coefficient:", transitivity(g)))


## Barabasi - Albert
initNodes <- 3
g <- graph.full (n = initNodes, directed = FALSE, loops = FALSE)
g4 <- barabasi.game(n = 100, m = 2, start.graph = g)    

plot(g4, vertex.label= NA, edge.arrow.size=0.02, vertex.size = 1, xlab = "Scale-free Network")


## Barabasi - Albert - Manual
#Parametrización
initNodes <- 3
newEdges <- 2
numLoops <- 100

#Cada cuantas iteraciones se realiza un plot
steps <- trunc(numLoops/10)

#Inicialización con un grafo completo, se marcan en rojo los nodos iniciales
g <- graph.full (n = initNodes, directed = FALSE, loops = FALSE)
V(g)$color <- "red"

par(mfrow = c(2, steps/2))

#Bucle de generación de grafos
for (i in seq(1,numLoops)){
    #Creamos una lista donde cada nodo aparece tantas veces como su grado
    V(g)$degree <- degree(g)
    mVertex <- get.data.frame(g, what = "vertices")
    mList <- list()
    for (j in seq(1,dim(mVertex)[1])){
        mList <- c(mList, rep(j, mVertex[j,"degree"]))
    }    
    mList <- unlist(mList)
    
    #Para los nuevos nodos, elegir al azar un destino dentro de la lista de nodos por grado
    for (j in seq(1,newEdges)){
        edgeDest <- mList[round(runif(1,min = 1, max = length(mList)))]
        #Añadir el nodo
        g <- add.vertices(g, nv = 1)
        #Añadir el enlace entre el nuevo nodo y el elegido al azar
        g <- add.edges(g,edges = c(length(V(g)), edgeDest))
    }

    #Si el paso actual esta en el intervalo de pintado, generar el plot
    if (i%%steps == 0){
        deg <- degree(g)
        plot(g,
             vertex.label= NA,
             vertex.color = V(g)$color,
             edge.arrow.size=0.02, 
             vertex.size = 1+2*log(deg),
             main = paste0("Step: ", i))        
    }
    
}
par(mfrow = c(1, 1))

















## Random networks

g1 <- graph.ring(500)
g2 <- erdos.renyi.game(500, 0.0035)
g3 <- rewire.edges( g1, prob = 0.5 )
g4 <- barabasi.game(500)    


par(mfrow=c(2,2))
plot(g1, vertex.label= NA, edge.arrow.size=0.02, vertex.size = 0.5, xlab = "Ring Network")
plot(g2, vertex.label= NA, edge.arrow.size=0.02, vertex.size = 0.5, xlab = "Random Network")
plot(g3, vertex.label= NA, edge.arrow.size=0.02, vertex.size = 0.5, xlab = "Small World Network")
plot(g4, vertex.label= NA, edge.arrow.size=0.02, vertex.size = 0.5, xlab = "Scale-free Network")

par(mfrow = c(1, 1))

## Diametro y Camino medio

g <- graph.ring(10)
diameter(g)
average.path.length(g)

shortest.paths(g)


g <- erdos.renyi.game(n=20,p=.12,directed = FALSE)
plot(g)
diameter(g)
average.path.length(g)


