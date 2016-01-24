setwd("/Users/fernando_perez/Documents/workspace_R/Master_IBM/data/")

library(igraph)
library(plyr)

inputFile <- "./twitter_d140.csv"
df <- read.csv(file = inputFile, header = TRUE, sep =";", stringsAsFactors = FALSE)

## Obtener Generadores de Tweets
dfParent <- df[is.na(df$parent),c("screen_name", "id")] 
dfParent <- unique(dfParent)
row.names(dfParent) <- NULL
colnames(dfParent) <- c("initiator", "ID")

## Asociar a cada Participante el screen_name del generador obteniendo como resultado
## los enlaces Participante, Inciador
mEdges <- merge(x = dfParent, y = df, by.x = "ID", by.y = "parent", all = TRUE)
mEdges <- mEdges[, c("screen_name", "initiator")]

colnames(mEdges) <- c("participant", "initiator")
mEdges <- mEdges[!is.na(mEdges$participant),]
mEdges <- mEdges[!is.na(mEdges$initiator),]
## eliminamos enlaces duplicados
mEdges <- unique(mEdges)
row.names(mEdges) <- NULL

## Confección de los vertices
initiators <- unique(dfParent[,1])
participants <- df[!df$screen_name %in% initiators, c("screen_name")]
participantsVertices <- unique(data.frame("user" = participants,
                              "initiator" = FALSE,
                              "userLabel" = "",  stringsAsFactors = FALSE))

initiatorsVertices <- data.frame("user" = unique(dfParent[,1]),
                      "initiator" = TRUE,
                      "userLabel" = "", stringsAsFactors = FALSE)

mVertices <- rbind (initiatorsVertices, participantsVertices, stringsAsFactors = FALSE)


## Limpieza huerfanos
## Buscar vertices que no aparezcan en ningún enlace como participante
userNotParticipant <- mVertices[!mVertices$user %in% mEdges$participant,]
## Descartar los iniciadores
orphans <- userNotParticipant[!userNotParticipant$user %in% mEdges$initiator,]
mVertices <- mVertices[!mVertices$user %in% orphans$user,]

## Marcar cuentas clave
mVertices[mVertices$user == "JavierMaroto","userLabel"] <- "JavierMaroto"
mVertices[mVertices$user == "ierrejon","userLabel"] <- "ierrejon"
mVertices[mVertices$user == "mariagv","userLabel"] <- "mariagv"
mVertices[mVertices$user == "Herzogoff","userLabel"] <- "Herzogoff"
mVertices[mVertices$user == "abrazopartio","userLabel"] <- "abrazopartio"
mVertices[mVertices$user == "ferdeparamo","userLabel"] <- "ferdeparamo"
row.names(mVertices) <- NULL

################# Grafo #################
g2 <- graph.data.frame(mEdges, directed=TRUE, vertices = as.data.frame(mVertices))

g2.communities <- walktrap.community(g2)

V(g2)$membership <- g2.communities$membership
V(g2)$color <- V(g2)$membership
V(g2)$degree <- degree(g2)

## Primer Plot sin filtros
plot(g2,
     vertex.color=V(g2)$color,
     vertex.size=2, 
     vertex.shape="circle",
     vertex.label= V(g2)$userLabel,
     vertex.label.dist=0.2,
     vertex.color=V(g2)$membership,
     vertex.label.family="sans", 
     vertex.label.cex=1,
     edge.arrow.size=0.01, 
     edge.arrow.width=0.01,
     edge.width=E(g2)$weight, 
     edge.color="gray")

## Filtrado para investigar la "endogamia"
## El nodo tiene que pertenece a un grupo con el que se conecta por al
## menos 2 enlaces
V(g2)$coreness <- graph.coreness(g2,mode = "all")
g2=delete.vertices(g2,which(V(g2)$coreness<2 &
                              V(g2)$userLabel == ""))

plot(g2,
     vertex.color=V(g2)$color,
     vertex.size=2, 
     vertex.shape="circle",
     vertex.label= V(g2)$userLabel,
     vertex.label.dist=0.2,
     vertex.color=V(g2)$membership,
     vertex.label.family="sans", 
     vertex.label.cex=1,
     edge.arrow.size=0.01, 
     edge.arrow.width=0.01,
     edge.width=E(g2)$weight, 
     edge.color="gray")




##Buscar Vértices que participan en mas de 2 comunidades
## Regeneramos los dataframes puesto que el grado se ha filtrado
dfVertices <- get.data.frame(g2, what = "vertices")
dfEdges <- get.data.frame(g2, what = "edges")

mJoin <- merge(x = dfEdges, y = dfVertices, by.x = "to", by.y = "name", all.x = TRUE)

## Contar con cuantas comunidades (membership) diferentes interactua
mAgr <- ddply(mJoin, c("from"), summarise,
               N = length(unique(membership)),
               nameNode = max(from))

## Usuarios que unen 2 o mas comunidades
mFltr <- mAgr[mAgr$N > 1,]

## Generamos vertices extendidos con la propiendad nameNode y N
## Solo se conserva en aquellos que conectan mas de 2 comunidades
## Para poder destacarlos en visualización
dfVerticesExt <- merge(x = dfVertices, y = mFltr, by.x = "name", by.y = "from", all.x = TRUE)
dfVerticesExt[is.na(dfVerticesExt$N), "nameNode"] <- ""
dfVerticesExt[is.na(dfVerticesExt$N), "N"] <- ""

g3 <- graph.data.frame(dfEdges, directed=TRUE, vertices = as.data.frame(dfVerticesExt))

#V(g3)$variety <- V(g3)$N
V(g3)$usrLabelExt <- paste0(V(g3)$userLabel, V(g3)$nameNode, V(g3)$N)
V(g3)$membership_char <- as.character(V(g3)$membership)


plot(g3,
     vertex.color=V(g3)$color,
     vertex.size=2, 
     vertex.shape="circle",
     vertex.label= V(g3)$userLabel,
     vertex.label.dist=0.2,
     vertex.color=V(g3)$membership,
     vertex.label.family="sans", 
     vertex.label.cex=1,
     edge.arrow.size=0.01, 
     edge.arrow.width=0.01,
     edge.width=E(g3)$weight, 
     edge.color="gray")

write.graph(g3, file=paste0("./",inputFile,"_result.graphml"), format="graphml")

