##http://christophergandrud.github.io/networkD3/
library(networkD3)
source <- c("A", "A", "A", "A",
         "B", "B", "C", "C", "D")
target <- c("B", "C", "D", "J",
            "E", "F", "G", "H", "I")
networkData <- data.frame(source, target)
networkData["value"] <- 1

# Plot
simpleNetwork(networkData)
data(MisLinks)
data(MisNodes)
forceNetwork(Links = MisLinks, Nodes = MisNodes,
             Source = "source", Target = "target",
             Value = "value", NodeID = "name",
             Group = "group", opacity = 0.8)

## Guardar en HTML
library(magrittr)
simpleNetwork(networkData) %>%
  saveNetwork(file = 'Net1.html')