#Función para generar links entre todas las palabras de una frase
#y todas ellas a su vez con la categorí

generateLinksGraph <- function(txt, category){
    txt <- paste(txt, category, sep=" ")
    txtSplit <- unlist(strsplit(txt, " "))
    gg <- graph.data.frame(expand.grid(txtSplit,txtSplit), directed=F)
    ss <- simplify(gg)
    pp <- as.data.frame(get.edgelist(ss))
    return(pp)
}