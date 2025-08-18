design = read.delim("D:/ffpe/peri_implants/240829_UNC52-VH01297_182_2223LG7NX-PI.mapping.txt")
design$condition = sapply(design$descr, function(x) as.character(strsplit(x,"_",fixed = T)[[1]][1]))
design$type[design$type == "x"] = "healthy"

design = design[order(design$type,design$condition),]
design$condition[design$type == "healthy"] = "HU-2-32"
design$compartment = "epithelial"
design$compartment[grep("stroma",design$descr,fixed = T)] = "stroma"



data = read.delim('D:/ffpe/peri_implants/TABLEs/240829_UNC52-VH01297_182_2223LG7NX-PI.bracken-0-0.biom.txt',row.names = 1)
data = data[rownames(data)!="Homo sapiens",]
data = data[,design$id]

rownames(data) = gsub("[","",rownames(data),fixed = T)
rownames(data) = gsub("]","",rownames(data),fixed = T)


library(ggplot2)
library(pheatmap)
library(gridExtra)
species <- c(
  "Campylobacter concisus", "Campylobacter showae", "Campylobacter ureolyticus",
  "Desulfobulbus sp. oral taxon 041", "Dialister invisus", "Dialister pneumosintes",
  "Eubacterium brachy", "Eubacterium nodatum", "Eubacterium saphenum",
  "Filifactor alocis", "Fretibacterium fastidiosum", "Fusobacterium gonidiaformans",
  "Fusobacterium hwasookii", "Fusobacterium nucleatum", "Fusobacterium periodonticum",
  "Parvimonas sp. oral taxon 393", "Porphyromonas gingivalis",
  "Prevotella buccae", "Prevotella denticola", "Prevotella intermedia",
  "Prevotella loescheii", "Prevotella melaninogenica", "Prevotella nigrescens",
  "Pseudoramibacter alactolyticus", "Selenomonas noxia", "Selenomonas sputigena",
  "Tannerella forsythia", "Tannerella sp. oral taxon HOT-286",
  "Treponema denticola", "Treponema medium", "Treponema socranskii", "Treponema vincentii"
)


design.choose = design[design$type !="healthy",]

mat = data[species,design.choose$id]

plt = function(mat,design.choose,tit){

mat.hm = data.frame(Peri_implantitis = rowMeans(mat[,design.choose$type == "Peri-implantitis"]),
                    Periodontitis = rowMeans(mat[,design.choose$type == "Periodontitis"]))

mat.hm.PI = mat.hm[order(mat.hm$Peri_implantitis,decreasing = T),]

mat.hm.Perio = mat.hm[order(mat.hm$Periodontitis),]

annotation_col <- data.frame(Condition = unique(design.choose$type))
rownames(annotation_col) <- colnames(mat.hm)


p1 = pheatmap(mat.hm.PI,
         cluster_rows = F,
         cluster_cols = F,
         scale = "row",
         annotation_col = annotation_col,
         show_colnames = F,
         breaks = c(seq(-1, 1, length.out = 101)),
         main = "order by PI",silent = T)[[4]]

p2 =pheatmap(mat.hm.Perio,
         cluster_rows = F,
         cluster_cols = F,
         scale = "row",
         annotation_col = annotation_col,
         show_colnames = F,
         breaks = c(seq(-1, 1, length.out = 101)),
         main = "order by Perio",silent = T)[[4]]

pdf(paste0(tit,".pdf"),height = 10,width = 15)
grid.arrange(p1,p2,ncol = 1)
dev.off()

write.csv(mat.hm,paste0(tit,".csv"))

return(mat.hm)
}

plt(mat,design.choose,"combined")

plt(mat[,design.choose$compartment == "stroma"],design.choose[design.choose$compartment == "stroma",],"stroma")
plt(mat[,design.choose$compartment == "epithelial"],design.choose[design.choose$compartment == "epithelial",],"epithelial")
