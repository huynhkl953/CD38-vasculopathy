rm(list=ls())
design = read.delim("D:/ffpe/peri_implants/240829_UNC52-VH01297_182_2223LG7NX-PI.mapping.txt")
design$condition = sapply(design$descr, function(x) as.character(strsplit(x,"_",fixed = T)[[1]][1]))
design$type[design$type == "x"] = "Healthy"

design = design[order(design$type,design$condition),]
design$condition[design$type == "healthy"] = "HU-2-32"
design$compartment = "epithelial"
design$compartment[grep("stroma",design$descr,fixed = T)] = "stroma"



data = read.delim('D:/ffpe/peri_implants/TABLEs/240829_UNC52-VH01297_182_2223LG7NX-PI.bracken-0-0.biom.txt',row.names = 1)
data = data[rownames(data)!="Homo sapiens",]
data = data[,design$id]



library(vegan)
library(phyloseq)
library(tidyverse)
library(patchwork)
library(agricolae)
library(FSA)
library(rcompanion)
library("ape")
library("vegan")
library("dplyr")
library("ggplot2")


shannon <- diversity(data ,MARGIN = 2)
simpson  <- diversity(data ,MARGIN = 2,index = "simpson")
richness  = specnumber(data ,MARGIN = 2)
evenness  = shannon/log(richness)

df = data.frame(shannon = shannon,
                simpson = simpson,
                richness = richness,
                evenness = evenness,
                condition = design$type,
                compartment = design$compartment)
write.csv(df,"alpha.csv")


df = rbind(df,df)
df$compartment[38:74] = "combined"

##### alpha diversity plot
p1 = ggplot(df, aes(x=compartment, y=shannon, color=condition)) +
  geom_boxplot() +
  labs(title = "Shannon", 
       x = "", y = "", color = "Condition") +
  theme_minimal()

p2 = ggplot(df, aes(x=compartment, y=simpson, color=condition)) +
  geom_boxplot() +
  labs(title = "Simpson", 
       x = "", y = "", color = "Condition") +
  theme_minimal()

p3 = ggplot(df, aes(x=compartment, y=richness, color=condition)) +
  geom_boxplot() +
  labs(title = "Richness", 
       x = "", y = "", color = "Condition") +
  theme_minimal()

p4 = ggplot(df, aes(x=compartment, y=evenness, color=condition)) +
  geom_boxplot() +
  labs(title = "Evenness", 
       x = "", y = "", color = "Condition") +
  theme_minimal()



pcoa_func = function(mat.data,mat.design,tit){
  
  
  bray_curtis_dist <- vegdist(t(mat.data), method = "bray")
  
  pcoa = pcoa(bray_curtis_dist)
  
  
  df = data.frame(PCOA1 = pcoa$vectors[,1],
                  PCOA2 = pcoa$vectors[,2],
                  Condition = mat.design$type)
  p = ggplot(df, aes(x=PCOA1, y=PCOA2, color=Condition)) +
    geom_point() +
    labs(title = tit, 
         x = "PCOA1", y = "PCOA2", color = "Condition") +
    theme_minimal()
  return(p)
  
}
##### pcoa plot
p5 = pcoa_func(data,design,"combined")
p6 = pcoa_func(data[,design$compartment == "epithelial"],design[design$compartment == "epithelial",],"epithelial")
p7 = pcoa_func(data[,design$compartment == "stroma"],design[design$compartment == "stroma",],"stroma")



library(gridExtra)

pdf("figure_alpha_beta.pdf",height = 10,width = 20)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,ncol = 4)
dev.off()