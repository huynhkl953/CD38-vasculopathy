library(readr)
library(ggplot2)
library(ggforce)
library(dplyr)
library(purrr)
setwd("C:/Users/huynhk4/Downloads/Quinn_v3/")
#### 1) Load the data
metadata=read_csv("C:/Users/huynhk4/Downloads/Quinn_Paper_2/metadata.csv")

# File paths for all your files (replace with your actual paths)
file_paths <- list(
  "C:/Users/huynhk4/Downloads/TACIT_250211-20250211T220114Z-001/TACIT_250211/CD373C70_230502_TACIT_v3.csv",
  "C:/Users/huynhk4/Downloads/TACIT_250211-20250211T220114Z-001/TACIT_250211/CST282NL_TACIT_v3.csv",
  "C:/Users/huynhk4/Downloads/TACIT_250211-20250211T220114Z-001/TACIT_250211/RCP22EL_P_TACIT_v3.csv",
  "C:/Users/huynhk4/Downloads/TACIT_250211-20250211T220114Z-001/TACIT_250211/Hu_2_32_TACIT_v3.csv",
  "C:/Users/huynhk4/Downloads/TACIT_250211-20250211T220114Z-001/TACIT_250211/4CDC4975_Disease_TACIT_v3.csv",
  "C:/Users/huynhk4/Downloads/TACIT_250211-20250211T220114Z-001/TACIT_250211/4D337YUL2_Disease_TACIT_v3.csv",
  "C:/Users/huynhk4/Downloads/TACIT_250211-20250211T220114Z-001/TACIT_250211/UPGS9982_Disease_TACIT_v3.csv",
  "C:/Users/huynhk4/Downloads/TACIT_250211-20250211T220114Z-001/TACIT_250211/perio_1_PCF_TACIT_Kevin_2102_v3.csv",
  "C:/Users/huynhk4/Downloads/TACIT_250211-20250211T220114Z-001/TACIT_250211/perio_2_PCF_TACIT_Kevin_2102_v3.csv",
  "C:/Users/huynhk4/Downloads/TACIT_250211-20250211T220114Z-001/TACIT_250211/DNZJND7F_TACIT_v3.csv",
  "C:/Users/huynhk4/Downloads/TACIT_250211-20250211T220114Z-001/TACIT_250211/F66LWFPZ_TACIT_v3.csv",
  "C:/Users/huynhk4/Downloads/TACIT_250211-20250211T220114Z-001/TACIT_250211/U7CCYJBP_TACIT_v3.csv",
  "C:/Users/huynhk4/Downloads/TACIT_250211-20250211T220114Z-001/TACIT_250211/XHPA93WLMG_TACIT_v3.csv",
  "C:/Users/huynhk4/Downloads/TACIT_250211-20250211T220114Z-001/TACIT_250211/XPN88VJQ_TACIT_v3.csv",
  "C:/Users/huynhk4/Downloads/TACIT_250211-20250211T220114Z-001/TACIT_250211/Z66BH47YQ_TACIT_v3.csv"
)


# Process all files, selectively modifying Sample for data_H1 and data_PD1
all_data <- map(file_paths, ~ {
  df <- read_csv(.x)
  colnames(df)=gsub("_",".",colnames(df))
  file_name <- basename(.x)
  df$Group2=file_name
  if (grepl("CD373C70_230502_TACIT.csv", file_name)) {
    df <- df %>% mutate(Sample = paste0("CD373C70B_", Group))
  } else if (grepl("4CDC4975_Disease_TACIT.csv", file_name)) {
    df <- df %>% mutate(Sample = paste0("4CDC4975_", Group))
  }else{
    df <- df %>% mutate(Sample = file_name)
  }
  df
})

#Combine all dataframes
combined_data <- bind_rows(all_data)

print(combined_data)

# Find common columns after adding Sample (more robust)
common_cols <- Reduce(intersect, lapply(all_data, names))

#Select common columns
combined_data <- combined_data[, common_cols]

# Load necessary libraries
library(ggplot2)
library(ggforce)
library(data.table)


my_colors <- c(
  "CD38 VECs"="black",
  "B cells" = "green",
  "DC cells" = "#BDA110",
  "Tc" = "red",
  "Th" = "#CC0000",
  "Neutrophil" = "#E4DB7A",
  "Epithelial" = "#DF8D97",
  "CD8+ T Cells" = "#CC0000",
  "Fibroblast2" = "#0000FF",
  "Macrophage" = "yellow",
  "NK cells" = "#4A90E2",
  "Others" = "grey90",
  "Fibroblast" = "#B59E88",
  "T Exhausted" = "#FF6347",
  "Treg" = "salmon",
  "VEC" = "#7B68EE",
  "LECs" = "#375614",
  "Memory T Cell" = "#D2B48C",
  "VEC progenitors" = "skyblue"
)


combined_data$Sample=gsub("\\.csv$", "", combined_data$Sample)


for (Tissue in unique(combined_data$Sample)) { # Use unique to avoid duplicates
  # Load the data
  data <- combined_data[combined_data$Sample == Tissue, ]
  data$TACIT=ifelse(data$TACIT%in%names(which(table(data$TACIT)==1)),"Others",data$TACIT)
  
  # Create the Voronoi diagram.  Using fill and a proper scale
  plot <- ggplot(data, aes(X, Y, fill = as.factor(data$TACIT))) + #Fixed this line
    geom_voronoi_tile(size = 0.1, color = "black", max.radius = 10) +
    scale_fill_manual(values = my_colors, name = "Cell Type") + #Fixed this line
    theme_classic(base_size = 15) +
    labs(title = Tissue, x = "X", y = "Y") 
  # Export the plot as an SVG file with error handling
  svg_filename <- paste0("C:/Users/huynhk4/Downloads/Quinn_v3/voronoi/",  gsub("\\.csv$", "", Tissue), ".svg")
  tryCatch({
    ggsave(svg_filename, plot = plot, device = "svg", width = 15, height = 10)
    cat("Saved:", svg_filename, "\n")
  }, error = function(e) {
    cat("Error saving", svg_filename, ":", e$message, "\n")
  })
}

combined_data$Sample=gsub("_v3","",combined_data$Sample)

data_final=merge(metadata,combined_data,"Sample")

# Shorten Sample names by removing specific parts
data_final$Sample <- gsub("Group_", "G", data_final$Sample) # Shorten "Group_" to "G"
data_final$Sample <- gsub("_TACIT", "", data_final$Sample)  # Remove "_TACIT"
data_final$Sample <- gsub("_Disease", "D", data_final$Sample) # Shorten "_Disease" to "D"
data_final$Sample <- gsub("_PCF", "PCF", data_final$Sample) # Keep "_PCF" as is, or modify as needed

# Optionally, remove any extra underscores or spaces
data_final$Sample <- gsub("__", "_", data_final$Sample) # Replace double underscores with single
data_final$Sample <- gsub("^_|_$", "", data_final$Sample) # Remove leading or trailing underscores

###############Proportion cell type between three group##############

data_ct=data.frame(CT=data_final$TACIT,Sample=data_final$Sample,Condition=data_final$Condition)
data_ct=data_ct[which(data_ct$CT!="Epithelial"),]
data_proportions <- data_ct %>%
  group_by(Sample, CT) %>%
  summarise(count = n()) %>%  # Count occurrences of each cell type
  ungroup() %>%
  group_by(Sample) %>%
  mutate(proportion = count / sum(count)) # Calculate the proportion

# Plot the proportions using ggplot2
ggplot(data_proportions, aes(x = Sample, y = proportion, fill = CT)) +
  geom_bar(stat = "identity", position = "stack") + # Stacked bar plot
  labs(title = "Proportion of Cell Types by Sample", x = "Sample", y = "Proportion", fill = "Cell Type") +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = my_colors, name = "Cell Type") # Rotate x-axis labels for better readability


data_proportions <- data_ct %>%
  group_by(Condition, CT) %>%
  summarise(count = n()) %>%  # Count occurrences of each cell type
  ungroup() %>%
  group_by(Condition) %>%
  mutate(proportion = count / sum(count)) # Calculate the proportion

# Plot the proportions using ggplot2
ggplot(data_proportions, aes(x = Condition, y = proportion, fill = CT)) +
  geom_bar(stat = "identity", position = "stack") + # Stacked bar plot
  labs(title = "Proportion of Cell Types by Condition", x = "Condition", y = "Proportion", fill = "Cell Type") +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = my_colors, name = "Cell Type") # Rotate x-axis labels for better readability






library(triwise)

GAP1_data_TACIT <- read_csv("C:/Users/huynhk4/Downloads/GAP1_data_TACIT.csv")
GAP3_data_TACIT <- read_csv("C:/Users/huynhk4/Downloads/GAP3_data_TACIT.csv")
GAP4_data_TACIT <- read_csv("C:/Users/huynhk4/Downloads/GAP4_data_TACIT.csv")
GAP5_data_TACIT <- read_csv("C:/Users/huynhk4/Downloads/GAP5_data_TACIT.csv")
GAP=data.frame(CT=c(GAP1_data_TACIT$TACIT,GAP3_data_TACIT$TACIT,GAP4_data_TACIT$TACIT,GAP5_data_TACIT$TACIT),
               Sample=c(rep("GAP_1",nrow(GAP1_data_TACIT)),
                        rep("GAP_3",nrow(GAP3_data_TACIT)),
                        rep("GAP_4",nrow(GAP4_data_TACIT)),
                        rep("GAP_5",nrow(GAP5_data_TACIT))),
               Condition=c(rep("GAP",nrow(GAP1_data_TACIT)),
                           rep("GAP",nrow(GAP3_data_TACIT)),
                           rep("GAP",nrow(GAP4_data_TACIT)),
                           rep("GAP",nrow(GAP5_data_TACIT))))

data_ct=data.frame(CT=data_final$TACIT,Sample=data_final$Sample,Condition=data_final$Condition)
data_ct=rbind(data_ct,GAP)
data_ct=data_ct[which(data_ct$Condition!="Healthy"),]


data_ct=data_ct[which(data_ct$CT!="Others"),]
#data_ct=data_ct[which(data_ct$CT%in%c("Tc","Th","DC cells","B cells","NK cells","Neutrophil","Treg")),]
data_proportions <- data_ct %>%
  group_by(Sample, Condition,CT) %>%
  summarise(count = n()) %>%  # Count occurrences of each cell type
  ungroup() %>%
  group_by(Sample) %>%
  mutate(proportion = count / sum(count)) # Calculate the proportion

head(data_proportions)

# Summarize data by taking the mean of the proportions for each Sample and Condition
data_summary <- data_proportions %>%
  group_by(CT, Condition) %>%
  summarise(mean_proportion = mean(proportion), .groups = 'drop')

library(tidyverse)

# Reshape the data to wide format
data_wide <- data_summary %>%
  pivot_wider(names_from = Condition, values_from = mean_proportion)

# Prepare an empty data frame for log2FC results
log2fc_results <- data.frame(CT = data_wide$CT)

# Define conditions to compare
conditions <- colnames(data_wide)[-1]  # Exclude the Sample column

# Loop through each condition to calculate log2FC
for (cond in conditions) {
  # Calculate mean of other conditions
  other_conditions_mean <- rowMeans(dplyr::select(data_wide, -one_of(c("CT", cond))), na.rm = TRUE)
  
  # Calculate log2FC for the current condition
  log2fc_values <- log2(data_wide[[cond]] / other_conditions_mean)
  
  # Add the result to the log2fc_results data frame
  log2fc_results[[paste0(cond)]] <- log2fc_values
}

# View the final log2FC results
print(log2fc_results)

rownames(log2fc_results)=log2fc_results$CT
log2fc_results=log2fc_results[,-1]
log2fc_results[is.na(log2fc_results)==T]=0
barycoords = transformBarycentric(log2fc_results)
str(barycoords)

Gdiffexp =rownames(barycoords[which(barycoords$r>1),])
plotDotplot(barycoords, Gdiffexp = rownames(barycoords) ,Goi = Gdiffexp,showlabels = T)


Gdiffexp =13
rownames(barycoords)
plotDotplot(barycoords, Gdiffexp = rownames(barycoords) ,Goi = Gdiffexp,showlabels = T)























library(broom) 
anova_results <- data_proportions %>%
  group_by(CT) %>%
  do(
    tidy(
      aov(proportion ~ Condition, data = .)
    )
  )

# View the results
print(anova_results)

write.csv(anova_results,"anova_results_three_group.csv")

table(data_proportions$Condition)


safe_t_test <- function(x, y) {
  if (length(x) < 2 || length(y) < 2) {
    return(NA)  # Return NA if there are insufficient observations
  }
  wilcox.test(x, y)$p.value  # Otherwise, perform the t-test
}

# Calculate log2FC and p-values
results <- data_proportions %>%
  group_by(CT) %>%
  summarize(
    # Healthy vs. Disease
    log2FC_Healthy_vs_Disease = log2(mean(proportion[Condition != "Healthy"]) / mean(proportion[Condition == "Healthy"])),
    p_value_Healthy_vs_Disease = safe_t_test(proportion[Condition != "Healthy"], proportion[Condition == "Healthy"]),
    
    # Peri-implantitis vs. Periodontitis
    log2FC_Peri_vs_Perio = log2(mean(proportion[Condition == "Peri-implantitis"]) / mean(proportion[Condition == "Periodontitis"])),
    p_value_Peri_vs_Perio = safe_t_test(proportion[Condition == "Peri-implantitis"], proportion[Condition == "Periodontitis"])
  ) %>%
  ungroup()

# View results
print(results)
results <- results %>%
  mutate(
    direction_Healthy_vs_Disease = case_when(
      log2FC_Healthy_vs_Disease > 0 & p_value_Healthy_vs_Disease < 0.05 ~ "Up in Disease",
      log2FC_Healthy_vs_Disease < 0 & p_value_Healthy_vs_Disease < 0.05 ~ "Up in Healthy",
      TRUE ~ "Not Significant"
    ),
    direction_Peri_vs_Perio = case_when(
      log2FC_Peri_vs_Perio > 0 & p_value_Peri_vs_Perio < 0.05 ~ "Up in Peri-implantitis",
      log2FC_Peri_vs_Perio < 0 & p_value_Peri_vs_Perio < 0.05 ~ "Up in Periodontitis",
      TRUE ~ "Not Significant"
    )
  )

# Filter out rows with NA p-values
results[is.na(results)==T]=1
color_palette <- c("Up in Disease" = "red", "Up in Healthy" = "blue", "Not Significant" = "gray")
# Volcano plot for Healthy vs. Disease
ggplot(results, aes(x = log2FC_Healthy_vs_Disease, y = -log10(p_value_Healthy_vs_Disease))) +
  geom_point(aes(color = direction_Healthy_vs_Disease), size = 3) +  # Color points by direction
  geom_text(
    aes(label = ifelse(direction_Healthy_vs_Disease != "Not Significant", CT, "")),  # Add text labels
    vjust = -0.5, hjust = 0.5, size = 3, color = "black"
  ) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +  # Significance threshold
  labs(
    x = "log2 Fold Change (Healthy vs. Disease)",
    y = "-log10(p-value)",
    title = "Volcano Plot: Healthy vs. Disease",
    color = "Direction"
  ) +
  theme_classic(base_size = 15)+xlim(-6,6)+  # Significance threshold
  scale_color_manual(values = color_palette)



color_palette <- c("Up in Disease" = "red", "Up in Healthy" = "blue", "Not Significant" = "gray")
# Volcano plot for Healthy vs. Disease
ggplot(results, aes(x = log2FC_Healthy_vs_Disease, y = -log10(p_value_Healthy_vs_Disease))) +
  geom_point(aes(color = direction_Healthy_vs_Disease), size = 3) +  # Color points by direction
  geom_text(
    aes(label = ifelse(direction_Healthy_vs_Disease != "Not Significant", CT, "")),  # Add text labels
    vjust = -0.5, hjust = 0.5, size = 3, color = "black"
  ) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +  # Significance threshold
  labs(
    x = "log2 Fold Change (Healthy vs. Disease)",
    y = "-log10(p-value)",
    title = "Volcano Plot: Healthy vs. Disease",
    color = "Direction"
  ) +
  theme_classic(base_size = 15)+xlim(-6,6)+  # Significance threshold
  scale_color_manual(values = color_palette)







write.csv(results,"wilcoxon_results_two_group.csv")









library(dplyr)

# Calculate the maximum x and y values for each sample
max_xy <- combined_data %>%
  group_by(Sample) %>%
  summarize(max_x = max(X), max_y = max(Y))

# Calculate cumulative offsets
max_xy <- max_xy %>%
  mutate(
    offset_x = cumsum(max_x + 10000), # Add a buffer of 100 (adjust as needed)
    offset_y = cumsum(max_y + 10000)  # Add a buffer of 100 (adjust as needed)
  )


# Add offsets to the combined_data
combined_data <- combined_data %>%
  left_join(max_xy, by = "Sample") %>%
  mutate(X = X + offset_x, Y = Y + offset_y) %>%
  select(-max_x, -max_y, -offset_x, -offset_y) # Remove temporary columns


combined_data[, 3] <- as.numeric(t(combined_data[, 3]))
combined_data[, 4] <- as.numeric(t(combined_data[, 4]))

# Write the data to a file
write.table(combined_data[, c(3, 4)], 
            file = "Quinn_v3_coordinate.txt", 
            sep = "\t", 
            row.names = FALSE, 
            col.names = FALSE)








#Now your 'combined_data' should have non-overlapping coordinates for each sample.

#Verify the changes (optional):
head(combined_data)
table(combined_data$Sample)




ggplot(combined_data, aes(X, Y, fill = as.factor(combined_data$TACIT))) + #Fixed this line
  geom_point(size = 0.1) +
  scale_fill_manual(values = my_colors, name = "Cell Type") + #Fixed this line
  theme_classic(base_size = 15) +
  labs(title = "Combine", x = "X", y = "Y") 




data_final=merge(metadata,combined_data,"Sample")
# Load the dplyr package for data manipulation
library(dplyr)
data_final_z <- data_final %>%
  group_by(Sample) %>%
  mutate(across(CD8:CD11c, ~ scale(.) %>% as.vector())) %>% # Adjust the range if necessary
  ungroup()





library(triwise)

data_ct=data.frame(CT=data_final_z$TACIT,Sample=data_final_z$Sample,Condition=data_final_z$Condition,data_final_z[,c(16,17,21,38)])
data_ct=data_ct[which(data_ct$CT=="CD38 VECs"),]
library(tidyverse)

data_ct_v2 <- data_ct %>%
  group_by(Sample,Condition) %>%
  summarise(
    Mean_Galectin.3 = mean(Galectin.3, na.rm = TRUE),
    Mean_HLA.A = mean(HLA.A, na.rm = TRUE),
    Mean_PD.L1 = mean(PD.L1, na.rm = TRUE),
    Mean_IFNG = mean(IFNG, na.rm = TRUE)
  )

data_ct_v2=data_ct_v2[which(data_ct_v2$Condition!="Healthy"),]

anova_results <- list(
  Galectin.3 = aov(Mean_Galectin.3 ~ Condition, data = data_ct_v2),
  HLA.A = aov(Mean_HLA.A ~ Condition, data = data_ct_v2),
  PD.L1 = aov(Mean_PD.L1 ~ Condition, data = data_ct_v2),
  IFNG = aov(Mean_IFNG ~ Condition, data = data_ct_v2)
)

# Summarize the ANOVA results
anova_summary <- lapply(anova_results, summary)


wilcox_results <- list(
  Galectin.3 = wilcox.test(Mean_Galectin.3 ~ Condition, data = data_ct_v2),
  HLA.A = wilcox.test(Mean_HLA.A ~ Condition, data = data_ct_v2),
  PD.L1 = wilcox.test(Mean_PD.L1 ~ Condition, data = data_ct_v2),
  IFNG = wilcox.test(Mean_IFNG ~ Condition, data = data_ct_v2)
)



# Reshape the data to wide format
data_wide <- data_ct %>%
  group_by(Condition) %>%
  summarise(
    Mean_Galectin.3 = mean(Galectin.3, na.rm = TRUE),
    Mean_HLA.A = mean(HLA.A, na.rm = TRUE),
    Mean_PD.L1 = mean(PD.L1, na.rm = TRUE),
    Mean_IFNG = mean(IFNG, na.rm = TRUE)
  )

# Prepare an empty data frame for log2FC results
log2fc_results <- data.frame(Condition = data_wide$Condition)

# Define conditions to compare
conditions <- colnames(data_wide)[-1]  # Exclude the Sample column

# Loop through each condition to calculate log2FC
for (cond in conditions) {
  # Calculate mean of other conditions
  other_conditions_mean <- rowMeans(dplyr::select(data_wide, -one_of(c("Condition", cond))), na.rm = TRUE)
  
  # Calculate log2FC for the current condition
  log2fc_values <- log2(data_wide[[cond]] / other_conditions_mean)
  
  # Add the result to the log2fc_results data frame
  log2fc_results[[paste0(cond)]] <- log2fc_values
}

# View the final log2FC results
print(log2fc_results)

log2fc_results=t(log2fc_results)
colnames(log2fc_results)=log2fc_results[1,]
log2fc_results=log2fc_results[-1,]

log2fc_results=as.data.frame(log2fc_results)
log2fc_results$Healthy=as.numeric(log2fc_results$Healthy)
log2fc_results$Periodontitis=as.numeric(log2fc_results$Periodontitis)
log2fc_results$`Peri-implantitis`=as.numeric(log2fc_results$`Peri-implantitis`)


#rownames(log2fc_results)=log2fc_results$Condition
#log2fc_results=log2fc_results[,-1]
# log2fc_results[is.na(log2fc_results)==T]=0
barycoords = transformBarycentric(log2fc_results)
str(barycoords)

Gdiffexp =c(3)
rownames(barycoords)
plotDotplot(barycoords, Gdiffexp = rownames(barycoords) ,Goi = Gdiffexp,showlabels = T)








# Check the resulting data frame
head(data_final_z)

library(pheatmap)

# Combine TACIT outcomes with relevant data columns
data_plot <- data.frame(TACIT = data_final_z$TACIT, data_final_z[,c("Pan.Cytokeratin","VIM","CD31","CD34","Collagen.IV","CD38","Podoplanin","CD45","HLA.DR","CD141","CD11c",
                        "CD20","CD3e","CD4","CD8","FOXP3","CD68","MPO","CD56")])


data_plot$TACIT=factor(data_plot$TACIT,levels = c("Epithelial","Fibroblast","VEC","CD38 VECs","VEC progenitors","LECs",
                                                  "DC cells","B cells","Tc","Th","Treg","Macrophage",
                                                  "Neutrophil","NK cells","Others"))
data_plot=data_plot[which(data_plot$TACIT!="Others"),]
# Calculate median values for each cell type in TACIT
mean_values_TACIT <- data_plot %>%
  group_by(TACIT) %>%
  summarise_all(~quantile(., 0.5)) %>%
  as.data.frame()

# Remove the 19th row and the "Others" cell type

rownames(mean_values_TACIT) <- mean_values_TACIT$TACIT
mean_values_TACIT <- mean_values_TACIT[, -1]

# Define color breaks and palette for heatmap
my.breaks <- c(seq(-3, 0, by = 0.1), seq(0.1, 3, by = 0.1))
my.colors <- c(colorRampPalette(colors = c("blue", "white"))(length(my.breaks) / 2),
               colorRampPalette(colors = c("white", "red"))(length(my.breaks) / 2))


# Scale mean values for heatmap visualization
aa <- scale(mean_values_TACIT)
rownames(aa)=rownames(mean_values_TACIT)
aa <- aa[, colSums(is.na(aa)) == 0]



# Generate heatmap
pheatmap(aa, cluster_cols = F,
         cluster_rows = F,  show_rownames  = TRUE,
         
         show_colnames = TRUE,
         fontsize_col = 12,
         fontsize_row = 12,breaks =my.breaks,color=my.colors)  # Adjust fontsize_row as needed to increase x-axis text size




data_final_z_sub=data_final_z[which(data_final_z$TACIT!="Others"),]

# Combine TACIT outcomes with relevant data columns
data_plot <- data.frame(TACIT = paste0(data_final_z_sub$Condition,"::",data_final_z_sub$TACIT), data_final_z_sub[,c("GZMB","Ki67","HLA.A","CD45RO",
                                                                  "PD.1","IDO1","ICOS",
                                                                    "IFNG")])

# Calculate median values for each cell type in TACIT
mean_values_TACIT <- data_plot %>%
  group_by(TACIT) %>%
  summarise_all(~quantile(., 0.5)) %>%
  as.data.frame()

# Remove the 19th row and the "Others" cell type

rownames(mean_values_TACIT) <- mean_values_TACIT$TACIT
mean_values_TACIT <- mean_values_TACIT[, -1]

# Define color breaks and palette for heatmap
my.breaks <- c(seq(-3, 0, by = 0.1), seq(0.1, 3, by = 0.1))
my.colors <- c(colorRampPalette(colors = c("blue", "white"))(length(my.breaks) / 2),
               colorRampPalette(colors = c("white", "red"))(length(my.breaks) / 2))


# Scale mean values for heatmap visualization
aa <- scale(mean_values_TACIT)
rownames(aa)=rownames(mean_values_TACIT)
aa <- aa[, colSums(is.na(aa)) == 0]



# Generate heatmap
pheatmap(aa, cluster_cols = T,
         cluster_rows = T,  show_rownames  = TRUE,
         
         show_colnames = TRUE,
         fontsize_col = 12,
         fontsize_row = 12,breaks =my.breaks,color=my.colors)  # Adjust fontsize_row as needed to increase x-axis text size


data_final_z_sub_sub=data_final_z_sub[which(data_final_z_sub$Condition!="Healthy"),]
data_plot <- data.frame(TACIT = paste0(data_final_z_sub$Condition,"::",data_final_z_sub$TACIT), data_final_z_sub[,c("GZMB","Ki67","HLA.A","CD45RO",
                                                                                                                    "PD.1","IDO1","ICOS",
                                                                                                                    "IFNG","Galectin.3")])


library(ggplot2)
library(dplyr)

# Example data
data_plot <- data_plot  # Your data
condition1 <- "Peri-implantitis"
condition2 <- "Periodontitis"

# Get unique cell types
cell_types <- unique(gsub(".*::", "", data_plot$TACIT))

# Initialize an empty dataframe to store results
all_results <- data.frame()

# Loop through all cell types and calculate log2FC and p-values
for (cell_type in cell_types) {
  # Subset data for the cell state and conditions
  subset_data <- data_plot %>%
    filter(grepl(paste0(cell_type, "$"), TACIT)) %>%
    mutate(Condition = ifelse(grepl(condition1, TACIT), condition1, condition2))
  
  # Remove columns that are not genes (e.g., TACIT and Condition)
  gene_columns <- colnames(subset_data)[!colnames(subset_data) %in% c("TACIT", "Condition")]
  
  # Perform t-tests and calculate log2 fold change
  results <- lapply(gene_columns, function(gene) {
    condition1_data <- subset_data[subset_data$Condition == condition1, gene]
    condition2_data <- subset_data[subset_data$Condition == condition2, gene]
    
    # Log2 fold change
    log2FC <- mean(condition1_data, na.rm = TRUE) - mean(condition2_data, na.rm = TRUE)
    
    # Determine which condition has higher expression
    higher_condition <- ifelse(log2FC > 0, condition1, condition2)
    
    # t-test
    t_test <- t.test(condition1_data, condition2_data)
    p_value <- t_test$p.value
    
    data.frame(CellType = cell_type, Gene = gene, Log2FC = log2FC, p_value = p_value, HigherCondition = higher_condition)
  })
  
  # Combine results into a dataframe
  results_df <- do.call(rbind, results)
  
  # Append to the all_results dataframe
  all_results <- rbind(all_results, results_df)
}

# Adjust p-values for multiple testing (optional)
all_results$p_adj <- p.adjust(all_results$p_value, method = "BH")

# Add significance column
all_results$Significance <- ifelse(all_results$p_adj < 0.05, "Significant", "Not Significant")

# Add a column for text labels (only for significant genes)
all_results$Label <- ifelse(all_results$Significance == "Significant", all_results$Gene, NA)

all_results$CellType_CS=paste0(all_results$CellType,"::",all_results$Gene,sep="")
all_results_sub=all_results[which(all_results$CellType%in%c("Th","Tc","B cells","NK cells","Macrophage","Neutrophil","Treg","DC cells")),]

# Volcano plot
volcano_plot <- ggplot(all_results_sub, aes(x = Log2FC, y = -log10(p_adj), color = HigherCondition, label = Label)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(label = CellType_CS), hjust = 1.1, vjust = 1.1, size = 3, color = "black", check_overlap = TRUE) +
  labs(title = paste("Volcano Plot: All Cell Types in", condition1, "vs", condition2),
       x = "Log2 Fold Change",
       y = "-log10(Adjusted p-value)",
       color = "Higher Expression in") +
  theme_classic(base_size = 15) +
  scale_color_manual(values = c("Peri-implantitis" = "blue", "Periodontitis" = "red"))

# Display the volcano plot
print(volcano_plot)

























data_plot=data.frame(Group=data_final$Sample,X=data_final$X,Y=data_final$Y,CellType=data_final$TACIT)



library(dplyr)
library(tidyr)

# Function to calculate Euclidean distance
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# Initialize an empty data frame to store results
results <- data.frame()

# List of cell types to calculate distance to
cell_types <- c("B cells", "DC cells", "Epithelial", "Macrophage", "Tc", "Th", "Treg")

# Loop through each group
for (group in unique(data_plot$Group)) {
  # Filter data for the current group
  group_data <- data_plot %>% filter(Group == group)
  
  # Filter CD38 VECs for the current group
  cd38_vecs <- group_data %>% filter(CellType == "CD38 VECs")
  
  # Loop through each CD38 VEC
  for (i in 1:nrow(cd38_vecs)) {
    cd38_x <- cd38_vecs$X[i]
    cd38_y <- cd38_vecs$Y[i]
    
    # Initialize a vector to store distances
    distances <- numeric(length(cell_types))
    
    # Loop through each cell type
    for (j in seq_along(cell_types)) {
      # Filter data for the current cell type
      cell_type_data <- group_data %>% filter(CellType == cell_types[j])
      
      # Calculate distances to all cells of the current type
      dist <- euclidean_distance(cd38_x, cd38_y, cell_type_data$X, cell_type_data$Y)
      
      # Find the minimum distance
      distances[j] <- min(dist)
    }
    
    # Add the results to the data frame
    results <- rbind(results, data.frame(
      Group = group,
      CD38_VEC_X = cd38_x,
      CD38_VEC_Y = cd38_y,
      CellType = cell_types,
      Distance = distances
    ))
  }
  print(group)
}

# View the results
print(results)
results=results[which(results$Distance!="Inf"),]
colnames(results)[1]="Sample"
results=merge(results,metadata,"Sample")



ggplot(results, aes(x = Condition, y = Distance, fill = Condition)) +
  geom_boxplot() +  # Create boxplots
  facet_wrap(~ CellType, scales = "free_y") +  # Facet by CellType
  stat_compare_means(
    method = "t.test",  # Use t-test for pairwise comparisons
    comparisons = list(c("Healthy", "Periodontitis"), c("Healthy", "Peri-implantitis"), c("Periodontitis", "Peri-implantitis")),  # Specify comparisons
    label = "p.signif",  # Display p-values (e.g., 0.012)
    size = 4,             # Adjust label size
    step.increase = 0.1
  ) +
  labs(
    x = "Condition",
    y = "Distance",
    title = "Distance to CD38 VECs by Condition and Cell Type",
    fill = "Condition"
  ) +
  theme_classic(base_size = 15) +
  theme(
    strip.text = element_text(size = 10, face = "bold"),  # Customize facet labels
    axis.text.x = element_text(angle = 45, hjust = 1)      # Rotate x-axis labels
  )




# Function to calculate pairwise p-values for a given CellType
calculate_pairwise_pvalues <- function(data, cell_type) {
  # Filter data for the current CellType
  cell_data <- data %>% filter(CellType == cell_type)
  
  # Perform pairwise t-tests
  p_values <- pairwise.t.test(cell_data$Distance, cell_data$Condition, p.adjust.method = "none")
  
  # Convert the p-value matrix to a data frame
  p_values_df <- as.data.frame(p_values$p.value)
  
  # Add CellType column
  p_values_df <- p_values_df %>%
    mutate(CellType = cell_type) %>%
    select(CellType, everything())
  
  return(p_values_df)
}

# List of CellTypes
cell_types <- unique(results$CellType)

# Calculate pairwise p-values for each CellType
p_value_table <- lapply(cell_types, function(cell_type) {
  calculate_pairwise_pvalues(results, cell_type)
}) %>%
  bind_rows()  # Combine results into a single data frame

# View the p-value table
print(p_value_table)

write.csv(results,"ttest_results_distance_CD38_two_group.csv")










data_plot=data.frame(Group=data_final$Sample,X=data_final$X,Y=data_final$Y,CellType=data_final$TACIT)



library(dplyr)
library(tidyr)

# Function to calculate Euclidean distance
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# Initialize an empty data frame to store results
results <- data.frame()

# List of cell types to calculate distance to
cell_types <- c("B cells", "Th")
list_group=unique(data_plot$Group)
list_group=list_group[-c(8,9)]
# Loop through each group
for (group in list_group) {
  # Filter data for the current group
  group_data <- data_plot %>% filter(Group == group)
  
  # Filter CD38 VECs for the current group
  cd38_vecs <- group_data %>% filter(CellType == "Tc")
  
  # Loop through each CD38 VEC
  for (i in 1:nrow(cd38_vecs)) {
    cd38_x <- cd38_vecs$X[i]
    cd38_y <- cd38_vecs$Y[i]
    
    # Initialize a vector to store distances
    distances <- numeric(length(cell_types))
    
    # Loop through each cell type
    for (j in seq_along(cell_types)) {
      # Filter data for the current cell type
      cell_type_data <- group_data %>% filter(CellType == cell_types[j])
      
      # Calculate distances to all cells of the current type
      dist <- euclidean_distance(cd38_x, cd38_y, cell_type_data$X, cell_type_data$Y)
      
      # Find the minimum distance
      distances[j] <- min(dist)
    }
    
    # Add the results to the data frame
    results <- rbind(results, data.frame(
      Group = group,
      CD38_VEC_X = cd38_x,
      CD38_VEC_Y = cd38_y,
      CellType = cell_types,
      Distance = distances
    ))
  }
  print(group)
}

# View the results
print(results)
results=results[which(results$Distance!="Inf"),]
colnames(results)[1]="Sample"
results=merge(results,metadata,"Sample")



ggplot(results, aes(x = Condition, y = Distance, fill = Condition)) +
  geom_boxplot() +  # Create boxplots
  facet_wrap(~ CellType, scales = "free_y") +  # Facet by CellType
  stat_compare_means(
    method = "t.test",  # Use t-test for pairwise comparisons
    comparisons = list(c("Healthy", "Periodontitis"), c("Healthy", "Peri-implantitis"), c("Periodontitis", "Peri-implantitis")),  # Specify comparisons
    label = "p.signif",  # Display p-values (e.g., 0.012)
    size = 4,             # Adjust label size
    step.increase = 0.1
  ) +
  labs(
    x = "Condition",
    y = "Distance",
    title = "Distance to Tc by Condition and Cell Type",
    fill = "Condition"
  ) +
  theme_classic(base_size = 15) +
  theme(
    strip.text = element_text(size = 10, face = "bold"),  # Customize facet labels
    axis.text.x = element_text(angle = 45, hjust = 1)      # Rotate x-axis labels
  )




# Function to calculate pairwise p-values for a given CellType
calculate_pairwise_pvalues <- function(data, cell_type) {
  # Filter data for the current CellType
  cell_data <- data %>% filter(CellType == cell_type)
  
  # Perform pairwise t-tests
  p_values <- pairwise.t.test(cell_data$Distance, cell_data$Condition, p.adjust.method = "none")
  
  # Convert the p-value matrix to a data frame
  p_values_df <- as.data.frame(p_values$p.value)
  
  # Add CellType column
  p_values_df <- p_values_df %>%
    mutate(CellType = cell_type) %>%
    select(CellType, everything())
  
  return(p_values_df)
}

# List of CellTypes
cell_types <- unique(results$CellType)

# Calculate pairwise p-values for each CellType
p_value_table <- lapply(cell_types, function(cell_type) {
  calculate_pairwise_pvalues(results, cell_type)
}) %>%
  bind_rows()  # Combine results into a single data frame

# View the p-value table
print(p_value_table)

write.csv(results,"ttest_results_distance_Tc_two_group.csv")











data_plot=data.frame(Group=data_final$Sample,X=data_final$X,Y=data_final$Y,CellType=data_final$TACIT)



library(dplyr)
library(tidyr)

# Function to calculate Euclidean distance
euclidean_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# Initialize an empty data frame to store results
results <- data.frame()

# List of cell types to calculate distance to
cell_types <- c("B cells", "Tc")
list_group=unique(data_plot$Group)
list_group=list_group[-c(3,9)]
# Loop through each group
for (group in list_group) {
  # Filter data for the current group
  group_data <- data_plot %>% filter(Group == group)
  
  # Filter CD38 VECs for the current group
  cd38_vecs <- group_data %>% filter(CellType == "Th")
  
  # Loop through each CD38 VEC
  for (i in 1:nrow(cd38_vecs)) {
    cd38_x <- cd38_vecs$X[i]
    cd38_y <- cd38_vecs$Y[i]
    
    # Initialize a vector to store distances
    distances <- numeric(length(cell_types))
    
    # Loop through each cell type
    for (j in seq_along(cell_types)) {
      # Filter data for the current cell type
      cell_type_data <- group_data %>% filter(CellType == cell_types[j])
      
      # Calculate distances to all cells of the current type
      dist <- euclidean_distance(cd38_x, cd38_y, cell_type_data$X, cell_type_data$Y)
      
      # Find the minimum distance
      distances[j] <- min(dist)
    }
    
    # Add the results to the data frame
    results <- rbind(results, data.frame(
      Group = group,
      CD38_VEC_X = cd38_x,
      CD38_VEC_Y = cd38_y,
      CellType = cell_types,
      Distance = distances
    ))
  }
  print(group)
}

# View the results
print(results)
results=results[which(results$Distance!="Inf"),]
colnames(results)[1]="Sample"
results=merge(results,metadata,"Sample")



ggplot(results, aes(x = Condition, y = Distance, fill = Condition)) +
  geom_boxplot() +  # Create boxplots
  facet_wrap(~ CellType, scales = "free_y") +  # Facet by CellType
  stat_compare_means(
    method = "t.test",  # Use t-test for pairwise comparisons
    comparisons = list(c("Healthy", "Periodontitis"), c("Healthy", "Peri-implantitis"), c("Periodontitis", "Peri-implantitis")),  # Specify comparisons
    label = "p.signif",  # Display p-values (e.g., 0.012)
    size = 4,             # Adjust label size
    step.increase = 0.1
  ) +
  labs(
    x = "Condition",
    y = "Distance",
    title = "Distance to Tc by Condition and Cell Type",
    fill = "Condition"
  ) +
  theme_classic(base_size = 15) +
  theme(
    strip.text = element_text(size = 10, face = "bold"),  # Customize facet labels
    axis.text.x = element_text(angle = 45, hjust = 1)      # Rotate x-axis labels
  )




# Function to calculate pairwise p-values for a given CellType
calculate_pairwise_pvalues <- function(data, cell_type) {
  # Filter data for the current CellType
  cell_data <- data %>% filter(CellType == cell_type)
  
  # Perform pairwise t-tests
  p_values <- pairwise.t.test(cell_data$Distance, cell_data$Condition, p.adjust.method = "none")
  
  # Convert the p-value matrix to a data frame
  p_values_df <- as.data.frame(p_values$p.value)
  
  # Add CellType column
  p_values_df <- p_values_df %>%
    mutate(CellType = cell_type) %>%
    select(CellType, everything())
  
  return(p_values_df)
}

# List of CellTypes
cell_types <- unique(results$CellType)

# Calculate pairwise p-values for each CellType
p_value_table <- lapply(cell_types, function(cell_type) {
  calculate_pairwise_pvalues(results, cell_type)
}) %>%
  bind_rows()  # Combine results into a single data frame

# View the p-value table
print(p_value_table)

write.csv(results,"ttest_results_distance_Tc_two_group.csv")


















##################################################################################
###################################Neighborhood###################################
##################################################################################

data_final=read.delim("C:/Users/huynhk4/Downloads/Quinn_Paper_2/combined_data_v3.txt")
data_final=data_final[,-c(1,2)]
data_final$ID=paste0(data_final$X,"_",data_final$Y,sep="")

neighbor_df=read_csv("C:/Users/huynhk4/Downloads/final_result_table (19).csv")
neighbor_df=neighbor_df[which(neighbor_df$TCN!="15"),]
neighbor_df$x_coordinate=as.numeric(neighbor_df$x_coordinate)
neighbor_df$y_coordinate=as.numeric(neighbor_df$y_coordinate)
neighbor_df$ID=paste0(neighbor_df$x_coordinate,"_",neighbor_df$y_coordinate,sep="")


data_final_TCN_CT=merge(data_final,neighbor_df,"ID")


neighbor_colors <- c(
  "1"="#8B201A",
  "2"="#D86170",
  "3"="#D2AEB2",
  "4"="#B59E88",
  "5"="#CFC1C0",
  "6"="#CCEBC5",
  "7"="#375614",
  "8"="#BDA110",
  "9"="#4A90E2",
  "10"="#5EAA97",
  "11"="#365880",
  "12"="#7592B4",
  "13"="#B9CCCC",
  "14"="#928337",
  "15"="#E4DB7A",
  "16"="#F5CBA7",
  "17"="#AED6F1",
  "18"="#D5F5E3",
  "19"="#F9E79F",
  "20"="#E8DAEF",
  "0"="#AED6F8"
)



data_plot=data.frame(CT=neighbor_df$Cell_Type,
                     TCN=neighbor_df$TCN)
data_plot=data_plot[which(data_plot$CT!="9 Others"),]
library(dplyr)
library(tidyr)
library(pheatmap)

# Summarize counts for each CT by TCN
data_summary <- data_plot %>%
  group_by(CT, TCN) %>%
  summarise(count = n(), .groups = 'drop')

# Create a wide format data frame for the heatmap
data_wide <- data_summary %>%
  pivot_wider(names_from = TCN, values_from = count, values_fill = list(count = 0))

# Convert to matrix and calculate proportions
data_matrix <- as.matrix(data_wide[,-1])  # Removing the CT column for matrix conversion
row.names(data_matrix) <- data_wide$CT

# Normalize by row sums to calculate proportions
data_matrix_prop <- sweep(data_matrix, 1, rowSums(data_matrix), FUN = "/")

# Create heatmap with annotations for the counts
pheatmap(data_matrix_prop,
         main = "Proportion of Cell Types in Each TCN",
         display_numbers = round(data_matrix_prop, 2),  # Display counts
         fontsize_number = 10,
         color = colorRampPalette(c("white", "blue"))(50),  # Adjust color palette
         cellwidth = 30,
         cellheight = 10)



colnames(data_final_TCN_CT)
colnames(data_final_TCN_CT)[45]="TACIT_TCN"


data_final=merge(metadata,data_final_TCN_CT,"Sample")
data_final_TCN_CT_unique <- data_final %>%
  distinct(ID, .keep_all = TRUE)  # Keeps all other columns


library(dplyr)
library(tidyr)


data_plot=data.frame(CT=data_final_TCN_CT_unique$Condition,
                     TCN=data_final_TCN_CT_unique$TACIT_TCN)


# Summarize total cell counts by CT and TCN
data_summary <- data_plot %>%
  group_by(CT, TCN) %>%
  summarise(total_cells = n(), .groups = 'drop') %>%
  group_by(TCN) %>%
  mutate(total_by_tcn = sum(total_cells)) %>%
  ungroup() %>%
  mutate(proportion = total_cells / total_by_tcn)  # Calculate proportion

# View summarized data
print(data_summary)



# Create a visual comparison
library(ggplot2)

# Plotting the proportions of TCN for each CT group
ggplot(data_summary, aes(x = factor(TCN), y = proportion, fill = CT)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of TCN by Cell Type",
       x = "Total Cell Number (TCN)",
       y = "Proportion",
       fill = "Cell Type") +
  theme_classic(base_size = 15)



data_final_plot=data.frame(SjD=data_final_TCN_CT_unique$Condition,Cluster=data_final_TCN_CT_unique$TACIT_TCN)


# Calculate the proportion of each Cluster within each SjD group
data_final_plot_prop <- data_final_plot %>%
  group_by(SjD, Cluster) %>%
  summarise(count = n()) %>%
  group_by(SjD) %>%
  mutate(prop = count / sum(count))

# Plot the stacked bar plot of proportions
ggplot(data_final_plot_prop, aes(x = as.factor(SjD), y = prop, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "", y = "Proportion", fill = "TCN") +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_manual(values = neighbor_colors)


data_final_plot=data.frame(SjD=data_final_TCN_CT_unique$Sample,Cluster=data_final_TCN_CT_unique$TACIT_TCN)


# Calculate the proportion of each Cluster within each SjD group
data_final_plot_prop <- data_final_plot %>%
  group_by(SjD, Cluster) %>%
  summarise(count = n()) %>%
  group_by(SjD) %>%
  mutate(prop = count / sum(count))

# Plot the stacked bar plot of proportions
ggplot(data_final_plot_prop, aes(x = as.factor(SjD), y = prop, fill = as.factor(Cluster))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "", y = "Proportion", fill = "TCN") +
  theme_classic(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_fill_manual(values = neighbor_colors)




library(pheatmap)
library(reshape2)

# Step 1: Count occurrences of each cell type in each group
cell_counts <- as.data.frame(table(data_plot$TCN, data_plot$CT))

# Step 2: Calculate proportions
cell_counts$Proportion <- ave(cell_counts$Freq, cell_counts$Var2, FUN = function(x) x / sum(x))

# Step 3: Reshape the data for heatmap
heatmap_data <- dcast(cell_counts, Var1 ~ Var2, value.var = "Proportion", fill = 0)
colnames(heatmap_data) <- c("Cell_Type", "Healthy", "Peri-implantitis", "Periodontitis")  # Rename columns for clarity

# Set row names
row.names(heatmap_data) <- heatmap_data$Cell_Type
heatmap_data$Cell_Type <- NULL  # Remove Cell_Type column now that it is the row names

# Step 4: Create the heatmap with pheatmap
pheatmap(scale(t(heatmap_data)),
         display_numbers = F,  # optional: display the proportion numbers in the cells
         cluster_rows = TRUE,      # optional: cluster rows
         cluster_cols = TRUE,      # optional: cluster columns
         main = "Proportions of Cell Types",  # Title for the heatmap
         color = colorRampPalette(c("skyblue", "red"))(1000),  # Color gradient
         border_color = NA,
         fontsize_col = 15,
         fontsize_row = 15
)










data_plot=data.frame(Group=data_final_TCN_CT_unique$Sample,
                     TCN=data_final_TCN_CT_unique$TACIT_TCN,
                     Meta=data_final_TCN_CT_unique$Condition)
data_plot$Meta=ifelse(data_plot$Meta=="Healthy","Healthy","Disease")


total_cells <- data_plot %>%
  group_by(Group) %>%
  summarize(Total_Cells = sum(TCN)) 

# Step 2: Calculate proportion of TCN for each patient
data_plot <- data_plot %>%
  left_join(total_cells, by = "Group") %>%
  mutate(Proportion_TCN = TCN / Total_Cells)

# View the data for confirmation
head(data_plot)

# Step 3: Create boxplot comparing healthy vs disease
ggplot(data_plot, aes(x = Meta, y = Proportion_TCN, fill = Meta)) +
  geom_boxplot() +
  labs(title = "Proportion of TCN Condition",
       x = "Group",
       y = "Proportion of TCN") +
  scale_fill_manual(values = c("Healthy" = "lightblue", "Disease" = "salmon")) +
  theme_classic(base_size = 15)+
  stat_compare_means(
    method = "wilcox.test",
    label = "p.signif",
    comparisons = list(c("Healthy", "Disease"))
  )+facet_wrap(~ TCN, scales = "free_y")












data_final_TCN_CT_unique

data_final_z <- data_final_TCN_CT_unique %>%
  group_by(Sample) %>%
  mutate(across(FOXP3:CD11c, ~ scale(.) %>% as.vector())) %>% # Adjust the range if necessary
  ungroup()

data_final_z_sub=data_final_z[which(data_final_z$TACIT!="Others"),]

table(data_final_z$TACIT_TCN)


data_final_z <- data_final_z  # Your data

# Create a mapping from unique numeric values to sequential letters
unique_values <- sort(unique(data_final_z$TACIT_TCN))  # Get unique numeric values
letter_mapping <- setNames(LETTERS[1:length(unique_values)], unique_values)  # Map to letters

# Replace numeric values with letters
data_final_z$TACIT_TCN <- letter_mapping[as.character(data_final_z$TACIT_TCN)]

table(data_final_z$TACIT_TCN)
data_final_z_sub=data_final_z[which(data_final_z$TACIT!="Others"),]
data_final_z_sub_sub=data_final_z_sub[which(data_final_z_sub$Condition!="Healthy"),]
data_plot <- data.frame(TACIT = paste0(data_final_z_sub_sub$Condition,"::",data_final_z_sub_sub$TACIT_TCN), data_final_z_sub_sub[,c("GZMB","Ki67","Galectin.3","HLA.A","CD45RO",
                                                                                                                                "PD.L1","PD.1","IDO1","CD107a","ICOS","CD38",
                                                                                                                                "IFNG")])

library(ggplot2)
library(dplyr)

# Example data
data_plot <- data_plot  # Your data
condition1 <- "Peri-implantitis"
condition2 <- "Periodontitis"

# Get unique cell types
cell_types <- unique(gsub(".*::", "", data_plot$TACIT))

# Initialize an empty dataframe to store results
all_results <- data.frame()

# Loop through all cell types and calculate log2FC and p-values
for (cell_type in cell_types) {
  # Subset data for the cell state and conditions
  subset_data <- data_plot %>%
    filter(grepl(paste0(cell_type, "$"), TACIT)) %>%
    mutate(Condition = ifelse(grepl(condition1, TACIT), condition1, condition2))
  
  # Remove columns that are not genes (e.g., TACIT and Condition)
  gene_columns <- colnames(subset_data)[!colnames(subset_data) %in% c("TACIT", "Condition")]
  
  # Perform t-tests and calculate log2 fold change
  results <- lapply(gene_columns, function(gene) {
    condition1_data <- subset_data[subset_data$Condition == condition1, gene]
    condition2_data <- subset_data[subset_data$Condition == condition2, gene]
    
    # Log2 fold change
    log2FC <- mean(condition1_data, na.rm = TRUE) - mean(condition2_data, na.rm = TRUE)
    
    # Determine which condition has higher expression
    higher_condition <- ifelse(log2FC > 0, condition1, condition2)
    
    # t-test
    t_test <- t.test(condition1_data, condition2_data)
    p_value <- t_test$p.value
    
    data.frame(CellType = cell_type, Gene = gene, Log2FC = log2FC, p_value = p_value, HigherCondition = higher_condition)
  })
  
  # Combine results into a dataframe
  results_df <- do.call(rbind, results)
  
  # Append to the all_results dataframe
  all_results <- rbind(all_results, results_df)
}

# Adjust p-values for multiple testing (optional)
all_results$p_adj <- p.adjust(all_results$p_value, method = "BH")

# Add significance column
all_results$Significance <- ifelse(all_results$p_adj < 0.05, "Significant", "Not Significant")

# Add a column for text labels (only for significant genes)
all_results$Label <- ifelse(all_results$Significance == "Significant", all_results$Gene, NA)

all_results$CellType_CS=paste0(all_results$CellType,"::",all_results$Gene,sep="")
#all_results_sub=all_results[which(all_results$CellType%in%c("Th","Tc","B cells","NK cells","Macrophage","Neutrophil","Treg","DC cells")),]


all_results

all_results

# Volcano plot
volcano_plot <- ggplot(all_results, aes(x = Log2FC, y = -log10(p_adj), color = HigherCondition, label = Label)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(label = CellType_CS), hjust = 1.1, vjust = 1.1, size = 3, color = "black", check_overlap = TRUE) +
  labs(title = paste("Volcano Plot: All Cell Types in", condition1, "vs", condition2),
       x = "Log2 Fold Change",
       y = "-log10(Adjusted p-value)",
       color = "Higher Expression in") +
  theme_classic(base_size = 15) +
  scale_color_manual(values = c("Peri-implantitis" = "blue", "Periodontitis" = "red"))

# Display the volcano plot
print(volcano_plot)

















library(tidyr)
library(dplyr)

# Assuming proportions_meta data frame is available (from previous responses)

results_list <- list()

for (tcn in unique(proportions_meta$TCN)) {
  subset_data <- subset(proportions_meta, TCN == tcn)
  
  unique_meta <- unique(subset_data$Meta)
  
  if (length(unique_meta) >= 2) { #Check for at least two unique Meta categories
    meta_pairs <- combn(unique_meta, 2, simplify = FALSE)
    pairwise_results <- lapply(meta_pairs, function(pair) {
      group1_data <- subset(subset_data, Meta == pair[1])$proportion
      group2_data <- subset(subset_data, Meta == pair[2])$proportion
      wilcox_test_result <- wilcox.test(group1_data, group2_data)
      data.frame(
        TCN = tcn,
        Group1 = pair[1],
        Group2 = pair[2],
        p.value = wilcox_test_result$p.value
      )
    })
    results_list[[as.character(tcn)]] <- do.call(rbind, pairwise_results)
  } else {
    #Handle the case where there is only one Meta category
    results_list[[as.character(tcn)]] <- data.frame(TCN = tcn, Group1 = unique_meta, Group2 = NA, p.value = NA)
  }
}
# Combine results for all TCNs
final_results <- do.call(rbind, results_list)

print(final_results)






for (Tissue in unique(data_final_TCN_CT_unique$Sample)) { # Use unique to avoid duplicates
  # Load the data
  data <- data_final_TCN_CT_unique[data_final_TCN_CT_unique$Sample == Tissue, ]
  data$TACIT=ifelse(data$TACIT%in%names(which(table(data$TACIT)==1)),"Others",data$TACIT)
  
  # Create the Voronoi diagram.  Using fill and a proper scale
  plot <- ggplot(data, aes(X, Y, fill = as.factor(data$TACIT_TCN))) + #Fixed this line
    geom_voronoi_tile(size = 0.1, color = "black", max.radius = 10) +
    scale_fill_manual(values = neighbor_colors, name = "Cell Type") + #Fixed this line
    theme_classic(base_size = 15) +
    labs(title = Tissue, x = "X", y = "Y") 
  # Export the plot as an SVG file with error handling
  svg_filename <- paste0("C:/Users/huynhk4/Downloads/Quinn_v3/voronoi/TCN_",  gsub("\\.csv$", "", Tissue), ".svg")
  tryCatch({
    ggsave(svg_filename, plot = plot, device = "svg", width = 15, height = 10)
    cat("Saved:", svg_filename, "\n")
  }, error = function(e) {
    cat("Error saving", svg_filename, ":", e$message, "\n")
  })
}

data_plot=data.frame(Group=data_final_TCN_CT_unique$Sample,
                     TCN=data_final_TCN_CT_unique$TACIT_TCN,
                     X=data_final_TCN_CT_unique$X,
                     Y=data_final_TCN_CT_unique$Y)



library(geometry)
library(splancs)
# Function to calculate area using convex hull
calculate_area <- function(x, y) {
  # Create a matrix of coordinates
  coords <- cbind(x, y)
  
  # Check if there are at least 3 points to compute a convex hull
  if (nrow(coords) < 3) {
    return(NA)  # Return NA if there are fewer than 3 points
  }
  
  # Calculate the convex hull
  hull <- chull(coords)
  
  # Extract the vertices of the convex hull
  hull_vertices <- coords[hull, , drop = FALSE]  # Ensure it's a matrix
  
  # Calculate the area of the convex hull
  area <- areapl(hull_vertices)  # Use areapl from splancs package
  
  return(area)
}

# Calculate the area for each TCN and Group
results <- data_plot %>%
  group_by(Group, TCN) %>%
  summarize(Area = calculate_area(X, Y), .groups = "drop")

# View the results
print(results)

colnames(results)[1]="Sample"

final_results=merge(results,metadata,"Sample")
ggplot(final_results, aes(x = as.factor(Condition), y = Area, fill = as.factor(TCN))) +
  geom_bar(stat = "identity", position = "dodge") +  # Grouped bar plot
  labs(
    x = "TCN",
    y = "Area",
    title = "Area for Each TCN Grouped by Condition",
    fill = "Condition"
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )+
  scale_fill_manual(values = neighbor_colors, name = "TCN")

final_results=merge(results,metadata,"Sample")
final_results <- final_results %>%
  group_by(Condition) %>%
  mutate(Total_Area = sum(Area, na.rm = TRUE)) %>%  # Ensure NA values are ignored
  ungroup() %>%
  mutate(Proportion = Area / Total_Area)

ggplot(final_results, aes(x = Condition, y = Proportion, fill = as.factor(TCN))) +
  geom_bar(stat = "identity") +  # Stacked bar plot
  labs(
    x = "Sample",
    y = "Proportion of Area",
    title = "Proportion of TCN by Area for Each Sample",
    fill = "TCN"
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )+
  scale_fill_manual(values = neighbor_colors, name = "TCN") 


final_results=merge(results,metadata,"Sample")
final_results <- final_results %>%
  group_by(Sample) %>%
  mutate(Total_Area = sum(Area)) %>%  # Calculate total area for each Condition
  ungroup() %>%
  mutate(Proportion = Area / Total_Area)

ggplot(final_results, aes(x = Sample, y = Proportion, fill = as.factor(TCN))) +
  geom_bar(stat = "identity") +  # Stacked bar plot
  labs(
    x = "Condition",
    y = "Proportion of Area",
    title = "Proportion of TCN by Area for Each Condition",
    fill = "TCN"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )

heatmap_data <- final_results %>%
  group_by(Condition, TCN) %>%
  summarize(Mean_Proportion = mean(Proportion, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = TCN, values_from = Mean_Proportion)  # Reshape for pheatmap

# Convert to a matrix (required by pheatmap)
heatmap_matrix <- as.matrix(heatmap_data[,-1])
rownames(heatmap_matrix) <- heatmap_data$Condition

# View the matrix
print(heatmap_matrix)

pheatmap(
  scale(heatmap_matrix),
  color = colorRampPalette(c("skyblue", "red"))(100),  # Set color gradient
  cluster_rows = T,  # Do not cluster rows
  cluster_cols = T,   # Do not cluster columns
  main = "Mean Proportion of TCN by Condition",
  show_rownames = TRUE,   # Show row names (Conditions)
  show_colnames = TRUE,   # Show column names (TCNs)
  fontsize_row = 10,      # Adjust row font size
  fontsize_col = 10       # Adjust column font size
)





data_plot=data.frame(Group=data_final_TCN_CT_unique$Sample,
                     TCN=data_final_TCN_CT_unique$TACIT_TCN,
                     Meta=data_final_TCN_CT_unique$Condition,
                     X=data_final_TCN_CT_unique$X,
                     Y=data_final_TCN_CT_unique$Y,
                     cell_id=1:nrow(data_final_TCN_CT_unique))
data_plot=data_plot[which(data_plot$Group=="XPN88VJQ_TACIT"),]

library(sf)
# Convert data to an sf object without a CRS (non-geographic)
data_sub_sf <- st_as_sf(data_plot, coords = c("X", "Y"), crs = NA)
data_plot$cell_id <- data_plot$cell_id

# Function to create a grid of windows
create_windows <- function(data, window_size, step_size) {
  bbox <- st_bbox(data)
  x_breaks <- seq(bbox["xmin"], bbox["xmax"] - window_size, by = step_size)
  y_breaks <- seq(bbox["ymin"], bbox["ymax"] - window_size, by = step_size)
  return(expand.grid(x = x_breaks, y = y_breaks))
}

# Function to find unique neighbors
unique_neighbors <- function(results) {
  results %>%
    distinct() %>%
    rowwise() %>%
    mutate(neighbors = list(unique(neighbors)))
}

# Function to find neighbors in a sliding window
find_neighbors_in_window <- function(window, data, window_size, distance_cutoff) {
  xmin <- window$x
  xmax <- window$x + window_size
  ymin <- window$y
  ymax <- window$y + window_size
  
  window_data <- data %>%
    filter(st_coordinates(.)[,1] >= xmin & st_coordinates(.)[,1] < xmax &
             st_coordinates(.)[,2] >= ymin & st_coordinates(.)[,2] < ymax)
  
  if (nrow(window_data) < 2) {
    return(data.frame(cell_id = character(0), neighbors = I(list())))
  }
  
  distances <- st_distance(window_data)
  diag(distances) <- NA
  distances[distances > distance_cutoff] <- NA
  
  neighbors <- lapply(seq_len(nrow(window_data)), function(i) {
    near <- which(!is.na(distances[i, ]))
    if (length(near) > 0) as.character(window_data$cell_id[near]) else NA
  })
  
  return(data.frame(cell_id = window_data$cell_id, neighbors = I(neighbors)))
}

# Parameters
window_size <- 500  # Size of the window
step_size <- 480  # Step size for sliding window
distance_cutoff <- 20  # Distance cutoff for neighbors

# Function to process each group
process_group <- function(group_data) {
  # Create windows for the group
  windows <- create_windows(group_data, window_size, step_size)
  
  # Find neighbors within each window and combine results
  results <- do.call(rbind, lapply(seq_len(nrow(windows)), function(i) {
    find_neighbors_in_window(windows[i, ], group_data, window_size, distance_cutoff)
  }))
  
  # Remove duplicates and group neighbors
  results <- unique_neighbors(results)
  
  # Combine results by cell_id
  results_df <- results %>%
    group_by(cell_id) %>%
    summarise(neighbors = list(unique(unlist(neighbors))))
  
  return(results_df)
}

# Apply the process to each group and combine results
all_results <- data_sub_sf  %>%
  group_map(~ process_group(.x)) %>%
  bind_rows()

# Display combined results
print(all_results)


motif_49_gland1=all_results


# Get unique group names
group_names <- unique(data_final_TCN_CT_unique$Sample)

# Loop through each group and assign to variables
for (group_name in group_names) {
  data_plot=data.frame(Group=data_final_TCN_CT_unique$Sample,
                       TCN=data_final_TCN_CT_unique$TACIT_TCN,
                       Meta=data_final_TCN_CT_unique$Condition,
                       X=data_final_TCN_CT_unique$X,
                       Y=data_final_TCN_CT_unique$Y,
                       cell_id=1:nrow(data_final_TCN_CT_unique))
  data_plot <- data_plot[which(data_plot$Group == group_name), ]
  
  # Convert data to an sf object
  data_sub_sf <- st_as_sf(data_plot, coords = c("X", "Y"), crs = NA)
  data_plot$cell_id <- data_plot$cell_id
  
  # Process the group
  results_df <- process_group(data_sub_sf)
  results_df$Group <- group_name #add group name to the output
  
  
  # Assign to a variable using group name
  assign(paste0("motif_", group_name), results_df)
  cat(paste("Assigned motif for", group_name, "to motif_", group_name, "\n")) #Provides feedback
}

print("Motif analysis complete.  Results assigned to variables.")



healthy=rbind(motif_SB23_1629_new,motif_SB18_4271_new,motif_SB19_2941,
              motif_SB19_7280,motif_SB20_4001_new,motif_SB21_3340)
data_plot <- data.frame(Group = data_final_TCN_CT_unique$Group,
                        TCN = data_final_TCN_CT_unique$TACIT_Neighbor,
                        Meta = data_final_TCN_CT_unique$meta,
                        X = data_final_TCN_CT_unique$X,
                        Y = data_final_TCN_CT_unique$Y,
                        cell_id = 1:nrow(data_final_TCN_CT_unique))
results_df=healthy
# Filter out rows where the 'neighbors' column contains NA values
results_df <- results_df %>%
  filter(!is.na(neighbors))
table(is.na(results_df$cell_id))
results_df <- results_df %>%
  filter(!is.na(cell_id))
table(is.na(results_df$neighbors))


results_df_expanded <- results_df %>%
  unnest(neighbors)

# Check column names after unnesting
colnames(results_df_expanded)[2]="Neighbor_ID"


results_df_expanded$Neighbor_cell_id=data_plot$TCN[match(results_df_expanded$cell_id, data_plot$cell_id)]
results_df_expanded$Neighbor_Neigbor_ID=data_plot$TCN[match(results_df_expanded$Neighbor_ID, data_plot$cell_id)]
results_df_expanded$Group=data_plot$Group[match(results_df_expanded$cell_id, data_plot$cell_id)]


# Load the required libraries
library(dplyr)
library(igraph)

# Prepare the data
# Filter out rows where Neighbor_cell_id is equal to Neighbor_Neigbor_ID
filtered_data <- results_df_expanded %>%
  filter(Neighbor_cell_id != Neighbor_Neigbor_ID)



#filtered_data=filtered_data[which(filtered_data$Neighbor_cell_id!=11),]
#filtered_data=filtered_data[which(filtered_data$Neighbor_Neigbor_ID!=11),]


# Count the frequency of each Neighbor_Neigbor_ID for each Neighbor_cell_id
connection_counts <- filtered_data %>%
  group_by(Neighbor_cell_id, Neighbor_Neigbor_ID) %>%
  summarise(count = n()) %>%
  ungroup()

# Calculate the total connections for each Neighbor_cell_id
total_connections <- connection_counts %>%
  group_by(Neighbor_cell_id) %>%
  summarise(total = sum(count))

# Merge the total connections back to calculate the proportion
connection_counts <- connection_counts %>%
  left_join(total_connections, by = "Neighbor_cell_id") %>%
  mutate(proportion = count / total)

# For each Neighbor_cell_id, select the top connection by proportion
top_connections <- connection_counts %>%
  group_by(Neighbor_cell_id) %>%
  slice_max(order_by = proportion, n = 1) %>%
  ungroup()

# Create the edge list for the graph
edges <- data.frame(from = top_connections$Neighbor_cell_id,
                    to = top_connections$Neighbor_Neigbor_ID,
                    weight = top_connections$proportion)

# Create the graph object
graph <- graph_from_data_frame(edges, directed = TRUE)
vertex_labels <- V(graph)$name

V(graph)$color <- user_colors[vertex_labels]
plot(graph,
     edge.width = E(graph)$weight * 10,  # Thickness of the edges proportional to the connection strength
     vertex.size = 15,                   # Size of the vertices
     vertex.label = V(graph)$name,       # Label with the cell names
     vertex.color = V(graph)$color,      # Color of the vertices
     edge.arrow.size = 0.5,              # Arrow size for directed edges
     main = "Motif SjD")


data_final_TCN_CT$ID_follow=paste0(data_final_TCN_CT$X_org,data_final_TCN_CT$Y_org,data_final_TCN_CT$Group,data_final_TCN_CT$meta,sep="_")


data_healthy=read_csv("data_final_healthy.csv")
data_covid=read_csv("data_final_Covid.csv")
data_sjd=read_csv("data_final_SjD.csv")
data_GVHD=read_csv("data_final_GVHD.csv")

common_cols=intersect(colnames(data_healthy),intersect(colnames(data_covid),
                                                       intersect(colnames(data_sjd),colnames(data_GVHD))))


data_healthy=data_healthy[,common_cols]
data_covid=data_covid[,common_cols]
data_sjd=data_sjd[,common_cols]
data_GVHD=data_GVHD[,common_cols]


data_combine=rbind(data.frame(data_covid,meta="COVID"),
                   data.frame(data_healthy,meta="healthy"),
                   data.frame(data_sjd,meta="sjd"),
                   data.frame(data_GVHD,meta="GVHD"))
data_combine$ID_follow=paste0(data_combine$X,data_combine$Y,data_combine$Group,data_combine$meta,sep="_")

data_combine_final_full=merge(data_combine,data_final_TCN_CT,"ID_follow")

data_combine_final_full_unique <- data_combine_final_full %>%
  distinct(ID_follow, .keep_all = TRUE)  # Keeps all other columns

data_combine_final_full_unique_sub=data_combine_final_full_unique[which(data_combine_final_full_unique$TCN%in%c(7,1,5,13)),]

# Group the data by the predicted label and calculate the mean for each column
data_plot=data.frame(TACIT=paste0(data_combine_final_full_unique_sub$TCN,"_",data_combine_final_full_unique_sub$meta.x),data_combine_final_full_unique_sub[,c(11,12,14,19,22,24,28,30)])
mean_values_TACIT <- data_plot %>%
  group_by(TACIT) %>%
  summarise_all(~quantile(., 0.5))
#    mean_values_TACIT[ , -1][mean_values_TACIT[ , -1] > 0] <- 1
mean_values_TACIT=as.data.frame(mean_values_TACIT)
rownames(mean_values_TACIT)=mean_values_TACIT$TACIT
mean_values_TACIT <- as.data.frame((mean_values_TACIT[,-1]))


my.breaks <- c(seq(-2, 0, by=0.1),seq(0.1, 2, by=0.1))
my.colors <- c(colorRampPalette(colors = c("blue", "white"))(length(my.breaks)/2), colorRampPalette(colors = c("white", "red"))(length(my.breaks)/2))

aa=scale(mean_values_TACIT)
aa <- as.data.frame(aa)

aa <- aa %>%
  select_if(~ !all(is.na(.)))
# Custom color palette from blue to white to red
color_palette <- colorRampPalette(c("blue", "white", "red"))(100)

# Define breaks for the color scale
breaks <- seq(-3, 3, length.out = 101)  # Adjust to match your data range
library(pheatmap)

pheatmap(aa,
         cluster_cols = T,
         cluster_rows = T,
         show_colnames = TRUE,
         fontsize_col = 15,
         fontsize_row = 15,scale = "row",clustering_method="ward.D2",color = my.colors,breaks = my.breaks)


data_plot=data.frame(Meta=data_combine_final_full_unique$meta.x,TCN=data_combine_final_full_unique$TCN,data_combine_final_full_unique[,c(11,12,14,19,22,24,28,30)])


# Get unique Meta groups and TCN levels
unique_meta <- unique(data_plot$Meta)
unique_TCN <- unique(data_plot$TCN)

# Initialize results data frame
all_results <- data.frame()

# Loop through markers
for (marker in markers) {
  # Loop through TCN levels
  for (tcn in unique_TCN) {
    # Loop through pairs of Meta groups
    for (i in 1:length(unique_meta)) {
      for (j in 1:length(unique_meta)) {
        # Subset data
        group1 <- data_plot[data_plot$Meta == unique_meta[i] & data_plot$TCN == tcn, marker]
        group2 <- data_plot[data_plot$Meta == unique_meta[j] & data_plot$TCN == tcn, marker]
        
        # Check for empty groups BEFORE calculating standard deviation
        if (length(group1) > 0 && length(group2) > 0) {
          # Perform t-test
          test_result <- t.test(group1, group2)
          tidy_result <- tidy(test_result)
          tidy_result$Meta1 <- unique_meta[i]
          tidy_result$Meta2 <- unique_meta[j]
          tidy_result$TCN <- tcn
          tidy_result$Marker <- marker
          all_results <- rbind(all_results, tidy_result)
        } else {
          empty_row <- data.frame(estimate=NA,estimate1 = NA, estimate2 = NA, statistic = NA, p.value = NA, 
                                  parameter = NA, conf.low = NA, conf.high = NA, method = NA, 
                                  alternative = NA, Meta1 = unique_meta[i], Meta2 = unique_meta[j], 
                                  TCN = tcn, Marker = marker)
          all_results <- rbind(all_results, empty_row)
        }
      }
    }
  }
}
all_results$p_adjusted <- p.adjust(all_results$p.value, method = "BH")

#Select relevant columns for export
all_results <- all_results[,c("Meta1", "Meta2", "TCN", "Marker", "p.value")]
colnames(all_results) <- c("Meta_Group1", "Meta_Group2", "TCN", "Marker", "p_value")







library(sf)


# Convert data to an sf object without a CRS (non-geographic)
data_sub_sf <- st_as_sf(data_final, coords = c("X", "Y"), crs = NA)
data_sub_sf$cell_id <- data_final$ID

# Function to create a grid of windows
create_windows <- function(data, window_size, step_size) {
  bbox <- st_bbox(data)
  x_breaks <- seq(bbox["xmin"], bbox["xmax"] - window_size, by = step_size)
  y_breaks <- seq(bbox["ymin"], bbox["ymax"] - window_size, by = step_size)
  return(expand.grid(x = x_breaks, y = y_breaks))
}

# Function to find unique neighbors
unique_neighbors <- function(results) {
  results %>%
    distinct() %>%
    rowwise() %>%
    mutate(neighbors = list(unique(neighbors)))
}

# Function to find neighbors in a sliding window
find_neighbors_in_window <- function(window, data, window_size, distance_cutoff) {
  xmin <- window$x
  xmax <- window$x + window_size
  ymin <- window$y
  ymax <- window$y + window_size
  
  window_data <- data %>%
    filter(st_coordinates(.)[,1] >= xmin & st_coordinates(.)[,1] < xmax &
             st_coordinates(.)[,2] >= ymin & st_coordinates(.)[,2] < ymax)
  
  if (nrow(window_data) < 2) {
    return(data.frame(cell_id = character(0), neighbors = I(list())))
  }
  
  distances <- st_distance(window_data)
  diag(distances) <- NA
  distances[distances > distance_cutoff] <- NA
  
  neighbors <- lapply(seq_len(nrow(window_data)), function(i) {
    near <- which(!is.na(distances[i, ]))
    if (length(near) > 0) as.character(window_data$cell_id[near]) else NA
  })
  
  return(data.frame(cell_id = window_data$cell_id, neighbors = I(neighbors)))
}

# Parameters
window_size <- 500  # Size of the window
step_size <- 400  # Step size for sliding window
distance_cutoff <- 20  # Distance cutoff for neighbors

# Function to process each group
process_group <- function(group_data) {
  # Create windows for the group
  windows <- create_windows(group_data, window_size, step_size)
  
  # Find neighbors within each window and combine results
  results <- do.call(rbind, lapply(seq_len(nrow(windows)), function(i) {
    find_neighbors_in_window(windows[i, ], group_data, window_size, distance_cutoff)
  }))
  
  # Remove duplicates and group neighbors
  results <- unique_neighbors(results)
  
  # Combine results by cell_id
  results_df <- results %>%
    group_by(cell_id) %>%
    summarise(neighbors = list(unique(unlist(neighbors))))
  
  return(results_df)
}
data_sub_sf$Group=data_final$Sample
# Apply the process to each group and combine results
all_results <- data_sub_sf %>%
  group_by(Group) %>%
  group_map(~ process_group(.x)) %>%
  bind_rows()

# Display combined results
print(all_results)




all_results_glands_v2=all_results

results_df=all_results_glands_v2
# Filter out rows where the 'neighbors' column contains NA values
results_df <- results_df %>%
  filter(!is.na(neighbors))
table(is.na(results_df$cell_id))
results_df <- results_df %>%
  filter(!is.na(cell_id))
table(is.na(results_df$neighbors))


results_df_expanded <- results_df %>%
  unnest(neighbors)

# Check column names after unnesting
colnames(results_df_expanded)[2]="Neighbor_ID"


results_df_expanded$Neighbor_cell_id=data_final$TACIT_TCN[match(results_df_expanded$cell_id, data_final$ID)]
results_df_expanded$Neighbor_Neigbor_ID=data_final$TACIT_TCN[match(results_df_expanded$Neighbor_ID, data_final$ID)]
results_df_expanded$Group=data_final$Sample[match(results_df_expanded$cell_id, data_final$ID)]
results_df_expanded$Condition=data_final$Condition[match(results_df_expanded$cell_id, data_final$ID)]


# Load the required libraries
library(dplyr)
library(igraph)

# Prepare the data
# Filter out rows where Neighbor_cell_id is equal to Neighbor_Neigbor_ID
filtered_data <- results_df_expanded %>%
  filter(Neighbor_cell_id != Neighbor_Neigbor_ID)


table(filtered_data$Group)

filtered_data=filtered_data[which(filtered_data$Condition%in%c("Periodontitis")),]
#filtered_data=filtered_data[which(filtered_data$Neighbor_cell_id!=11),]
#filtered_data=filtered_data[which(filtered_data$Neighbor_Neigbor_ID!=11),]


# Count the frequency of each Neighbor_Neigbor_ID for each Neighbor_cell_id
connection_counts <- filtered_data %>%
  group_by(Neighbor_cell_id, Neighbor_Neigbor_ID) %>%
  summarise(count = n()) %>%
  ungroup()

# Calculate the total connections for each Neighbor_cell_id
total_connections <- connection_counts %>%
  group_by(Neighbor_cell_id) %>%
  summarise(total = sum(count))

# Merge the total connections back to calculate the proportion
connection_counts <- connection_counts %>%
  left_join(total_connections, by = "Neighbor_cell_id") %>%
  mutate(proportion = count / total)

# For each Neighbor_cell_id, select the top connection by proportion
top_connections <- connection_counts %>%
  group_by(Neighbor_cell_id) %>%
  slice_max(order_by = proportion, n = 2) %>%
  ungroup()

# Create the edge list for the graph
edges <- data.frame(from = top_connections$Neighbor_cell_id,
                    to = top_connections$Neighbor_Neigbor_ID,
                    weight = top_connections$proportion)

# Create the graph object
graph <- graph_from_data_frame(edges, directed = TRUE)
vertex_labels <- V(graph)$name

V(graph)$color <- neighbor_colors[vertex_labels]
plot(graph,
     edge.width = E(graph)$weight * 10,  # Thickness of the edges proportional to the connection strength
     vertex.size = 15,                   # Size of the vertices
     vertex.label = V(graph)$name,       # Label with the cell names
     vertex.color = V(graph)$color,      # Color of the vertices
     edge.arrow.size = 0.5,              # Arrow size for directed edges
     main = "Motif Periodontitis")






data_final_sub=data_final[which(data_final$Condition!="Healthy"),]
data_final_sub=data_final_sub[which(data_final_sub$TACIT=="CD38 VECs"),]

data_final_sub=data_final_sub[,c(1,2,27,36)]

# Assuming your data is stored in a data frame called `data_final_sub`

result <- data_final_sub %>%
  group_by(Sample) %>%
  summarise(
    total = n(),
    count_gt_5 = sum(CD31 > 3),
    percent_gt_5 = (count_gt_5 / total) * 100
  )

print(result)

result_meta=merge(result,metadata,"Sample")
library(ggpubr)
# Assuming the merged data frame is named `result_merged`
ggplot(result_meta, aes(x = Condition, y = percent_gt_5, fill = Condition)) +
  geom_boxplot() +
  theme_classic(base_size = 15) +
  labs(
    title = "",
    x = "Condition",
    y = ""
  )+
  stat_compare_means(aes(group = Condition), 
                     method = "wilcox.test", 
                     label = "p.format", 
                     label.y = max(result_meta$percent_gt_5) + 5)







