library(ggplot2)
library(readxl) 
library(writexl)
library(tidyverse)
library(viridis)
#### Loading data ###############################################################
all <- read_excel("Table S1_Identif proteins, HCP, TSC, Subcell locat, GO, MW, pI.xlsx", 
                  skip = 2, n_max = 353)

########## Fig 7A-C Barplot of identified proteins #################################################################
Identif_tidy <- read_excel("Identif barplot.xlsx")

#visualize a barplot of sample (in x axis) vs value (in y axis) and grouping by Proteins, PSMs and Peptides
a <- 
Identif_tidy %>% filter(category %in% c("Proteins")) %>%
  mutate(value = round(value, 1)) %>% #filter with one decimal place the values
  ggplot( aes(x=sample, y=value, fill= category) ) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=value), vjust=1.5, size=4, color="white") +
  theme_bw() +
  label(y = "Counts") +
  #scale_fill_manual(values = c("Proteins" = "gray34")) +
  scale_fill_viridis(discrete = T) +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5, color = "black"),
        axis.text.y = element_text(size = 16),
        strip.text = element_blank(),
        legend.position = "inside",
        legend.justification = "top",
        legend.justification.inside = c(.97, .97),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold") )
  
b <- 
Identif_tidy %>% filter(category %in% c("PSMs", "Peptides", "TSC", "HCP spec count")) %>%
  ggplot( aes(x=sample, y=value, fill=category) ) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label=value), 
            position = position_stack(vjust = .5), size = 3, 
            color=c("white","white","white","white","white","white","white",
                    "white","white","white","white","white","white","white",
                    "black","black","black","black","black","black", "black",
                    "black","black","black","black","black","black", "black") ) +
  scale_fill_viridis(discrete = T) +
  ylab(label = "Counts") +
  theme_bw() +
  label(y = "Counts") +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5, color = "black" ),
        axis.text.y = element_text(size = 16),
        strip.text = element_blank(),
        legend.position = "inside",
        legend.justification = "top",
        legend.justification.inside = c(.97, .97),
        legend.title = element_blank(),
        legend.text = element_text(size = 11, face = "bold") )
  
c <- 
Identif_tidy %>% filter(category %in% c("% HCP", "% EcA2")) %>%
  mutate(value = round(value, 1)) %>% #filter with one decimal place the values
  ggplot( aes(x=sample, y=value, fill=category) ) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = value), 
            position = position_stack(vjust = .5), size = 4,
            color = c("white","white","white","white","white","white","white",
                      "black","black","black","black","black","black", "black")) +
  scale_fill_viridis(discrete = T) +
  theme_bw() +
  label(y = "Counts") +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16, angle = 90, hjust = 1, vjust = .5, color = "black"),
        axis.text.y = element_text(size = 16),
        strip.text = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face="bold") )

cowplot::plot_grid(a, b, c, nrow = 1, align = "hv", labels = "AUTO",
                   label_size = 26, label_fontface = "bold")


########## Fig S7. Bubble plot of MW vs pI #################################################################
all$Type <- ifelse(all$Protein_name == "L-asparaginase type II", "EcA2", "HCP")
#convert the dataframe in long format using the columns Leu, S1 to S3, and P1 to P3 into column TSC
all_tidy <- all %>% 
  pivot_longer(cols = c(Leu, S1, S2, S3, P1, P2, P3), 
               names_to = "Sample", 
               values_to = "TSC")
all_tidy <- all_tidy %>% dplyr::filter(TSC >= 2)

all_tidy %>% 
  ggplot( aes(x=pI, y=MW_kDa) ) +
  geom_point(aes(size=TSC, color=Type),
             alpha=0.5) +
  theme_classic( ) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.7, linetype = "dotted"),
        panel.grid.major.x = element_line(color = "gray", size = 0.7, linetype = "dotted"),
        panel.background   = element_rect( colour = "black"),
        strip.background = element_blank(), strip.placement = "outside",
        #legend.box.background = element_rect(),
        legend.box.margin = margin(1,1,1,1),
        legend.key = element_blank() ) +
  xlab(label = "pI") +
  ylab(label = "MW (kDa)") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 12, color="black", face = "bold"),
        legend.title = element_text(size = 12, color="black", face = "bold"),
        legend.position = "inside", 
        legend.justification = "bottom",
        legend.justification.inside = c(1, 0),
        legend.key.size = unit(.1, "cm"),
        axis.text.x = element_text(size = 14, color = "black", face = "bold"),
        axis.text.y = element_text(size = 14, color="black", face = "bold"),
        strip.text = element_text(size = 14, face = "bold") ) +
  scale_size_continuous(range = c(2, 15) )  +
  xlim(4, 12) + ylim(5, 160) +
  labs(title = NULL, subtitle = NULL, caption = NULL, tag = NULL ) +
  facet_wrap(vars(Sample), 
             dir = "h", 
             nrow=2,
             #scales = "free",
             strip.position = "top",
             axes = "all", 
             axis.labels = "all_y" ) 

################### Fig 7G. Dot plot of Top 5 HCP #####################################################
all_tidy <- all_tidy %>% arrange(desc(TSC)) 
all_tidy$Top5 = FALSE
#save in excell all_tidy
write_xlsx(all_tidy, "all_tidy.xlsx")

#modified in excell and load again
all_tidy <- read_excel("all_tidy.xlsx")

all_tidy$Short_name <- str_wrap(all_tidy$Short_name, width = 30)

g <- 
all_tidy %>%  dplyr::filter(Top5 == TRUE) %>% 
  ggplot( aes(x=Sample, 
              y=Short_name) ) +
  geom_point(aes(size=TSC, 
                 color=MW_kDa) ) +
  #geom_segment(aes(xend=0, yend = term)) + # inserta linea horizontal desde cada term
 # scale_color_gradient2(low="white", mid="brown", high="blue", midpoint=37) +
  scale_color_continuous(low="green3", high="blue") +
  theme_classic( ) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.7, linetype = "dotted"),
        panel.grid.major.x = element_line(color = "gray", size = 0.7, linetype = "dotted"),
        panel.background   = element_rect(fill = "white", colour = "grey50"),
        legend.box.background = element_blank(),
        legend.box.margin = margin(6,6,6,6),
        legend.key = element_blank() ) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12, face = "bold"),
        legend.position = "right", 
        axis.text.x = element_text(size = 15, angle=45, hjust=1, face = "bold", color="black"),
        axis.text.y = element_text(size = 12, color="black", face = "bold")
        #strip.text = element_text(size = 12) 
  ) +
  scale_size_continuous(range = c(3, 8) )  +
  labs(title = NULL, subtitle = NULL, caption = NULL, tag = NULL) + ggtitle("")

################### Fig 7F. Dot plot of 5 common proteins among all samples #####################################################
f <- 
all_tidy %>%  dplyr::filter(Common_all == TRUE) %>% 
  ggplot( aes(x=pI, y=Short_name) ) +
  geom_point(aes(size=MW_kDa, color=Subcellular_location),
             alpha=0.5) +
  theme_classic( ) +
  theme(panel.grid.major.y = element_line(color = "gray", size = 0.7, linetype = "dotted"),
        panel.grid.major.x = element_line(color = "gray", size = 0.7, linetype = "dotted"),
        panel.background   = element_rect( colour = "black"),
        strip.background = element_blank(), strip.placement = "outside",
        #legend.box.background = element_rect(),
        legend.box.margin = margin(1,1,1,1),
        legend.key = element_blank() ) +
  xlab(label = "pI") +
  ylab(label = "") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold", color = "black"),
        legend.text = element_text(size = 12, color="black", face = "bold"),
        legend.title = element_text(size = 12, color="black", face = "bold"),
        legend.position = "right", 
        legend.key.size = unit(.1, "cm"),
        legend.box="vertical",
        axis.text.x = element_text(size = 14, angle=90, hjust=1, vjust = .5, color="black", face = "bold" ),
        axis.text.y = element_text(size = 14, color="black", face = "bold"),
        strip.text = element_text(size = 14, face = "bold") ) +
  scale_size_continuous(range = c(4, 15) )  +
  #xlim(4, 12) + ylim(5, 160) +
  labs(title = NULL, subtitle = NULL, caption = NULL, tag = NULL )

cowplot::plot_grid(f, g, nrow = 1, align = "hv")

############## Preparing the data for perseus enrichment ################################################################
all$detected_Leu [all$Leu >= 2] <- "+" 
all$detected_P1 [all$P1 >= 2] <- "+"
all$detected_P2 [all$P2 >= 2] <- "+"
all$detected_P3 [all$P3 >= 2] <- "+"
all$detected_S1 [all$S1 >= 2] <- "+"
all$detected_S2 [all$S2 >= 2] <- "+"
all$detected_S3 [all$S3 >= 2] <- "+"
write_csv(all, "E:/ASNase2_HCP/v02/all.csv")
write.table(all, "E:/ASNase2_HCP/v02/all.txt", sep = "\t", quote = FALSE, row.names = FALSE)

############## Fig S8. PTM-shepperd output ################################################################
global_modsummary <- read_delim("global.modsummary.tsv", 
                                delim = "\t", escape_double = FALSE, 
                                trim_ws = TRUE)
global_modsummary <- as.data.frame(global_modsummary)

glob_modsumm_tidy <- global_modsummary %>% 
  pivot_longer(cols = c(Leu_PSMs, S1_PSMs, S2_PSMs, S3_PSMs, P1_PSMs, P2_PSMs, P3_PSMs,
                        Leu_percent_PSMs, S1_percent_PSMs, S2_percent_PSMs, S3_percent_PSMs,
                        P1_percent_PSMs, P2_percent_PSMs, P3_percent_PSMs), 
               names_to = "Sample", 
               values_to = "values")
glob_modsumm_tidy$category <- ifelse(grepl("_percent_", glob_modsumm_tidy$Sample), "%_PSMs", "PSMs")
#rename in the column Sample the values with the names of the samples
glob_modsumm_tidy$Sample <- gsub("_PSMs", "", glob_modsumm_tidy$Sample)
glob_modsumm_tidy$Sample <- gsub("_percent", "", glob_modsumm_tidy$Sample)

#Fig_S8B <- 
glob_modsumm_tidy %>% dplyr::filter(category == "%_PSMs") %>% dplyr::filter (values >= 1) %>% 
  mutate(values = round(values, 1)) %>% 
  dplyr::filter(!Modification %in%  c("None", "First isotopic peak","Second isotopic peak","Isotopic peak error")) %>% 
  #replace the names c("Unannotated mass-shift 251.0282", "Addition of lysine due to transpeptidation/Addition of K") in the rows of column modification by the name "+251" and "add. K"
  mutate(Modification = ifelse(Modification == "Unannotated mass-shift 251.0282", "+251.0282", Modification),
         Modification = ifelse(Modification == "Addition of lysine due to transpeptidation/Addition of K", "add. K", Modification),
         Modification = ifelse(Modification == "Dehydration/Pyro-glu from E", "pyrE (-H2O)", Modification),
         Modification = ifelse(Modification == "Pyro-glu from Q/Loss of ammonia", "pyrQ (-NH3)", Modification),
         Modification = ifelse(Modification == "Acetaldehyde +26", "Acetaldeh +26", Modification) ) %>% 
  ggplot( aes(x=Modification, y=values, fill = Sample) ) +
  geom_bar(stat="identity", position="stack") +
  geom_text(aes(label = values), 
            position = position_stack(vjust = .5), size = 4,
            color="black" ) +
  scale_fill_viridis(discrete = T) +
  theme_bw() + ylab(label = "% PSMs") +
  theme(plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 14, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, angle = 30, hjust=1, vjust=1, face = "bold", color = "black"),
        axis.text.y = element_text(size = 14, face = "bold",color = "black"),
        strip.text = element_blank(),
        legend.position = "right",
        legend.title = element_text(size = 12, face="bold"),
        legend.text = element_text(size = 12, face="bold") )

#load the excel file with the tidy data filtered by Top1
glob_modsumm_tidy <- read_excel("glob_modsumm_tidy.xlsx")
glob_modsumm_tidy <- as.data.frame(glob_modsumm_tidy)
#replace the value "251.0282" in the column Modification by the name "+251.0282"
glob_modsumm_tidy$Modification <- ifelse(glob_modsumm_tidy$Modification == "251.0282", "+251.0282", glob_modsumm_tidy$Modification)
glob_modsumm_tidy$Modification <- ifelse(glob_modsumm_tidy$Modification == "Formylation", "Formylation (+27.9949)", glob_modsumm_tidy$Modification)
glob_modsumm_tidy$Modification <- ifelse(glob_modsumm_tidy$Modification == "Acetaldeh +26", "Acetaldehyde (+26.0156)", glob_modsumm_tidy$Modification)

glob_modsumm_tidy$Modification <- str_wrap(glob_modsumm_tidy$Modification, width = 20)

#Fig_S8A <- 
glob_modsumm_tidy %>% dplyr::filter(Top1 == "TRUE") %>% 
    ggplot( aes(x=Modification, y=values, fill = Sample) ) +
    geom_bar(stat="identity", position="stack") +
    geom_text(aes(label=values), 
              position = position_stack(vjust = .5), size = 3.5) +
  #insert a text in the plot "Top 1%" in the position 1.5 and 200
    annotate("text", x = 7, y = 5000, label = "~1.0-3.8% total PSMs", size = 5, color = "black") +
    scale_fill_viridis(discrete = T) +
    ylab(label = "PSMs") +
    theme_bw() +
    theme(plot.title = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16, face = "bold", color = "black"),
          axis.text.x = element_text(size = 12, angle = 30, hjust = 1, vjust = 1, color = "black", face = "bold"),
          axis.text.y = element_text(size = 12, color = "black", face = "bold"),
          strip.text = element_blank(),
          legend.position = "inside",
          legend.justification = "left",
          legend.justification.inside = c(.03, .97),
          legend.title = element_blank(),
          legend.text = element_text(size = 11, face = "bold") ) 
  



