library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggVennDiagram)

###### Loading data ##############################################################################
all <- read_excel("Table S1_Identif proteins, HCP, TSC, Subcell locat, GO, MW, pI.xlsx", 
                  skip = 2, n_max = 353)
Leu <- all %>% filter(Leu > 0) %>% select(Uniprot_ID)
P1 <- all %>% filter(P1 > 0) %>% select(Uniprot_ID)
P2 <- all %>% filter(P2 > 0) %>% select(Uniprot_ID)
P3 <- all %>% filter(P3 > 0) %>% select(Uniprot_ID)
S1 <- all %>% filter(S1 > 0) %>% select(Uniprot_ID)
S2 <- all %>% filter(S2 > 0) %>% select(Uniprot_ID)
S3 <- all %>% filter(S3 > 0) %>% select(Uniprot_ID)

#change the column name to Leu, P1, P2, P3, S1, S2, S3
colnames(Leu)[1] <- "Leu"
colnames(P1)[1] <- "P1"
colnames(P2)[1] <- "P2"
colnames(P3)[1] <- "P3"
colnames(S1)[1] <- "S1"
colnames(S2)[1] <- "S2"
colnames(S3)[1] <- "S3"

#convert the files to matrix
Leu <- as.matrix(Leu)
P1 <- as.matrix(P1)
P2 <- as.matrix(P2)
P3 <- as.matrix(P3)
S1 <- as.matrix(S1)
S2 <- as.matrix(S2)
S3 <- as.matrix(S3)

#create a list for each comparison
All <- list(Leu=Leu, P1 = P1, P2 = P2, P3 = P3, S1 = S1, S2 = S2, S3 = S3)
Leu_super <- list(Leu = Leu, S1 = S1, S2 = S2, S3 = S3)
Leu_pellet <- list(Leu = Leu, P1 = P1, P2 = P2, P3 = P3)
super <- list(S1 = S1, S2 = S2, S3 = S3)
pellet <- list(P1 = P1, P2 = P2, P3 = P3)

###### #Fig 7D. upset plot #####################################################################
ggVennDiagram(All,
              category.names = names(All),
              force_upset = TRUE,
              set_color = "black",
              set_size=10,
              label_color="black",
              label_size=10,
              label_txtWidth = 40,
              label_alpha = 1,
              relative_height = 3, 
              relative_width = 0.3,
              order.set.by = "name", 
              order.intersect.by = "size",
              edge_lty = "solid",
              edge_size = 5) 

##################### Fig 7E. Venn diagram #######################################
ggVennDiagram(Leu_pellet,
              set_size = 7, 
              label_size = 7,
              label = "count",
              label_alpha=0 ) + scale_fill_distiller(palette = "Blues", direction = 1)

ggVennDiagram(Leu_super,
              set_size = 7, 
              label_size = 7,
              label = "count",
              label_alpha=0 ) + scale_fill_distiller(palette = "Blues", direction = 1)

ggVennDiagram(super,
              set_size = 7, 
              label_size = 7,
              label = "count",
              label_alpha=0 ) + scale_fill_distiller(palette = "Blues", direction = 1)

ggVennDiagram(pellet,
              set_size = 7, 
              label_size = 7,
              label = "count",
              label_alpha=0 ) + scale_fill_distiller(palette = "Blues", direction = 1)




