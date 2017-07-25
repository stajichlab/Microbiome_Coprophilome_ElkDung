library(ggplot2)
library(vegan)
library(dplyr)
library(scales)
library(grid)
library(reshape2)
library(phyloseq)
library(magrittr)

meta <- read.table("Coprophile_meta.tsv",header=T,sep="\t",row.names=1,
                   stringsAsFactors=FALSE)
otus <- read.table("Coprophile_OTU_pellets_16S.tsv",
                   header=T,sep="\t",row.names=1)
otus$taxonomy <- NULL
colnames(otus)
sum(otus)
#otus <- head(otus,n=120L)
otus <- as(as.matrix(otus), "matrix")
OTU = otu_table(otus, taxa_are_rows = TRUE)
sampleData <- sample_data(meta)

rownames(sampleData)
colnames(sampleData)

taxmat <- read.table("Coprophile_taxonomy.tsv",header=T,sep="\t",row.names=1)
taxmat <- as(as.matrix(taxmat),"matrix")
TAX = tax_table(taxmat)
head(TAX)

phylo = phyloseq(OTU, TAX,sampleData)
phylo

total = median(sample_sums(phylo))
standf = function(x, t=total) round(t * (x / sum(x)))
phylostand = transform_sample_counts(phylo, standf)

phylum <- phylo %>%
  tax_glom(taxrank = "Phylum") %>%                     # agglomerate at phylum level
  transform_sample_counts(function(x) {x/sum(x)} ) %>% # Transform to rel. abundance
  psmelt() %>%                                         # Melt to long format
#  filter(Abundance > 0.001) %>%                         # Filter out low abundance taxa
  arrange(Phylum)                                      # Sort data frame alphabetically by phylum

phylum_colors <- c(
  "#CBD588", "#5F7FC7", "darkgreen","#DA5724", "#508578", "#CD9BCD",
   "orange","#AD6F3B", "#D14285", "#652926", "#C84248", 
  "#8569D5", "#5E738F","#D1A33D", "#8A7C64", "#599861",
 "red","orange","blue","yellow","green","purple","black","grey","magenta","darkgreen","heather","burntorange","darkblue"
)

pdf("Phyla_plot.pdf")


ggplot(phylum, aes(x = factor(SampleAge), y = Abundance, fill = Phylum)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = phylum_colors) +
#  scale_x_discrete(
#    breaks = c("3200", "7000", "11000"),
#    labels = c("Low-3.2k", "Med-7k", "High-11k"), 
#    drop = FALSE
#  ) +
  # Remove x axis title
  theme( axis.text.x = element_text(angle = 60, hjust = 1)) + 
  #
  guides(fill = guide_legend(reverse = TRUE, keywidth = 1, keyheight = 1)) +
  xlab("Age (days) of Pellet") + ylab("Relative Abundance (Phyla > 2%) \n") +
  ggtitle("Phylum Composition of Dung Pellet")

class <- phylo %>%
  tax_glom(taxrank = "Class") %>%                     # agglomerate at Class level
  transform_sample_counts(function(x) {x/sum(x)} ) %>% # Transform to rel. abundance
  psmelt() %>%                                         # Melt to long format
#  filter(Abundance > 0.02) %>%                         # Filter out low abundance taxa
  arrange(Class)                                      # Sort data frame alphabetically by phylum

genus <- phylo %>%
  tax_glom(taxrank = "Genus") %>%                     # agglomerate at Class level
  transform_sample_counts(function(x) {x/sum(x)} ) %>% # Transform to rel. abundance
  psmelt() %>%                                         # Melt to long format
#  filter(Abundance > 0.02) %>%                         # Filter out low abundance taxa
  arrange(Genus)                                      # Sort data frame alphabetically by phylum

plot_bar(phylostand, "Phylum", fill="Class",facet_grid=~SampleAge)
plot_bar(phylostand, "Phylum", fill="Class",facet_grid=~PelletGroup)

ggplot(class, aes(x = Sample, y = Abundance, fill = Class,facet_grid=SampleAge)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = phylum_colors) +
#  scale_x_discrete(
#    breaks = c("3200", "7000", "11000"),
#    labels = c("Low-3.2k", "Med-7k", "High-11k"), 
#    drop = FALSE
#  ) +
  # Remove x axis title
  theme( axis.text.x = element_text(angle = 60, hjust = 1)) + 
  #
  guides(fill = guide_legend(reverse = TRUE, keywidth = 1, keyheight = 1)) +
    xlab("Age (days) of Pellet") + ylab("Relative Abundance (Class > 2%) \n") +
  ggtitle("Class Composition of Dung Pellets") 

ggplot(class, aes(x = factor(PelletGroup), y = Abundance, fill = Class)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = phylum_colors) +
  # Remove x axis title
  theme( axis.text.x = element_text(angle = 60, hjust = 1)) + 
  #
  guides(fill = guide_legend(reverse = TRUE, keywidth = 1, keyheight = 1)) +
    xlab("Pellet Group") + ylab("Relative Abundance (Class > 2%) \n") +
  ggtitle("Class Composition of Dung Pellets") 


ggplot(class, aes(x = Sample, y = Abundance, fill = Class)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = phylum_colors) +
  theme( axis.text.x = element_text(angle = 60, hjust = 1)) + 
  #
  guides(fill = guide_legend(reverse = TRUE, keywidth = 1, keyheight = 1)) +
    xlab("Sample") + ylab("Relative Abundance (Class > 2%) \n") +
  ggtitle("Class Composition of Dung Pellets") 

