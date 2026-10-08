#Matthew Davies mattdavies1601@gmail.com
#script to visualise emm clusters over time, separated by invasive, non-invasive and carriage

#This code was written to work on only a very specific excel spreadsheet output from the reflab's internal database

#Input MUST have columns: 'EMM-typering' & 'FirstOfafnamedatum' & 'studie' & 'labcode' & 'labnaam'
#Carriage is determined by containing 'OKIDOKI' or 'RETROGAS drager' in the 'studie' column
#Noninvasive is determined by having 'GGDGAS' and 'GAS studie GGD Amsterdam' in the labcode and labnaam columns
#Everything else is ASSUMED to be invasive- not inherently correct but for the reflab database it should be 'good enough'. 

#packages to install and open
#e.g install.packages("readxl")

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(scales)
library(ggplot2)
library(cowplot)
library(ggtext)

#adjust to location of file (ensure / and not \)
setwd("G:/divg/MMI-LEB/Nina/Matt/scripts/emmtype x time") #you need to change this to the location with the excel spreadsheet

#####open raw data, clean and preprocess
raw.data <- read_excel("metadata_example_input.xlsx") #you need to change this to your metadata spreadsheet
raw.data <- as.data.frame(raw.data)
raw.data <- raw.data %>% 
  filter_at(vars(FirstOfafnamedatum,ons_nummer,`EMM-typering`),
            all_vars(!is.na(.))) #this removes rows without a collection date or EMM-typering value
raw.data <- raw.data %>% 
  filter(!grepl('geen|Deletie', `EMM-typering`)) #you include here data in the EMM-typering column to be removed (add text within the '' to add additional values, e.g. 'geen|Deletie|empty|nothing')

raw.data <- raw.data %>%
  mutate(year = floor_date(ymd(raw.data$FirstOfafnamedatum),"year"),
         month = floor_date(ymd(raw.data$FirstOfafnamedatum),"month")) #simplifies collection date into year or month columns

raw.data$`EMM-typering` <- str_replace(raw.data$`EMM-typering`, 
                                       regex("cluster clade", ignore_case = TRUE), "cluster") #prevents later problem with splitting EMM-typering 

raw.data <- raw.data %>% 
  separate_wider_delim(`EMM-typering`,
                       delim = regex("\\s*Cluster\\s* | \\s*Clade\\s* | \\s*clade\\s*", ignore_case = TRUE), 
                       names = c("emmtype_subtype", "emmtype_cluster"),
                       too_few = "align_start",
                       cols_remove = FALSE) #splits EMM-typering into two new cols for cluster and emm subtype

raw.data <- raw.data %>%
  separate(emmtype_cluster, 
           into = c("emmtype_cluster_letter", "delete"), 
           sep = "(?<=[A-Za-z])(?=[0-9])",
           remove = FALSE) #splits emmtype_cluster into just the letters (e.g. A-C, D, E, M, X, Y). Be aware that all M clusters will be grouped into 1, they aren't necessarily a part of the same cluster!

raw.data <- raw.data %>%
  mutate(emmtype = str_remove_all(emmtype_subtype, "[A-Za-z]") |> as.numeric() |> floor()) #removes all letters from emmsubtype, converts to a number and rounds down

invasive.cols <- c("meningitis", "menisepsis", "sepsis", "pneumonie", 
                   "GAS;  fasciitis necroticans","Invasief, niet meningitis",
                   "GAS; STSS","GAS; puerperaal","GAS; normaal steriel") #cols containing values which, when TRUE, confirm invasive infection. Add more if needed!

raw.data <- raw.data %>%
  mutate(isolate_type = case_when(
    coalesce(`1` == "Liquorstam", FALSE) | coalesce(`2` == "Bloedstam", FALSE) | if_any(all_of(invasive.cols), ~ . %in% TRUE) ~ "invasive",
    (grepl("OKIDOKI", studie) %in% TRUE | studie == "RETROGAS drager") & 
      !if_all(all_of(invasive.cols), ~ . %in% TRUE) ~ "carriage",
    (labcode == "GGDGAS" & labnaam == "GAS studie GGD Amsterdam") & 
      !if_all(all_of(invasive.cols), ~ . %in% TRUE) ~ "noninvasive",
    TRUE ~ "invasive"
  )) #the default assumption is that data in this spreadsheet are from invasive infections, unless stated otherwise.
#If you want to separate confirmed invasive (i.e. blood/CSF or TRUE in invasive.cols) with unconfirmed invasive then change "invasive" on the previous line to something like (TRUE ~ "assumed_invasive") 


#####create plots to compare the counts for each cluster, split by invasive, noninvasive and carriage, over time
c25.named <- c("A-C3" = "dodgerblue3", 
               "A-C4" = "darkorange3",
               "A-C5" = "palegreen4",
               "D2" = "#9A3D9A",
               "D3" = "#6A3D9A",
               "D4" = "#FF7F00",
               "E1" = "maroon", 
               "E2" = "gold1",
               "E3" = "orchid",
               "E5" = "#fF7D3A",
               "E4" = "skyblue1",
               "E6" = "yellow4",
               "E7" = "#fA3D9A",
               "M105" = "#CAB2D6",
               "M122" = "#FDBF6F",
               "M14" = "gray70", 
               "M164" = "khaki2",
               "M185" = "#FB9A99",
               "M218" = "palegreen1",
               "M236" = "#b48f9A",
               "M29" = "#e48f9A",
               "M5" = "deeppink",
               "M55" = "pink",
               "M6" = "darkturquoise", 
               "M74" = "steelblue4",
               "M95" = "blue",
               "X" = "#E31A1C",
               "Y" = "#5A386B") #add cluster names as necessary, I added these colours for ease of visualising as automatic assignment is ugly

plt.data <- raw.data[!is.na(raw.data$emmtype_cluster),]

counts <- plt.data %>%
  count(isolate_type, year, emmtype_cluster) #counts the freq of each emmtype_cluster by isolate type (carriage/noninvasive/invasive) and year
all_clusters <- sort(unique(counts$emmtype_cluster))
counts <- counts %>%
  mutate(emmtype_cluster = factor(emmtype_cluster, levels = all_clusters))

emm_levels <- sort(unique(plt.data$emmtype_cluster))
x.limits <- range(counts$year)

#function to create plots
plot_category <- function(category) {
  rel_df <- counts %>%
    filter(isolate_type == category) %>% 
    group_by(year) %>%
    mutate(proportion = n / sum(n)) %>%
    ungroup()
  
  year.text <- counts %>%
    filter(isolate_type == category) %>%
    group_by(year) %>%
    summarise(total = sum(n))
  
  ggplot(rel_df, aes(x = year, y = proportion, fill = factor(emmtype_cluster))) + 
    geom_col(position = "fill", show.legend = TRUE) + 
    scale_y_continuous(labels = scales::percent_format()) + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    coord_cartesian(xlim = x.limits) +
    theme_classic() +
    scale_fill_manual(values = c25.named, limits = all_clusters, drop = FALSE) +
    geom_text(data = year.text, aes(x = year, y = 1.04, label = total),
              inherit.aes = F,
              size = 4) +
    labs(y = paste(category, "isolates", sep = " "), fill = expression(italic(emm)-cluster)) +
    theme(axis.text.x = element_text(angle = 90, size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),
          axis.text = element_text(size=10),
          legend.text = element_text(size=14),
          legend.title = element_text(size = 16))
} #adjust features of the figures here (e.g. font size, labels)

plot.c <- plot_category("carriage")
plot.n <- plot_category("noninvasive")
plot.i <- plot_category("invasive")
#plot.a <- plot_category("assumed_invasive") #only to add when distinguishing between confirmed invasive and assumed invasive infections. Don't forget to include "plot.a" in plot_grid below

plt.legend <- get_legend(plot.i) #Uses the legend from the invasive plot, assuming that the invasive plot contains all of the emmtypes as this is usually the most diverse

plot.c <- plot_category("carriage") + theme(legend.position = "none",
                                            axis.text.x = element_blank(),
                                            axis.ticks.x = element_blank())
plot.n <- plot_category("noninvasive") + theme(legend.position = "none",
                                               axis.text.x = element_blank(),
                                               axis.ticks.x = element_blank())
# plot.a <- plot_category("assumed_invasive") + theme(legend.position = "none",
#                                                axis.text.x = element_blank(),
#                                                axis.ticks.x = element_blank())
plot.i <- plot_category("invasive") + theme(legend.position = "none")

plot_grid(plot_grid(plot.c, plot.n, plot.i, ncol = 1, align = "year"),
          plot_grid(plt.legend, ncol = 1),
          rel_widths = c(1, 0.2)) #plots all three figures together for easy comparison. Remove plot.x as necessary.

start.year <- format(x.limits[1], "%Y")
end.year <- format(x.limits[2], "%Y")

ggsave(paste0(format(Sys.time(), "%Y%m%d"), "_emm_clusters_",
              start.year, "-", end.year, ".svg"),
       width = 45, height = 26, units = "cm", dpi = 300) #edit dimensions of the svg as necessary

#####create plots to compare the counts for each cluster letter group (i.e. A-C only instead of A-C3, A-C4 and A-C5)
c25.group <- c("A-C" = "dodgerblue3", 
               "D" = "orchid",
               "E" = "maroon", 
               "M" = "#CAB2D6",
               "X" = "#9FC7EA",
               "Y" = "#5A386B") #add cluster names as necessary

plt.data.gr <- raw.data[!is.na(raw.data$emmtype_cluster_letter),]

counts.gr <- plt.data.gr %>%
  count(isolate_type, year, emmtype_cluster_letter)
all_clusters.gr <- sort(unique(counts.gr$emmtype_cluster_letter))
counts.gr <- counts.gr %>%
  mutate(emmtype_cluster_letter = factor(emmtype_cluster_letter, levels = all_clusters.gr))

emm_levels.gr <- sort(unique(plt.data.gr$emmtype_cluster_letter))
x.limits.gr <- range(counts.gr$year)


plot_category.gr <- function(category) {
  rel_df <- counts.gr %>%
    filter(isolate_type == category) %>% 
    group_by(year) %>%
    mutate(proportion = n / sum(n)) %>%
    ungroup()
  
  year.text <- counts.gr %>%
    filter(isolate_type == category) %>%
    group_by(year) %>%
    summarise(total = sum(n))
  
  ggplot(rel_df, aes(x = year, y = proportion, fill = factor(emmtype_cluster_letter))) + 
    geom_col(position = "fill", show.legend = TRUE) + 
    scale_y_continuous(labels = scales::percent_format()) + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    coord_cartesian(xlim = x.limits.gr) +
    theme_classic() +
    scale_fill_manual(values = c25.group, limits = all_clusters.gr, drop = FALSE) +
    geom_text(data = year.text, aes(x = year, y = 1.04, label = total),
              inherit.aes = F,
              size = 4) +
    labs(y = paste(category, "isolates", sep = " "), fill = expression(italic(emm)-cluster (grouped))) +
    theme(axis.text.x = element_text(angle = 90, size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),
          axis.text = element_text(size=10),
          legend.text = element_text(size=14),
          legend.title = element_text(size = 16))
}

plot.c.gr <- plot_category.gr("carriage")
plot.n.gr <- plot_category.gr("noninvasive")
plot.i.gr <- plot_category.gr("invasive")

plt.legend.gr <- get_legend(plot.i.gr)

plot.c.gr <- plot_category.gr("carriage") + theme(legend.position = "none",
                                            axis.text.x = element_blank(),
                                            axis.ticks.x = element_blank())
plot.n.gr <- plot_category.gr("noninvasive") + theme(legend.position = "none",
                                               axis.text.x = element_blank(),
                                               axis.ticks.x = element_blank())
plot.i.gr <- plot_category.gr("invasive") + theme(legend.position = "none")

plot_grid(plot_grid(plot.c.gr, plot.n.gr, plot.i.gr, ncol = 1, align = "year"),
          plot_grid(plt.legend.gr, ncol = 1),
          rel_widths = c(1, 0.2))

start.year.gr <- format(x.limits.gr[1], "%Y")
end.year.gr <- format(x.limits.gr[2], "%Y")

ggsave(paste0(format(Sys.time(), "%Y%m%d"), "_emm_clusters_grouped_",
              start.year.gr, "-", end.year.gr, ".svg"),
       width = 45, height = 26, units = "cm", dpi = 300) #edit size of svg as necessary


#################plot by emmtype over time instead of emm_cluster
#will not be as nice to visualise as there are significantly more emmtypes compared to emm clusters
#all emm-subtypes have been grouped into their base types for ease of visualisation (e.g. emm3.93 is now emm3)
#be careful when interpreting these figures as subtypes are not equal to each other (e.g. emm3.93 was an outbreak strain whereas emm3.1 was not)
emmtype.data <- raw.data[!is.na(raw.data$emmtype),]
emmtype.data$emmtype <- as.factor(emmtype.data$emmtype)

emmtypesum <- emmtype.data %>%
  count(emmtype)
emmtypesum$emmtype <- as.factor(emmtypesum$emmtype)
emmtypesum <- emmtypesum %>% 
  mutate(emmtype_grouped = if_else(n < 15, "Other", as.character(emmtype)))

emmtype.data <- emmtype.data %>%
  left_join(emmtypesum %>% select(emmtype, emmtype_grouped), by = "emmtype")

counts.emmtype <- emmtype.data %>%
  count(isolate_type, year, emmtype_grouped)
all_clusters.emmtype <- sort(unique(counts.emmtype$emmtype_grouped))
counts.emmtype <- counts.emmtype %>%
  mutate(emmtype_grouped = factor(emmtype_grouped, levels = all_clusters.emmtype))

emm_levels.emmtype <- sort(unique(emmtype.data$emmtype_grouped))
x.limits.emmtype <- range(counts.emmtype$year)

c31.named <- c("1" = "#60a1db",
               "102" = "#616edc",
               "104" = "#63c04c",
               "11" = "#944cc2",
               "12" = "#aab936",
               "2" = "#d077e1",
               "22" = "#4e8d2b",
               "27" = "#d14ba1",
               "28" = "#4dc381",
               "3" = "#db4478",
               "33" = "#40894c",
               "4" = "#43c4c6",
               "44" = "#d64250",
               "49" = "#d5522b",
               "58" = "#e58e63",
               "6" = "#d99b33",
               "60" = "#5d6fb3",
               "73" = "#8d8d2d",
               "75" = "#8c509b",
               "76" = "#91b76f",
               "77" = "#c88fcf",
               "8" = "#616d2c",
               "81" = "#a04c73",
               "82" = "#54af8f",
               "83" = "#a94b4e",
               "87" = "#297250",
               "89" = "#e4848f",
               "9" = "#c3ad68",
               "92" = "#a3592b",
               "94" = "#8d6d2e",
               "Other" = "grey80") #add emmtype names as necessary

plot_category.emmtype <- function(category) {
  rel_df <- counts.emmtype %>%
    filter(isolate_type == category) %>%
    group_by(year) %>%
    mutate(proportion = n / sum(n)) %>%
    ungroup()
  
  year.text <- counts.emmtype %>%
    filter(isolate_type == category) %>%
    group_by(year) %>%
    summarise(total = sum(n))
  
  ggplot(rel_df, aes(x = year, y = proportion, fill = factor(emmtype_grouped))) + 
    geom_col(position = "fill", show.legend = TRUE) + 
    scale_y_continuous(labels = scales::percent_format()) + 
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    coord_cartesian(xlim = x.limits.emmtype) +
    theme_classic() +
    scale_fill_manual(values = c31.named, limits = all_clusters.emmtype, drop = FALSE) + 
    geom_text(data = year.text, aes(x = year, y = 1.04, label = total),
              inherit.aes = F,
              size = 4) +
    labs(y = paste(category, "isolates", sep = " "), fill = expression(italic(emm)-type)) +
    theme(axis.text.x = element_text(angle = 90, size = 14),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 16),
          axis.text = element_text(size=10),
          legend.text = element_text(size=14),
          legend.title = element_text(size = 16))
}

plot.c.emmtype <- plot_category.emmtype("carriage")
plot.n.emmtype <- plot_category.emmtype("noninvasive")
plot.i.emmtype <- plot_category.emmtype("invasive")

plt.legend.emmtype <- get_legend(plot.i.emmtype)

plot.c.emmtype <- plot_category.emmtype("carriage") + theme(legend.position = "none",
                                            axis.text.x = element_blank(),
                                            axis.ticks.x = element_blank())
plot.n.emmtype <- plot_category.emmtype("noninvasive") + theme(legend.position = "none",
                                               axis.text.x = element_blank(),
                                               axis.ticks.x = element_blank())
plot.i.emmtype <- plot_category.emmtype("invasive") + theme(legend.position = "none")

plot_grid(plot_grid(plot.c.emmtype, plot.n.emmtype, plot.i.emmtype,
                    ncol = 1, align = "year"),
          plot_grid(plt.legend.emmtype, ncol = 1),
          rel_widths = c(1, 0.2))

start.year.emmtype <- format(x.limits.emmtype[1], "%Y")
end.year.emmtype <- format(x.limits.emmtype[2], "%Y")

ggsave(paste0(format(Sys.time(), "%Y%m%d"), "_emmtypes_",
              start.year.emmtype, "-", end.year.emmtype, ".svg"),
       width = 45, height = 26, units = "cm", dpi = 300) #edit dimensions of svg as necessary

# #####visualise just the invasive isolates so we can see the results for puerperal sepsis cases
# inv.data <- raw.data %>%
#   filter(isolate_type == 'invasive')
# 
# inv.data <- inv.data %>%
#   mutate(invasive_type = case_when(
#     !(geslacht == "M") & 
#       leeftijdjr < 55 & 
#       leeftijdjr > 10 &
#     (studie == "PGAS" |
#       coalesce(`GAS; puerperaal`, FALSE) |
#       coalesce(`3` %in% c("Cervix",
#                           "Lochia",
#                           "Placenta"), FALSE)
#      ) ~ "puerperal sepsis",
#     TRUE ~ "invasive (other)")) #selecting non-males between the age of 10-55 and if they are recorded as part of the PGAS study or isolate was sourced from cervix, lochia or placenta
# 
# plt.inv.data <- inv.data[!is.na(inv.data$emmtype_cluster),]
# 
# inv.counts <- plt.inv.data %>%
#   count(invasive_type, year, emmtype_cluster)
# # all_clusters <- sort(unique(counts$emmtype_cluster))
# # counts <- counts %>%
# #   mutate(emmtype_cluster = factor(emmtype_cluster, levels = all_clusters))
# 
# inv.emm_levels <- sort(unique(plt.inv.data$emmtype_cluster))
# inv.x.limits <- range(inv.counts$year)
# 
# plot_invasive <- function(inv_type) {
#   rel_df <- inv.counts %>%
#     filter(invasive_type == inv_type) %>% 
#     group_by(year) %>%
#     mutate(proportion = n / sum(n)) %>%
#     ungroup()
#   
#   year.text <- inv.counts %>%
#     filter(invasive_type == inv_type) %>%
#     group_by(year) %>%
#     summarise(total = sum(n))
#   
#   ggplot(rel_df, aes(x = year, y = proportion, fill = factor(emmtype_cluster))) + 
#     geom_col(position = "fill", show.legend = TRUE) + 
#     scale_y_continuous(labels = scales::percent_format()) + 
#     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#     coord_cartesian(xlim = x.limits) +
#     theme_classic() +
#     scale_fill_manual(values = c25.named, limits = all_clusters, drop = FALSE) +
#     geom_text(data = year.text, aes(x = year, y = 1.04, label = total),
#               inherit.aes = F,
#               size = 4) +
#     labs(y = paste(inv_type, "isolates", sep = " "), fill = expression(italic(emm)-cluster)) +
#     theme(axis.text.x = element_text(angle = 90, size = 14),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 16),
#           axis.text = element_text(size=10),
#           legend.text = element_text(size=14),
#           legend.title = element_text(size = 16))
# }
# 
# plot.puerperal <- plot_invasive("puerperal sepsis") + theme(legend.position = "none",
#                                                            axis.text.x = element_blank(),
#                                                            axis.ticks.x = element_blank())
# plot.inv.other <- plot_invasive("invasive (other)") + theme(legend.position = "none",
#                                                            axis.text.x = element_blank(),
#                                                            axis.ticks.x = element_blank())
# 
# plot_grid(plot_grid(plot.puerperal, plot.inv.other, plot.i,
#                     ncol = 1, align = "year"),
#           plot_grid(plt.legend, ncol = 1),
#           rel_widths = c(1, 0.2))
# 
# #visualise by emm cluster grouped by only their letter
# inv.counts.gr <- plt.inv.data %>%
#   count(invasive_type, year, emmtype_cluster_letter)
# # all_clusters.gr <- sort(unique(counts.gr$emmtype_cluster_letter))
# # counts.gr <- counts.gr %>%
# #   mutate(emmtype_cluster_letter = factor(emmtype_cluster_letter, levels = all_clusters.gr))
# 
# inv.emm_levels.gr <- sort(unique(plt.inv.data$emmtype_cluster_letter))
# inv.x.limits.gr <- range(counts.gr$year)
# 
# plot_invasive.gr <- function(inv_type) {
#   rel_df <- inv.counts.gr %>%
#     filter(invasive_type == inv_type) %>% 
#     group_by(year) %>%
#     mutate(proportion = n / sum(n)) %>%
#     ungroup()
#   
#   year.text <- inv.counts.gr %>%
#     filter(invasive_type == inv_type) %>%
#     group_by(year) %>%
#     summarise(total = sum(n))
#   
#   ggplot(rel_df, aes(x = year, y = proportion, fill = factor(emmtype_cluster_letter))) + 
#     geom_col(position = "fill", show.legend = TRUE) + 
#     scale_y_continuous(labels = scales::percent_format()) + 
#     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
#     coord_cartesian(xlim = inv.x.limits.gr) +
#     theme_classic() +
#     scale_fill_manual(values = c25.group, limits = all_clusters.gr, drop = FALSE) +
#     geom_text(data = year.text, aes(x = year, y = 1.04, label = total),
#               inherit.aes = F,
#               size = 4) +
#     labs(y = paste(inv_type, "isolates", sep = " "), fill = expression(italic(emm)-cluster (grouped))) +
#     theme(axis.text.x = element_text(angle = 90, size = 14),
#           axis.title.x = element_blank(),
#           axis.title.y = element_text(size = 16),
#           axis.text = element_text(size=10),
#           legend.text = element_text(size=14),
#           legend.title = element_text(size = 16))
# }
# 
# plot.puerperal.gr <- plot_invasive.gr("puerperal sepsis") + theme(legend.position = "none",
#                                                             axis.text.x = element_blank(),
#                                                             axis.ticks.x = element_blank())
# plot.inv.other.gr <- plot_invasive.gr("invasive (other)") + theme(legend.position = "none",
#                                                             axis.text.x = element_blank(),
#                                                             axis.ticks.x = element_blank())
# 
# plot_grid(plot_grid(plot.puerperal.gr, plot.inv.other.gr, plot.i.gr,
#                     ncol = 1, align = "year"),
#           plot_grid(plt.legend.gr, ncol = 1),
#           rel_widths = c(1, 0.2))
# 
# #add ggsave command