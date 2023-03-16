# Dependencies
library(tidyverse)
library(readxl)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(reshape2)
library(gtable)
library(grid)
library(cowplot)

#ROUND 1----
#Load the data
fileName = 'Round1/data/results-for-enlight-conse-2023-03-16-0639.xlsx'
d = read_excel(fileName)

# Number of subjects
NSubjects = length(d$`Unique Response Number`)

##FIGURE 2: round 1 - ratings of initial item list----

##step 1: data crunching


## Main questions
ratingScaleLevels = c('X – Don\'t know or recognise this quantity/item and cannot evaluate', 
                    '1 – Very unimportant', '2 – Quite unimportant', '3 – Unimportant', 
                    '4 – Neither unimportant nor important', '5 – Important', 
                    '6 – Quite important', '7 – Very important')

d1 <- lapply(d[, c(seq(17, 22), #11 protocol-level aspects
                   seq(26, 30), #12 spectral characteristics
                   seq(34, 36), #13 photometric quantities
                   seq(40, 45), #14 CIE S026 quantities
                   seq(49, 54), #15 CIE S026 melanopic quantities
                   seq(58, 64), #16 colour quantities
                   seq(68, 70), #17 colour rendition metrics
                   seq(74, 77), #18 source-level characteristics
                   seq(81, 84), #19 measurement-level characteristics
                   seq(88, 92), #20 instrument-level characteristics
                   seq(96, 100), #21 spatial characteristics 
                   seq(104, 106), #22 temporal characteristics
                   seq(110, 113))], #23 experimental setting-level
             function(data) ordered(data, ratingScaleLevels))

d2 <- melt(d1)

# Assign labels to question blocks
d2$group <- ordered(substring(d2$L1, 1, 2), seq(11, 23), c('Protocol-level', 'Spectral characteristics', 
                                                           'Photometric quantities', 'CIE S026 quantities',
                                                           'CIE S026 melanopic quantities',
                                                           'Colour quantities',
                                                           'Colour rendition metrics',
                                                           'Source-level',
                                                           'Measurement-level',
                                                           'Instrument-level',
                                                           'Spatial characteristics',
                                                           'Temporal characteristics',
                                                           'Experimental setting-level'))



# Strip numbers in front of variable names
d2$L1 <- ordered(d2$L1, unique(d2$L1))
levels(d2$L1)[53] <- "21.4. Relative size of stimulus (e.g. angular extent in degrees)"
levels(d2$value) <- ratingScaleLevels


### Thresholding to find those that were rated highly
b = prop.table(table(as.numeric(d2$value), d2$L1), 2)


## Get the categories
categories <- factor(substring(colnames(b), 1, 2), seq(11, 23), c('Protocol-level', 'Spectral characteristics', 
                                                           'Photometric quantities', 'CIE S026 quantities',
                                                           'CIE S026 melanopic quantities',
                                                           'Colour quantities',
                                                           'Colour rendition metrics',
                                                           'Source-level',
                                                           'Measurement-level',
                                                           'Instrument-level',
                                                           'Spatial characteristics',
                                                           'Temporal characteristics',
                                                           'Experimental setting-level'))

# Update the names
cat_num <- substring(colnames(b), 1,2)
item_name <- substring(colnames(b), 7)
df_cat <- data.frame(cat_num, item_name)
df_cat$category <- NA
df_cat$category[df_cat$cat_num == 11] <- "Protocol-level aspects"
df_cat$category[df_cat$cat_num == 12] <- "Spectral characteristics"
df_cat$category[df_cat$cat_num == 13] <- "Photometric quantities"
df_cat$category[df_cat$cat_num == 14] <- "CIE S026 quantities"
df_cat$category[df_cat$cat_num == 15] <- "CIE S026 melanopic quantities"
df_cat$category[df_cat$cat_num == 16] <- "Colour quantities"
df_cat$category[df_cat$cat_num == 17] <- "Colour rendition metrics"
df_cat$category[df_cat$cat_num == 18] <- "Source-level characteristics"
df_cat$category[df_cat$cat_num == 19] <- "Measurement-level characteristics"
df_cat$category[df_cat$cat_num == 20] <- "Instrument-level characteristics"
df_cat$category[df_cat$cat_num == 21] <- "Spatial characteristics"
df_cat$category[df_cat$cat_num == 22] <- "Temporal characteristics"
df_cat$category[df_cat$cat_num == 23] <- "Experimental setting-level"
 

colnames(b) <- substring(colnames(b), 7)

##
b1 <- melt(data.frame(b))
b1$Var1 <- ordered(b1$Var1, seq(8, 1), rev(ratingScaleLevels))

b1 <- b1 %>% left_join(df_cat, by=c("Var2" = "item_name")) #%>% 
b1 <-  b1 %>%  group_by(Var2) %>% 
   mutate(rating_incl = value[Var1 == "5 – Important"] + value[Var1 == "6 – Quite important"] + value[Var1 == "7 – Very important"],
          rating_excl = value[Var1 == "3 – Unimportant"] + value[Var1 == "2 – Quite unimportant"] + value[Var1 == "1 – Very unimportant"] + value[Var1 == "X – Don't know or recognise this quantity/item and cannot evaluate"],
          rating_unknown = value[Var1 == "X – Don't know or recognise this quantity/item and cannot evaluate"],
          incl_yn = ifelse(rating_incl > 0.75, "y","n"),
          excl_yn = ifelse(rating_excl > 0.75, "y","n")) %>% 
  ungroup() %>% 
  arrange(category, rating_incl)
  

 
b1$category2 <- factor(b1$category, levels=c('Protocol-level aspects', 'Spectral characteristics', 
                                            'Photometric quantities', 'CIE S026 quantities',
                                            'CIE S026 melanopic quantities',
                                            'Colour quantities',
                                            'Colour rendition metrics',
                                            'Source-level characteristics',
                                            'Measurement-level characteristics',
                                            'Instrument-level characteristics',
                                            'Spatial characteristics',
                                            'Temporal characteristics',
                                            'Experimental setting-level'))
##step 2: visualization

p1 <- ggplot(b1, aes(x = reorder(Var2, rating_incl)))+
  geom_bar(aes(fill = Var1, y = value*100), stat = "identity", position=position_fill(reverse = TRUE)) + 
  coord_flip(clip = "off") + theme_bw() +
  facet_grid(rows="category2", scales="free", space="free_y")+
  theme(legend.position = c(-1.1, -0.1), legend.title = element_blank()) +
  scale_fill_manual(values = rev(c('#DCDCDC', brewer.pal(7, "PRGn"))), drop=FALSE) +
  scale_y_continuous(expand=c(0,0))+
  guides(fill = guide_legend(reverse = TRUE, nrow=4), alpha="none") + 
  theme( strip.text.y = element_text(face = "bold", angle=0, size=7), strip.background = element_blank(), panel.grid=element_blank(), axis.text=element_text(color="#000000", size=7),
         axis.title.x=element_text(size=7), legend.text=element_text(size=7), plot.title = element_text(size=10),
         legend.justification = "left", legend.key.width = unit(8, "pt"), legend.key.height=unit(8, "pt"),panel.border=element_rect(fill="transparent", color="black"),
         legend.margin = margin(t = 0, r = 0, b = 0, l = -7, unit = "cm"), axis.title.y=element_blank(), legend.position = "bottom", axis.ticks=element_line(color="black") ) + 
  xlab('Proportion') + ylab("Proportion")+
  geom_hline(yintercept=0.75, size=0.4, color="grey60", linetype="dotted")



#work around to display facet titles on top: https://stackoverflow.com/questions/70356131/add-space-argument-to-facet-wrap
gt <- ggplotGrob(p1)
panels <-c(subset(gt$layout, grepl("panel", gt$layout$name), se=t:r))
for(i in rev(panels$t-1)) {
  gt = gtable_add_rows(gt, unit(0.5, "lines"), i)
}
panels <-c(subset(gt$layout, grepl("panel", gt$layout$name), se=t:r))
strips <- c(subset(gt$layout, grepl("strip-r", gt$layout$name), se=t:r))
stripText = gtable_filter(gt, "strip-r")
for(i in 1:length(strips$t)) {
  gt = gtable_add_grob(gt, stripText$grobs[[i]]$grobs[[1]], t=panels$t[i]-1, l=5)
}
gt = gt[,-6]
for(i in panels$t) {
  gt$heights[i-1] = unit(0.8, "lines")
  gt$heights[i-2] = unit(0.2, "lines")
}
grid.newpage()
grid.draw(gt)

pdf("manuscript-output/Figure2_Round1_Frequencies_RAW.pdf", height = 10, width = 5.5)
grid.draw(gt)
dev.off()



##TEXT: Round 1 - Analysis to provide numbers for manuscript text  ----
df_res <- b1 %>% distinct(Var2, category, rating_incl, incl_yn, rating_excl, excl_yn, rating_unknown)

length(unique(df_res$Var2)) #61 items in total
sum(df_res$rating_incl >= 0.75) #24 items rated as definitely included / essential
sum(df_res$rating_incl < 0.75) #37 items did not reach inclusion threshold

sum(df_res$rating_excl >= 0.75) #0 items rated as definitely excluded

#range of participants rating as excluded 
min(df_res$rating_excl[df_res$rating_incl < 0.75]) #8%
max(df_res$rating_excl[df_res$rating_incl < 0.75]) #60%
#i.e. indeed, none reach the threshold for exclusion

#unknown/not recognized
df_res %>% arrange(desc(rating_unknown)) %>% filter(rating_unknown >= 0.2) #18 items were unknown to >= 20% of participants
hist(df_res$rating_unknown)

#save list of rating, arranged by rating
df_ranking <- df_res %>% arrange(desc(rating_incl))
write_csv(df_ranking, file = "Round1/outputs/Round1_item-rating.csv")


##TABLE 1: Round 1 - demographics ----
#removed 

#ROUND 2----

##Load the data
file_version <- "2023-03-16-0637"
fileName <- paste0("Round2/data/results-for-enlight-conse-",file_version,".xlsx")
dat <- read_excel(fileName) %>% rename(ID = "1. Please enter your unique 5-character ID.") %>% 
  distinct(ID,.keep_all = T)

nSubjects <- length(unique(dat$ID))

##SUPP FIG 1A: Round 2 - No. of participants voting against inclusion of posthoc included items-----

dat_notinclude <- dat %>% select(ID, matches("^3.[1-9].b") | matches("^3.a.[1-9].b")) %>%
  pivot_longer(-ID) %>% 
  mutate(item = str_split(name, "[.] ", simplify=T)[,3])

dat_notinclude_summ <- dat_notinclude %>% group_by(item) %>% 
  summarize(n_notinclude = sum(value %in% "Should not be included")) %>% 
  mutate(prop_notinclude = n_notinclude/nSubjects*100) %>% 
  arrange(desc(n_notinclude))


pl_notinclude <- ggplot(dat_notinclude_summ, aes(x=fct_reorder(item, prop_notinclude), y=prop_notinclude))+
  geom_bar(stat="identity", position="stack", fill="#BCBDDC") +
  geom_hline(yintercept = 50, color="black", linetype="dotted", size=0.35)+
  scale_y_continuous(expand=expansion(mult=c(0,0.02)), breaks=seq(0, 100, by=20), limit=c(0, 100))+
  coord_flip(clip = "off") + 
  theme_bw() +
  ylab("Percentage (%)")+
  theme( strip.text.y = element_text(face = "bold", angle=0, size=7), panel.grid=element_blank(), axis.text=element_text(color="#000000", size=7),
         axis.title.x=element_text(size=7), legend.text=element_text(size=7), plot.title = element_text(size=10, face="bold", hjust=1),
         legend.justification = "center", legend.key.width = unit(8, "pt"), legend.key.height=unit(8, "pt"),legend.title = element_blank(),
         legend.margin = margin(t = 0, r = 0, b = 0, l = -8, unit = "cm"), axis.title.y=element_blank(), panel.border=element_blank(),
         axis.line = element_line(color="black"), axis.ticks=element_line(color="black"), 
         legend.position = "bottom", text=element_text(size=7, color="black")) +
  ggtitle(paste0("Percentage of votes against inclusion of posthoc items, (n = ", nSubjects, ")" ))#+
#labs(caption=paste0("v", file_version))


##SUPP FIG 1B: Round 2 - Votes for inclusion of excluded items----
dat_include <- dat %>% select(ID, matches("^4.[1-9]")) %>% 
  pivot_longer(-ID) %>% 
  mutate(item = str_split(name, "[.] ", simplify=T)[,2])

dat_include_summ <- dat_include %>% group_by(item) %>% 
  summarize(n_include = sum(value %in% "Should be included")) %>% 
  mutate(prop_include = n_include/nSubjects*100) %>% 
  arrange(desc(n_include))


pl_include <- ggplot(dat_include_summ, aes(x=fct_reorder(item, prop_include), y=prop_include))+
  geom_bar(stat="identity", position="stack", fill="#BCBDDC") +
  geom_hline(yintercept = 50, color="black", linetype="dotted", size=0.35)+
  scale_y_continuous(expand=expansion(mult=c(0,0.02)), breaks=seq(0, 100, by=20), limit=c(0, 100))+
  coord_flip(clip = "off") + theme_bw() +
  ylab("Percentage (%)")+
  theme( strip.text.y = element_text(face = "bold", angle=0, size=7), panel.grid=element_blank(), axis.text=element_text(color="#000000", size=7),
         axis.title.x=element_text(size=7), legend.text=element_text(size=7), plot.title = element_text(size=10, face="bold", hjust=1),
         legend.justification = "center", legend.key.width = unit(8, "pt"), legend.key.height=unit(8, "pt"),legend.title = element_blank(),
         legend.margin = margin(t = 0, r = 0, b = 0, l = -8, unit = "cm"), axis.title.y=element_blank(), panel.border=element_blank(),
         axis.line = element_line(color="black"), axis.ticks=element_line(color="black"), 
         legend.position = "bottom", text=element_text(size=7, color="black")) +
  ggtitle(paste0("Percentage of votes for inclusion of excluded items (n = ", nSubjects, ")"))

pl_rate <- plot_grid(pl_notinclude, pl_include, nrow=2, rel_heights = c(1.1, 2.3), align="hv", axis="tblr", labels=c("A","B"), label_size=10)
ggsave(pl_rate, filename = paste0("manuscript-output/SuppFig1_Round2_votes-for-inclusion-and-exclusion_RAW.pdf"), width=7, height=6)

lvls_format <- c("Text",  "Table", "Figure")

##SUPP FIG2: Round 2 - format reporting of items ----
dat_reporting <- dat %>% select(ID, matches("^2.[1-9]") | matches("^3.[1-9].a") | matches("^3.a.[1-9].a")) %>% 
  pivot_longer(-ID) %>% 
  mutate(item = str_split(name, "[.] ", simplify=T)[,3],
         value1 = str_split(value, ",", simplify=T)[,1],
         value2 = str_split(value, ",", simplify=T)[,2],
         value3 = str_split(value, ",", simplify=T)[,3]) %>% select(-value, -name) %>% 
  pivot_longer(cols=c(value1, value2, value3)) %>% select(-name) %>% 
  filter(value %in% lvls_format)

dat_reporting_summ <- dat_reporting %>% group_by(item, value) %>% summarize(n=n()) %>% 
  ungroup() %>% group_by(item) %>% 
  mutate(n_text = n[value == "Text"],
         prop = n/nSubjects)

#dat_reporting_summ %>% group_by(item) %>% summarize(value_pref = value[n==max(n)]) %>% View()

pl_reporting <- ggplot(dat_reporting_summ, aes(x=fct_reorder(item, n_text), y=prop*100, fill=factor(value, levels=rev(lvls_format))))+
  facet_wrap(~factor(value, levels=(lvls_format)), ncol=3)+
  geom_bar(stat="identity", position="stack") +
  coord_flip(clip = "off") + theme_bw() +
  scale_y_continuous(expand=expansion(mult=c(0,0.1)), breaks=seq(0, 100, by=25))+
  scale_fill_manual(values = brewer.pal(5, "Purples")[3:5], drop=T) +
  ylab("Percentage of participants that voted for format (%)")+
  guides(fill = guide_legend(reverse = TRUE, nrow=1), alpha="none") + 
  geom_hline(yintercept = 100, color="black", linetype="dotted", size=0.2)+
  theme( strip.text = element_text(face = "bold", angle=0, size=7), panel.grid=element_blank(), axis.text=element_text(color="#000000", size=7),
         axis.title.x=element_text(size=7), legend.text=element_text(size=7), plot.title = element_text(size=10, face="bold"),
         legend.justification = "center", legend.key.width = unit(8, "pt"), legend.key.height=unit(8, "pt"), legend.title = element_blank(),
         legend.margin = margin(t = 0, r = 0, b = 0, l = -8, unit = "cm"), axis.title.y=element_blank(), 
         axis.line = element_line(color="black"), axis.ticks=element_line(color="black"), panel.border=element_rect(color="black"),
         legend.position = "none", text=element_text(size=7, color="black"), strip.background = element_blank()) +
  ggtitle(paste0("Format preference of checklist items (n = ", nSubjects, " participants)"))#+
#labs(caption=paste0("v", file_version))

ggsave(pl_reporting, filename = paste0("manuscript-output/SuppFig2_Round2_reporting-format_RAW.pdf"), width=8, height=4)



#ROUND 4-----

# Load the data
fileName <- "Round4/data/results-for-enlight-round-2023-03-16-0636.xlsx"
dat_orig <- read_excel(fileName)

# 3, 4, 5, 6, 7
# 9, 10, 11, 12
# 14, 15, 16
# 18, 19
# 21, 22, 22, 23, 24
# 26, 27, 28
# 30, 31, 32
# 34, 35, 36

# Extract data and calculate proportions of participants that rate items as mandatory regardless of experimental condition
dat <- dat_orig[c(seq(3, 6), seq(9, 12), seq(14, 16), seq(18, 19), seq(21, 24), seq(26, 28), seq(30, 32))]
dat <- dat %>% gather %>% mutate(value = recode(value, "No" = 0, "Yes" = 1))
dat_prop <- dat %>% group_by(key) %>% summarise(prop = mean(value, na.rm = TRUE))

# Group the items
dat_prop$group <- NA
dat_prop$group[startsWith(dat_prop$key, "2")] <- "Protocol"
dat_prop$group[startsWith(dat_prop$key, "3")] <- "Measurement"
dat_prop$group[startsWith(dat_prop$key, "4")] <- "Participant"
dat_prop$group[startsWith(dat_prop$key, "5")] <- "Light source"
dat_prop$group[startsWith(dat_prop$key, "6")] <- "Light level"
dat_prop$group[startsWith(dat_prop$key, "7")] <- "Colour"
dat_prop$group[startsWith(dat_prop$key, "8")] <- "Temporal and spatial characteristics"

# Clean up variable names
dat_prop$key <- substr(dat_prop$key, 6, 100)

# Fix the var names with alpha
dat_prop$key[16] <- "alpha-opic irradiance OR radiance (including melanopic)"
dat_prop$key[17] <- "alpha-opic equivalent daylight illuminance OR luminance (EDI/EDL, including melanopic)"

# Generate the group for the facet
dat_prop$group <- factor(dat_prop$group, levels=c("Protocol", "Measurement", "Participant", "Light source", "Light level", "Colour", "Temporal and spatial characteristics"))

# Variable for binarization
dat_prop$mandatory <- dat_prop$prop > 0.75 

nSubjectsR4 <- length(unique(dat_orig$`Unique Response Number`))

##SUPP FIG 3: Round 4 - rating of items as mandatory-----
p1 <- ggplot(dat_prop) + geom_bar(aes(x=key, y=100*prop, fill=mandatory), stat ="identity") + coord_flip() + 
  facet_grid(group ~ ., scales = "free", space = "free_y", labeller = label_value, switch="x") + 
  geom_hline(yintercept = 75, linetype="dotted", color = "grey60", size=0.4) + 
  scale_fill_manual(values = c("#CBCBC8", "#762A83")) + 
  theme_minimal() + 
  scale_y_continuous(expand=expansion(mult=c(0,0.02)), breaks=seq(0, 100, by=20), limit=c(0, 100))+
  theme( strip.text.y = element_text(face = "bold", angle=0, size=7), strip.background = element_blank(), panel.grid=element_blank(), axis.text=element_text(color="#000000", size=7),
         axis.title.x=element_text(size=7), legend.text=element_text(size=7), plot.title = element_text(size=8, hjust=1, face="bold"),
         legend.justification = "left", legend.key.width = unit(8, "pt"), legend.key.height=unit(8, "pt"),panel.border=element_rect(fill="transparent", color="black"),
         legend.margin = margin(t = 0, r = 0, b = 0, l = -7, unit = "cm"), axis.title.y=element_blank(), legend.position = "none", axis.ticks=element_line(color="black") ) + 
  ggtitle(paste0("Percentage of votes for mandatory inclusion of items (n = ", nSubjectsR4, ")"))+
  xlab("") + 
  ylab("Percentage of respondents [%]")

gt <- ggplotGrob(p1)
panels <-c(subset(gt$layout, grepl("panel", gt$layout$name), se=t:r))
for(i in rev(panels$t-1)) {
  gt = gtable_add_rows(gt, unit(0.5, "lines"), i)
}
panels <-c(subset(gt$layout, grepl("panel", gt$layout$name), se=t:r))
strips <- c(subset(gt$layout, grepl("strip-r", gt$layout$name), se=t:r))
stripText = gtable_filter(gt, "strip-r")
for(i in 1:length(strips$t)) {
  gt = gtable_add_grob(gt, stripText$grobs[[i]]$grobs[[1]], t=panels$t[i]-1, l=5)
}
gt = gt[,-6]
for(i in panels$t) {
  gt$heights[i-1] = unit(0.8, "lines")
  gt$heights[i-2] = unit(0.2, "lines")
}
grid.newpage()
grid.draw(gt)

ggsave('manuscript-output/SuppFig3_Round4_rating-mandatory-proportions_RAW.pdf', gt, height=4.5, width=6)



## Data on satisfaction
dat_rating <- dat_orig[seq(34, 36)]

dat_rating$`17. How satisfied are you with the ENLIGHT modified Delphi process?` <- factor(dat_rating$`17. How satisfied are you with the ENLIGHT modified Delphi process?`, levels=c("Very dissatisfied", "Dissatisfied", "Unsure", "Satisfied", "Very satisfied"))
dat_rating$`18. How satisfied are you with the ENLIGHT checklist?` <- factor(dat_rating$`18. How satisfied are you with the ENLIGHT checklist?`, levels=c("Very dissatisfied", "Dissatisfied", "Unsure", "Satisfied", "Very satisfied"))
dat_rating$`19. How satisfied are you with the ENLIGHT guidelines?` <- factor(dat_rating$`19. How satisfied are you with the ENLIGHT guidelines?`, levels=c("Very dissatisfied", "Dissatisfied", "Unsure", "Satisfied", "Very satisfied"))

dat_rating_summary <- gather(dat_rating)
dat_rating_summary$value <- factor(dat_rating_summary$value)
dat_rating_summary$key <- substr(dat_rating_summary$key, 5, 100)

dat_rating_summary %>% na.omit() %>% count(key)

##FIGURE 3: Round 4 - satisfaction of participants with ENLIGHT-----
ggplot((dat_rating_summary), aes(fill=factor(value, levels=rev(c("Very dissatisfied", "Dissatisfied", "Unsure", "Satisfied", "Very satisfied"))), x=key)) + 
  geom_bar(position=position_fill(reverse = TRUE)) + 
  scale_y_continuous(labels = scales::percent, expand=c(0,0)) + 
  theme_minimal() + 
  scale_fill_manual(values = rev(brewer.pal(5, "PiYG")), drop=FALSE) + 
  coord_flip(clip="off") + 
  theme( strip.text.y = element_text(face = "bold", angle=0, size=7), strip.background = element_blank(), panel.grid=element_blank(), axis.text=element_text(color="#000000", size=7),
         axis.title.x=element_text(size=7), legend.text=element_text(size=7), plot.title = element_text(size=8, hjust=1, face="bold"),
         legend.justification = "left", legend.key.width = unit(8, "pt"), legend.key.height=unit(8, "pt"),panel.border=element_rect(fill="transparent", color="black"),
         legend.margin = margin(t = 0, r = 0, b = 0, l = -7, unit = "cm"), axis.title.y=element_blank(), legend.position = "bottom", axis.ticks=element_line(color="black"),
         legend.title=element_blank()) + 
  xlab("") + 
  ylab("Percentage of responders [%]") + 
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  ggtitle(paste0("Participant rating of ENLIGHT process and outcomes (n = ", nSubjectsR4,")"))

ggsave('manuscript-output/Figure3_Round4_satisfaction_RAW.pdf', height=1.5, width=6)

