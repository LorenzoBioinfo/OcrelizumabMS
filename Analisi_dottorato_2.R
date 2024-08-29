#################################################################################
# Analisi dottorato
# MS T CELL MAIT

library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggprism)
library(tibble)
library(stringr)

info_cl<-read_excel("./TabelloneOcrelizumab FINALE (conte e %) - 05Agosto24.xlsx")

info_cl<-info_cl[,c(1:8,606)]

info_cl<-info_cl %>%
  group_by(`Codice pz`) %>%  
  mutate(Valore_t0 =`EDSS`[Timepoint == "T0"],  
         Difference_EDSS = `EDSS`- Valore_t0,
         Ratio_EDSS=`EDSS`/ Valore_t0)
info_cl<-info_cl %>%
  group_by(`Codice pz`) %>%  
  mutate(Valore_t0 =`NFL (pg/ml)`[Timepoint == "T0"],  
         Difference_NFL = `NFL (pg/ml)`- Valore_t0,
         Ratio_NFL=`NFL (pg/ml)`/ Valore_t0)

######### Datasheet T-MAIT

data_4<-read_excel("./TabelloneOcrelizumab FINALE (conte e %) - 05Agosto24.xlsx",
                   sheet = "MS_T & MAIT",)
colnames(data_4) <- gsub(" ", "", colnames(data_4))
names(data_4)
data_4<-as.data.frame(data_4)


data_4[, 6:184] = lapply(data_4[, 6:184], as.numeric)

data_4$TP
for(col in names(data_4[!is.na(data_4$TP),])[6:184]){
  print(col)
  
  data_4[,col]<-as.numeric(data_4[,col])
  test<-kruskal.test(data_4[,col]~data_4$Timepoint,data = data_4)
  
  
  if(test$p.value<0.05){
  p<-ggplot(data_4[!is.na(data_4$TP),],
            aes(factor(Timepoint),
                                           data_4[!is.na(data_4$TP),col]))+
    geom_boxplot(outliers = F)+
    ggbeeswarm::geom_quasirandom(shape=21,size=3,
                                 aes(fill=factor(Timepoint)))+
    ggprism::theme_prism()+
    stat_compare_means(comparisons = list(c("T0","T1"),
                                          c("T0","T2"),
                                          c("T0","T3"),
                                          c("T0","T4"),
                                          c("T0","T5"),
                                          c("T0","T6")))+
    ylab(col)+ggsci::scale_fill_aaas()+xlab("")
  print(p)
}
}

dim(data_4)
head(data_4[,1:5])


data_1$Code<-paste0(data_1$`Codice pz`,"_",data_1$Timepoint)
data_4$Code<-paste0(data_4$`Codicepz`,"_",data_4$Timepoint)
merge1_4<-merge(data_1,data_4,by="Code")
dim(merge1_4)
X <- merge1_4$`NFL (pg/ml)`
names(merge1_4)


#############################################
# per ogni timepoint, scelgo confronto quelli con
# alto o basso NFL rispetto al valore mediano di quel 
# tp
merge1_4$Timepoint.x
for (tm in unique(merge1_4$Timepoint.x)){
  
  
  tem<-merge1_4[merge1_4$Timepoint.x==tm,]
  
  mediana<-median(tem$`NFL (pg/ml)`,na.rm = T)
  
  tem$Group<-0
  tem$Group<-ifelse(tem$`NFL (pg/ml)`<=mediana,
                    "Low","High")
  
  for (col in names(tem)[16:141] ){
    
    if (!all(tem[,col]==0)& col!="DN2/MFICD71"&col!="DN2/MFICD31")
      test<-wilcox.test(tem[!is.na(tem$`NFL (pg/ml)`),col]~Group,data=
                          tem[!is.na(tem$`NFL (pg/ml)`),],exact=F,
                        na.action = "na.omit")
    if (test$p.value<0.05){
      print(tm)
      print(col)
      print(test$p.value)
      print("=====")
      
      p1<-ggplot(tem[!is.na(tem$`NFL (pg/ml)`),],aes(
        factor(Group,
               levels = c("Low","High")),
        tem[!is.na(tem$`NFL (pg/ml)`),col]))+
        geom_boxplot(outliers = F)+
        ggbeeswarm::geom_quasirandom(shape=21,
                                     size=3,
                                     aes(fill=Group))+
        ggprism::theme_prism(base_size = 15)+
        ylab(col)+xlab("")+ggtitle(paste0(tm," ",col))+
        stat_compare_means(comparisons = list(c("Low",
                                                "High")))+
        ggsci::scale_fill_aaas()
      print(p1)

    }
  }
  
}

#################################
# Differenze in base alla difference
# NFL tx - NFL t0

merge1_4$Group_diff<-
  ifelse(merge1_4$Difference<=0,"M",
         "P")

merge1_4$Group_ratio<-
  ifelse(merge1_4$Ratio<=1,"M",
         "P")
for (col in names(merge1_4)[16:194]){
  test<-
    wilcox.test(merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                           !is.na(merge1_4$Group_diff),col]~
                  Group_ratio,data=merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                                              !is.na(merge1_4$Group_diff),])
  if (test$p.value<0.05){
    
    pl<-ggplot(merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                          !is.na(merge1_4$Group_diff),],
               aes(
                 y=merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                              !is.na(merge1_4$Group_diff),col],
                 x=factor(Group_diff,
                          levels = c("M","P"))))+
      geom_boxplot(outliers = F)+
      ggbeeswarm::geom_quasirandom(shape=21,
                                   size=3,
                                   aes(fill=Group_diff))+
      ggprism::theme_prism(base_size = 15)+
      ylab(col)+
      xlab("")+facet_wrap(~Timepoint.x,nrow = 1)+
      stat_compare_means(comparisons = list(c("M","P")))+
      ggsci::scale_fill_aaas()
    
    print(pl)
    
    
    pl2<-ggplot(merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                           !is.na(merge1_4$Group_diff),],
                aes(
                  y=merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                               !is.na(merge1_4$Group_diff),col],
                  x=factor(Group_diff,
                           levels = c("M","P"))))+
      geom_boxplot(outliers = F)+
      ggbeeswarm::geom_quasirandom(shape=21,
                                   size=3,
                                   aes(fill=Group_diff))+
      ggprism::theme_prism(base_size = 15)+
      ylab(col)+
      xlab("")+
      stat_compare_means(comparisons = list(c("M","P")))+
      ggsci::scale_fill_aaas()
    
    print(pl2)
    
    
  }
}

#################################
# Differenze in base alla rati
# NFL tx / NFL t0

merge1_4$Group_ratio<-
  ifelse(merge1_4$Ratio<=1,"M",
         "P")


for (col in names(merge1_4)[16:194]){
  
  test<-
    wilcox.test(merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                           !is.na(merge1_4$Group_ratio),col]~
                  Group_ratio,data=merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                                              !is.na(merge1_4$Group_ratio),])
  if (test$p.value<0.05){
    pl<-ggplot(merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                          !is.na(merge1_4$Group_ratio),],
               aes(
                 y=merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                              !is.na(merge1_4$Group_ratio),col],
                 x=factor(Group_ratio,
                          levels = c("M","P"))))+
      geom_boxplot(outliers = F)+
      ggbeeswarm::geom_quasirandom(shape=21,
                                   size=3,
                                   aes(fill=Group_ratio))+
      ggprism::theme_prism(base_size = 15)+
      ylab(col)+
      xlab("")+facet_wrap(~Timepoint.x,nrow = 1)+
      stat_compare_means(comparisons = list(c("M","P")))+
      ggsci::scale_fill_aaas()
    
    print(pl)
    
    
    pl2<-ggplot(merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                           !is.na(merge1_4$Group_ratio),],
                aes(
                  y=merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                               !is.na(merge1_4$Group_ratio),col],
                  x=factor(Group_ratio,
                           levels = c("M","P"))))+
      geom_boxplot(outliers = F)+
      ggbeeswarm::geom_quasirandom(shape=21,
                                   size=3,
                                   aes(fill=Group_ratio))+
      ggprism::theme_prism(base_size = 15)+
      ylab(col)+
      xlab("")+
      stat_compare_means(comparisons = list(c("M","P")))+
      ggsci::scale_fill_aaas()
    print(pl2)
    
  }
}


###############################
# correlazione con diff e valore
for (col in names(merge1_4)[16:194]){
  
  test<-cor.test(merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                            !is.na(merge1_4$Group_diff)&
                            merge1_4$Timepoint.x!="T0",col],
                 merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                            !is.na(merge1_4$Group_diff)&
                            merge1_4$Timepoint.x!="T0","Difference"])
  
  if (test$p.value<0.05){
    
    pp<-ggplot(merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                          !is.na(merge1_4$Group_ratio)&
                          merge1_4$Timepoint.x!="T0",],
               aes(Difference,merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                                         !is.na(merge1_4$Group_ratio)&
                                         merge1_4$Timepoint.x!="T0",col]))+
      geom_point(fill="darkred",
                 shape=21,size=3)+ylab(col)+
      geom_smooth(method = "lm",se=F)+
      stat_cor()+
      ggprism::theme_prism(base_size = 15)
    
    print(pp)
    
    
  }
}

##########################
# correlazione con Ratio e valore
for (col in names(merge1_4)[16:194]){
  
  test<-cor.test(merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                            !is.na(merge1_4$Group_ratio)&
                            merge1_4$Timepoint.x!="T0",col],
                 merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                            !is.na(merge1_4$Group_ratio)&
                            merge1_4$Timepoint.x!="T0","Ratio"])
  
  if (test$p.value<0.05){
    
    pp<-ggplot(merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                          !is.na(merge1_4$Group_ratio)&
                          merge1_4$Timepoint.x!="T0",],
               aes(Ratio,merge1_4[!is.na(merge1_4$`NFL (pg/ml)`)&
                                    !is.na(merge1_4$Group_ratio)&
                                    merge1_4$Timepoint.x!="T0",col]))+
      geom_point(fill="darkblue",
                 shape=21,size=3)+ylab(col)+
      geom_smooth(method = "lm",se=F)+
      stat_cor()+
      ggprism::theme_prism(base_size = 15)
    #facet_wrap(~Timepoint)
    
    print(pp)
    
  }
}


##############
# Alluvial plot
library(tidyverse)
library(ggalluvial)

names(merge1_4)

for (name in c("CD3/CD4/Tconv/Naive/",
               "CD3/CD4/Tconv/Nonnaive/",
               "CD3/CD4/Tconv/Nonnaive/Th1-17/",
               "CD3/CD4/Treg/Nonnaive/",
               "CD3/CD4/Treg/Naive/",
               "CD3/CD4-/MAIT",
               "CD3/CD4-/NonMAIT/CD8/Naive/",
               "CD3/CD4-/NonMAIT/CD8/Nonnaive/"
               
               )){
  
  print(name)
  
  cols_to_pivot <- names(merge1_4)[grepl(name, names(merge1_4)) & !grepl("MFI", names(merge1_4))
                                   & !grepl("non", names(merge1_4))
                                   &!grepl("Median", names(merge1_4))
                                   &!grepl("count", names(merge1_4))]
  
  if (name=="nonASC/"){
    cols_to_pivot <- names(merge1_4)[grepl(name, names(merge1_4)) & !grepl("MFI", names(merge1_4))]
    
  }
  df_long <- merge1_4 %>%
    pivot_longer(cols = all_of(cols_to_pivot),
                 names_to = "Population",
                 values_to = "Value")
  
  df_medians <- df_long %>%
    group_by(Timepoint.x, Population) %>%
    summarize(MedianValue = median(Value, na.rm = TRUE))
  
  df_medians$Population<-str_replace(df_medians$Population,
                                     name,"")
  df_medians$Population<-str_replace(df_medians$Population,
                                     "\\|Freq.ofParent","")
  df_medians[df_medians$MedianValue==0,]$MedianValue=0.0001
  allu<-ggplot(df_medians,
               aes(x = Timepoint.x, stratum =Population,
                   alluvium =Population,
                   y = MedianValue,
                   fill = Population, label = Population)) +
    # scale_x_discrete(expand = c(.1, .1)) +
    geom_flow() +
    geom_stratum(alpha = .5) +
    geom_text(stat = "stratum", size = 3) +
    theme(legend.position = "none")+
    ggprism::theme_prism()+
    ggtitle(name)
  print(allu)
}

########################################

for (name in c("CD3/CD4/Tconv/Naive/",
               "CD3/CD4/Tconv/Nonnaive/",
               "CD3/CD4/Tconv/Nonnaive/Th1-17/",
               "CD3/CD4/Treg/Nonnaive/",
               "CD3/CD4/Treg/Naive/",
               "CD3/CD4-/MAIT/",
               "CD3/CD4-/NonMAIT/CD8/Naive/",
               "CD3/CD4-/NonMAIT/CD8/Nonnaive/"
)){
  
  print(name)
  
  cols_to_pivot <- names(merge1_4)[grepl(name, names(merge1_4)) & !grepl("MFI", names(merge1_4))
                                   & !grepl("non", names(merge1_4))
                                   &!grepl("count", names(merge1_4)) &!grepl("Median", names(merge1_4))
                                   |
                                     grepl("NFL", names(merge1_4))  
  ]
  
  if (name=="nonASC/"){
    cols_to_pivot <- names(merge1_4)[ grepl("NFL", names(merge1_4)) | grepl(name, names(merge1_4)) & !grepl("MFI", names(merge1_4))]
    
  }
  df_long <- merge1_4 %>%
    pivot_longer(cols = all_of(cols_to_pivot),
                 names_to = "Population",
                 values_to = "Value")
  
  df_medians <- df_long %>%
    group_by(Timepoint.x, Population) %>%
    summarize(MedianValue = median(Value, na.rm = TRUE))
  
  df_medians$Population<-str_replace(df_medians$Population,
                                     name,"")
  df_medians$Population<-str_replace(df_medians$Population,
                                     "\\|Freq.ofParent","")
  
  df_medians[df_medians$MedianValue==0,]$MedianValue=0.0001
  allu<-ggplot(df_medians,
               aes(x = Timepoint.x, stratum =Population,
                   alluvium =Population,
                   y = MedianValue,
                   fill = Population, label = Population)) +
    # scale_x_discrete(expand = c(.1, .1)) +
    geom_flow() +
    geom_stratum(alpha = .5) +
    geom_text(stat = "stratum", size = 3) +
    theme(legend.position = "none")+
    ggprism::theme_prism()+
    ggtitle(name)
  print(allu)
}


##################################################

min_max_scaling <- function(x) {
  return((x - min(x,na.rm = T)) / (max(x,na.rm = T) - min(x,na.rm = T)))
}

# Applicazione su un dataframe

for (name in c("CD3/CD4/Tconv/Naive/",
               "CD3/CD4/Tconv/Nonnaive/",
               "CD3/CD4/Tconv/Nonnaive/Th1-17/",
               "CD3/CD4/Treg/Nonnaive/",
               "CD3/CD4/Treg/Naive/",
               "CD3/CD4-/MAIT/",
               "CD3/CD4-/NonMAIT/CD8/Naive/",
               "CD3/CD4-/NonMAIT/CD8/Nonnaive/")){
  
  print(name)
  
  cols_to_pivot <- names(merge1_4)[grepl(name, names(merge1_4)) & !grepl("MFI", names(merge1_4))
                                   & !grepl("non", names(merge1_4))&!grepl("Median", names(merge1_4))
                                   &!grepl("count", names(merge1_4)) |
                                     grepl("NFL", names(merge1_4))  
  ]
  
  if (name=="nonASC/"){
    cols_to_pivot <- names(merge1_4)[ grepl("NFL", names(merge1_4)) | grepl(name, names(merge1_4)) & !grepl("MFI", names(merge1_4))]
    
  }
  df_normalized <-  merge1_4  %>%
    mutate(across(where(is.numeric), min_max_scaling))
  df_long <- df_normalized  %>%
    pivot_longer(cols = all_of(cols_to_pivot),
                 names_to = "Population",
                 values_to = "Value")
  
  df_medians <- df_long %>%
    group_by(Timepoint.x, Population) %>%
    summarize(MedianValue = median(Value, na.rm = TRUE))
  
  df_medians$Population<-str_replace(df_medians$Population,
                                     name,"")
  df_medians$Population<-str_replace(df_medians$Population,
                                     "\\|Freq.ofParent","")
  
  df_medians[df_medians$MedianValue==0&!is.na(df_medians$MedianValue),]$MedianValue=0.0001
  allu<-ggplot(df_medians,
               aes(x = Timepoint.x, stratum =Population,
                   alluvium =Population,
                   y = MedianValue,
                   fill = Population, label = Population)) +
    # scale_x_discrete(expand = c(.1, .1)) +
    geom_flow() +
    geom_stratum(alpha = .5) +
    geom_text(stat = "stratum", size = 3) +
    theme(legend.position = "none")+
    ggprism::theme_prism()+
    ggtitle(name)
  print(allu)
  plt2<-ggplot()+
    geom_line(data=df_medians[df_medians$Population!="NFL (pg/ml)",],
              aes(Timepoint.x,
                                                                         MedianValue,
                                                                         group=Population),color="grey")+
    
    geom_line(data=df_medians[df_medians$Population==
                                "NFL (pg/ml)",],
              aes(Timepoint.x,MedianValue,
                  group=Population),color="red")+
    geom_point(df_medians,mapping=aes(Timepoint.x,
                                      MedianValue,
                                      fill=Population),shape=21,size=3)+
    ggprism::theme_prism(base_size = 15)
  print(plt2)
}




for (col in names(merge1_4)[16:194]){
  df_normalized <-  merge1_4  %>%
    mutate(across(where(is.numeric), min_max_scaling))
  p<-ggplot(df_normalized)+
    geom_line(aes(Timepoint.x, df_normalized[,col],group=`Codice pz`))+
    geom_point(aes(Timepoint.x, df_normalized[,col],fill=`Codice pz`),shape=21)+
    geom_line(aes(Timepoint.x, `NFL (pg/ml)`,group=`Codice pz`),color="grey")+
    geom_point(aes(Timepoint.x, `NFL (pg/ml)`),shape=21,fill="darkred")+
    facet_wrap(~`Codice pz`)+ylab(col)+
    ggprism::theme_prism()
  #print(p)
  
  p2<-ggplot(merge1_4)+
    geom_line(aes(Timepoint.x, merge1_4[,col],group=`Codice pz`))+
    geom_point(aes(Timepoint.x, merge1_4[,col],fill=`Codice pz`),shape=21)+
    geom_line(aes(Timepoint.x, `NFL (pg/ml)`,group=`Codice pz`),color="grey")+
    geom_point(aes(Timepoint.x, `NFL (pg/ml)`),shape=21,fill="darkred")+
    facet_wrap(~`Codice pz`)+ylab(col)+
    ggprism::theme_prism()
  print(p2)
}



for (name in names(merge1_4)[16:194]){
  df_long <- merge1_4 %>%
    pivot_longer(cols = c(name, `NFL (pg/ml)`), names_to = "Marker", values_to = "Value")
  
  p<-ggplot(df_long, aes(x = Timepoint.x, y = Value,
                         
                         group = interaction(`Codice pz`, Marker))) +
    geom_line(aes(linetype = "Patient Line",color = Marker), size = 0.8,
              alpha=0.2) +  # Linee per ogni paziente
    stat_smooth(aes(group = Marker, linetype = "Trend Line",
                    color = Marker), 
                method = "loess", se = FALSE, 
                size = 2) +  # Linee di tendenza
    scale_linetype_manual(values = c("Patient Line" = "solid", "Trend Line" = "dashed")) +  # Differenziazione del tipo di linea
    labs(title = paste0(name," e NFL nel Tempo per Paziente"),
         x = "Timepoint",
         y = "Valore",
         color = "Marker") +
    ggprism::theme_prism()+ggsci::scale_color_d3()
  
  print(p)
}


### MAIT T conta

data_5<-read_excel("./TabelloneOcrelizumab FINALE (conte e %) - 05Agosto24.xlsx",sheet = "MS_T & MAIT CONTA")
colnames(data_5) <- gsub(" ", "", colnames(data_5))
names(data_5)

data_5<-as.data.frame(data_5)
names(data_5)
for(col in names(data_5[!is.na(data_5$Timepoint),])[5:146]){
  print(col)
  p<-ggplot(data_5[!is.na(data_5$Timepoint),],aes(factor(Timepoint),
                                           data_5[!is.na(data_5$Timepoint),col]))+
    geom_boxplot(outliers = F)+
    ggbeeswarm::geom_quasirandom(shape=21,size=3,aes(fill=factor(Timepoint)))+
    ggprism::theme_prism()+
    stat_compare_means(comparisons = list(c("T0","T1"),
                                          c("T0","T2"),
                                          c("T0","T3"),
                                          c("T0","T4"),
                                          c("T0","T5"),
                                          c("T0","T6")))+
    ylab(col)+xlab("")
  print(p)
}


## Alluvial
cols_to_pivot <- names(data_5)[5:17]
df_long <- data_3  %>%
  pivot_longer(cols = all_of(cols_to_pivot),
               names_to = "Population",
               values_to = "Value")
df_medians <- df_long %>%
  group_by(TP, Population) %>%
  summarize(MedianValue = median(Value, na.rm = TRUE))

df_medians$Population<-str_replace(df_medians$Population,
                                   "/mLcount","")

df_medians[df_medians$MedianValue==0&!is.na(df_medians$MedianValue),]$MedianValue=0.0001
allu<-ggplot(df_medians,
             aes(x = TP, stratum =Population,
                 alluvium =Population,
                 y = MedianValue,
                 fill = Population, label = Population)) +
  # scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none")+
  ggprism::theme_prism()+
  ggtitle(name)
print(allu)



ggplot(df_medians[df_medians$TP!=0&!is.na(df_medians$TP),],
       aes(x = factor(TP), stratum =Population,
           alluvium =Population,
           y = MedianValue,
           fill = Population, label = Population)) +
  # scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none")+
  ggprism::theme_prism()+
  ggtitle("COUNT")+ylim(c(0,3000))



#####################################################

#####

data_4$Timepoint
library(lubridate)

info_cl$Eta <- floor(interval(info_cl$`Date of Birth`, now()) / years(1))
info_cl$Groupeta<-ifelse(info_cl$Eta<=48,"Young","Old")
table(data_4$Timepoint)
table(info_cl$Timepoint)
data_4_cl<-merge(info_cl,data_4,
                 by.x=c("Codice pz",
                        "Timepoint"), by.y=c("Codicepz","Timepoint"))


for( col in names(data_4_cl)[20:199]){
  
  print(col)
  temp<-data_4_cl[!is.na(data_4_cl[,col])&
                    !is.na(data_4_cl[,"Fenotipo clinico"]),]
  res<-vector()
  for (time in unique(temp$Timepoint)){
    
    if (!all(temp[temp$Timepoint==time,col]==0) &
        !all(temp[temp$Timepoint==time,"Fenotipo clinico"]==
             temp[temp$Timepoint==time,"Fenotipo clinico"][1])){
      test<-wilcox.test(temp[temp$Timepoint==time,col]~
                          temp[temp$Timepoint==time,"Fenotipo clinico"])
      
      if (test$p.value<0.05){
        res<-c(res,T)
      }
    }
  }
  
  if (any(res)){
    pl1<-ggplot(data_4_cl[!is.na(data_4_cl$`Fenotipo clinico`),],
                aes(`Fenotipo clinico`,data_4_cl[!is.na(data_4_cl$`Fenotipo clinico`),col]))+
      geom_boxplot(outliers = F)+
      ggbeeswarm::geom_quasirandom(shape=21,size=3,
                                   aes(fill=`Fenotipo clinico`))+
      ggprism::theme_prism(base_size = 10)+ggsci::scale_fill_aaas()+
      facet_wrap(~Timepoint,nrow=1)+
      stat_compare_means(comparisons = list(c("PP","RR")))+
      ylab(col)
    #print(pl1)
  }
  
  
  
  temp<-data_4_cl[!is.na(data_4_cl[,col])&
                    !is.na(data_4_cl[,"Groupeta"]),]
  res<-vector()
  for (time in unique(temp$Timepoint)){
    
    if (!all(temp[temp$Timepoint==time,col]==0) &
        !all(temp[temp$Timepoint==time,"Groupeta"]==
             temp[temp$Timepoint==time,"Groupeta"][1])){
      test<-wilcox.test(temp[temp$Timepoint==time,col]~
                          temp[temp$Timepoint==time,"Groupeta"])
      
      if (test$p.value<0.05){
        res<-c(res,T)
      }
    }
  }
  
  if (any(res)){
    
    pl2<-ggplot(data_4_cl[!is.na(data_4_cl$`Groupeta`),],
                aes(Groupeta,data_4_cl[!is.na(data_4_cl$Groupeta),col]))+
      geom_boxplot(outliers = F)+
      ggbeeswarm::geom_quasirandom(shape=21,size=3,
                                   aes(fill=Groupeta))+
      ggprism::theme_prism(base_size = 10)+ggsci::scale_fill_aaas()+
      facet_wrap(~Timepoint,nrow=1)+
      stat_compare_means(comparisons = list(c("Young","Old")))+
      ylab(col)
    print(pl2)
  }
}






######

both<-info_cl[info_cl$Ratio_NFL<1&
                info_cl$Ratio_EDSS<1&
                info_cl$Timepoint=="T6",]$`Codice pz`

nflnoedss<-info_cl[info_cl$Ratio_NFL<1&
                     info_cl$Ratio_EDSS==1&
                     info_cl$Timepoint=="T6",]$`Codice pz`
nfledsspeggio<-info_cl[info_cl$Ratio_NFL<1&
                         info_cl$Ratio_EDSS>1&
                         info_cl$Timepoint=="T6",]$`Codice pz`
tuttopeggio<-info_cl[info_cl$Ratio_NFL>1&
                       info_cl$Ratio_EDSS>1&
                       info_cl$Timepoint=="T6",]$`Codice pz`


data_4_cl$Status<-0

data_4_cl[data_4_cl$`Codice pz`%in%both,]$Status<-"dNFL dEDSS"
data_4_cl[data_4_cl$`Codice pz`%in%nflnoedss,]$Status<-"dNFL sEDSS"
data_4_cl[data_4_cl$`Codice pz`%in%nfledsspeggio,]$Status<-"dNFL iEDSS"
data_4_cl[data_4_cl$`Codice pz`%in%tuttopeggio,]$Status<-"iNFL iEDSS"

data_4_cl[data_4_cl$Timepoint.y=="T0",]$Status
for( col in names(data_4_cl)[20:65]){
  
  print(col)
  temp<-data_4_cl[!is.na(data_4_cl[,col])&
                    !data_4_cl[,"Status"]==0,]
  res<-vector()
  for (time in unique(temp$Timepoint)){
    
    if (!all(temp[temp$Timepoint==time,col]==0) &
        !all(temp[temp$Timepoint==time,"Status"]==
             temp[temp$Timepoint==time,"Status"][1])){
      test<-kruskal.test(temp[temp$Timepoint==time,col]~
                           temp[temp$Timepoint==time,"Status"])
      
      if (test$p.value<0.05){
        res<-c(res,T)
      }
    }
  }
  
  
  pl1<-ggplot(data_4_cl[!data_4_cl$Status==0,],
              aes(Status,data_4_cl[!data_4_cl$Status==0,col]))+
    geom_boxplot(outliers = F)+
    ggbeeswarm::geom_quasirandom(shape=21,size=3,
                                 aes(fill=Status))+
    ggprism::theme_prism(base_size = 10,axis_text_angle = 45)+ggsci::scale_fill_aaas()+
    facet_wrap(~Timepoint,nrow=1)+
    stat_compare_means(comparisons = list(c("dNFL dEDSS","dNFL iEDSS"),
                                          c("dNFL dEDSS","dNFL sEDSS"),
                                          c("dNFL dEDSS","iNFL iEDSS"),
                                          c("dNFL iEDSS","dNFL sEDSS"),
                                          c("dNFL iEDSS","iNFL iEDSS"),
                                          c("dNFL sEDSS","iNFL iEDSS")))+
    ylab(col)
  print(pl1)
}







data_4_cl$Status2<-0

data_4_cl[data_4_cl$`Codice pz`%in%c(both,
                                     nflnoedss),]$Status2<-"Stable"
data_4_cl[data_4_cl$`Codice pz`%in%c(nfledsspeggio,tuttopeggio),]$Status2<-"Worsening"

for( col in names(data_4_cl)[20:160]){
  
  print(col)
  temp<-data_4_cl[!is.na(data_4_cl[,col])&
                    !data_4_cl[,"Status2"]==0,]
  res<-vector()
  for (time in unique(temp$Timepoint)){
    
    if (!all(temp[temp$Timepoint==time,col]==0) &
        !all(temp[temp$Timepoint==time,"Status2"]==
             temp[temp$Timepoint==time,"Status2"][1])){
      test<-wilcox.test(temp[temp$Timepoint==time,col]~
                          temp[temp$Timepoint==time,"Status2"])
      
      if (test$p.value<0.05){
        res<-c(res,T)
      }
    }
  }
  
  if (any(res)){
    pl1<-ggplot(data_4_cl[!data_4_cl$Status2==0,],
                aes(Status2,data_4_cl[!data_4_cl$Status2==0,col]))+
      geom_boxplot(outliers = F)+
      ggbeeswarm::geom_quasirandom(shape=21,size=3,
                                   aes(fill=Status2))+
      ggprism::theme_prism(base_size = 10,axis_text_angle = 45)+
      ggsci::scale_fill_aaas()+
      facet_wrap(~Timepoint,nrow=1)+
      stat_compare_means(comparisons = list(c("Stable","Worsening")))+
      ylab(col)
    print(pl1)
  }
}



###############################
# NFL
info_cl$Ratio_NFL
data_4_cl$Status3<-0
data_4_cl$Status3<-ifelse(data_4_cl$Ratio_NFL<=1,
                          "Stable","Worsening")

View(data_4_cl)


for( col in names(data_4_cl)[20:160]){
  
  print(col)
  temp<-data_4_cl[!is.na(data_4_cl[,col])&
                    !data_4_cl[,"Status3"]==0&
                    !is.na(data_4_cl$Status3),]
  res<-vector()
  for (time in unique(temp$Timepoint)){
    
    if (!all(temp[temp$Timepoint==time,col]==0) &
        !all(temp[temp$Timepoint==time,"Status3"]==
             temp[temp$Timepoint==time,"Status3"][1])){
      test<-wilcox.test(temp[temp$Timepoint==time,col]~
                          temp[temp$Timepoint==time,"Status3"])
      
      if (test$p.value<0.05){
        res<-c(res,T)
      }
    }
  }
  
  if (any(res)){
    pl1<-ggplot(data_4_cl[!data_4_cl$Status3==0,],
                aes(Status3,data_4_cl[!data_4_cl$Status3==0,col]))+
      geom_boxplot(outliers = F)+
      ggbeeswarm::geom_quasirandom(shape=21,size=3,
                                   aes(fill=Status3))+
      ggprism::theme_prism(base_size = 10,axis_text_angle = 45)+
      ggsci::scale_fill_aaas()+
      facet_wrap(~Timepoint,nrow=1)+
      stat_compare_means(comparisons = list(c("Stable","Worsening")))+
      ylab(col)
    print(pl1)
  }
}




