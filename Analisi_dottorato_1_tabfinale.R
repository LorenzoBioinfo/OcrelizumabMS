###############
# Analisi Dottorato
# Primi 2 Sheets (NFL e B %)
#############
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggprism)
library(tibble)
library(stringr)

######### Datasheet 1 # NFL
data_1<-read_excel("./TabelloneOcrelizumab FINALE (conte e %) - 05Agosto24.xlsx",sheet = "MS_NFL")

# Optione 1
ggplot(data_1,aes(Timepoint,`NFL (pg/ml)`,group=`Codice pz`))+
  geom_line(linetype="dashed")+
  geom_point(aes(size=`NFL (pg/ml)`,fill=`Codice pz`),shape=21)+
  ggprism::theme_prism()


# Opzione 2
ggplot(data_1,aes(Timepoint,`Codice pz`,group=`Codice pz`))+
  geom_line(linetype="dashed",alpha=0.1)+
  geom_point(aes(size=`NFL (pg/ml)`,fill=`Codice pz`),shape=21)+
  ggprism::theme_prism()+scale_size_continuous(range = c(1,8))


data_1[!is.na(data_1$`NFL (pg/ml)`),]%>%group_by(`Codice pz`)%>%
  summarise(n=n())%>%arrange(-n)%>%print(n=Inf)

diff_T0_T3<-data.frame()
for(pz in unique(data_1$`Codice pz`)){
  t0<-data_1[data_1$`Codice pz`==pz &
           data_1$Timepoint=="T0",]$`NFL (pg/ml)`
  t3<-data_1[data_1$`Codice pz`==pz &
           data_1$Timepoint=="T3",]$`NFL (pg/ml)`
  
  if (all(!is.na(t0),!is.na(t3))){
    diff=t3-t0
    temp=data.frame(Paziente=pz,
                    diff=diff)
    diff_T0_T3=rbind(diff_T0_T3,temp)
  }
  
}

dim(diff_T0_T3)
data_1[data$Timepoint%in%c("T0","T3")&!is.na(data$`NFL (pg/ml)`),]%>%group_by(`Codice pz`)%>%
  summarise(n=n())%>%filter(n==2)%>%print(n=Inf)


data_1<-data_1 %>%
  group_by(`Codice pz`) %>%  
  mutate(Valore_t0 =`NFL (pg/ml)`[Timepoint == "T0"],  
         Difference = `NFL (pg/ml)`- Valore_t0,
         Ratio=`NFL (pg/ml)`/ Valore_t0)
#
ggplot(data_1,aes(Timepoint,Difference,group=`Codice pz`))+
  geom_line(linetype="dashed",color="grey80")+
  geom_point(aes(fill=`Codice pz`),size=4,shape=21)+
  ggprism::theme_prism()+geom_hline(yintercept =0,color="darkorange",linetype="dashed")+
  ylim(c(-60,+30))

#
ggplot(data_1,aes(Timepoint,Ratio,group=`Codice pz`))+
  geom_line(linetype="dashed",color="grey80")+
  geom_point(aes(fill=`Codice pz`),shape=21,size=3)+
  ggprism::theme_prism()+geom_hline(yintercept =1,color="darkorange",linetype="dashed")+
  ylim(c(-0,+3))



############################################
# Info btw M vs F, Old vs young, PP e RR and about EDSS
info_cl<-read_excel("./TabelloneOcrelizumab FINALE (conte e %) - 05Agosto24.xlsx")

info_cl<-info_cl[,c(1:8,606)]

names(info_cl)
# Sex
ggplot(info_cl,aes(SEX,`NFL (pg/ml)`))+
  geom_boxplot(outliers = F)+
  ggbeeswarm::geom_quasirandom(aes(fill=SEX),shape=21,size=3)+
  stat_compare_means(comparisons = list(c("M","F")))+
  ggprism::theme_prism(base_size = 15)+
  facet_wrap(~Timepoint,nrow = 1)

ggplot(info_cl[!is.na(info_cl$`Fenotipo clinico`),],aes(`Fenotipo clinico`,`NFL (pg/ml)`))+
  geom_boxplot(outliers = F)+
  ggbeeswarm::geom_quasirandom(aes(fill=`Fenotipo clinico`),shape=21,size=3)+
  stat_compare_means(comparisons = list(c("PP","RR")))+
  ggprism::theme_prism(base_size = 15)+
  facet_wrap(~Timepoint,nrow = 1)



ggplot(info_cl[!is.na(info_cl$`Fenotipo clinico`),],aes(`Fenotipo clinico`,`NFL (pg/ml)`))+
  geom_boxplot(outliers = F)+
  ggbeeswarm::geom_quasirandom(aes(fill=`Fenotipo clinico`),shape=21,size=3)+
  stat_compare_means(comparisons = list(c("PP","RR")))+
  ggprism::theme_prism(base_size = 15)+
  facet_wrap(~Timepoint,nrow = 1)

############################
library(lubridate)

info_cl$Eta <- floor(interval(info_cl$`Date of Birth`, now()) / years(1))

median(info_cl[info_cl$Timepoint=="T0",]$Eta)
info_cl$Groupeta<-ifelse(info_cl$Eta<=48,"Young","Old")
ggplot(info_cl,aes(`Groupeta`,`NFL (pg/ml)`))+
  geom_boxplot(outliers = F)+
  ggbeeswarm::geom_quasirandom(aes(fill=`Groupeta`),shape=21,size=3)+
  stat_compare_means(comparisons = list(c("Young","Old")))+
  ggprism::theme_prism(base_size = 15)+
  facet_wrap(~Timepoint,nrow = 1)


ggplot(info_cl,aes(`Groupeta`,`NFL (pg/ml)`))+
  geom_boxplot(outliers = F)+
  ggbeeswarm::geom_quasirandom(aes(fill=`Groupeta`),shape=21,size=3)+
  stat_compare_means(comparisons = list(c("Young","Old")))+
  ggprism::theme_prism(base_size = 15)+
  facet_wrap(~SEX,nrow = 1)

ggplot(info_cl,aes(`Groupeta`,`NFL (pg/ml)`))+
  geom_boxplot(outliers = F)+
  ggbeeswarm::geom_quasirandom(aes(fill=`Groupeta`),shape=21,size=3)+
  stat_compare_means(comparisons = list(c("Young","Old")))+
  ggprism::theme_prism(base_size = 15)+
  facet_wrap(~`Fenotipo clinico`,nrow = 1)

ggplot(info_cl[info_cl$Timepoint%in%c("T0","T6"),],aes(`Groupeta`,`EDSS`))+
  geom_boxplot(outliers = F)+
  ggbeeswarm::geom_quasirandom(aes(fill=`Groupeta`),shape=21,size=3)+
  stat_compare_means(comparisons = list(c("Young","Old")))+
  ggprism::theme_prism(base_size = 15)+
  facet_wrap(~`Timepoint`,nrow = 1)
#
ggplot(info_cl,aes(Eta,`NFL (pg/ml)`))+
  geom_point(aes(fill=Timepoint),shape=21,size=3)+
  geom_smooth(method = "lm",se=F)+
  stat_cor()+facet_wrap(~`Timepoint`,nrow = 1)+
  ggprism::theme_prism(base_size = 15)


ggplot(info_cl,aes(Eta,EDSS))+
  geom_point(aes(fill=Timepoint),shape=21,size=3)+
  geom_smooth(method = "lm",se=F)+
  stat_cor()+facet_wrap(~`Timepoint`,nrow = 1)+
  ggprism::theme_prism(base_size = 15)

ggplot(info_cl[!is.na(info_cl$`Fenotipo clinico`),],aes(Eta,`NFL (pg/ml)`))+
  geom_point(aes(fill=Timepoint),shape=21,size=3)+
  geom_smooth(method = "lm",se=F)+
  stat_cor()+facet_wrap(~`Fenotipo clinico`+`Timepoint`,nrow = 2)+
  ggprism::theme_prism(base_size = 15)

ggplot(info_cl[!is.na(info_cl$`Fenotipo clinico`),],aes(Eta,`NFL (pg/ml)`))+
  geom_point(aes(fill=Timepoint),shape=21,size=3)+
  geom_smooth(method = "lm",se=F)+
  stat_cor()+facet_wrap(~`SEX`+`Timepoint`,nrow = 2)+
  ggprism::theme_prism(base_size = 15)

##############################################


info_cl<-info_cl %>%
  group_by(`Codice pz`) %>%  
  mutate(Valore_t0 =`EDSS`[Timepoint == "T0"],  
         Difference = `EDSS`- Valore_t0,
         Ratio=`EDSS`/ Valore_t0)


# Opzione 2
ggplot(info_cl,aes(Timepoint,`Codice pz`,group=`Codice pz`))+
  geom_line(linetype="dashed",alpha=0.1)+
  geom_point(aes(size=`EDSS`,fill=`Codice pz`),shape=21)+
  ggprism::theme_prism()+scale_size_continuous(range = c(1,8))

#
ggplot(info_cl[info_cl$Timepoint%in%c("T0","T6"),],aes(Timepoint,Difference,group=`Codice pz`))+
  geom_line(linetype="dashed",color="grey80")+
  geom_point(aes(fill=`Codice pz`),size=4,shape=21,
             position = position_jitter(width = 0.02,height = 0.02),alpha=.7)+
  ggprism::theme_prism()+geom_hline(yintercept =0,color="darkorange",linetype="dashed")

#
ggplot(info_cl[info_cl$Timepoint%in%c("T0","T6"),],aes(Timepoint,Ratio,group=`Codice pz`))+
  geom_line(linetype="dashed",color="grey80")+
    geom_point(aes(fill=`Codice pz`),shape=21,size=3)+
  ggprism::theme_prism()+geom_hline(yintercept =1,color="darkorange",linetype="dashed")


##############################################
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


ggplot(info_cl[info_cl$Timepoint=="T6",],
       aes(Difference_NFL,Difference_EDSS))+
  geom_point(aes(fill=`Fenotipo clinico`),shape=21,size=3)+
  ggprism::theme_prism()+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  xlim(c(-20,20))+
  ylim(c(-2,2))


ggplot(info_cl[info_cl$Timepoint=="T6",],
       aes(Ratio_NFL,Ratio_EDSS))+
  geom_point(aes(fill=`Fenotipo clinico`),shape=21,size=3)+
  ggprism::theme_prism()+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 1)+
  xlim(c(0,2))+
  ylim(c(0,2))

ggplot(info_cl[info_cl$Timepoint=="T6",],
       aes(Ratio_NFL,Difference_EDSS))+
  geom_point(aes(fill=`Fenotipo clinico`),shape=21,size=3)+
  ggprism::theme_prism()+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 0)
 # xlim(c(0,2))+
  #ylim(c(0,2))



print(info_cl[info_cl$Timepoint=="T6",
              c("EDSS","Difference_EDSS","Ratio_NFL","Codice pz")],
      n=Inf)
  


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











##############################################################
##############################################################
################################################################################

#B perc
data_2<-read_excel("./TabelloneOcrelizumab FINALE (conte e %) - 05Agosto24.xlsx",sheet = "MS_B %")
colnames(data_2) <- gsub(" ", "", colnames(data_2))
names(data_2)

data_2<-as.data.frame(data_2)
data_2$TP
for(col in names(data_2[!is.na(data_2$TP),])[5:131]){
  print(col)
  p<-ggplot(data_2[!is.na(data_2$TP),],aes(factor(TP),
                                           data_2[!is.na(data_2$TP),col]))+
    geom_boxplot(outliers = F)+
    ggbeeswarm::geom_quasirandom(shape=21,size=3,aes(fill=factor(TP)))+
    ggprism::theme_prism()+
    stat_compare_means(comparisons = list(c("0","1"),
                                          c("0","2"),
                                          c("0","3"),
                                          c("0","4"),
                                          c("0","5"),
                                          c("0","6")))+
    ylab(col)+ggsci::scale_fill_aaas()+xlab("")
  print(p)
}

dim(data_1)
dim(data_2)
data_2$Codice_pz<-paste0("OCRX-",data_2$SampleID)
data_2[data_2$SampleID%in%c(1,2,3,4,5,6,7,8,9),]$Codice_pz<-paste0("OCRX-0",data_2[data_2$SampleID%in%c(1,2,3,4,5,6,7,8,9),]$SampleID)


data_1$Code<-paste0(data_1$`Codice pz`,"_",data_1$Timepoint)
data_2$Code<-paste0(data_2$`Codice_pz`,"_","T",data_2$TP)

merge1_2<-merge(data_1,data_2,by="Code")

X <- merge1_2$`NFL (pg/ml)`
names(merge1_2)
# Calcoliamo la covarianza tra X e tutte le altre variabili
covariances <- sapply(merge1_2[,c(16:141)], 
                      function(y) cov(X, y, use = "complete.obs"))

sing<-vector()
for (col in names(merge1_2)[c(16:141)]){
 
c<-cov(merge1_2$`NFL (pg/ml)`,
    merge1_2[,col],use = "complete.obs")
cr<-cor.test(merge1_2$`NFL (pg/ml)`,
         merge1_2[,col],use = "complete.obs")

if (cr$p.value<0.05){
  print(col)
 sing<-c(sing,col)
}
}


#############################################
# per ogni timepoint, scelgo confronto quelli con
# alto o basso NFL rispetto al valore mediano di quel 
# tp

for (tm in unique(merge1_2$Timepoint)){
  
  
  tem<-merge1_2[merge1_2$Timepoint==tm,]
  
  mediana<-median(tem$`NFL (pg/ml)`,na.rm = T)
  
  tem$Group<-0
  tem$Group<-ifelse(tem$`NFL (pg/ml)`<=mediana,
              "Low","High")

  for (col in names(tem)[15:141] ){
   
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
        stat_compare_means()+
        ggsci::scale_fill_aaas()
      print(p1)


    }
  }
  
}

#################################
# Differenze in base alla difference
# NFL tx - NFL t0

merge1_2$Group_diff<-
  ifelse(merge1_2$Difference<=0,"M",
         "P")


for (col in names(merge1_2)[15:141]){
  test<-
    wilcox.test(merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                           !is.na(merge1_2$Group_diff),col]~
                  Group_ratio,data=merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                                              !is.na(merge1_2$Group_diff),])
  if (test$p.value<0.05){
   
  pl<-ggplot(merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                        !is.na(merge1_2$Group_diff),],
             aes(
    y=merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                 !is.na(merge1_2$Group_diff),col],
    x=factor(Group_diff,
           levels = c("M","P"))))+
  geom_boxplot(outliers = F)+
  ggbeeswarm::geom_quasirandom(shape=21,
                               size=3,
                               aes(fill=Group_diff))+
  ggprism::theme_prism(base_size = 15)+
  ylab(col)+
  xlab("")+facet_wrap(~Timepoint,nrow = 1)+
  stat_compare_means(comparisons = list(c("M","P")))+
    ggsci::scale_fill_aaas()

 print(pl)
 
 
 pl2<-ggplot(merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                       !is.na(merge1_2$Group_diff),],
            aes(
              y=merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                           !is.na(merge1_2$Group_diff),col],
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

merge1_2$Group_ratio<-
  ifelse(merge1_2$Ratio<=1,"M",
         "P")


for (col in names(merge1_2)[15:141]){
  
  test<-
    wilcox.test(merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                           !is.na(merge1_2$Group_ratio),col]~
                  Group_ratio,data=merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                           !is.na(merge1_2$Group_ratio),])
 if (test$p.value<0.05){
  pl<-ggplot(merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                        !is.na(merge1_2$Group_ratio),],
             aes(
               y=merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                            !is.na(merge1_2$Group_ratio),col],
               x=factor(Group_ratio,
                        levels = c("M","P"))))+
    geom_boxplot(outliers = F)+
    ggbeeswarm::geom_quasirandom(shape=21,
                                 size=3,
                                 aes(fill=Group_ratio))+
    ggprism::theme_prism(base_size = 15)+
    ylab(col)+
    xlab("")+facet_wrap(~Timepoint,nrow = 1)+
    stat_compare_means(comparisons = list(c("M","P")))+
    ggsci::scale_fill_aaas()
  
  print(pl)
  
  
  pl2<-ggplot(merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                         !is.na(merge1_2$Group_ratio),],
              aes(
                y=merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                             !is.na(merge1_2$Group_ratio),col],
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
for (col in names(merge1_2)[15:141]){

  test<-cor.test(merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                           !is.na(merge1_2$Group_diff)&
                            merge1_2$Timepoint!="T0",col],
                   merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                              !is.na(merge1_2$Group_diff)&
                              merge1_2$Timepoint!="T0","Difference"])
  
  if (test$p.value<0.05){
   
    pp<-ggplot(merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                      !is.na(merge1_2$Group_ratio)&
                        merge1_2$Timepoint!="T0",],
               aes(Difference,merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                                         !is.na(merge1_2$Group_ratio)&
                                         merge1_2$Timepoint!="T0",col]))+
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
for (col in names(merge1_2)[15:141]){
  
  test<-cor.test(merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                            !is.na(merge1_2$Group_ratio)&
                            merge1_2$Timepoint!="T0",col],
                 merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                            !is.na(merge1_2$Group_ratio)&
                            merge1_2$Timepoint!="T0","Ratio"])
  
  if (test$p.value<0.05){
    
    pp<-ggplot(merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                          !is.na(merge1_2$Group_ratio)&
                          merge1_2$Timepoint!="T0",],
               aes(Ratio,merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                                         !is.na(merge1_2$Group_ratio)&
                                    merge1_2$Timepoint!="T0",col]))+
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

names(merge1_2)

for (name in c("CD19\\+",
               "ASC/","SW/",
               "cMem","DN/","DN1\\+DN3/","DN2/",
               "Tr/","Naive/","USW/","USWmemory/",
               "USWPB/","nonASC/")){
  
  print(name)

cols_to_pivot <- names(merge1_2)[grepl(name, names(merge1_2)) & !grepl("MFI", names(merge1_2))
                                 & !grepl("non", names(merge1_2))
                                 &!grepl("count", names(merge1_2))]

if (name=="nonASC/"){
  cols_to_pivot <- names(merge1_2)[grepl(name, names(merge1_2)) & !grepl("MFI", names(merge1_2))]
  
}
df_long <- merge1_2 %>%
  pivot_longer(cols = all_of(cols_to_pivot),
               names_to = "Population",
               values_to = "Value")

df_medians <- df_long %>%
  group_by(Timepoint, Population) %>%
  summarize(MedianValue = median(Value, na.rm = TRUE))

df_medians$Population<-str_replace(df_medians$Population,
                                   name,"")

df_medians[df_medians$MedianValue==0,]$MedianValue=0.0001
allu<-ggplot(df_medians,
       aes(x = Timepoint, stratum =Population,
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

for (name in c("CD19\\+",
               "ASC/","SW/",
               "cMem","DN/","DN1\\+DN3/","DN2/",
               "Tr/","Naive/","USW/","USWmemory/",
               "USWPB/","nonASC/")){
  
  print(name)
  
  cols_to_pivot <- names(merge1_2)[grepl(name, names(merge1_2)) & !grepl("MFI", names(merge1_2))
                                   & !grepl("non", names(merge1_2))
                                   &!grepl("count", names(merge1_2)) |
                                     grepl("NFL", names(merge1_2))  
                                   ]
  
  if (name=="nonASC/"){
    cols_to_pivot <- names(merge1_2)[ grepl("NFL", names(merge1_2)) | grepl(name, names(merge1_2)) & !grepl("MFI", names(merge1_2))]
    
  }
  df_long <- merge1_2 %>%
    pivot_longer(cols = all_of(cols_to_pivot),
                 names_to = "Population",
                 values_to = "Value")
  
  df_medians <- df_long %>%
    group_by(Timepoint, Population) %>%
    summarize(MedianValue = median(Value, na.rm = TRUE))
  
  df_medians$Population<-str_replace(df_medians$Population,
                                     name,"")
  
  df_medians[df_medians$MedianValue==0,]$MedianValue=0.0001
  allu<-ggplot(df_medians,
               aes(x = Timepoint, stratum =Population,
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

for (name in c("CD19\\+/",
               "ASC/","SW/",
               "cMem","DN/","DN1\\+DN3/","DN2/",
               "Tr/","Naive/","USW/","USWmemory/",
               "USWPB/","nonASC/")){
  
  print(name)
  
  cols_to_pivot <- names(merge1_2)[grepl(name, names(merge1_2)) & !grepl("MFI", names(merge1_2))
                                   & !grepl("non", names(merge1_2))
                                   &!grepl("count", names(merge1_2)) |
                                     grepl("NFL", names(merge1_2))  
  ]
  
  if (name=="nonASC/"){
    cols_to_pivot <- names(merge1_2)[ grepl("NFL", names(merge1_2)) | grepl(name, names(merge1_2)) & !grepl("MFI", names(merge1_2))]
    
  }
  df_normalized <-  merge1_2  %>%
    mutate(across(where(is.numeric), min_max_scaling))
  df_long <- df_normalized  %>%
    pivot_longer(cols = all_of(cols_to_pivot),
                 names_to = "Population",
                 values_to = "Value")
  
  df_medians <- df_long %>%
    group_by(Timepoint, Population) %>%
    summarize(MedianValue = median(Value, na.rm = TRUE))
  
  df_medians$Population<-str_replace(df_medians$Population,
                                     name,"")
  
  df_medians[df_medians$MedianValue==0&!is.na(df_medians$MedianValue),]$MedianValue=0.0001
  allu<-ggplot(df_medians,
               aes(x = Timepoint, stratum =Population,
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
    geom_line(data=df_medians[df_medians$Population!="NFL (pg/ml)",],aes(Timepoint,
                  MedianValue,
                  group=Population),color="grey")+
    
   geom_line(data=df_medians[df_medians$Population==
                               "NFL (pg/ml)",],
             aes(Timepoint,MedianValue,
                 group=Population),color="red")+
   geom_point(df_medians,mapping=aes(Timepoint,
                                     MedianValue,
                                     fill=Population),shape=21,size=3)+
    ggprism::theme_prism(base_size = 15)
 print(plt2)
}




for (col in names(merge1_2)[15:141]){
df_normalized <-  merge1_2  %>%
  mutate(across(where(is.numeric), min_max_scaling))
p<-ggplot(df_normalized)+
  geom_line(aes(Timepoint, df_normalized[,col],group=`Codice pz`))+
  geom_point(aes(Timepoint, df_normalized[,col],fill=`Codice pz`),shape=21)+
  geom_line(aes(Timepoint, `NFL (pg/ml)`,group=`Codice pz`),color="grey")+
  geom_point(aes(Timepoint, `NFL (pg/ml)`),shape=21,fill="darkred")+
  facet_wrap(~`Codice pz`)+ylab(col)+
  ggprism::theme_prism()
#print(p)

p2<-ggplot(merge1_2)+
  geom_line(aes(Timepoint, merge1_2[,col],group=`Codice pz`))+
  geom_point(aes(Timepoint, merge1_2[,col],fill=`Codice pz`),shape=21)+
  geom_line(aes(Timepoint, `NFL (pg/ml)`,group=`Codice pz`),color="grey")+
  geom_point(aes(Timepoint, `NFL (pg/ml)`),shape=21,fill="darkred")+
  facet_wrap(~`Codice pz`)+ylab(col)+
  ggprism::theme_prism()
print(p2)
}



for (name in names(merge1_2)[15:141]){
df_long <- merge1_2 %>%
  pivot_longer(cols = c(name, `NFL (pg/ml)`), names_to = "Marker", values_to = "Value")

p<-ggplot(df_long, aes(x = Timepoint, y = Value,
                    
                    group = interaction(`Codice pz`, Marker))) +
  geom_line(aes(linetype = "Patient Line",color = Marker), size = 0.8,
            alpha=0.2) +  # Linee per ogni paziente
  stat_smooth(aes(group = Marker, linetype = "Trend Line",
                  color = Marker), 
              method = "loess", se = FALSE, 
              size = 2) +  # Linee di tendenza
  scale_linetype_manual(values = c("Patient Line" = "solid", "Trend Line" = "dashed")) +  # Differenziazione del tipo di linea
  labs(title = paste0("Andamento di ",name," e NFL nel Tempo per Paziente"),
       x = "Timepoint",
       y = "Valore",
       color = "Marker") +
  ggprism::theme_prism()+ggsci::scale_color_d3()

print(p)
}


### B conta

data_3<-read_excel("./TabelloneOcrelizumab FINALE (conte e %) - 05Agosto24.xlsx",sheet = "MS_B CONTA")
colnames(data_3) <- gsub(" ", "", colnames(data_3))
names(data_3)

data_3<-as.data.frame(data_3)
names(data_3)
for(col in names(data_3[!is.na(data_3$TP),])[5:17]){
  print(col)
  p<-ggplot(data_3[!is.na(data_3$TP),],aes(factor(TP),
                                           data_3[!is.na(data_3$TP),col]))+
    geom_boxplot(outliers = F)+
    ggbeeswarm::geom_quasirandom(shape=21,size=3,aes(fill=factor(TP)))+
    ggprism::theme_prism()+
    stat_compare_means(comparisons = list(c("0","1"),
                                          c("0","2"),
                                          c("0","3"),
                                          c("0","4"),
                                          c("0","5"),
                                          c("0","6")))+
    ylab(col)+xlab("")
  print(p)
}


## Alluvial
cols_to_pivot <- names(data_3)[5:17]

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
dim(info_cl)

names(info_cl)
head(info_cl)
head(data_2)
data_2$TP<-paste0("T",data_2$TP)
data_2_cl<-merge(info_cl,data_2,
                 by.x=c("Codice pz",
"Timepoint"), by.y=c("Codice_pz","TP"))



names(data_2_cl)



for( col in names(data_2_cl)[18:144]){
  
  print(col)
  temp<-data_2_cl[!is.na(data_2_cl[,col])&
                    !is.na(data_2_cl[,"Fenotipo clinico"]),]
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
  pl1<-ggplot(data_2_cl[!is.na(data_2_cl$`Fenotipo clinico`),],
              aes(`Fenotipo clinico`,data_2_cl[!is.na(data_2_cl$`Fenotipo clinico`),col]))+
    geom_boxplot(outliers = F)+
    ggbeeswarm::geom_quasirandom(shape=21,size=3,
                                 aes(fill=`Fenotipo clinico`))+
    ggprism::theme_prism(base_size = 10)+ggsci::scale_fill_aaas()+
    facet_wrap(~Timepoint,nrow=1)+
    stat_compare_means(comparisons = list(c("PP","RR")))+
    ylab(col)
  #print(pl1)
  }
  
  
  
  temp<-data_2_cl[!is.na(data_2_cl[,col])&
                    !is.na(data_2_cl[,"Groupeta"]),]
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

  pl2<-ggplot(data_2_cl[!is.na(data_2_cl$`Groupeta`),],
              aes(Groupeta,data_2_cl[!is.na(data_2_cl$Groupeta),col]))+
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

data_2_cl$Status<-0

data_2_cl[data_2_cl$`Codice pz`%in%both,]$Status<-"dNFL dEDSS"
data_2_cl[data_2_cl$`Codice pz`%in%nflnoedss,]$Status<-"dNFL sEDSS"
data_2_cl[data_2_cl$`Codice pz`%in%nfledsspeggio,]$Status<-"dNFL iEDSS"
data_2_cl[data_2_cl$`Codice pz`%in%tuttopeggio,]$Status<-"iNFL iEDSS"

for( col in names(data_2_cl)[18:144]){
  
  print(col)
  temp<-data_2_cl[!is.na(data_2_cl[,col])&
                    !data_2_cl[,"Status"]==0,]
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
  
 
    pl1<-ggplot(data_2_cl[!data_2_cl$Status==0,],
                aes(Status,data_2_cl[!data_2_cl$Status==0,col]))+
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
  

}



data_2_cl$Status2<-0

data_2_cl[data_2_cl$`Codice pz`%in%c(both,
                                     nflnoedss),]$Status2<-"Stable"

data_2_cl[data_2_cl$`Codice pz`%in%c(nfledsspeggio,tuttopeggio),]$Status2<-"Worsening"

for( col in names(data_2_cl)[18:144]){
  
  print(col)
  temp<-data_2_cl[!is.na(data_2_cl[,col])&
                    !data_2_cl[,"Status2"]==0,]
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
  pl1<-ggplot(data_2_cl[!data_2_cl$Status2==0,],
              aes(Status2,data_2_cl[!data_2_cl$Status2==0,col]))+
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









