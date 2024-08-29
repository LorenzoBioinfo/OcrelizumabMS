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
library(moonBook)
library(webr)
library(lubridate)
###############
info<-read_excel("./TabelloneOcrelizumab FINALE (conte e %) - 05Agosto24.xlsx")
info_t0<-info[info$Timepoint=="T0",]
names(info_t0)
sum<-info_t0%>%group_by(SEX,`Fenotipo clinico`)%>%
  summarize(count=n())

sum[is.na(sum$`Fenotipo clinico`),]$`Fenotipo clinico`<-"NA"
names(sum)[2]<-"Fenotipo"

PieDonut(sum, aes(SEX, Fenotipo ,count=count),r0 = 0.5, r1 = 0.9,showPieName = F,
         labelposition = 0,pieLabelSize = 7,donutLabelSize = 7)



info_t0%>%group_by(SEX)%>%summarise(n(),median=median(Eta))
info_t0%>%group_by(`Fenotipo clinico`)%>%summarise(n(),median=median(Eta))
info_t0%>%group_by(`Fenotipo clinico`)%>%summarise(n())

info_t0$Eta <- floor(interval(info_t0$`Date of Birth`, now()) / years(1))
ggplot(info_t0,aes(Eta))+
  geom_density(aes(color=Fenotipo))+
  ggprism::theme_prism()+ylab("Age")+
  xlab("")+ggsci::scale_color_cosmic()

ggplot(info_t0,aes(SEX,Eta))+
  geom_boxplot()+
  ggbeeswarm::geom_quasirandom(shape=21,size=3,aes(fill=SEX))+
  stat_compare_means(comparisons = list(c("M","F")))+
  ggprism::theme_prism()+ylab("Age")+
  xlab("")

####################################################################
# Heatmap followup

#NFL, B, T, MAIT, INNATA, RMN,TEST NEUROPSIC
matrice<-matrix(0,ncol=7*7,nrow = 38)
rownames(matrice)<-unique(info_t0$`Codice pz`)
v1<-c("NFL", "B", "T","MAIT", "INNATA", "RMN","TESTNEUROPSIC")
v2<-c("T0","T1","T2","T3","T4","T5","T6")
combinazioni <- expand.grid(v1, v2)
risultato <- paste(combinazioni$Var1, combinazioni$Var2, sep = "_")
colnames(matrice)<-risultato

for (patient in unique(info$`Codice pz`)){
  for (tm in v2){
    for( tab in v1){
      
      if(tab =="NFL"){
        if(!is.na(info[info$`Codice pz`==patient&
                       info$Timepoint==tm,"NFL (pg/ml)"])){
          matrice[patient,paste0(tab,"_",tm)]<-1
        }
      }
      if(tab =="B"){
        if(!is.na(info[info$`Codice pz`==patient&
                       info$Timepoint==tm,"CD19+"])){
          matrice[patient,paste0(tab,"_",tm)]<-2
        }
        
      }
      if(tab =="T"){
        if(!is.na(info[info$`Codice pz`==patient&
                       info$Timepoint==tm,"CD3 | Freq. of Parent"])){
          matrice[patient,paste0(tab,"_",tm)]<-3
        }
      }
      if(tab =="MAIT"){
        if(!is.na(info[info$`Codice pz`==patient&
                       info$Timepoint==tm,"CD3/CD4-/MAIT | Freq. of CD3"])){
          matrice[patient,paste0(tab,"_",tm)]<-4
        }
        
      }
      if(tab =="INNATA"){
        if(!is.na(info[info$`Codice pz`==patient&
                       info$Timepoint==tm,"cDC1_BREF"])){
          matrice[patient,paste0(tab,"_",tm)]<-5
        }

      }
      if(tab =="RMN"){
        if(!is.na(info[info$`Codice pz`==patient&
                       info$Timepoint==tm,"EDSS"])){
          matrice[patient,paste0(tab,"_",tm)]<-6
        }
        
        
      }
      if(tab =="TESTNEUROPSIC"){
        if(!is.na(info[info$`Codice pz`==patient&
                       info$Timepoint==tm,"SRTLTS"])){
          matrice[patient,paste0(tab,"_",tm)]<-7
        }
        
      }
    }
  }

}

library(pheatmap)
c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "#CAB2D6",
  "yellow3",
  "gold1",
  "skyblue2",
  "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)

annocol<-data.frame(Timepoint=rep(v2,each=7))
rownames(annocol)<-colnames(matrice)

pheatmap(matrice,gaps_col = c(7,14,21,28,35,42),cluster_cols = F,cluster_rows = F,
         labels_row = rownames(matrice),legend = F,
         color=c("white",c25[1:7]),annotation_col = annocol,annotation_legend = F
        ,labels_col = rep(v1,7),annotation_names_col = F)



######### Datasheet 1 # NFL
data_1<-read_excel("./TabelloneOcrelizumab FINALE (conte e %) - 05Agosto24.xlsx",sheet = "MS_NFL")

# Optione 1
ggplot(data_1,aes(Timepoint,`NFL (pg/ml)`,group=`Codice pz`))+
  geom_line(linetype="dashed",color="grey80")+
  stat_summary(fun = mean, aes(group = 1), 
               geom = "line", 
               color = "darkred", size = 1.2)+
  geom_point(aes(size=`NFL (pg/ml)`,fill=`Codice pz`),shape=21)+
  ggprism::theme_prism(base_size = 15)

ggplot(data_1,aes(Timepoint,`NFL (pg/ml)`))+
  geom_boxplot(outliers = F)+
  ggbeeswarm::geom_quasirandom(aes(fill=`Timepoint`),shape=21)+
  stat_compare_means(comparison=list(c("T0","T1"),
                          c("T0","T2"),
                          c("T0","T3"),
                          c("T0","T4"),
                          c("T0","T5"),
                          c("T0","T6")))+
  ggprism::theme_prism()+xlab("")



# Opzione 2
ggplot(data_1,aes(Timepoint,`Codice pz`,group=`Codice pz`))+
  geom_line(linetype="dashed",alpha=0.1)+
  geom_point(aes(size=`NFL (pg/ml)`,fill=`Codice pz`),shape=21)+
  ggprism::theme_prism()+scale_size_continuous(range = c(1,8))

data_1[!is.na(data_1$`NFL (pg/ml)`),]%>%group_by(`Codice pz`)%>%
  summarise(n=n())%>%arrange(-n)%>%print(n=Inf)


data_1<-data_1 %>%
  group_by(`Codice pz`) %>%  
  mutate(Valore_t0 =`NFL (pg/ml)`[Timepoint == "T0"],  
         Difference = `NFL (pg/ml)`- Valore_t0,
         Ratio=`NFL (pg/ml)`/ Valore_t0)
#
ggplot(data_1,aes(Timepoint,Difference,group=`Codice pz`))+
  geom_line(linetype="dashed",color="grey80")+
  geom_point(aes(fill=`Codice pz`),size=4,shape=21)+
  ggprism::theme_prism()+geom_hline(yintercept =0,color="darkred",linetype="dashed")+
  ylim(c(-60,+30))+xlab("")+ylab("Difference NFL Tx-T0")

#
ggplot(data_1,aes(Timepoint,Ratio,group=`Codice pz`))+
  geom_line(linetype="dashed",color="grey80")+
  geom_point(aes(fill=`Codice pz`),shape=21,size=3)+
  ggprism::theme_prism()+geom_hline(yintercept =1,size=1,color="darkorange",linetype="dashed")+
  ylim(c(-0,+3))+xlab("")+ylab("Ratio NFL Tx/T0")



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
  facet_wrap(~Timepoint,nrow = 1)+xlab("")

ggplot(info_cl[!is.na(info_cl$`Fenotipo clinico`),],aes(`Fenotipo clinico`,`NFL (pg/ml)`))+
  geom_boxplot(outliers = F)+
  ggbeeswarm::geom_quasirandom(aes(fill=`Fenotipo clinico`),shape=21,size=3)+
  stat_compare_means(comparisons = list(c("PP","RR")))+
  ggprism::theme_prism(base_size = 15)+
  ggsci::scale_fill_cosmic()+
  facet_wrap(~Timepoint,nrow = 1)+xlab("")


############################
library(lubridate)
info_cl$Eta <- floor(interval(info_cl$`Date of Birth`, now()) / years(1))

median(info_cl[info_cl$Timepoint=="T0",]$Eta)
info_cl$Groupeta<-ifelse(info_cl$Eta<=48,"Young","Old")
ggplot(info_cl,aes(factor(Groupeta,
                          levels = c("Young","Old")),`NFL (pg/ml)`))+
  geom_boxplot(outliers = F)+
  ggbeeswarm::geom_quasirandom(aes(fill=`Groupeta`),shape=21,size=3)+
  stat_compare_means(comparisons = list(c("Young","Old")))+
  ggprism::theme_prism(base_size = 15)+
  facet_wrap(~Timepoint,nrow = 1)+
  ggsci::scale_fill_d3()+xlab("")


ggplot(info_cl,aes(`Groupeta`,`NFL (pg/ml)`))+
  geom_boxplot(outliers = F)+
  ggbeeswarm::geom_quasirandom(aes(fill=`Groupeta`),shape=21,size=3)+
  stat_compare_means(comparisons = list(c("Young","Old")))+
  ggprism::theme_prism(base_size = 15)+
  facet_wrap(~SEX+Timepoint,nrow = 2)

ggplot(info_cl,aes(`Groupeta`,`NFL (pg/ml)`))+
  geom_boxplot(outliers = F)+
  ggbeeswarm::geom_quasirandom(aes(fill=`Groupeta`),shape=21,size=3)+
  stat_compare_means(comparisons = list(c("Young","Old")))+
  ggprism::theme_prism(base_size = 15)+
  facet_wrap(~`Fenotipo clinico`,nrow = 1)

ggplot(info_cl[info_cl$Timepoint%in%c("T0","T6"),],
       aes(`Groupeta`,`EDSS`))+
  geom_boxplot(outliers = F)+
  ggbeeswarm::geom_quasirandom(aes(fill=`Groupeta`),shape=21,size=3)+
  stat_compare_means(comparisons = list(c("Young","Old")))+
  ggprism::theme_prism(base_size = 15)+
  facet_wrap(~`Timepoint`,nrow = 1)+
  ggsci::scale_fill_d3()+xlab("")
#
ggplot(info_cl,aes(Eta,`NFL (pg/ml)`))+
  geom_point(aes(fill=Timepoint),shape=21,size=3)+
  geom_smooth(method = "lm",se=F)+
  stat_cor()+facet_wrap(~`Timepoint`,nrow = 1)+
  ggprism::theme_prism(base_size = 15)


ggplot(info_cl[info_cl$Timepoint%in%c("T0","T6"),],aes(Eta,EDSS))+
  geom_point(aes(fill=Timepoint),shape=21,size=3)+
  geom_smooth(method = "lm",se=F)+
  stat_cor()+facet_wrap(~`Timepoint`,nrow = 1)+
  ggprism::theme_prism(base_size = 15)+ggsci::scale_fill_cosmic()+
  xlab("")

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

ggplot(info_cl[info_cl$Timepoint%in%c("T0","T6"),],
       aes(Timepoint,EDSS))+
  geom_boxplot(outliers = F)+
  ggbeeswarm::geom_quasirandom(aes(fill=Timepoint),
                                   shape=21,size=3)+
  stat_compare_means(comparisons = list(c("T0","T6")))+
  ggprism::theme_prism(base_size = 15)+xlab("")+
  scale_fill_manual(values = c("#F8766D","#FB61D7"))


ggplot(info_cl[info_cl$Timepoint%in%c("T0","T6")&
                 !is.na(info_cl$`Fenotipo clinico`),],
       aes(`Fenotipo clinico`,EDSS))+
  geom_boxplot(outliers = F)+
  ggbeeswarm::geom_quasirandom(aes(fill=Timepoint),
                               shape=21,size=3)+
  stat_compare_means(comparisons = list(c("PP","RR")))+
  ggprism::theme_prism(base_size = 15)+
  facet_wrap(~Timepoint)+
  ggsci::scale_fill_cosmic()+xlab("")


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
  ggprism::theme_prism()+geom_hline(yintercept =0,color="darkorange",linetype="dashed")+
  xlab("")+ylab("Difference EDSS Tx-T0")

#
ggplot(info_cl[info_cl$Timepoint%in%c("T0","T6"),],aes(Timepoint,Ratio,group=`Codice pz`))+
  geom_line(linetype="dashed",color="grey80")+
    geom_point(aes(fill=`Codice pz`),shape=21,size=3)+
  ggprism::theme_prism()+geom_hline(yintercept =1,color="darkorange",linetype="dashed")+
  xlab("")+ylab("Ratio EDSS Tx/T0")

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
  geom_point(aes(fill=Groupeta),shape=21,size=4)+
  ggprism::theme_prism()+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 1)+
  xlim(c(0,2))+
  ylim(c(0,2))+ggsci::scale_fill_bmj()

ggplot(info_cl[info_cl$Timepoint%in%c("T0","T6"),],
       aes(`NFL (pg/ml)`,EDSS))+
  geom_point(aes(fill=Timepoint),shape=21,size=4)+
  ggprism::theme_prism()+
  ggsci::scale_fill_bmj()+facet_wrap(~Timepoint,nrow = 1)+
  scale_fill_manual(values = c("#F8766D","#FB61D7"))+
  geom_smooth(se=F,method = "lm")+
  stat_cor()

ggplot(info_cl[info_cl$Timepoint=="T6",],
       aes(Ratio_NFL,Difference_EDSS))+
  geom_point(aes(fill=`Fenotipo clinico`),shape=21,size=3)+
  ggprism::theme_prism()+
  geom_vline(xintercept = 1)+
  geom_hline(yintercept = 0)+
  
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
data_2[!is.na(data_2$TP),]$TP<-paste0("T",data_2[!is.na(data_2$TP),]$TP)
n=0
Res<-data.frame()
for(col in names(data_2[!is.na(data_2$TP),])[5:131]){
  
  test<-kruskal.test(data_2[!is.na(data_2$TP),col]~
                       data_2[!is.na(data_2$TP),]$TP)
  temp<-data.frame(Var=col,p.value=test$p.value)
  #Res<-rbind(Res,temp)
  if (test$p.value<=0.01){
    n=n+1
  p<-ggplot(data_2[!is.na(data_2$TP),],aes(factor(TP),
                                           data_2[!is.na(data_2$TP),col]))+
    geom_boxplot(outliers = F)+
    ggbeeswarm::geom_quasirandom(shape=21,size=3,aes(fill=factor(TP)))+
    ggprism::theme_prism()+
    stat_compare_means(comparisons = list(c("T0","T1"),
                                          c("T0","T2"),
                                          c("T0","T3"),
                                          c("T0","T4"),
                                          c("T0","T5"),
                                          c("T0","T6")))+
    ylab(col)+ggsci::scale_fill_aaas()+xlab("")
  #print(p)
  p2<-ggplot(data_2[!is.na(data_2$TP),],aes(factor(TP),
                                           data_2[!is.na(data_2$TP),col]))+
    geom_boxplot(outliers = F)+
    geom_line(aes(group=Codice_pz),color="grey80")+
    geom_point(shape=21,size=3,aes(fill=factor(TP)))+
    ggprism::theme_prism()+
    stat_compare_means(comparisons = list(c("T0","T1"),
                                          c("T0","T2"),
                                          c("T0","T3"),
                                          c("T0","T4"),
                                          c("T0","T5"),
                                          c("T0","T6")))+
    ylab(col)+ggsci::scale_fill_aaas()+xlab("")
  print(p2)

  }
  
}




Res$p.value_adj<-p.adjust(Res$p.value,method = "bonferroni")
dim(Res[Res$p.value_adj<0.05,])


matrixT0<-matrix(0,nrow=38,ncol=97)
matrixT1<-matrix(0,nrow=38,ncol=97)
matrixT2<-matrix(0,nrow=38,ncol=97)
matrixT3<-matrix(0,nrow=38,ncol=97)
matrixT4<-matrix(0,nrow=38,ncol=97)
matrixT5<-matrix(0,nrow=38,ncol=97)
matrixT6<-matrix(0,nrow=38,ncol=97)
rownames(matrixT0)<-unique(data_2$Codice_pz)
rownames(matrixT1)<-unique(data_2$Codice_pz)
rownames(matrixT2)<-unique(data_2$Codice_pz)
rownames(matrixT3)<-unique(data_2$Codice_pz)
rownames(matrixT4)<-unique(data_2$Codice_pz)
rownames(matrixT5)<-unique(data_2$Codice_pz)
rownames(matrixT6)<-unique(data_2$Codice_pz)


colnames(matrixT0)<-paste0(Res[Res$p.value_adj<0.05,]$Var,"_T0")
colnames(matrixT1)<-paste0(Res[Res$p.value_adj<0.05,]$Var,"_T1")
colnames(matrixT2)<-paste0(Res[Res$p.value_adj<0.05,]$Var,"_T2")
colnames(matrixT3)<-paste0(Res[Res$p.value_adj<0.05,]$Var,"_T3")
colnames(matrixT4)<-paste0(Res[Res$p.value_adj<0.05,]$Var,"_T4")
colnames(matrixT5)<-paste0(Res[Res$p.value_adj<0.05,]$Var,"_T5")
colnames(matrixT6)<-paste0(Res[Res$p.value_adj<0.05,]$Var,"_T6")


matrix_tot<-cbind(matrixT0,matrixT1,matrixT2,
                  matrixT3,matrixT4,matrixT5,matrixT6)
for( patient in rownames(matrix_tot)){
  for(var in Res[Res$p.value_adj<0.05,]$Var){
    for(time in c("T0","T1","T2","T3","T4","T5","T6")){
      
      
      if(all(is.na(data_2[data_2$Codice_pz==patient&
                data_2$TP==time,var]))){
        matrix_tot[patient,
                   paste0(var,"_",time)]<-NA
      }else{
        matrix_tot[patient,
                   paste0(var,"_",time)]<-max(data_2[data_2$Codice_pz==patient&
                                                   data_2$TP==time,var],na.rm = T)
      }
      
      
      
      
    }
  }
}

pheatmap::pheatmap(matrix_tot,cluster_cols = F,
                   cluster_rows = F,
                   scale = "column",
                   fontsize_col = 4,gaps_col = c(97,97*2,97*3,97*4,97*5,
                                                 97*6))
##############################

data_2[!is.na(data_2$`CD19+`),]%>%
  group_by(Codice_pz)%>%
  summarise(n=n())%>%arrange(-n)%>%
  print(n=Inf)


data_2[!is.na(data_2$`CD19+`),]%>%
  group_by(TP)%>%summarize(n())


lista<-list(T0=data_2[!is.na(data_2$`CD19+`)&
                     data_2$TP=="T0","Codice_pz"],
            T1=data_2[!is.na(data_2$`CD19+`)&
                        data_2$TP=="T1","Codice_pz"],
            T2=data_2[!is.na(data_2$`CD19+`)&
                       data_2$TP=="T2","Codice_pz"],
            T3=data_2[!is.na(data_2$`CD19+`)&
                        data_2$TP=="T3","Codice_pz"],
            T4=data_2[!is.na(data_2$`CD19+`)&
                        data_2$TP=="T4","Codice_pz"],
            T5=data_2[!is.na(data_2$`CD19+`)&
                        data_2$TP=="T5","Codice_pz"],
T6=data_2[!is.na(data_2$`CD19+`)&
                   data_2$TP=="T6","Codice_pz"])
library(UpSetR)
upset(fromList(lista),order.by = "freq",
      sets = rev(c("T0","T1","T2","T3","T4","T5","T6")),
      keep.order = TRUE,main.bar.color = "darkred",
      sets.bar.color = "darkblue",text.scale = 2)

dim(data_1)
dim(data_2)
data_2$Codice_pz<-paste0("OCRX-",data_2$SampleID)
data_2[data_2$SampleID%in%c(1,2,3,4,5,6,7,8,9),]$Codice_pz<-paste0("OCRX-0",data_2[data_2$SampleID%in%c(1,2,3,4,5,6,7,8,9),]$SampleID)


data_1$Code<-paste0(data_1$`Codice pz`,"_",data_1$Timepoint)
data_2$Code<-paste0(data_2$`Codice_pz`,"_",data_2$TP)

merge1_2<-merge(data_1,data_2,by="Code")

X <- merge1_2$`NFL (pg/ml)`
names(merge1_2)

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

Res<-data.frame()
p <- list()
n=0
for (tm in c("T0","T1","T2","T3","T4","T5","T6")){
  
  
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
    temp<-data.frame(Var=col,Time=tm,p.value=test$p.value)
    Res<-rbind(Res,temp)
    if (test$p.value<0.01){
      
      n=n+1
     
     # print(tm)
      print(col)
    #  print(test$p.value)
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
        stat_compare_means(comparisons = list(c("Low","High")))+
        ggsci::scale_fill_aaas()
      print(p1)
    p[[paste0(col,tm)]]<-p1
    }
  }
}
Res[Res$p.value<0.01,]%>%dim()

cat(paste0(Res[Res$p.value<0.01,]$Var, " ",
           Res[Res$p.value<0.01,]$Time),sep = "\n")




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
n=0

for (col in names(merge1_2)[15:141]){
 
  if (!grepl("MFI",col)){
  temp_test<-merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                        !is.na(merge1_2$Group_ratio),]
  
  pvalues<-c()

  for( time in unique(temp_test$Timepoint)[2:7]){
    
  if(!all(temp_test[temp_test$Timepoint==time,col]==0)){  

    test<-
    wilcox.test(temp_test[temp_test$Timepoint==time,col]~
                  temp_test[temp_test$Timepoint==time,"Group_ratio"])
  }
  pvalues<-c(pvalues,test$p.value<0.01)
  }
  
  
 if (any(pvalues)){
   print(col)
  pl<-ggplot(merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                        !is.na(merge1_2$Group_ratio)&
                        merge1_2$Timepoint!="T0",],
             aes(
               y=merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                            !is.na(merge1_2$Group_ratio)&
                            merge1_2$Timepoint!="T0",col],
               x=factor(Group_ratio,
                        levels = c("M","P"))))+
    geom_boxplot(outliers = F)+
    ggbeeswarm::geom_quasirandom(shape=21,
                                 size=3,
                                 aes(fill=Group_ratio))+
    ggprism::theme_prism(base_size = 10)+
    ylab(col)+
    xlab("")+facet_wrap(~Timepoint,nrow = 1)+
    stat_compare_means(comparisons = list(c("M","P")))+
    ggsci::scale_fill_aaas()
  
  n=n+1
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
  #print(pl2)
  
}
}
}







"USWPB/CD80"


merge1_2[merge1_2$Timepoint=="T1"&
           !is.na(merge1_2$Group_ratio),]%>%
  ggplot(aes(Group_diff,`USWcount`))+
  geom_boxplot(outliers=F)+
  ggbeeswarm::geom_quasirandom(aes(fill=Group_ratio),shape=21,size=3)+
  theme_prism()+stat_compare_means(comparisons = list(c("M","P")))+
  ggsci::scale_fill_aaas()+xlab("")

###############################
# correlazione con diff e valore
N=0
for (col in names(merge1_2)[15:141]){

  test<-cor.test(merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                           !is.na(merge1_2$Group_diff)&
                            merge1_2$Timepoint!="T0",col],
                   merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                              !is.na(merge1_2$Group_diff)&
                              merge1_2$Timepoint!="T0","Difference"])
  
  if (test$p.value<0.05){
   N=N+1
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
      ggprism::theme_prism(base_size = 15)+
      facet_wrap(~Timepoint,nrow = 1)
    
    print(pp)

    
  }
}

##########################
# correlazione con Ratio e valore
N=0
for (col in names(merge1_2)[15:141]){
  print(col)
  if(!grepl("MFI",col)&!grepl("USWPB/CD24",col)){
  pvalues<-c()
  for (time in c("T1","T2","T3","T4","T5","T6")){
  test<-cor.test(merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                            !is.na(merge1_2$Group_ratio)&
                            merge1_2$Timepoint!="T0"&
                            merge1_2$Timepoint==time,col],
                 merge1_2[!is.na(merge1_2$`NFL (pg/ml)`)&
                            !is.na(merge1_2$Group_ratio)&
                            merge1_2$Timepoint!="T0"&
                            merge1_2$Timepoint==time,"Ratio"],)
  
  if(!is.na(test$p.value)){
  pvalues<-c(pvalues,test$p.value<0.01)
  }
  }
  if (any(pvalues)){
    N=N+1
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
      ggprism::theme_prism(base_size = 15)+
      facet_wrap(~Timepoint,nrow = 1)
    
    print(pp)
  
  }
}
}
N
##############
# Alluvial plot
library(tidyverse)
library(ggalluvial)

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
    ggprism::theme_prism(base_size = 15)+
   ggtitle(name)
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


############################################################


fantastic11<-merge1_2%>%group_by(`Codice_pz`)%>%
  summarise(n=n())%>%arrange(-n)%>%
  filter(n==7)


Completesamples<-merge1_2[merge1_2$`Codice pz`%in%fantastic11$Codice_pz,]


names(Completesamples)

for ( col in names(Completesamples)[15:141]){
  
  if (length(na.omit(Completesamples[,col]))==77){
  p<-ggplot(Completesamples,
            aes(Timepoint,
                Completesamples[,col]))+
    geom_boxplot()+
    geom_point(shape=21,aes(fill=`Codice pz`),size=3)+
    geom_line(aes(group=`Codice pz`),color="grey80")+
    ggprism::theme_prism()+ylab(col)
  print(p)
  }
  
}

names(Completesamples)[2]<-"ID"
N=0
library(afex)
sign<-c()
for ( col in names(Completesamples)[15:141]){
  
  if (length(na.omit(Completesamples[,col]))==77){
    
    
    risultato_anova <- aov_ez(
      id = "ID",          # Colonna con l'ID dei soggetti
      dv = col,            # Variabile dipendente
      within = "Timepoint",     # Variabile indipendente (entro i soggetti, es: diversi timepoint)
      data = Completesamples      # Il tuo dataframe con i dati
    )
    
    # Visualizza i risultati
    if(risultato_anova$anova_table$`Pr(>F)`<0.01){
    N=N+1
    print(col)
    p<-ggplot(Completesamples,
              aes(Timepoint,
                  Completesamples[,col]))+
      geom_boxplot()+
      geom_point(shape=21,aes(fill=ID),size=3)+
      geom_line(aes(group=ID),color="grey80")+
      ggprism::theme_prism()+ylab(col)
    print(p)
    
    sign<-c(sign,col)
  }
  }
}






library(clusterMLD)
Completesamples$Time<-as.numeric(str_replace(Completesamples$Timepoint,"T",""))

svd(Completesamples$Time)

Cluster.object<-LongDataCluster(x=Completesamples$Time,
                Y=Completesamples$`SW/ASC`,
              id=Completesamples$ID) 
Cluster.object$Dat.label
DendroPlot(Cluster.object)
MeanPlot(Cluster.object,No.Cluster = 4)



library("kml")

Completesamples$`NFL (pg/ml)`
survey_data_wide <- Completesamples %>% 
  dplyr::arrange(Time) %>% 
  tidyr::pivot_wider(id_cols = ID, 
                     names_from = Time, 
                     values_from = `NFL (pg/ml)`)
#survey_data_wide <- as.data.frame(survey_data_wide)
#survey_data_cld <- kml::cld(survey_data_wide,
                            timeInData = 2:7, maxNA = 0)

#class(survey_data_cld)
#kml::kml(survey_data_cld, nbRedrawing = 5)
#kml::choice(survey_data_cld)




library(latrend)
data(latrendData)
head(latrendData)


names(Completesamples)[2]<-"Id"
prova<-merge1_2
names(prova)[c(2,5)]<-c("Id","Time")
prova$Time<-as.integer(str_replace(prova$Time,"T",""))
Completesamples$`NFL (pg/ml)`
plotTrajectories(Completesamples, 
                 latrend.time = "Time",
                 latrend.id = "Id", 
                 response = "NFL (pg/ml)")

kmlMethod <- lcMethodKML("NFL (pg/ml)", nClusters = 3)
model <- latrend(kmlMethod, data =Completesamples)
summary(model)
plot(model)+ggprism::theme_prism()

clust<-latrend::fittedTrajectories(model)
sclust<-clust[!duplicated(clust[,c("Id","Cluster")]),]

latrend::m
Completesamples[Completesamples$Id%in%
                  sclust[sclust$Cluster=="A",]$Id,]

info[info$`Codice pz`%in%sclust[sclust$Cluster=="B",]$Id&
       info$Timepoint=="T0",]$EDSS%>%
 table()


kmlMethods <- lcMethods(kmlMethod, nClusters = 1:10)
models <- latrendBatch(kmlMethods, data = Completesamples)
metric(models, c("WMAE", "BIC"))
plotMetric(models, c("Dunn", "ASW", "WMAE", "WRSS", "BIC", "estimationTime"))



##########################

lista<-list(T0=data_2[!is.na(data_2$`CD19+`)&
                        data_2$TP=="T0","Codice_pz"],
           #T1=data_2[!is.na(data_2$`CD19+`)&
          #             data_2$TP=="T1","Codice_pz"],
           # T2=data_2[!is.na(data_2$`CD19+`)&
            #            data_2$TP=="T2","Codice_pz"],
            T3=data_2[!is.na(data_2$`CD19+`)&
                        data_2$TP=="T3","Codice_pz"],
            T4=data_2[!is.na(data_2$`CD19+`)&
                        data_2$TP=="T4","Codice_pz"],
            T5=data_2[!is.na(data_2$`CD19+`)&
                        data_2$TP=="T5","Codice_pz"],
            T6=data_2[!is.na(data_2$`CD19+`)&
                        data_2$TP=="T6","Codice_pz"])
library(UpSetR)
upset(fromList(lista),order.by = "freq",
      sets = rev(c("T0","T3","T4","T5","T6")),
      keep.order = TRUE,main.bar.color = "darkred",
      sets.bar.color = "darkblue",text.scale = 2)



fantastic21<-merge1_2[!merge1_2$Timepoint%in%
                        c("T2","T1"),]%>%group_by(`Codice_pz`)%>%
  summarise(n=n())%>%arrange(-n)%>%
  filter(n==5)


Completesamples21<-merge1_2[!merge1_2$Timepoint%in%c("T1","T2") &merge1_2$`Codice pz`%in%fantastic21$Codice_pz,]
dim(Completesamples21)

names(Completesamples21)

table(Completesamples21$Timepoint)
for ( col in names(Completesamples21)[15:141]){
  
  if (length(na.omit(Completesamples21[,col]))==105){
    p<-ggplot(Completesamples21,
              aes(Timepoint,
                  Completesamples21[,col]))+
      geom_boxplot()+
      geom_point(shape=21,aes(fill=`Codice pz`),size=3)+
      geom_line(aes(group=`Codice pz`),color="grey80")+
      ggprism::theme_prism()+ylab(col)
    print(p)
  }
  
}

names(Completesamples21)[2]<-"ID"
N=0
library(afex)
sign<-c()
for ( col in names(Completesamples21)[15:141]){
  
  if (length(na.omit(Completesamples21[,col]))==105){
    
    
    risultato_anova <- aov_ez(
      id = "ID",          # Colonna con l'ID dei soggetti
      dv = col,            # Variabile dipendente
      within = "Timepoint",     # Variabile indipendente (entro i soggetti, es: diversi timepoint)
      data = Completesamples21      # Il tuo dataframe con i dati
    )
    
    # Visualizza i risultati
    if(risultato_anova$anova_table$`Pr(>F)`<0.01){
      N=N+1
      print(col)
      p<-ggplot(Completesamples21,
                aes(Timepoint,
                    Completesamples21[,col]))+
        geom_boxplot()+
        geom_point(shape=21,aes(fill=ID),size=3)+
        geom_line(aes(group=ID),color="grey80")+
        ggprism::theme_prism()+ylab(col)
      print(p)
      
      sign<-c(sign,col)
    }
  }
}

N

names(Completesamples21)[2]<-"Id"
Completesamples21$Time<-as.numeric(str_replace(Completesamples21$Timepoint,"T",""))

plotTrajectories(Completesamples21, 
                 latrend.time = "Time",
                 latrend.id = "Id", 
                 response = "NFL (pg/ml)")

kmlMethod <- lcMethodKML("NFL (pg/ml)", nClusters = 2)
model <- latrend(kmlMethod, data =Completesamples21)
summary(model)
plot(model)+ggprism::theme_prism()

clust<-latrend::fittedTrajectories(model)
sclust<-clust[!duplicated(clust[,c("Id","Cluster")]),]






