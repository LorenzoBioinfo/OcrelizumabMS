############### TOPIC MODELING
#cells as words,
#cell types as terms,
#patient samples as documents,
#biological processes as topics.

library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggprism)
library(tibble)
library(stringr)
library(topicmodels)
library(slam)
library(tidytext)
library(tidyverse)
library(ComplexHeatmap)
library(survival)
library(RColorBrewer)

count<-read_excel("./TabelloneOcrelizumab FINALE (conte e %) - 05Agosto24.xlsx",sheet = "MS_T_MAIT_FLOW")
count<-as.data.frame(count)
count$Code<-paste0(count$`Codice pz`,"_",count$Timepoint)

fantastic17<-count[!is.na(count$`CD3/CD4/Tconv/CM`)&
        !count$Timepoint%in%c("T1","T2"),]%>%
  group_by(`Codice pz`)%>%
  summarize(n=n())%>%filter(n==5)

count<-count[count$`Codice pz`%in%fantastic17$`Codice pz`&
               !count$Timepoint%in%c("T1","T2"),]
names(count)
data.matrix<-count[,6:41]
dim(data.matrix)
data.matrix[,1:36]<-lapply(data.matrix[,1:36],as.integer)
rownames(data.matrix)<-count$Code
df_clean <- data.matrix[rowSums(is.na(data.matrix)) != ncol(data.matrix), ]

## prepare data format
count_matrix_triplet <- slam::as.simple_triplet_matrix(as.matrix(df_clean))


library("ldatuning")



lda<-LDA(count_matrix_triplet, control = list(seed = 2021,
                                              burnin = 100,thin = 100,
                                              iter = 100), k =3, method = "Gibbs")

para<-posterior(lda)
colnames(para$topics)<-paste0("topic",colnames(para$topics))
rownames(para$terms)<-paste0("topic",rownames(para$terms))

#Topic content matrix (cell type-by-topic matrix) represents 
#topics as different discrete distributions over cell types (clusters)
head(round(para$terms[,1:5],3))

#Topic prevalence matrix (topic-by-sample matrix)
#displays topic proportions estimated per sample
## show first five rows
head(round(para$topics,3))

#
terms(lda,5)


ap_topics <- tidy(lda, matrix = "beta")
ap_topics$topic<-as.factor(paste0("topic",ap_topics$topic))

### make barplots
ap_topics$term<-gsub("cluster","",ap_topics$term)
#term_reorder<-as.character(c(0,2,6,11,9,8,7,1,4,10,12,14,15,3,5,13,16,17,18,19))
ggbarplot(ap_topics, x = "term", y = "beta",color = "topic",fill = "topic",
          palette = c("#D55E00", "#009E73", "#0072B2"),
          facet.by = "topic",nrow = 3)+xlab("T cell cluster")+
  scale_x_discrete()+ylab("topic weight")+
  ggprism::theme_prism(axis_text_angle = 45)


### calculate the lift
wordcounts <- colSums(data.matrix)
logbeta <- lda@beta
emp.prob <- log(wordcounts) - log(sum(wordcounts))
lift <- logbeta - rep(emp.prob, each = nrow(logbeta))
colnames(lift)<-lda@terms
lift<-as.data.frame(lift)
lift$topics<-paste0("topic",1:3)

#### name three topics based on the content
new_label<-c("Exhaustion Topic","Naive Topic","Activation Topic")
names(new_label)<-c("topic1","topic2","topic3")

### show top 10 representative clusters for each topic
lift_top_terms <- lift %>% gather(.,key = cluster,value = lift,-topics) %>%
  group_by(topics) %>%
  slice_max(lift, n = 10) %>% 
  ungroup() %>%
  arrange(topics, -lift)

lift_top_terms %>%
  mutate(cluster = reorder_within(cluster, lift, topics)) %>%
  ggplot(aes(lift, cluster, fill = factor(topics))) +
  geom_col(show.legend = FALSE,orientation = "y") +
  facet_wrap(~ topics, scales = "free",labeller = labeller(topics = new_label)) + 
  scale_y_reordered()+theme_bw()+scale_fill_manual(values=c("#D55E00", "#009E73", "#0072B2"))+
  xlab("lift (log-ratio of topic weight over T cell cluster frequency)")+ylab("T cell cluster")



annotation_column = data.frame(
  topics = factor(rep(c("Exhaustion","Naive","Activation"),each = 3))
)

pt_meta<-unique(count$`Codice pz`)

## reshape the gamma matrix (topic fraction) for heatmap representation
rownames(para$topics)<-count$`Codice pz`
gamma_data<-as.data.frame(para$topics)
gamma_data$pt<-as.factor(count$`Codice pz`)
gamma_data$time<-as.factor(count$Timepoint)
gamma_wide_wna<-gamma_data %>% arrange(pt, time)%>%
  reshape(.,timevar = "time",idvar = "pt",direction = "wide") 
rownames(gamma_wide_wna)<-gamma_wide_wna$pt
head(gamma_wide_wna)



#
pt_meta<-count%>% select(`Codice pz`) %>% 
  arrange(`Codice pz`) %>% distinct(`Codice pz`,.keep_all = TRUE)
colnames(pt_meta)<-c("pt")
rownames(pt_meta)<-pt_meta$pt

pt_meta_wo6<-pt_meta[-6,]  

## we want to use the order of patients (rows) outside the heatmap.
## Thus patients are pre-grouped before heatmap
ourdist<-dist(gamma_wide_wna[,-1])  ## again, excluding pt 17-162-08 
re<-hclust(ourdist)
plot(re)
group<-as.data.frame(cutree(re,3))  
colnames(group)<-"Group"
pt_meta<-cbind(pt_meta,group)
pt_meta$Group<-factor(pt_meta$Group)
levels(pt_meta$Group)<-c("group3","group1","group2","group1")



## heatmap
group_df<-as.data.frame(pt_meta[,2])
rownames(group_df)<-pt_meta$pt
ComplexHeatmap::pheatmap(as.matrix(gamma_wide_wna[,-1]),cluster_cols = F,
                         cluster_rows = F,
                         drop_levels = TRUE,
                         col = colorRampPalette(c("white", "orange", "firebrick2"))(50),
                         gaps_col =c(3,6,9,12),
                         fontsize_row= 7,
                         annotation_row = group_df)



## stackplots for week 0,3,6
ps_list<-list()
timepoint<-c("A","B","C","D","E","F")
week<-c("T0","T3","T4","T5","T6")
listatime<-paste0("T",c(0,3:6))
for (wk in c(1:5)){
  cols<-paste0(c("topic1.","topic2.","topic3."),week[wk])
  topic_fraction<-gamma_wide_wna %>% 
    select(cols,"pt") %>%  
    gather(key = "Topic",value = "fraction",-pt) %>% na.omit()
  topic_fraction$Topic<-as.factor(topic_fraction$Topic)
  levels(topic_fraction$Topic)<-c("Naive topic", "Exhaustion topic","Activation topic")
  
  p1<-ggbarplot(topic_fraction,"pt","fraction",fill = "Topic",palette = c("#D55E00", "#009E73","#0072B2"),color = "Topic")+scale_x_discrete(limits = my_orders[my_orders %in% topic_fraction$pt],labels = my_labels[my_orders %in% topic_fraction$pt])+ylab("Topic fraction")+xlab("Patient")
  ps_list[[wk]]<-ggpar(p1,legend = "none",title = week[wk],font.xtickslab  = c(5,"bold"),xtickslab.rt = 45)
}

library(patchwork)
wrap_plots(ps_list,guides = "collect",nrow = 2,ncol=4)


## Parepare input data. add assigned group to each patient
integrate_data<-merge(gamma_data,pt_meta,by="pt")
integrate_data_reformat<-integrate_data %>% 
  gather(key = "Topic", value = "Proportion",topic1, topic2, topic3)
integrate_data_reformat$week<-as.factor(integrate_data$time)
levels(integrate_data_reformat$week)<-c("T0","T3","T4","T5","T6")


### boxplots show pharmacodynamics of four groups
color <- c(group1 = "#7570B3", group2 = "#E7298A", group3 = "#66A61E",group4 = "#D95F02")
new_label<-c("Exhaustion Topic","Naive Topic","Activation Topic")
names(new_label)<-c("topic1","topic2","topic3")
ps_list<-list()
groups<-c("group1","group2","group3")
for (group in groups){
  ps_list[[group]]<-integrate_data_reformat %>%
    filter(Group == group) %>%
    ggboxplot(.,x = "week", y = "Proportion",add = "point",fill = color[group])+geom_line(aes(group = pt),color = 'darkgray')+theme(legend.position = "none")+facet_wrap(vars(Topic),nrow = 1,labeller = labeller(Topic = new_label))+ggtitle(group)
}
wrap_plots(ps_list,nrow = 4)








