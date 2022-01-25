library(ggplot2)
library(dplyr)

setwd("C:/Users/Hanna/Desktop/LU/Chapman/Research_2/Data Augmentation - Text/main")


dd <- read.csv("other/CNN_Paper_embeddings.csv")
dd$Model <- factor(dd$Model, levels = c("CNN", "RNN", "GRU", "LSTM", "Bi_LSTM"))
dd$Balanced_Acc <- (dd$Precision + dd$Recall)/2


dd_grouped <- dd %>%
  group_by(Disease,Model,Embedding) %>%
  summarise(AUC_ROC=mean(AUC.ROC,na.rm = TRUE), AUC_PR=mean(AUC.PR,na.rm = TRUE),
            Balanced_Acc=mean(Balanced_Acc,na.rm = TRUE), 
            Precision=mean(Precision, na.rm=TRUE),
            Recall=mean(Recall, na.rm=TRUE),Specificity=mean(Specificity, na.rm=TRUE),
            F1_score=mean(F1,na.rm = TRUE),
            Running_Time=mean(Running.Time,na.rm = TRUE))

dd_grouped$Model <- factor(dd_grouped$Model, levels = c("CNN", "RNN", "GRU", "LSTM",
                                                        "Bi_LSTM"))



prevalence <- read.csv("other/prevalence.csv")
names(prevalence)[1] <- "Disease"

dd_performance <- merge(dd_grouped, prevalence, by = "Disease")

# write.csv(dd_performance,'other/grouped_embeddings_5_models.csv')

##################################################################
# Side-by-side

library("gridExtra")  

auc_roc <- ggplot(dd_performance, aes(x=Prevalence, y=AUC_ROC, group = Embedding)) +
  geom_point(aes(col=Embedding), size=2) +
  geom_line(aes(col=Embedding), size=0.5) +
  scale_x_continuous(breaks=c(0.1, 0.3, 0.5, 0.7)) +
  # ylim(0,1) +
  facet_grid(Model ~ .) +
  scale_color_manual(values=c("blue", "deeppink", "darkturquoise")) +
  labs(x = "Disease Prevalence",
       y = "AUC-ROC",
       title = "a. AUC-ROC") +
  theme(text = element_text(size = 24),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5)) 

auc_pr <- ggplot(dd_performance, aes(x=Prevalence, y=AUC_PR, group = Embedding)) +
  geom_point(aes(col=Embedding), size=2) +
  geom_line(aes(col=Embedding), size=0.5) +
  scale_x_continuous(breaks=c(0.1, 0.3, 0.5, 0.7)) +
  # ylim(0,1) +
  facet_grid(Model ~ .) +
  scale_color_manual(values=c("blue", "deeppink", "darkturquoise")) +
  labs(x = "Disease Prevalence",
       y = "AUC-PR",
       title = "b. AUC-PR") +
  theme(text = element_text(size = 24),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5))

f1_score <- ggplot(dd_performance, aes(x=Prevalence, y=F1_score, group = Embedding)) +
  geom_point(aes(col=Embedding), size=2) +
  geom_line(aes(col=Embedding), size=0.5) +
  scale_x_continuous(breaks=c(0.1, 0.3, 0.5, 0.7)) +
  # ylim(0,1) +
  facet_grid(Model ~ .) +
  scale_color_manual(values=c("blue", "deeppink", "darkturquoise")) +
  labs(x = "Disease Prevalence",
       y = "F1 Score",
       title = "c. F1 Score") +
  theme(text = element_text(size = 24),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5))

accuracy <- ggplot(dd_performance, aes(x=Prevalence, y=Balanced_Acc, group = Embedding)) +
  geom_point(aes(col=Embedding), size=2) +
  geom_line(aes(col=Embedding), size=0.5) +
  scale_x_continuous(breaks=c(0.1, 0.3, 0.5, 0.7)) +
  # ylim(0,1) +
  facet_grid(Model ~ .) +
  scale_color_manual(values=c("blue", "deeppink", "darkturquoise")) +
  labs(x = "Disease Prevalence",
       y = "Balanced Accuracy",
       title = "d. Balanced Accuracy") +
  theme(text = element_text(size = 24),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5))

tiff(file="figures/figure_embeddings4.tiff", width=20, height=15, units="in", res=300)
grid.arrange(auc_roc, auc_pr, f1_score, accuracy, 
             ncol = 2, nrow = 2) 
dev.off()

########################################################################
# Plot training time

dd2 <- dd %>%
  group_by(Model, Embedding) %>%
  summarise(Running_Time = mean(Running_Time)) %>%
  select(c(Model, Embedding, Running_Time)) %>%
  distinct()

tiff(file="figures/embeddings_training_time.tiff", width=20, height=15, units="in", res=250)
ggplot(dd2) +
  geom_point(aes(y = reorder(Embedding, -Running_Time), x = Running_Time)) +
  geom_bar(aes(y = reorder(Embedding, -Running_Time), x = Running_Time), 
           stat = "identity", width = 0.05) +
  facet_grid(Model ~ .) +
  labs(y = "Embedding", x = "Average Training Time (in Seconds)") +
  theme(text = element_text(size = 28))
dev.off()


