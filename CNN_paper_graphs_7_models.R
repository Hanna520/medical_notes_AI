library(ggplot2)
library(dplyr)

setwd("C:/Users/Hanna/Desktop/LU/Chapman/Research_2/Data Augmentation - Text/main")


dd <- read.csv("other/no_embedding_20_epocs_7_models.csv")
names(dd)[1] <- "Disease"
dd$Balanced_Acc <- (dd$Precision + dd$Recall)/2


dd_grouped <- dd %>%
  group_by(Disease,Model) %>%
  # summarise(n=n())
  summarise(AUC_ROC=mean(AUC.ROC,na.rm = TRUE), AUC_PR=mean(AUC.PR,na.rm = TRUE),
            Balanced_Acc=mean(Balanced_Acc,na.rm = TRUE), 
            Precision=mean(Precision, na.rm=TRUE),
            Recall=mean(Recall, na.rm=TRUE),Specificity=mean(Specificity, na.rm=TRUE),
            F1_score=mean(F1,na.rm = TRUE),
            Running_Time=mean(Running.Time,na.rm = TRUE))

dd_grouped$Model <- factor(dd_grouped$Model, levels = c("CNN", "RNN", "GRU", "LSTM",
                                                        "Bi_LSTM", "Transformer", "BERT"))

# write.csv(dd_grouped,'other/grouped_no_embedding_7_models.csv')

prevalence <- read.csv("other/prevalence.csv")
names(prevalence)[1] <- "Disease"

dd_performance <- merge(dd_grouped, prevalence, by = "Disease")

######################################
# Training Time
######################################
# Average Training Time
dd2 <- dd %>%
  group_by(Model) %>%
  summarise(running_time = mean(Running.Time)) %>%
  # summarise(Time_Per_Epoc = mean(Time_Per_Epoc)) %>%
  select(c(Model, running_time)) %>%
  distinct()



tiff(file="figures/training_time.tiff", width=8, height=5, units="in", res=250)
ggplot(dd2) +
  geom_point(aes(y = reorder(Model, -running_time), x = running_time)) +
  geom_bar(aes(y = reorder(Model, -running_time), x = running_time), 
           stat = "identity", width = 0.04) +
  labs(y = "Model", x = "Average Training Time (in Seconds)") +
  theme(text = element_text(size = 20)) +
  scale_y_discrete(labels = rev(c("CNN", "RNN", "GRU", "LSTM", "Bi_LSTM", 
                                  "Transformer", "BERT")))
dev.off()


# AUC-ROC vs AUC-PR
reg <- lm((AUC.ROC - AUC.PR)~Percent_Disease, data = dd)
summary(reg)

tiff(file="figures/auc_roc_pr.tiff", width=15, height=10, units="in", res=250)
ggplot(dd) +
  geom_point(aes(x=Percent_Disease, y=AUC.ROC - AUC.PR, col=Model), size=4) +
  geom_hline(yintercept=0, size=1) +
  geom_vline(xintercept=0.5, size=1) +
  geom_smooth(aes(x=Percent_Disease, y=AUC.ROC - AUC.PR), 
              method="lm", se=TRUE, col="red", size=1) +
  scale_x_continuous(breaks=c(0.1, 0.3, 0.5, 0.7)) +
  ylim(c(-0.2,0.8)) +
  geom_text(x = 0.3, y = 0.75, label = "Y = 0.517 - 1.02X", 
            family="Times", size = 12, bold = FALSE) +
  geom_text(x = 0.3, y = 0.65, label = "R-squared = 0.937", 
            family="Times", size = 12, bold = FALSE)+
  labs(x = "Disease Prevalence", y = "AUC-ROC Minus AUC-PR") +
  theme(text = element_text(size = 28)) 
dev.off()


##################################################################
# Side-by-side

library("gridExtra")  

# AUC-ROC
auc_roc <- ggplot(dd_performance) +
  geom_point(aes(x=Prevalence, y=AUC_ROC, col=Model)) +
  geom_line(aes(x=Prevalence, y=AUC_ROC, col=Model), size=1.5) +
  scale_x_continuous(breaks=c(0.1, 0.3, 0.5, 0.7)) +
  ylim(0,1) +
  labs(x = "Disease Prevalence",
       y = "AUC-ROC",
       title = "a. AUC-ROC") +
  theme(text = element_text(size = 24),
        plot.title = element_text(hjust = 0.5))  


# AUC-PR

auc_pr <- ggplot(dd_performance) +
  geom_point(aes(x=Prevalence, y=AUC_PR, col=Model)) +
  geom_line(aes(x=Prevalence, y=AUC_PR, col=Model), size=1.5) +
  scale_x_continuous(breaks=c(0.1, 0.3, 0.5, 0.7)) +
  ylim(0,1) +
  labs(x = "Disease Prevalence",
       y = "AUC_PR",
       title = "b. AUC-PR") +
  theme(text = element_text(size = 24),
        plot.title = element_text(hjust = 0.5)) 


# F1-Score

f1_score <- ggplot(dd_performance) +
  geom_point(aes(x=Prevalence, y=F1_score, col=Model)) +
  geom_line(aes(x=Prevalence, y=F1_score, col=Model), size=1.5) +
  scale_x_continuous(breaks=c(0.1, 0.3, 0.5, 0.7)) +
  ylim(0,1) +
  labs(x = "Disease Prevalence",
       y = "F1 Score",
       title = "c. F1 Score") +
  theme(text = element_text(size = 24),
        plot.title = element_text(hjust = 0.5)) 


# Accuracy

accuracy <- ggplot(dd_performance) +
  geom_point(aes(x=Prevalence, y=Balanced_Acc, col=Model)) +
  geom_line(aes(x=Prevalence, y=Balanced_Acc, col=Model), size=1.5) +
  scale_x_continuous(breaks=c(0.1, 0.3, 0.5, 0.7)) +
  ylim(0,1) +
  labs(x = "Disease Prevalence",
       y = "Balanced Accuracy",
       title = "d. Balanced Accuracy") +
  theme(text = element_text(size = 24),
        plot.title = element_text(hjust = 0.5)) 


tiff(file="figures/revised_figure1.tiff", width=20, height=15, units="in", res=300)
grid.arrange(auc_roc, auc_pr, f1_score, accuracy, 
             ncol = 2, nrow = 2) 
dev.off()

###############################################################################

####################### AUC-ROC vs AUC-PR ########################

# AUC-ROC vs AUC-PR
dd3 <- merge(dd, prevalence, by = "Disease")
dd3$Model <- factor(dd3$Model, levels = c("CNN", "RNN", "GRU", "LSTM",
                                          "Bi_LSTM", "Transformer", "BERT"))
reg <- lm((AUC.ROC - AUC.PR)~Prevalence, data = dd3)
summary(reg)

tiff(file="figures/auc_roc_pr.tiff", width=15, height=10, units="in", res=250)
ggplot(dd3) +
  geom_point(aes(x=Prevalence, y=AUC.ROC - AUC.PR, col=Model), size=2) +
  geom_hline(yintercept=0, size=1) +
  geom_vline(xintercept=0.5, size=1) +
  geom_smooth(aes(x=Prevalence, y=AUC.ROC - AUC.PR), 
              method="lm", se=TRUE, col="red", size=1) +
  scale_x_continuous(breaks=c(0.1, 0.3, 0.5, 0.7)) +
  ylim(c(-0.2,0.8)) +
  geom_text(x = 0.3, y = 0.75, label = "Y = 0.475 - 0.926X", 
            family="Times", size = 11, bold = FALSE) +
  geom_text(x = 0.3, y = 0.65, label = "R-squared = 0.931", 
            family="Times", size = 11, bold = FALSE)+
  labs(x = "Disease Prevalence", y = "AUC-ROC Minus AUC-PR") +
  theme(text = element_text(size = 28)) 
dev.off()



######################################
# Training Time
######################################
# Average Training Time
dd2 <- dd %>%
  group_by(Model) %>%
  summarise(running_time = mean(Running.Time)) %>%
  select(c(Model, running_time)) %>%
  distinct()

ggplot(dd2) +
  geom_point(aes(y = reorder(Model, -running_time), x = running_time)) +
  geom_bar(aes(y = reorder(Model, -running_time), x = running_time), 
           stat = "identity", width = 0.04) +
  labs(y = "Model", x = "Average Running Time (in Seconds)") +
  theme(text = element_text(size = 20)) +
  scale_y_discrete(labels = rev(c("CNN", "RNN", "GRU", "LSTM", "Bi_LSTM")))

