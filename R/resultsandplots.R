#############results##########
############# confusion matrix ##############
# logistic
glmcm <- confusionMatrix(as.factor(glm_pred), as.factor(test$readmitted))
glmaccuracy <- glmcm$overall[1]
pltcm <- as.data.frame(glmcm$table)
pltcm$Prediction <- factor(pltcm$Prediction, levels=rev(levels(pltcm$Prediction)))
pltcm_gg_glm <- ggplot(pltcm, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction")+ggtitle("Logistic Regression")+
  theme(plot.title = element_text(hjust = 0.5))
pltcm_gg_glm
# ggsave("pltcm_gg_glm.png",width=1172, height=1015, units="px")


# naive bayesian
nbcm <- confusionMatrix(as.factor(nb_pred), as.factor(test$readmitted))
nbaccuracy <- nbcm$overall[1]
pltcm <- as.data.frame(glmcm$table)
pltcm$Prediction <- factor(pltcm$Prediction, levels=rev(levels(pltcm$Prediction)))
pltcm_gg_nb <- ggplot(pltcm, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction")+ggtitle("Naive Bayesian")+
  theme(plot.title = element_text(hjust = 0.5))
pltcm_gg_nb
# ggsave("pltcm_gg_nb.png",width=1172, height=1015, units="px")


# decision tree
rpcm <- confusionMatrix(as.factor(rp_pred), as.factor(test$readmitted))
rpaccuracy <- rpcm$overall[1]
pltcm <- as.data.frame(rpcm$table)

pltcm$Prediction <- factor(pltcm$Prediction, levels=rev(levels(pltcm$Prediction)))
pltcm_gg_rp <- ggplot(pltcm, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction")+ggtitle("Decision Tree")+
  theme(plot.title = element_text(hjust = 0.5))
pltcm_gg_rp
# ggsave("pltcm_gg_rp.png",width=1172, height=1015, units="px")

# random forest

rfcm <- confusionMatrix(as.factor(rf_pred_tuned), as.factor(test$readmitted))
rfaccuracy <- rfcm$overall[1]
pltcm <- as.data.frame(rfcm$table)

pltcm$Prediction <- factor(pltcm$Prediction, levels=rev(levels(pltcm$Prediction)))
pltcm_gg_rf <- ggplot(pltcm, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Reference",y = "Prediction")+ggtitle("Random Forest")+
  theme(plot.title = element_text(hjust = 0.5))
pltcm_gg_rf
# ggsave("pltcm_gg_rf.png",width=1172, height=1015, units="px")


# report:
as.numeric(c(glmaccuracy,nbaccuracy,rpaccuracy,rfaccuracy))
# [1] 0.6150032 0.5867926 0.6122893 0.6161088

######## variable importance ##########
importance_rf <- model_rftuned$importance %>% data.frame() %>% 
  arrange(MeanDecreaseAccuracy) %>% 
  mutate(orders=c(1:nrow(model_rftuned$importance)))
importance_rf <- importance_rf %>% mutate(Variables=row.names(importance_rf))

importance_rf %>% 
  mutate(Variables = forcats::fct_reorder(Variables, MeanDecreaseAccuracy)) %>%
  ggplot( aes(x=Variables, y=MeanDecreaseAccuracy)) +
  geom_bar(stat="identity",color='black', fill="#f68060", alpha=.6, width=.6) +
  coord_flip() +  xlab("") +  theme_bw() + labs(y = "Importance (random forest)")

# ggsave("Importance_rf.png",width=1118, height=954, units="px")
importance_rp <- tune_rpart$variable.importance %>% data.frame() %>%
  mutate(Importance=./sum(.),Variables=row.names(importance_rp))
importance_rp %>% 
  mutate(Variables = forcats::fct_reorder(Variables, Importance)) %>%
  ggplot( aes(x=Variables, y=Importance)) +
  geom_bar(stat="identity",color='black', fill="#f68060", alpha=.6, width=.6) +
  coord_flip() +  xlab("") +  theme_bw() + labs(y = "Importance (decision tree)")
# ggsave("Importance_rp.png",width=1118, height=954, units="px")

########### ROC plot ##############
roc_glm<- pROC::roc(test$readmitted,glm.probs)  # logistic regression
roc_nb<- pROC::roc(test$readmitted,nb_probs[,2])  # naive bayesian
roc_tune_rp<- pROC::roc(test$readmitted,tune_rp_probs[,2]) # decision tree
roc_rf_tuned<- pROC::roc(test$readmitted,rf_probs_tuned[,2]) # random forest

# png("ROC.png",width=409, height=355, units="px")
color_roc<-c("skyblue","palegreen","forestgreen", "darkblue")
plot(roc_glm,col=color_roc[1])
lines(roc_nb,col=color_roc[2])
lines(roc_tune_rp,col=color_roc[3])
lines(roc_rf_tuned,col=color_roc[4])
legend("bottomright",
       legend=c("logistic regression","naive bayesian","decision tree","random forest"),
       col = color_roc,lwd = 2)
# dev.off()



######## AUC ##########
auc_glm <- pROC::auc(test$readmitted, glm_probs) #0.6539
auc_nb <- pROC::auc(test$readmitted, nb_probs[, 2]) #0.6407
auc_rp <- pROC::auc(test$readmitted, tune_rp_probs[, 2]) #0.6104
auc_rf <- pROC::auc(test$readmitted, as.numeric(rf_probs_tuned[, 2])) #0.6563

as.numeric(c(auc_glm, auc_nb, auc_rp, auc_rf))

########report##########
table1<- data.frame(Accuracy=c(0.615,0.587,0.612,0.616),
                    Specificity = c(0.806, 0.913, 0.757, 0.755),
                    Sensitivity = c(0.397, 0.214, 0.447, 0.457),
                    PosPredValue= c(0.641, 0.682, 0.617, 0.620),
                    NegPredValue= c(0.604, 0.570, 0.610, 0.614),
                    AUC = c(0.654,0.641,0.610,0.656))
knitr::kable(t(table1),format = "pandoc",
             caption = "Confusion Matrix Parameters and Area Under Curve",
             col.names=c("logistic regression","naive bayesian",
                         "decision tree","random forest"))


######### decision tree plot ###########

# png("tuned_decisiontree.png",width=470, height=310, units="px")
rpart.plot(tune_rpart, extra = 100) #final results-- decision tree
# dev.off()



