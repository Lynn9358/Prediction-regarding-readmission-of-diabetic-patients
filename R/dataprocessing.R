############## pre processing ###############
diadata = read.csv("~/Downloads/FINAL/diabetic_data.csv") # import original data

#Recode for medical
Med = c(0,length = 24)
for (i in 1: 23) {
  Med[i]= length(diadata[,i+24][which(diadata[,i+24]=="Steady"|
                                        diadata[,i+24]=="Up"|
                                        diadata[,i+24]=="Down")])
}
names(Med) = colnames(diadata[,c(25:47)])
barplot(sort(Med,decreasing = TRUE))
head(sort(Med,decreasing = TRUE),n = 10)


##remove missing data in gender
diadata = diadata[diadata$gender != "Unknown/Invalid", ]
diadata = diadata[diadata$race != "?", ]

## giving data to readmission 
cl_data = subset(diadata,select = -c(encounter_id,patient_nbr,weight,payer_code, medical_specialty,examide,citoglipton,acetohexamide,chlorpropamide, acetohexamide,tolbutamide, miglitol,troglitazone,tolazamide, glipizide.metformin,glimepiride.pioglitazone,metformin.rosiglitazone,metformin.pioglitazone,glyburide.metformin  ))
cl_data$readmitted[cl_data$readmitted == "NO"] <- 0
cl_data$readmitted[cl_data$readmitted == ">30"] <- 1
cl_data$readmitted[cl_data$readmitted == "<30"] <- 1


#Recode for medical
Med = c(0,length = 10)
for (i in 1: 10) {
  Med[i]= length(cl_data[,i+19][which(cl_data[,i+19]=="Steady"|
                                        cl_data[,i+19]=="Up"|
                                        cl_data[,i+19]=="Down")])
}
names(Med) = colnames(cl_data[,c(20:29)])
barplot(sort(Med,decreasing = TRUE))


## convert data in diag_1 to diag_3 start with V or E to 1000
cl_data$diag_1_recode <- cl_data$diag_1
cl_data$diag_1_recode[which(unlist(lapply("^(E|V)",grepl,cl_data$diag_1)))]<-"1000"
cl_data$diag_1_recode <- as.character(trunc(as.numeric(cl_data$diag_1_recode)))
# summary(as.factor(cl_data$diag_1_recode))

cl_data$diag_2_recode <-cl_data$diag_2
cl_data$diag_2_recode[which(unlist(lapply("^(E|V)",grepl,cl_data$diag_2)))]<-"1000"
cl_data$diag_2_recode <- as.character(trunc(as.numeric(cl_data$diag_2_recode)))
# summary(as.factor(cl_data$diag_2_recode))

cl_data$diag_3_recode <-cl_data$diag_3
cl_data$diag_3_recode[which(unlist(lapply("^(E|V)",grepl,cl_data$diag_3)))]<-"1000"
cl_data$diag_3_recode <- as.character(trunc(as.numeric(cl_data$diag_3_recode)))
# summary(as.factor(cl_data$diag_3_recode))

## unit value in diag
cl_data$diag_recode <- "Others"
cl_data$diag_recode[which(cl_data$diag_1_recode=="427"|
                            cl_data$diag_2_recode=="427"|
                            cl_data$diag_3_recode=="427")]<- "427"

cl_data$diag_recode[which(cl_data$diag_1_recode=="401"|
                            cl_data$diag_2_recode=="401"|
                            cl_data$diag_3_recode=="401")]<- "401"

cl_data$diag_recode[which(cl_data$diag_1_recode=="414"|
                            cl_data$diag_2_recode=="414"|
                            cl_data$diag_3_recode=="414")]<- "414"

cl_data$diag_recode[which(cl_data$diag_1_recode=="276"|
                            cl_data$diag_2_recode=="276"|
                            cl_data$diag_3_recode=="276")]<- "276"

cl_data$diag_recode[which(cl_data$diag_1_recode=="428"|
                            cl_data$diag_2_recode=="428"|
                            cl_data$diag_3_recode=="428")]<- "428"

cl_data$diag_recode[which(cl_data$diag_1_recode=="250"|
                            cl_data$diag_2_recode=="250"|
                            cl_data$diag_3_recode=="250")]<- "250"

cl_data = subset(cl_data,select = -c(diag_1,diag_2,diag_3,diag_1_recode,diag_2_recode,diag_3_recode))

############## descriptive analysis #########################

library(ggplot2)
#age
p1 = ggplot(aes(x=age, fill=readmitted), data=cl_data) + geom_bar(position = position_stack()) + ggtitle('Age vs. Readmitted') +  theme(plot.title = element_text(hjust = 0.5))
p1
ggsave("p1.png")

#Inpatient
p2 = ggplot(aes(x=number_inpatient, fill=readmitted), data=cl_data) + geom_bar(position = position_stack()) + ggtitle('Number_inpatient vs. Readmitted') + theme(plot.title = element_text(hjust = 0.5))
p2
ggsave("p2.png")

#diag_recode
p3 = ggplot(aes(x=diag_recode, fill=readmitted), data=cl_data) + geom_bar(position = position_stack()) + ggtitle('Diag_recode vs. Readmitted') + theme(plot.title = element_text(hjust = 0.5))
p3
ggsave("p3.png")


