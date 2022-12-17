install.packages("naivebayes")
install.packages("psych")
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

diadata = read.csv("~/Downloads/FINAL/diabetic_data.csv")

#Recode for medical
Med = c(0,length = 24)
for (i in 1: 23) {
  Med[i]= length(diadata[,i+24][which(diadata[,i+24]=="Steady"|
                                        diadata[,i+24]=="Up"|
                                        diadata[,i+24]=="Down")])
}
names(Med) = colnames(diadata[,c(25:47)])
barplot(sort(Med,decreasing = TRUE))





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
summary(as.factor(cl_data$diag_1_recode))

cl_data$diag_2_recode <-cl_data$diag_2
cl_data$diag_2_recode[which(unlist(lapply("^(E|V)",grepl,cl_data$diag_2)))]<-"1000"
cl_data$diag_2_recode <- as.character(trunc(as.numeric(cl_data$diag_2_recode)))
summary(as.factor(cl_data$diag_2_recode))

cl_data$diag_3_recode <-cl_data$diag_3
cl_data$diag_3_recode[which(unlist(lapply("^(E|V)",grepl,cl_data$diag_3)))]<-"1000"
cl_data$diag_3_recode <- as.character(trunc(as.numeric(cl_data$diag_3_recode)))
summary(as.factor(cl_data$diag_3_recode))

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



a = vector()
b = list()
c = list()

for(i in 1:ncol(cl_data)){
  a[i] = length(which(cl_data[,i] == "?"))
  b[[i]] = unique(cl_data[,i])
  c[[i]] = table(cl_data[,i])
}

names(cl_data)
print(c)




##Naive Bayes
model <- naive_bayes(readmitted ~ ., data = cl_data, usekernel = T) 
p <- predict(model, cl_data, type = 'prob')


cl_data$metformin_recode <- cl_data$metformin
cl_data$metformin_recode[which(cl_data$metformin_recode=="Steady"|
                            cl_data$metformin_recode=="Up"|
                            cl_data$metformin_recode=="Down")]<- 1
cl_data$metformin_recode[which(cl_data$metformin_recode=="No")]<- 0











diag = c(cl_data$diag_1,cl_data$diag_2,cl_data$diag_3)
table_diag = as.matrix(table(diag))
table_diag_order = table_diag[order(table_diag[,1]),]
tail(table_diag_order)













length(which(diadata[,4] == "Unknown/Invalid"))

sort(unique(cl_data$admission_source_id))
length(unique(cl_data$discharge_disposition_id))

##NULL value
length(which((cl_data[,5] == "18") |(cl_data[,5] == "26") ))
[1] 3691


summary(lm(formula =  readmitted ~  factor(race) + factor(gender) + factor(age) + factor(admission_type_id)+ factor(dis) , data = cl_data))

summary(lm(formula =  readmitted ~  factor(race)+  factor(gender)+   factor(age)+  factor(admission_type_id)+  factor(discharge_disposition_id)+ factor(admission_source_id)+ time_in_hospital + num_lab_procedures + num_procedures+ num_medications+ number_outpatient+ number_emergency + number_inpatient  , data = cl_data))
fit = lm(formula =  readmitted ~  factor(race)+  factor(gender)+   factor(age)+  factor(admission_type_id)+  factor(discharge_disposition_id)+ factor(admission_source_id)+ time_in_hospital + num_lab_procedures + num_procedures+ num_medications+ number_outpatient+ number_emergency + number_inpatient  , data = cl_data)

