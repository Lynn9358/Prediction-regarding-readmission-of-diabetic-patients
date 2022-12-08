diadata = read.csv("~/Downloads/FINAL/diabetic_data.csv")
##remove missing data in gender
diadata = diadata[diadata$gender != "Unknown/Invalid", ]

cl_model = subset(diadata,select = -c(encounter_id,patient_nbr,weight,payer_code, medical_specialty,examide,citoglipton))
names(cl_model)
diag = rbind(cl_model$diag_1,cl_model$diag_2,cl_model$diag_3)

a = vector()
b = list()

for(i in 1:ncol(cl_model)){
a[i] = length(which(cl_model[,i] == "?"))
b[[i]] = unique(cl_model[,i])
}

print(a)
print(b)

length(which(diadata[,4] == "Unknown/Invalid"))

sort(unique(cl_model$admission_source_id))
length(unique(cl_model$discharge_disposition_id))

##NULL value
length(which((cl_model[,5] == "18") |(cl_model[,5] == "26") ))
[1] 3691


