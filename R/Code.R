full_model = read.csv("~/Downloads/FINAL/diabetic_data.csv")
cl_model = subset(full_model,select = -c(encounter_id,patient_nbr,weight,payer_code, medical_specialty,examide,citoglipton))

a = vector()
b = list()

for(i in 1:ncol(cl_model)){
a[i] = length(which(cl_model[,i] == "?"))
b[[i]] = unique(cl_model[,i])
}

print(a)
print(b)

length(which(full_model[,4] == "Unknown/Invalid"))
