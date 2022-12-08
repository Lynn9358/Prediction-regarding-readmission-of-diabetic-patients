full_model = read.csv("~/Downloads/FINAL/diabetic_data.csv")

a = vector()
b = list()

for(i in 1:50){
a[i] = length(which(full_model[,i] == "?"))
b[[i]] = unique(full_model[,i])
}

print(a)
print(b)

length(which(full_model[,4] == "Unknown/Invalid"))
