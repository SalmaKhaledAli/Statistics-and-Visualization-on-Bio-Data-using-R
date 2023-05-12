# Read the file 
path.name <- "C:/Users/Salma/Downloads/Assignment_05_GE_Data.txt"
dataFrame   <- read.table(path.name, sep = '', header = T, fill = T)
#dataFrame <- read.csv(path.name, header=FALSE, strip.white=T, sep='')
dataFrame

G_Healthy <- dataFrame[1]
G_Healthy

G_Cancerous <- dataFrame[2]
G_Cancerous

G_Healthy.v = c()


for (i in 1:nrow(G_Healthy)){
  G_Healthy.v[i] <- as.double(G_Healthy[i,])
}
G_Healthy.v

G_Cancerous.v = c()


for (i in 1:nrow(G_Cancerous)){
  G_Cancerous.v[i] <- as.double(G_Cancerous[i,])
}

G_Cancerous.v

### Use the First Method 

if(shapiro.test(G_Healthy.v)$p.value > 0.05 && shapiro.test(G_Cancerous.v)$p.value > 0.05){
  #check for variance equality
  if(var.test(G_Healthy.v, G_Cancerous.v)$p.value >= 0.05){
    print("t Test")
    
    t.test(x= G_Healthy.v, y= G_Cancerous.v, paired= FALSE, alternative= "two.sided", var.equal= TRUE)$p.value
  }else{
    print("welch Test")
    
    t.test(x= G_Healthy.v, y= G_Cancerous.v, paired= FALSE, alternative= "two.sided", var.equal= FALSE)$p.value
  }
  
}else{
  print("Wilcox Test, Because The Samples are not normally distributed")
  wilcox.test(x= G_Healthy.v, y= G_Cancerous.v, alternative= "two.sided")$p.value
  Data2 <- capture.output(print("Wilcox Test, Because The Samples are not normally distributed"), 
                          print(paste("The p_value of the 1st Method", wilcox.test(x= G_Healthy.v, y= G_Cancerous.v, alternative= "two.sided")$p.value)))
  
  writeLines(Data2, con = file("C:/Users/Salma/Downloads/Assignement_05_txt.txt"))
}

### Use The Second Method

T_Healthy = mean(G_Healthy.v)
T_Cancerous  = mean(G_Cancerous.v)

T_obs = T_Cancerous - T_Healthy


Permutations =  1000000
T_perm = vector(mode = "double", length = Permutations) 
Y = c(G_Healthy.v, G_Cancerous.v) 
L = length(Y)

for (i in 1:Permutations) 
{
  Ind_Healthy = sample(L, L/2) 
  Y_Healthy_perm = Y[Ind_Healthy]
  Y_Cancerous_perm = Y[-Ind_Healthy] 
  
  T_Healthy_perm = mean(Y_Healthy_perm)
  
  T_Cancerous_perm = mean(Y_Cancerous_perm)
  
  T_perm[i] = T_Cancerous_perm - T_Healthy_perm
}

p_value = length(which(T_perm >= T_obs)) / Permutations

print(paste("The p_value is ", p_value))
Data <- capture.output(print(paste("The p_value of the 2nd Method is ", p_value)))

write(Data, file = "C:/Users/Salma/Downloads/Assignement_05_txt.txt", append = TRUE)