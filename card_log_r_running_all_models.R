## import data
list_of_models <- 
  read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Models/not PCA/list_of_models_to_run_for_r_27aug2015.csv", 
           stringsAsFactors=FALSE)
list_of_uni_full = 
  read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Models/not PCA/list_of_models_to_run_uni_full_27aug2015.csv",
           stringsAsFactors=FALSE)

card_data <- 
  read.csv("C:/Users/Kaiya/Dropbox/Docs for Brian/THESIS/Field Work/Cardinal Data Analysis/Master Files/CSVs/card_data_analysis_csv_7july2015.csv")
card_data_models = card_data[,c(1:8,12,18,20,24,27,31,34,38,78,81,83,86)]
colnames(card_data_models)=c(colnames(card_data_models[1:9]),"Chips.Resp",colnames(card_data_models[11:20]))
names(card_data_models)

names(list_of_models)
attach(card_data_models)

library(AICcmodavg)

## refactor type and song
card_data_models$Type = as.factor(card_data_models$Type)
levels(card_data_models$Type) = c("Cactus.Wren","Texas","Bill.Williams","Portal")
print(levels(card_data_models$Type))

card_data_models$Song = as.factor(card_data_models$Song)
levels(card_data_models$Song) = c("Cactus.Wren",
                           "Texas.1","Texas.2","Texas.4","Texas.5","Texas.10","Texas.11","Texas.12",
                           "Bill.Williams.1","Bill.Williams.2","Bill.Williams.4","Bill.Williams.5","Bill.Williams.6",
                           "Portal.4","Portal.14","Portal.21","Portal.23","Portal.24")
print(levels(card_data_models$Song))


# WORK WITH EVERY SINGLE MODEL
## column as own list
head(list_of_models$Chips.Resp[1])
glm(list_of_models$Chips.Resp[1],data=card_data_models)

# loop of model AICc - note, Chips Resp is NOT a binary, it is currently 0,1,2
N = 5
len = length(list_of_models$Chips.Resp)
model.matrix = as.data.frame(matrix(0,ncol=N,nrow=len))
for (i in 1:len) {
  x = c(AICc(glm(list_of_models$Chips.Resp[i],data=card_data_models,family="binomial")),
        AICc(glm(list_of_models$Close.Resp.As.Number[i],data=card_data_models)),
        AICc(glm(list_of_models$Flyby.Resp[i],data=card_data_models)),
        AICc(glm(list_of_models$Songs.Less.24.Resp[i],data=card_data_models)),
        AICc(glm(list_of_models$Songs.24.Plus.Resp[i],data=card_data_models)))
  model.matrix[i,] = as.numeric(x)
}
colnames(model.matrix)=colnames(list_of_models[1:5])
model.matrix = cbind(Model = list_of_models$Add.Terms.Only,model.matrix)

write.csv(model.matrix,file="model_matrix_allmodels_aicc_mar62016.csv")

# WORK WITH ONLY UNIVARIATES AND FULL, PRE FIRST
N = 11
col=5
pre.matrix = as.data.frame(matrix(0,ncol=col,nrow=N))
x = rep(NA,col)
# for loop, with if statements
for (i in 1:N) {
  # stuff for loop without if statements
  #  x = c(AICc(glm(list_of_uni_full$Chips.Pre[i],data=card_data_models,family="binomial")),
  #        AICc(glm(list_of_uni_full$Close.Pre.As.Number[i],data=card_data_models)),
  #        AICc(glm(list_of_uni_full$Flyby.Pre[i],data=card_data_models)),
  #        AICc(glm(list_of_uni_full$Songs.Less.24.Pre[i],data=card_data_models)),
  #        AICc(glm(list_of_uni_full$Songs.24.Plus.Pre[i],data=card_data_models)))
  if (list_of_uni_full$Chips.Pre[i]==1) { ## if statement for intercept only vs not for each variable
    x[1] = AICc(glm(Chips.Pre~1,data=card_data_models,family="binomial"))
  }   else {
    x[1] = AICc(glm(list_of_uni_full$Chips.Pre[i],data=card_data_models,family="binomial"))
  }
  if (list_of_uni_full$Close.Pre.As.Number[i]==1) { ## if statement for intercept only vs not for each variable
    x[2] = AICc(glm(Close.Pre.As.Number~1,data=card_data_models))
  }  else {
    x[2] = AICc(glm(list_of_uni_full$Close.Pre.As.Number[i],data=card_data_models))
  }
  if (list_of_uni_full$Flyby.Pre[i]==1) { ## if statement for intercept only vs not for each variable
    x[3] = AICc(glm(Flyby.Pre~1,data=card_data_models))
  }  else {
    x[3] = AICc(glm(list_of_uni_full$Flyby.Pre[i],data=card_data_models))
  }  
  if (list_of_uni_full$Songs.Less.24.Pre[i]==1) { ## if statement for intercept only vs not for each variable
    x[4] = AICc(glm(Songs.Less.24.Pre~1,data=card_data_models))
  }  else {
    x[4] = AICc(glm(list_of_uni_full$Songs.Less.24.Pre[i],data=card_data_models))
  }  
  if (list_of_uni_full$Songs.24.Plus.Pre[i]==1) { ## if statement for intercept only vs not for each variable
    x[5] = AICc(glm(Songs.24.Plus.Pre~1,data=card_data_models))
  }  else {
    x[5] = AICc(glm(list_of_uni_full$Songs.24.Plus.Pre[i],data=card_data_models))
  }
  pre.matrix[i,] = as.numeric(x)
} 
colnames(pre.matrix)=colnames(list_of_uni_full[2:6])
pre.matrix = cbind(Pre.List = list_of_uni_full$Pre.List[1:N],pre.matrix)

write.csv(pre.matrix,file="model_matrix_unifull_pre_aicc_mar62016.csv")

# work with only univariates and full, resp
N = 17
col=5
resp.matrix = as.data.frame(matrix(0,ncol=col,nrow=N))
x = rep(NA,col)
# for loop, with if statements
for (i in 1:N) {
  # stuff for loop without if statements
  #  x = c(AICc(glm(list_of_uni_full$Chips.Resp[i],data=card_data_models,family="binomial")),
  #        AICc(glm(list_of_uni_full$Close.Resp.As.Number[i],data=card_data_models)),
  #        AICc(glm(list_of_uni_full$Flyby.Resp[i],data=card_data_models)),
  #        AICc(glm(list_of_uni_full$Songs.Less.24.Resp[i],data=card_data_models)),
  #        AICc(glm(list_of_uni_full$Songs.24.Plus.Resp[i],data=card_data_models)))
  if (list_of_uni_full$Chips.Resp[i]==1) { ## if statement for intercept only vs not for each variable
    x[1] = AICc(glm(Chips.Resp~1,data=card_data_models,family="binomial"))
#    x[1] = NA
  }   else {
    x[1] = AICc(glm(list_of_uni_full$Chips.Resp[i],data=card_data_models,family="binomial"))
  }
  if (list_of_uni_full$Close.Resp.As.Number[i]==1) { ## if statement for intercept only vs not for each variable
    x[2] = AICc(glm(Close.Resp.As.Number~1,data=card_data_models))
#    x[2] = NA
  }  else {
    x[2] = AICc(glm(list_of_uni_full$Close.Resp.As.Number[i],data=card_data_models))
  }
  if (list_of_uni_full$Flyby.Resp[i]==1) { ## if statement for intercept only vs not for each variable
    x[3] = AICc(glm(Flyby.Resp~1,data=card_data_models))
#    x[3] = NA
  }  else {
    x[3] = AICc(glm(list_of_uni_full$Flyby.Resp[i],data=card_data_models))
  }  
  if (list_of_uni_full$Songs.Less.24.Resp[i]==1) { ## if statement for intercept only vs not for each variable
    x[4] = AICc(glm(Songs.Less.24.Resp~1,data=card_data_models))
#    x[4] = NA
  }  else {
    x[4] = AICc(glm(list_of_uni_full$Songs.Less.24.Resp[i],data=card_data_models))
  }  
  if (list_of_uni_full$Songs.24.Plus.Resp[i]==1) { ## if statement for intercept only vs not for each variable
    x[5] = AICc(glm(Songs.24.Plus.Resp~1,data=card_data_models))
#    x[5] = NA
  }  else {
    x[5] = AICc(glm(list_of_uni_full$Songs.24.Plus.Resp[i],data=card_data_models))
  }
  resp.matrix[i,] = as.numeric(x)
} 
colnames(resp.matrix)=colnames(list_of_uni_full[8:12])
resp.matrix = cbind(Resp.List = list_of_uni_full$Resp.List[1:N],resp.matrix)

write.csv(resp.matrix,file="model_matrix_unifull_resp_aicc_mar62016.csv")

## weighted AICc values matrix
library(qpcR)
weights.resp.matrix = as.data.frame(matrix(0,ncol=5,nrow=17))
colnames(weights.resp.matrix)=colnames(list_of_uni_full[8:12])
weights.resp.matrix[,1]=as.numeric(akaike.weights(resp.matrix$Chips.Resp)$weights)
weights.resp.matrix[,2]=as.numeric(akaike.weights(resp.matrix$Close.Resp.As.Number)$weights)
weights.resp.matrix[,3]=as.numeric(akaike.weights(resp.matrix$Flyby.Resp)$weights)
weights.resp.matrix[,4]=as.numeric(akaike.weights(resp.matrix$Songs.Less.24.Resp)$weights)
weights.resp.matrix[,5]=as.numeric(akaike.weights(resp.matrix$Songs.24.Plus.Resp)$weights)
weights.resp.matrix = cbind(Resp.List = list_of_uni_full$Resp.List,weights.resp.matrix)

write.csv(weights.resp.matrix,file="model_matrix_unifull_resp_weights_mar62016.csv")

## delta AICc values matrix
deltaAIC.resp.matrix = as.data.frame(matrix(0,ncol=5,nrow=length(resp.matrix$Close.Resp.As.Number)))
colnames(deltaAIC.resp.matrix)=colnames(list_of_uni_full[8:12])
deltaAIC.resp.matrix[,1]=as.numeric(akaike.weights(resp.matrix$Chips.Resp)$deltaAIC)
deltaAIC.resp.matrix[,2]=as.numeric(akaike.weights(resp.matrix$Close.Resp.As.Number)$deltaAIC)
deltaAIC.resp.matrix[,3]=as.numeric(akaike.weights(resp.matrix$Flyby.Resp)$deltaAIC)
deltaAIC.resp.matrix[,4]=as.numeric(akaike.weights(resp.matrix$Songs.Less.24.Resp)$deltaAIC)
deltaAIC.resp.matrix[,5]=as.numeric(akaike.weights(resp.matrix$Songs.24.Plus.Resp)$deltaAIC)
deltaAIC.resp.matrix = cbind(Resp.List = list_of_uni_full$Resp.List,deltaAIC.resp.matrix)

write.csv(deltaAIC.resp.matrix,file="model_matrix_unifull_resp_deltaAIC_mar62016.csv")

## relatively likelihoods values matrix
rel.LL.resp.matrix = as.data.frame(matrix(0,ncol=5,nrow=length(resp.matrix$Close.Resp.As.Number)))
colnames(rel.LL.resp.matrix)=colnames(list_of_uni_full[8:12])
rel.LL.resp.matrix[,1]=as.numeric(akaike.weights(resp.matrix$Chips.Resp)$rel.LL)
rel.LL.resp.matrix[,2]=as.numeric(akaike.weights(resp.matrix$Close.Resp.As.Number)$rel.LL)
rel.LL.resp.matrix[,3]=as.numeric(akaike.weights(resp.matrix$Flyby.Resp)$rel.LL)
rel.LL.resp.matrix[,4]=as.numeric(akaike.weights(resp.matrix$Songs.Less.24.Resp)$rel.LL)
rel.LL.resp.matrix[,5]=as.numeric(akaike.weights(resp.matrix$Songs.24.Plus.Resp)$rel.LL)
rel.LL.resp.matrix = cbind(Resp.List = list_of_uni_full$Resp.List,rel.LL.resp.matrix)

write.csv(rel.LL.resp.matrix,file="model_matrix_unifull_resp_likelihood_mar62016.csv")