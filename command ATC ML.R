# ATC Machine learning Basic

# breast cancer

setwd("C:/Users/USER/Google Drive/04. Lain-lain/sadasa academy/Machine Learning basic")
df <- read.table("breast-cancer-wisconsin.txt", sep=",", header=F)

# step 1
####################################
# view dataset, evaluate, and modify
####################################

str(df)
summary(df)
View(df)

# step 2
####################################
# data management
####################################

colnames(df) <- (c("sample_code_number", 
                   "clump_thickness", 
                   "uniformity_of_cell_size", 
                   "uniformity_of_cell_shape", 
                   "marginal_adhesion", 
                   "single_epithelial_cell_size", 
                   "bare_nuclei", 
                   "bland_chromatin", 
                   "normal_nucleoli", 
                   "mitosis", 
                   "classes"))

str(df)
summary(df)
View(df)

#df$bare_nuclei <- as.numeric(as.character(df$bare_nuclei))
df$bare_nuclei <- as.numeric(df$bare_nuclei)

unique(df$classes)
table(df$classes)

df$cl <- ifelse(df$classes == 2, "benign",
                ifelse(df$classes == 4, "malignant", NA))


str(df)
df$cl <- as.factor(df$cl)
summary(df)
head(df)

# ready?

# step 3
##################
# identify and handling missing data
##################

library(mice)
md.pattern(df)
summary(df)

missingData <- df[is.na(df$bare_nuclei),]


# omitting NA
library(tidyverse)
df1 <- df %>%
  drop_na() %>%
  select(cl, everything(), -sample_code_number, -classes)

#write.csv(df1, "databaru.csv")
getwd()

str(df)
str(df1)

# step 4
######################
# descriptive analysis
######################

barplot(table(df1$cl))

table(df1$cl)

library(ggplot2)
ggplot(df1, aes(x = cl, fill = cl)) +
  geom_bar()

# visualize using area plot
str(df1)
c(1, 2, 3,4)
c(1:10)

a <- gather(df1, x, y, clump_thickness:mitosis)
View(a)

gather(df1, x, y, clump_thickness:mitosis) %>%
  ggplot(aes(x = y, color = cl, fill = cl)) +
  geom_density(alpha = 0.3) +
  facet_wrap( ~ x, scales = "free", ncol = 3)

# MACHINE LEARNING STEP 1
###################
# MODEL DEVELOPMENT : DECISION TREE -> CLASSIFICATION MODEL
###################
#install.packages(rpart)
set.seed(213546) 

library(rpart)

# formula = y~x1+x2+x3
# formula = y~.

mod <- rpart(cl~., data=df1)
printcp(mod)
plotcp(mod)
plot(mod)
plot(mod, uniform=T)
text(mod,cex=1,digits=3,font=2)
summary(mod)

mod1 <- rpart(cl~., data=df1, cp=0.018)
printcp(mod1)
plot(mod1)
plotcp(mod1)
plot(mod1, uniform=T)
text(mod1,cex=1,digits=3,font=2)

?rpart

library(rpart.plot)
rpart.plot(mod1)

#rm(list=ls())
##################
# regression model
##################
library(partykit)
df2 <- df1
str(df2)
df2$pred <- predict(as.party(mod1),type="node")	

table(df2$pred)
table(df2$pred, df2$cl)
addmargins(table(df2$pred))

View(df2)

mod3 <- glm(family=binomial,data=df2,cl~factor(pred)) 
summary(mod3)

library(epiDisplay)
lroc(mod3)
lroc(mod3)$auc

#######################
# data splitting
#######################

set.seed(1234)
library(caret)

index <- createDataPartition(df1$cl, p = 0.75, list = FALSE) # tergantung proporsi yang akan digunakan
train_data <- df1[index, ]
test_data  <- df1[-index, ]

# visualize trainig dan testing set
bind_rows(data.frame(group = "train", train_data),
          data.frame(group = "test", test_data)) %>%
  gather(x, y, clump_thickness:mitosis) %>%
  ggplot(aes(x = y, color = group, fill = group)) +
  geom_density(alpha = 0.3) +
  facet_wrap( ~ x, scales = "free", ncol = 3)

##################
# prediction model
##################
prob <- predict(mod1,test_data,type="prob")  # node
pred <- ifelse(prob[,2]> 0.5,1, 0)
tab <- table(pred,test_data$cl)
addmargins(tab)
acc <- 100*sum(diag(tab))/sum(tab)
acc

################
# visualize pred model
################

test_data$cln <- ifelse(test_data$cl=="benign", 0,1)
xCoord <- prob[,2]
yCoord <- as.integer(as.character(test_data$cln))
plot(xCoord,yCoord,xlab="x coordinate",ylab="",ylim=c(-0.1,1.1),xlim=c(0,0.8), las=1)
mtext(side=3,adj=-0.1,line=0.2,"Breast Cancer")
mtext(side=3,adj=0.95,line=0.2,"699 observation")
mtext(side=3,adj=1.07,line=0.2,"Total",font=3)
mtext(side=3,adj=0.5,line=1.4,"Decision Tree",font=2)
testSum <- table(test_data$cln)
axis(side=4,at=c(0.2,0.8),lab=testSum, las=1)
abline(v=0.5,col=2,lwd=2)
text(c(0.2,0.8,0.2,0.8),c(0.2,0.2,0.8,0.8),tab)
lg <- paste("Accuracy:",round(acc,2),"%")
legend("right",inset=0.02,leg=lg,x.intersp=0.2,bg="ivory")
col <- as.integer(as.character(test_data$cln))				#97.06


#################3
table(df$pred)
