kar=read.csv("car.csv")
str(kar)
install.packages("dplyr")
library("dplyr")
install.packages("ggplot2")
library("ggplot2")
kar=as.data.frame(kar)
summary("kar")
# we need to plot a graph which determine the car mpg based on various factors like weight accelaration etc

#for this we are going to plot graphs according to the various varation 

#ggplot is used for this
#all the possible plot of given data set
plot(kar,col="blue")
#this shows majorly car mpg is depends on car hp as much as it is
qplot(kar$mpg,fill=kar$hp,geom="density",alpha=I(.8)) 
#now plot btween car mpg and its accelration 
qplot(kar$mpg,fill=kar$acc)
#plot car mpg vs weight
qplot(kar$mpg,fill=kar$wt,col="green")

#as we get the dependent variable of given data set to form a good model
#in this we can determine the given dependent variable of mpg is acc wt,hp


#creating a liner regression model based on this 


#linear mode 1

#seprating car which have mpg>25 and orgin is 2
kar %>% filter(origin==2 & mpg>25) -> car_mpg_o
View(car_mpg_o)
kar %>% filter(disp>300) -> car_disp
View(car_disp)

#plot of mpg with respect to other all factors that affect this
ggplot(data = kar,aes(x=mpg)) + geom_histogram()

#splittin the data in train and test 

install.packages("caTools")
library("caTools")
sample.split(kar$mpg,SplitRatio=0.75) -> split_tag 

head(split_tag)

subset(kar,split_tag==T) -> train

subset(kar,split_tag==F) -> test 

nrow(train)
nrow(test)

lm(kar$mpg~kar$hp) -> linear_model1

predict(linear_model1,newdata = test) -> myresult1
cbind(Actual=kar$mpg,predicted=myresult1) -> finaldata1
View(finaldata1) 

as.data.frame(finaldata1) -> final_data1

(final_data1$Actual - final_data1$predicted) -> error1

cbind(final_data1,error1) -> final_data1

sqrt(mean((final_data1$error1)^2)) -> result1
# the value of result one is came out to be 3.549289


#linaer model 2
#cosidering all varaibles that affect mpg of car 

lm(kar$mpg~kar$hp + kar$acc + kar$wt) ->linea_model2
predict(linea_model2,newdata = test) -> myresult2
cbind(Actual=kar$mpg,predicted=myresult2) -> finaldata2
View(finaldata2) 

as.data.frame(finaldata2) -> final_data2

(final_data2$Actual - final_data2$predicted) ->error2

cbind(final_data2,error2) -> final_data2

sqrt(mean((final_data2$error2)^2)) -> result2
cbind(result1,result2) -> result
View(result)





# from result data set we can conclude that linaer regression model 2 is much better than model 1 as 
#given  mean sqrt error is much less in in model 2 in compare to 1
# model 2 is much accurate than model 1


write.csv(result,"result.csv",row.names = FALSE)
write.csv(finaldata1,"finaldata1.csv",row.names = FALSE)
write.csv(finaldata2,"finaldata2.csv",row.names = FALSE)
write.csv(car_disp,"car_disp.csv",row.names = FALSE)
write.csv(car_mpg_o,"car_mpg_o.csv",row.names = FALSE)
