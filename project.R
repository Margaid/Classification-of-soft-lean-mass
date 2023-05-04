setwd("C:/Users/olaki/Documents/inżynieria biomedyczna - projekt")
#ANTRO
#loading data
antro=read.csv("dane_antropo_13.csv")

#glimpse of data

#columns' names
names(antro)

#variables' type
str(antro)

#deleting one column with the date of measurement (data.badania), because data is duplicated
antro=antro[,-c(2)]

#checking whether the rest of data is duplicated
dup=which(duplicated(antro)==TRUE)
#deleting duplicated rows
antro=antro[-dup,]

#changing type of variable id to categorical
antro[,1]=factor(antro[,1])
summary(antro[,1], maxsum=300)

#changing type of variable DataBadania from char to date using lubridate
library(lubridate)
antro$DataBadania=as.Date(parse_date_time(antro$DataBadania, c("ymd", "dmy")))

# replacing values equal to 0 or no values with NA
antro[antro==0]=NA
antro[antro==""]=NA

summary(antro)

#the lowest height is too small - only 17.2
which(grepl(17.2, antro$Wysokosc))
antro[718,] #checking the rest of data for this row
#replacing height with 172
antro$Wysokosc[antro$Wysokosc==17.2]=172

#inhalation and exhaust circuit
for (x in 1:length(antro)){
  if(!is.na(antro[x,6])|!is.na(antro[x,7])){
    # checking data on whether the inhalation circuit is less or equal than the exhaust circuit
    #if the exhaust circuit is greater -> variables swap places
    if(antro[x,6]>antro[x,7]){
      zam=antro[x,6]
      antro[x,6]=antro[x,7]
      antro[x,7]=zam
    }
    #checking whether the inhalation circuit and the exhaust circuit are the same  
    #mean differ chest circumference between exhale and inhale is from 3,5 to 6 cm
    #if values are the same -> adding 4 cm to inhalation circuit 
    if (antro[x,6]==antro[x,7]){
      antro[x,7]=antro[x,7]+4
    }
    # checking whether chest mobility is above 12 cm because some sources tell
    # max mobility is up to 12 cm
    if(antro[x,7]-antro[x,6]>12 ){
      #if the answer is yes -> inhalation circuit = exhale circuit + 10
      antro[x,7]=antro[x,6]+10
    }
    #checking whether chest mobility is under 1 cm
    if(antro[x,7]-antro[x,6]<1){
      #if yes -> inhalation circuit = exhale circuit + 2
      antro[x,7]=antro[x,6]+2
    }
  }
}

#diastolic and systolic pressure
for(x2 in 1:length(antro)){
  if(!is.na(antro[x2,19])&!is.na(antro[x2,20])){
    # checking whether diastolic pressure is higher than systolic
    # if yes-> they swap places
    if(antro[x2,19]<antro[x2,20]){
      zam2=antro[x2,19]
      antro[x2,19]=antro[x2,20]
      antro[x2,20]=antro[x2,19]
    }
  }
}

summary(antro)
# max sitting height is too tall (175.5), so I'm checking rest of the data
max_sitting_height=which(grepl(175.5, antro$WysokoscSiedzeniowa))
antro[max_sitting_height,]
#changing sitting height to 75.5, because it's 54% of height -> which is the correct proportion 
#everything around 50% is correct
antro[max_sitting_height,]$WysokoscSiedzeniowa=75.5

summary(antro)
max_siiting_height_2=which(grepl(101.40, antro$WysokoscSiedzeniowa))
row_max_sitting_height_2=antro[max_siiting_height_2,]
print(paste("sitting height/height =",round(row_max_sitting_height_2$WysokoscSiedzeniowa/row_max_sitting_height_2$Wysokosc*100,2),"%"))
#so this max sitting height is correct

#rest of the data - outliers
#variables for correcting
detecting.outliers=c(8,9,10,12:15,17,19:21)
#matrix of scatterplots for variables not in detecting_outliers
pairs(~.,antro[,-detecting.outliers], col="red")
#matrix of scatterplots for variables in detecting_outliers
pairs(~.,antro[,detecting.outliers], col="purple")
#boxplot for detecting_outliers
boxplot(antro[,detecting.outliers])

#searching for limit values
a=.1
range=qnorm(1-a/2)

#changing extreme values to NA
for (i in detecting.outliers){
  values.out=boxplot(antro[,i],plot=F,range=range)$out
  antro[is.element(antro[,i],values.out),i]=NA
  
}

boxplot(antro[detecting.outliers],range=range,las=2)
########################################################
########################################################
########################################################
#INBODY
#reading data
inbo=read.csv("dane_inbody.csv")

#glimpse of the data
#deleting unnecessary or duplicated columns 
inbo=inbo[,-c(2,3,122:222)]

#changing missing values to NA
inbo[inbo==""]=NA

#looking for duplicated rows
which(duplicated(inbo)==TRUE)
#there are no duplicated rows

#type of first 5 variables
str(inbo[,1:5])

#changing type of id to categorical
inbo[,1]=factor(inbo[,1])
summary(inbo[,1], maxsum = 300)

#changing type of DateOfBirth to date
inbo$DateOfBirth=as.Date(parse_date_time(inbo$DateOfBirth, c("ymd", "dmy")))
#changing type of DataBadania to datetime 
inbo$DataBadania=as.POSIXct(inbo$DataBadania, format="%Y.%m.%d. %H:%M:%S")

#checking unique vlaues in variable Sex
unique(inbo$Sex)
# there are only F and M, so I'm changing this to 0 and 1, respectively
inbo$Sex[inbo$Sex=="F"]=0
inbo$Sex[inbo$Sex=="M"]=1
#changing type of Sex to categorical
inbo$Sex=as.factor(inbo$Sex)

#changing type of Age to integer
inbo$Age=as.integer(inbo$Age)

library(wesanderson)
paleta=wes_palette(name="Darjeeling2")
#histogram of Age
hist(inbo$Age, col=paleta, main="Histogram of Age")

#finding values greater than 21 in Age
too.old=which(inbo$Age>21) 
id.too.old=inbo$id[too.old] 
#checking rest of the data
inbo[inbo$id %in% id.too.old, 1:10]
#correcting the first date, because it differs from the rest for this id
inbo[34,2]="1999-11-16"
#now I'm correcting the Age
inbo[34,3]=15

#checking whether the age is correct with date of examination and date of birth
ag=c()
for(x in 1:length(inbo)){
  if(floor(as.numeric(difftime(inbo[x,"DataBadania"], inbo[x,"DateOfBirth"], units = "days"))/365.242199)!=inbo[x,"Age"]){
    ag=c(ag,x)
  }
}
if(is.null(ag)){
  print("age is correct in every row")
} else{
  inbo[ag,1:5]
}


#checking if everywhere is the same gender for one id
for (i in levels(inbo$id)){
  b=inbo$Sex[inbo$id==i]
  #changing sex to the one of which there are more
  if(!(all(b==1) | all(b==0))){
    inbo$Sex[inbo$id==i]=names(which.max(table(b)))
  }
}

summary(inbo)
#deleting columns with word Impedance
imp=grep("Impedance",names(inbo),fixed=TRUE) 
inbo=inbo[,-imp]

#deleting values less or equal to 0, besides columns where these values can appear
col.with.0=c(75:77)
col.with.0.to.delete=setdiff(c(1:ncol(inbo)),col.with.0)
inbo[,col.with.0.to.delete][inbo[,col.with.0.to.delete]<=0]=NA


str(inbo, list.len=length(inbo))

#changing type of Visceral Fat level to categorical
inbo[,82]=factor(inbo[,82])

#number of na in every column
colSums(is.na(inbo))
#I'm leaving missing values in df



#merging antro and inbo datasets

#deleting id and date of measurement in inbo
inbo.for.new.matrix=inbo[-c(1,5)]

num.col.antro=ncol(antro)-1
num.col.inbo=ncol(inbo.for.new.matrix)
#number of rows new matrix
num.rows=nrow(antro)
#number of columns new matrix
num.cols=num.col.antro+num.col.inbo+1

#creating new matrix
m=matrix(NA, nrow=num.rows, ncol=num.cols, dimnames=list(ID=1:nrow(antro),
                                             colnames=c("ID",names(antro)[-1],names(inbo.for.new.matrix))))

#creating data frame from matrix
m=as.data.frame(m)
#column filling -> id
m[,1]=row.names(m)
#changing type of column, where are dates
dates=c(2,22)
m[,dates] = lapply(m[,dates], as.Date, origin="1899-12-30")
#values from antro
m[,2:21]=antro[,2:21]

#id and date of measurement from antro (to correctly merge antro and inbo)
id.date.row=antro[,1:2]
#adding id from new df corresponding to their id from antro
id.date.row=cbind(id.date.row,1:nrow(antro))

#merging according to date of measurement
for (j in 1:nrow(inbo))
{
  id2 = inbo[j, 1]
  date2=inbo[j, 5]
  for(i in 1:nrow(antro)){
    date = id.date.row[i,2]
    #sometimes the second measurement was a month later, so months are equal or one month difference
    if (((year(date)==year(date2)))&(((month(date)==month(date2))|(month(date)+1==month(date2)))&(id2==antro[i,1])))
    {
      wiersz=id.date.row[i,3]
      m[wiersz,22:124]=inbo.for.new.matrix[j,]
    } 
  }
}


#changing type of variables
m[,1]=as.integer(m[,1]) 

m[,"Sex"]=ifelse(m$Sex == "2", 1, 0)
m[,"Sex"]=factor(m[,"Sex"])


#linear correlation 

chosen.variables=c(3,4,5,25,28,31,34,49,69)
soft.lean.mass=c(46:48)

#strong correlation between Soft lean mass and LowerLimit_FFMofRightArmNormalRange_, UpperLimit_FFMofRightArmNormalRange_,
# LowerLimit_FFMofLeftArmNormalRange_, UpperLimit_FFMofLeftArmNormalRange_, LowerLimit_FFMofTrunkNormalRange_
#UpperLimit_FFMofTrunkNormalRange_, UpperLimit_FFMofRightLegNormalRange_, UpperLimit_FFMofLeftLegNormalRange_,
#but there are 599 missing values, so I'm not using this variables
#TargetWeight (508 NA)
#FFMofRightLeg, FFMofLeftLeg, BMR_BasalMetabolicRate_ (398 NA)


library(corrplot)
library(PerformanceAnalytics)
M=cor(x=m[,chosen.variables], y=m[,soft.lean.mass], 
      use="pairwise.complete.obs", method="pearson")
#kolorki=colorRampPalette(c("royalblue4","white","red4"))(200)
#corrplot(M, col=kolorki)
corrplot(M,method="number")
chart.Correlation(m[,c(chosen.variables, soft.lean.mass)], hostogram=TRUE, pch=21, method="pearson")


#creating df with chosen variables
d=m[,chosen.variables]

#soft lean mass
slm=rep(NA, nrow(m))
#creating variable for predicting
#if soft lean mass is between normal range -> 1, else 0
slm[(m[,46]<m[,47])|(m[,46]>m[,48])]=0
slm[(m[,46]>=m[,47])&(m[,46]<=m[,48])]=1

#changing type to categorical
slm=factor(slm)
summary(slm)
#checking if class is balanced
options(digits = 2)
prop.table(table(slm))
#mild degree of imbalanced data

#joining slm with rest of the data
d=cbind(d,slm)
names(d)[ncol(d)]="SoftLeanMass"


#classification evaluation
evaluation<-function(confusion.matrix){
  tp=confusion.matrix[1,1]
  fp=confusion.matrix[1,2]
  fn=confusion.matrix[2,1]
  tn=confusion.matrix[2,2]
  acc=(tp+tn)/(tp+tn+fp+fn)
  sen=tp/(tp+fn)
  spe=tn/(tn+fp)
  pre=tp/(tp+fp)
  
  f1=2*pre*sen/(pre+sen)
  
  df=data.frame(acc,sen,spe,pre,f1)
  return(df)
}  

#data normalization
normalize <- function(x) {
  return(x-min(x))/(max(x)-min(x))
}
#shuffling data
shuffle <- function(x) {
  return (x[sample(1:nrow(x)), ])
}

library(caTools)
#dealing with missing values
#deleting missing values from d
d2=subset(d,!is.na(d$SoftLeanMass))
summary(d2$SoftLeanMass) 
#deleting rows with missing values
d2=d2[complete.cases(d2),]

split=sample.split(d2$SoftLeanMass, SplitRatio=0.7)


#KNN
library(class)
results.knn=matrix(nrow=30, ncol=5*10)
colnames(results.knn)=rep(c("acc","sen","spe","pre","f1"), length.out=5*10)
results.knn=as.data.frame(results.knn)
for (j in 1:10){
  mean.knn=matrix(nrow=30,ncol=5)
  #knn neighbors from 1 to 30
  for (k in 1:30){
    res.knn=matrix(nrow=1000, ncol=5)
    res.knn=as.data.frame(res.knn)
    #repeating 1000 times for every number of neighbor
    for (i in 1:1000){
      d2=shuffle(d2)
      #splitting data
      train_set=subset(d2,split==TRUE)
      test_set=subset(d2,split==FALSE)
      trainX=train_set[,-ncol(d2)]
      testX=test_set[,-ncol(d2)]
      trainY=train_set[,ncol(d2)]
      testY=test_set[,ncol(d2)]
      trains=normalize(trainX)
      tests=normalize(testX)
      
      #knn
      knn.pred=knn(trains,tests, trainY,k=k)
      res=table(knn.pred,testY)
      res.knn[i,]=evaluation(res)
      
    }
    mean.knn[k,]=colMeans(res.knn)
  }
  #saving plot -> mean results for each trial
  while(names(dev.cur())!="null device")dev.off()
  png(paste(paste("knn",j,sep="_"),".png",sep=""),width=1000,height=800)
  matplot(mean.knn, type = c("b"),pch=1,col = 1:5, xlab="number of neighbors",
          ylab="accuracy, sensitivity, specificity, precision, f1", main=paste("KNN ",j)) 
  legend("topleft", legend = c("acc","sen","spe","pre","f1"), col=1:5, pch=1)
  dev.off()
  
  results.knn[,(5*(j-1)+1):(5*j)]=mean.knn
  
}

#creating dataframe for mean results from every trial
mean.results.knn=matrix(nrow=30,ncol=5)
colnames(mean.results.knn)=c("acc","sen","spe","pre","f1")
mean.results.knn=as.data.frame(mean.results.knn)
#u?rednienie wynik?w k ?redniej ka?dej pr?by
v=1
for (i in colnames(mean.results.knn)){
  for(j in 1:30){
    pom=results.knn[j,(colnames(results.knn)==i)]
    mean.results.knn[j,v]=rowMeans(pom)
  }
  v=v+1
}

#zapisanie wykresu
png("KNN.png",width=1000,height=800)
#wykres pokazujacy u?rednione wyniki w zale?no?ci od liczby s?siad?W
matplot(mean.results.knn, type = c("b"),pch=1,col = 1:5, xlab="warto?? k",
        ylab="mean accuracy, sensitivity, specificity, precision, f1", main="KNN") 
legend("topleft", legend = c("acc","sen","spe","pre","f1"), col=1:5, pch=1)
dev.off()


#naive bayes
library(naivebayes)

#stworzenie macierzy i ramek danych
wyniki.bayes=matrix(nrow=10, ncol=5)
colnames(wyniki.bayes)=rep(c("acc","sen","spe","pre","f1"), length.out=5)

#j ->liczba pr?b
for (j in 1:10){
  wyn.bayes=matrix(nrow=1000, ncol=5)
  wyn.bayes=as.data.frame(wyn.bayes)
  #powt?rzenie dla ka?dej pr?by 1000 razy
  for (i in 1:1000){
    #mieszanie wierszy
    d2=shuffle(d2)
    #podzia? na zbi?r trenuj?cy i testujacy 
    train_set=subset(d2,split==TRUE)
    test_set=subset(d2,split==FALSE)
    trainX=train_set[,-ncol(d2)]
    testX=test_set[,-ncol(d2)]
    trainY=train_set[,ncol(d2)]
    testY=test_set[,ncol(d2)]
    
    #naive bayes
    model=naive_bayes(SoftLeanMass ~ ., data = train_set) 
    bayes.pred=predict(model, testX)
    wyn.bayes[i,]=evaluation(table(bayes.pred,testY))
    
    
  }  
  #u?rednione wyniki ka?dej pr?by
  wyniki.bayes[j,]=colMeans(wyn.bayes)
  
  #zapisanie wykresu -> u?rednione wyniki dla ka?dej z pr?b
  while(names(dev.cur())!="null device")dev.off()
  png(paste(paste("naiveBayes",j,sep="_"),".png",sep=""),width=1000,height=800)
  kol=c("slategray2", "seashell2","lightpink2","wheat2","cornflowerblue")
  barplot(wyniki.bayes[j,], col=kol, main=paste("Naive Bayes",j,sep=" ") , 
          names=c("dokladno??","czu?o??","specyficzno??","precyzja","F1"), ylim=c(0,1))
  dev.off()
  
}
#u?rednienie wszystkich wynik?w
wyniki2=colMeans(wyniki.bayes)
#zapisanie wynik?w do pliku
png(paste("naiveBayes",".png",sep=""),width=1000,height=800)
kol=c("slategray2", "seashell2","lightpink2","wheat2","cornflowerblue")
barplot(wyniki2, col=kol, main="Naive Bayes" , 
        names=c("dokladno??","czu?o??","specyficzno??","precyzja","F1"), ylim=c(0,1))
dev.off()

#wy?wietlenie wynik?w
options(digits=4)
wyniki
wyniki2
