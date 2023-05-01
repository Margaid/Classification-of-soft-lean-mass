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
#usuni?cie niepotrzebnych lub powtarzaj?cych si? kolumn
inbo=inbo[,-c(2,3,122:222)]

#brak warto?ci zamieniane na NA
inbo[inbo==""]=NA

#sprawdzenie czy jakie? wiersze si? duplikuj?
which(duplicated(inbo)==TRUE)
#?adne wiersze si? nie powtarzaj?

#sprawdzenie typu pierwszych pi?ciu zmiennych
str(inbo[,1:5])

#zmiana zmiennje id na zmienn? kategoryczna
inbo[,1]=factor(inbo[,1])
#zobaczenie ile jest ka?dego id
summary(inbo[,1], maxsum = 300)

#zmiana typu zmiennej dateofbirth na date
inbo$DateOfBirth=as.Date(parse_date_time(inbo$DateOfBirth, c("ymd", "dmy")))
#zmiana typu zmiennej data badania na datetime 
inbo$DataBadania=as.POSIXct(inbo$DataBadania, format="%Y.%m.%d. %H:%M:%S")

#sprawdzenie czy w zmiennej Sex s? inne warto?ci ni? M i F
unique(inbo$Sex)
# nie ma
#zamiana female na 0 i male na 1
inbo$Sex[inbo$Sex=="F"]=0
inbo$Sex[inbo$Sex=="M"]=1
#zmiana tpyu zmiennej na kategoryczn?
inbo$Sex=as.factor(inbo$Sex)

#Zamiana zmiennej Age na zmienn? ca?kowit?
inbo$Age=as.integer(inbo$Age)
library(wesanderson)
paleta=wes_palette(name="Darjeeling2")
#wykres s?upkowy dla zmiennej Age
hist(inbo$Age, col=paleta, main="Age")

#warto?ci wi?ksze od 21
za.stary=which(inbo$Age>21) 
inbo$id[za.stary] 
#sprawdzenie reszty danych, gdzie wiek jest za du?y
inbo[inbo$id==142,1:10]
#poprawienie pierwszej daty urodzenia, gdy? r??ni si? od pozosta?ych dw?ch
inbo[34,2]="1999-11-16"
#zmienie wieku na odpowiedni
inbo[34,3]=15

#sprawdzenie czy w reszcie danych zgadza si? wiek z dat? urodzenia i badaniem
ag=c()
for(x in 1:872){
  if(floor(as.numeric(difftime(inbo[x,"DataBadania"], inbo[x,"DateOfBirth"], units = "days"))/365.242199)!=inbo[x,"Age"]){
    ag=c(ag,x)
  }
}
inbo[ag,1:5]
#jeden wiersz, kt?ry zosta? wskazany, ma poprawny wiek

#sprawdznie czy jest przypisana ta sama p?e? do tego samego id
for (i in levels(inbo$id)){
  b=inbo$Sex[inbo$id==i]
  #zamiana, je?li p?e? nie jest wsz?dzie taka sama na t?, kt?rej jest wi?cej
  if(!(all(b==1) | all(b==0))){
    inbo$Sex[inbo$id==i]=names(which.max(table(b)))
  }
  
}

summary(inbo)
#usuni?cie kolumn, gdzie wyst?puje s?owo Impedance
imp=grep("Impedance",names(inbo),fixed=TRUE) 
inbo=inbo[,-imp]

#usuni?cie 0 i warto?ci mniejszych z wyj?tkiem kolumn, gdzie mog? wyst?powa?
kol_moga_byc_0=c(75:77)
kolumny_z_0_do_usun=setdiff(c(1:ncol(inbo)),kol_moga_byc_0)
inbo[,kolumny_z_0_do_usun][inbo[,kolumny_z_0_do_usun]<=0]=NA

#zmiana typu zmiennej Visceral Fat level na zmienna kategoryczna
inbo[,82]=factor(inbo[,82])

str(inbo, list.len=105)

#po??czenia antro i inbo
lk_antro=ncol(antro)-1
lk_inbo=ncol(inbo)-1
#liczba wierszy nowej macierzy
lw=nrow(antro)
#liczba kolumn nowej macierzy
lk=lk_antro+lk_inbo+1
#stworzenie nowej macierzy
m=matrix(NA, nrow=lw, ncol=lk, dimnames=list(ID=1:nrow(antro),
                                             Zmienne=c("ID",names(antro)[-1],names(inbo)[-1])))
#zamienie macierzy na ramk? danych
m=as.data.frame(m)
#dodanie wpis?w do kolumny id
m[,1]=row.names(m)
#zmiana typu zmiennej na date
daty=c(2,22,25)
m[,daty] = lapply(m[,daty], as.Date, origin="1899-12-30")
#przypisanie danych z antro do m
m[,2:21]=antro[,2:21]
#przypisanie id i daty badania z antro
id.data.wiersz=antro[,1:2]
#dodanie do tego wiersz odpowiadaj?cy w nowej ramce danych
id.data.wiersz=cbind(id.data.wiersz,1:nrow(antro))

#po??czenie  antro i inbo
for (j in 1:nrow(inbo))
{
  id2 = inbo[j, 1]
  data2=inbo[j, 5]
  for(i in 1:nrow(antro)){
    data = id.data.wiersz[i,2]
    if (((year(data)==year(data2)))&(((month(data)==month(data2))|(month(data)+1==month(data2)))&(id2==antro[i,1])))
    {
      wiersz=id.data.wiersz[i,3]
      m[wiersz,22:125]=inbo[j,2:105]
    } 
  }
}
#zmiana typu zmiennych
m[,1]=as.integer(m[,1]) 
m[,24]=factor(m[,24])
library(dummies)
#zamiana zmiennych kategorycznych na fikcyjne
m=dummy.data.frame(m)
#usuni?cie dw?ch kolumn, kt?re nam nie odpowiadaj?
m=m[,-c(24,26)] 
#zmiana nazwy kolumny
names(m)[24]="Sex"

#sprawdzanie korelacji mi?dzy danymi
dane.z.antro=c(3,4,5,26,29,32,35,50,70)
dane.z.inbo=c(47:49)
summary(m[,c(117:121)])

#du?a korelacja Soft lean mass z LowerLimit_FFMofRightArmNormalRange_, UpperLimit_FFMofRightArmNormalRange_,
# LowerLimit_FFMofLeftArmNormalRange_, UpperLimit_FFMofLeftArmNormalRange_, LowerLimit_FFMofTrunkNormalRange_
#UpperLimit_FFMofTrunkNormalRange_, UpperLimit_FFMofRightLegNormalRange_, UpperLimit_FFMofLeftLegNormalRange_,
#ale jest a? 599 brakuj?cych warto?ci
#TargetWeight (508 NA)
#FFMofRightLeg, FFMofLeftLeg, BMR_BasalMetabolicRate_ (398 NA)


library(corrplot)
library(PerformanceAnalytics)
M=cor(x=m[,dane.z.antro], y=m[,dane.z.inbo], 
      use="pairwise.complete.obs", method="pearson")
kolorki=colorRampPalette(c("royalblue4","white","red4"))(200)
#wykres korelacji soft lean mass i innych danych
corrplot(M, col=kolorki)
corrplot(M,method="number")
chart.Correlation(m[,c(dane.z.antro, dane.z.inbo)], hostogram=TRUE, pch=21, method="pearson")

#przypisanie wybranych danych do d
d=m[,c(3,4,5,26,29,32,35,50,70)]

#soft lean mass
slm=rep(NA, nrow(m))
#je?li miesci sie w dolnej i g?rnej granicy to 1, inaczej 0
slm[(m[,47]<m[,48])|(m[,47]>m[,49])]=0
slm[(m[,47]>=m[,48])&(m[,47]<=m[,49])]=1

#zmiana typu zmiennej na kategoryczn?
slm=factor(slm)
summary(slm)

#po??czenie slm z reszt? danych
d=cbind(d,slm)
names(d)[ncol(d)]="SoftLeanMass"



#ocena klasyfikatora
ocena<-function(macierz.bledow){
  tp=macierz.bledow[1,1]
  fp=macierz.bledow[1,2]
  fn=macierz.bledow[2,1]
  tn=macierz.bledow[2,2]
  acc=(tp+tn)/(tp+tn+fp+fn)
  sen=tp/(tp+fn)
  spe=tn/(tn+fp)
  pre=tp/(tp+fp)
  
  f1=2*pre*sen/(pre+sen)
  
  jakosc.ramka=data.frame(acc,sen,spe,pre,f1)
  return(jakosc.ramka)
}  
#normalizacja danych
normalize <- function(x) {
  return(x-min(x))/(max(x)-min(x))
}
#mieszanie danych
shuffle <- function(x) {
  return (x[sample(1:nrow(x)), ])
}

library(caTools)
#pozbycie si? na z kolumny SoftLeanMass
d2=subset(d,!is.na(d$SoftLeanMass))
summary(d2$SoftLeanMass) #sprawdzenie czy zosta?y ju? tylko 0 i 1
#gdzie s? NA
complete.cases(d2)
#usuni?cie wierszy, gdzie znajduje si? NA
d2=d2[complete.cases(d2),]
#mieszanie wierszy
d2=shuffle(d2)
#podzielenie na zbi?r testuj?cy i waliduj?cy
split=sample.split(d2$SoftLeanMass, SplitRatio=0.7)
train_set=subset(d2,split==TRUE)
test_set=subset(d2,split==FALSE)
#sprawdzenie proporcji miedzy 0 a 1
options(digits = 2)
prop.table(table(test_set[,ncol(d2)]))


#KNN
library(class)
#stworzenie macierzy i ramek danych
wyniki.knn=matrix(nrow=30, ncol=5*10)
colnames(wyniki.knn)=rep(c("acc","sen","spe","pre","f1"), length.out=5*10)
wyniki.knn=as.data.frame(wyniki.knn)
for (j in 1:10){
  srednia.knn=matrix(nrow=30,ncol=5)
  #dla liczby s?siad?w od 1 do 30  
  for (k in 1:30){
    wyn.knn=matrix(nrow=1000, ncol=5)
    wyn.knn=as.data.frame(wyn.knn)
    #powt?rzenie dla ka?dego z s?siad?w 1000 razy
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
      trains=normalize(trainX)
      tests=normalize(testX)
      
      #knn
      knn.pred=knn(trains,tests, trainY,k=k)
      wyn=table(knn.pred,testY)
      wyn.knn[i,]=ocena(wyn)
      
    }
    srednia.knn[k,]=colMeans(wyn.knn)
  }
  #zapisanie wykresu -> u?rednione wyniki dla ka?dej z pr?b
  while(names(dev.cur())!="null device")dev.off()
  png(paste(paste("knn",j,sep="_"),".png",sep=""),width=1000,height=800)
  matplot(srednia.knn, type = c("b"),pch=1,col = 1:5, xlab="warto?? k",
          ylab="?rednia dok?adno??, czu?o??, specyficzno??, precyzja, f1", main=paste("KNN ",j)) 
  legend("topleft", legend = c("acc","sen","spe","pre","f1"), col=1:5, pch=1)
  dev.off()
  
  wyniki.knn[,(5*(j-1)+1):(5*j)]=srednia.knn
  
}
#stworzenie macierzy, gdzie b?d? u?rednione wyniki ka?dej pr?by
wyniki=matrix(nrow=30,ncol=5)
colnames(wyniki)=c("acc","sen","spe","pre","f1")
wyniki=as.data.frame(wyniki)
#u?rednienie wynik?w k ?redniej ka?dej pr?by
v=1
for (i in colnames(wyniki)){
  print(i)
  for(j in 1:30){
    pom=wyniki.knn[j,(colnames(wyniki.knn)==i)]
    wyniki[j,v]=rowMeans(pom)
  }
  v=v+1
}

#zapisanie wykresu
png("KNN.png",width=1000,height=800)
#wykres pokazujacy u?rednione wyniki w zale?no?ci od liczby s?siad?W
matplot(wyniki, type = c("b"),pch=1,col = 1:5, xlab="warto?? k",
        ylab="?rednia dok?adno??, czu?o??, specyficzno??, precyzja, f1", main="KNN") 
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
    wyn.bayes[i,]=ocena(table(bayes.pred,testY))
    
    
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
