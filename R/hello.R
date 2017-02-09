cross_validate<-function(df,dep,N,ss,sr)
{
  library("caTools")
  library("rpart")
  set.seed(ss)
  spl <- sample.split(df[,dep], SplitRatio = sr)
  #Randomly splits into two subsets 0.7 and 0.3 of the original file
  #into training and testing
  train <- subset(df, spl == TRUE)
  testing <- subset(df, spl ==FALSE)
  #We will compare two models- predictor
  indep=list()
  print("Enter the independent variables")
  i<-1
  while(i<=N){
    str=readline()
    indep[[i]]<-str
    i<-i+1
  }
  b <- paste(indep, collapse ="+")
  first.tree<-rpart(as.formula(paste(dep,b,sep="~")),data=train)
  second.tree<-rpart(as.formula(paste(dep,'.',sep="~")), data=train)
  pred1.tree<-predict(first.tree,newdata=testing, type='class')
  pred2.tree<-predict(second.tree, newdata=testing, type='class')
  mean1<-mean(pred1.tree==testing[,dep])
  mean2<-mean(pred2.tree==testing[,dep])

  return(c(mean1,mean2))

}
