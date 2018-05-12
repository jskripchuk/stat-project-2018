x<-read.csv(file=file.choose(),sep=",")
x1<-x[,1]
y1<-x[,2]
x1<-x1[!is.na(x1)]
y1<-y1[!is.na(y1)]
n1<-length(x1)
n2<-length(y1)
CSK<-as.character(x[1,3])
DI<-as.character(x[2,3])
x1y1<-c(x1,y1)

if(CSK=="K"){
  print("Data given is proportional data of successes and failures, performing two-sided Z-test of equal proportions at a 0.05 significance level")
  s1<-sum(x1==1)
  f1<-sum(x1==0)
  s2<-sum(y1==1)
  f2<-sum(y1==0)
  if(s1 < 10 || s2 < 10 || f1 < 10 || f2 < 10)
    print("Z test of proportions aborted, counts of successes and failures not all at least 10")
  else{
    proptest<-prop.test(c(s1,s2),c(n1,n2),p=NULL,alternative="two.sided",correct = FALSE)
    if(proptest$p.value <= 0.05)
      cat("Pvalue:", proptest$p.value, "Reject null hypothesis" )
    else
      cat("Pvalue:", proptest$p.value, "Fail to reject null hypothesis")
  }
} else if (CSK == "S"){
  print("Data given for a test of spread, performing F-test")
  normx1<-shapiro.test(x1)
  normy1<-shapiro.test(y1)
  if(normx1$p.value <= 0.05 || normy1$p.value < 0.05)
    print("F-test aborted, one or more samples not normal")
  else{
    ftest<-var.test(x1,y1,alternative = "two.sided")
    if(ftest$p.value <= 0.05)
      cat("Pvalue:", ftest$p.value, "Reject the null hypothesis")
    else
      cat("Pvalue:", ftest$p.value, "Fail to reject the null hypothesis")
  }
  
}

    
