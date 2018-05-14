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


if(CSK == 'C')
{
  if(DI == 'I')
  {
    #size greater than 40
    if(n1 <= 40 || n2 <= 40)
    {
      test1<-shapiro.test(x1)
      test2<-shapiro.test(y1)
      
      #is normal?
      if(test1$p.value <= .05 || test2$p.value <= .05)
      {
        print("test failed because data are not normal")
        exit()
      }
      
      Q1x<-quantile(x1,.25, type = 6)
      Q3x<-quantile(x1,.75, type = 6)
      lowerOutlierx<-(Q1x - (1.5*IQR(x1, type=6)))
      upperOutlierx<-(Q3x + (1.5*IQR(x1, type=6)))
      LXoutliers<-x1<lowerOutlierx
      UXoutliers<-x1>upperOutlierx
      
      Q1y<-quantile(y1,.25, type = 6)
      Q3y<-quantile(y1,.75, type = 6)
      lowerOutliery<-(Q1y - (1.5*IQR(y1, type=6)))
      upperOutliery<-(Q3y + (1.5*IQR(y1, type=6)))
      LYoutliers<-y1<lowerOutliery
      UYoutliers<-y1>upperOutliery
      
      #outliers?
      if(LXoutliers == TRUE || UXoutliers == TRUE || LYoutliers == TRUE || UYoutliers == TRUE)
      {
        print("test failed because outliers exist")
        exit()
      }
    }
    
    #checks stdev ratio
    large<-max(sd(x1),sd(y1))
    small<-min(sd(x1),sd(y1))
    check<-large/small
    
    if(check > 2)
    {
      #pooled
      test3<-t.test(x1, y1,
                    alternative = c("two.sided"), paired = FALSE, var.equal = TRUE,
                    conf.level = 0.95)
      
      if(test3$p.value < .05)
      {
        print("We are testing the difference of means with a pooled two sample t-test")
        print(paste("We reject the null since the p value is ",test3$p.value))
      }
      else
      {
        print("We are testing the difference of means with a pooled two sample t-test")
        print(paste("We fail to reject the null since the p value is ",test3$p.value))
      }
      
    }
    else
    {
      #t test
      test3<-t.test(x1, y1,
                    alternative = c("two.sided"), paired = FALSE, var.equal = FALSE,
                    conf.level = 0.95)
      
      if(test3$p.value < .05)
      {
        print("We are testing the difference of means with a two sample t-test")
        print(paste("We reject the null since the p value is ",test3$p.value))
      }
      else
      {
        print("We are testing the difference of means with a two sample t-test")
        print(paste("We fail to reject the null since the p value is ",test3$p.value))
      }
    }
  }
  else if(DI == 'D')
  {
    z<-x1-y1
    testd<-shapiro.test(z)
    if(testd$p.value <= .05)
    {
      #not normal
      signs<-c()
      pos<-c()
      z<-x1-y1
      for (i in 1:length(z)) 
        { 
        #counts successes/positive values
        if(z[i] > 0)
        {
          pos<-c(pos,z[i])
        }
        #counts none 0 differences
        if(z[i] > 0 || z[i] < 0)
        {
          signs<-c(signs,z[i])
        }
      }
      
      #performs binom test
      len<-length(signs)
      sign<-binom.test(pos, len, alternative = c("two.sided"), conf.level = 0.95)
      
      if(sign$p.value < .05)
      {
        print("We are testing the difference of medians with a sign test")
        print(paste("We reject the null since the p value is ",sign$p.value))
      }
      else
      {
        print("We are testing the difference of medians with a sign test")
        print(paste("We fail to reject the null since the p value is ",sign$p.value))
      }
      
    }
    else
    {
      #normal
      match<-t.test(x1, y1,
                    alternative = c("two.sided"), paired = TRUE, var.equal = FALSE,
                    conf.level = 0.95)
      
      if(match$p.value < .05)
      {
        print("We are testing the difference of paired means with a a matched pairs two sample t-test")
        print(paste("We reject the null since the p value is ",match$p.value))
      }
      else
      {
        print("We are testing the difference of paired means with a a matched pairs two sample t-test")
        print(paste("We fail to reject the null since the p value is ",match$p.value))
      }
      
    }
    
  }
}else if(CSK=="K"){
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

    
