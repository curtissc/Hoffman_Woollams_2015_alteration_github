setwd("D:/Box Sync/Rice/Projects/Dissertation Frequency project/Hoffman & Woollams 2015 model")

x <- read.csv("D:/Box Sync/Rice/Projects/Dissertation Frequency project/All word data across databases 8.28.18.csv", header=T, row.names=1)
      x <- x[!is.na(x$Lg_Subtlex) & !is.na(x$H.SemD), c('word', 'Lg_Subtlex', 'H.SemD')] # n = 30209
      x <- x[order(x$Lg_Subtlex), ]
      
      
      f.q <- quantile(x$Lg_Subtlex, probs = seq(0, 1, 1/6))
      s.q <- quantile(x$H.SemD, probs = seq(0, 1, 1/6))
      
      x$Freq_q <- NA
      x$SemD_q <- NA
      for(i in 1:6){
            if(i==1){
                  x[x$Lg_Subtlex<f.q[i+1], 'Freq_q'] <- i
                  x[x$H.SemD<s.q[i+1], 'SemD_q'] <- i
            }
            x[x$Lg_Subtlex>f.q[i] & x$Lg_Subtlex<=f.q[i+1], 'Freq_q'] <- i
            x[x$H.SemD>s.q[i] & x$H.SemD<=s.q[i+1], 'SemD_q'] <- i
      }
      
      table(x$Freq_q, x$SemD_q) # rows = freq, cols = semD
      
      hist(x[x$Freq_q==2 & x$SemD_q==2, 'Lg_Subtlex'])
      hist(x[x$Freq_q==2 & x$SemD_q==5, 'Lg_Subtlex'])
      hist(x[x$Freq_q==5 & x$SemD_q==5, 'Lg_Subtlex'])
      hist(x[x$Freq_q==5 & x$SemD_q==2, 'Lg_Subtlex'])
      
# freq sample
      
      set.seed(666)
      cbind(c(sample(x[x$Freq_q==5 & x$SemD_q==5, 'Lg_Subtlex'], 24, replace=F), 
        sample(x[x$Freq_q==2 & x$SemD_q==5, 'Lg_Subtlex'], 24, replace=F),
        sample(x[x$Freq_q==5 & x$SemD_q==2, 'Lg_Subtlex'], 24, replace=F),
        sample(x[x$Freq_q==2 & x$SemD_q==2, 'Lg_Subtlex'], 24, replace=F)))
      