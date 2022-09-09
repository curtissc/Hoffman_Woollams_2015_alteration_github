
# H&W Model with frequencies
      p <- "C:/cygwin/home/Curtiss Chapman/Lens/Models/Hoffman & Woollams 2015/H_W_freqs/out/"
      out.lists <- list.files(p, pattern=".out$")
      orig.reps.avg <- read.csv("D:/Box Sync/Rice/Projects/Dissertation Frequency project/Hoffman & Woollams 2015 model/Average representations (orig & freq).csv", header=F)

# evaluate model representations
      out.dfs <- vector('list', length(out.lists))
      for(i in 1:length(out.lists)){
        out.dfs[[i]] <- read.table(paste0(p, out.lists[i]), header=F)
      }
            out.dfs <- lapply(out.dfs, function(f) f <- f[,1:(ncol(f)-1)]) # take off the last column, which is just "e"s
      # total activation of semantic units over each word on average across the 28 time steps
            # the way these data are structured are as follows:
            # each of the files read in is 2688 x 50
            # each column is a unit of the semantic layer
            # every 28 consecutive rows is a consecutive time set for 1 word presentation, and there are 96 words (28*96 = 2688)
                  lapply(out.dfs, function(f) dim(f)) # check that the files have the right dimensions (2688 x 50 all the way down)
            # average across the 50 units for every time point
                  sem.sums <- lapply(out.dfs, function(f) rowSums(f)) # the sum of activation for each word vector
            # make it into a matrix, where it's words x time point (96 x 28)
                  sem.sumz <- lapply(sem.sums, function(f) matrix(f, 96, 28, byrow=T))
            
      # get a look at the average ending representations with all 50 units still intact (to calculate cross-entropy error)
            # take only every 28th row in each df, then average representations across dfs
                  end.acts <- lapply(out.dfs, function(f) f[seq(28,2688,28),])
                  end.acts.avg <- (end.acts[[1]]+end.acts[[2]]+end.acts[[3]]+end.acts[[4]]+end.acts[[5]]+end.acts[[6]]+end.acts[[7]]+end.acts[[8]]+end.acts[[9]]+end.acts[[10]])/10
            # now calculate entropy of the average representations compared to the actual average representation for all 96 items
                  cee <- rowSums(orig.reps.avg*log(end.acts.avg)+(1-orig.reps.avg)*log(1-end.acts.avg))*-1
            # was CEE different between word groups?
                  # HD vs. LD
                        t.test(cee[1:48], cee[49:96]) # yes, more entropy for high diversity
                  # HF vs. LF
                        t.test(cee[c(1:24,49:72)], cee[c(25:48,73:96)], var.equal=T) # yes, more entropy for low frequency
      # average activation for HD/LD
            # for each time point, average across the high diversity (1-48) and low diversity (49-96) words
                  mean.act.time <- lapply(sem.sumz, function(f) rbind(HD=colMeans(f[1:48,]), LD=colMeans(f[49:96,])))
                  m.act.time.df <- do.call(rbind, mean.act.time)
                  m.act.time.df <- m.act.time.df[c(seq(1, nrow(m.act.time.df), 2),seq(2, nrow(m.act.time.df), 2)),]
            # average across all time points for HD and LD within models
                  mean.act.summary <- do.call(rbind, lapply(mean.act.time, function(f) rowMeans(f)))
                  mean.act.summary.t <- rbind(colMeans(m.act.time.df[1:10,]), colMeans(m.act.time.df[11:20,]))
                  colnames(mean.act.summary.t) <- paste0("t", 1:28)
                  mean.act.summary.t <- cbind.data.frame(SemD=c("HD", "LD"), mean.act.summary.t)
                  library(tidyr)
                  m.a.s.t <- gather(mean.act.summary.t, time, mean.act, 2:ncol(mean.act.summary.t), factor_key=T)
                  
            # !!!! average across all time points in each model and average word representations across models
                  sem.sum.timeless <- lapply(sem.sumz, rowMeans)
                  model.wd.mean.acts <- cbind.data.frame(SemD=rep(c("HD", "LD"), each=48),
                                                         Freq=rep(rep(c("HF","LF"), each=24), 2),
                                                         do.call(cbind, sem.sum.timeless))
                  model.wd.mean.acts$mean.act <- rowMeans(model.wd.mean.acts[,3:ncol(model.wd.mean.acts)])
                  
            ##################################################
            # look at mean difference between HD & LD
                  boxplot(mean.act.summary)
                  t.test(mean.act.summary[,1], mean.act.summary[,2]) # means for orig. model match Paul Hoffman's activations
            # look at HD/LD difference across model time
                  library(ggplot2)
                  ggplot(m.a.s.t, aes(x=time, y=mean.act, color=SemD)) +
                    geom_point()
            ####################################################
            
      # polarity for HD & LD
            # get polarity for each unit across the models
                  t28 <- lapply(out.dfs, function(f) f <- f[seq(28, nrow(f), 28),])
                  polarities <- lapply(t28, function(f) {
                    pol <- matrix(NA, 96, 50)
                    for(i in 1:50){
                      for(j in 1:96){
                        a <- f[j,i]
                        pol[j,i] <- a*log2(a)+ (1-a)*log2(1-a) +1
                      }}
                      pol
                    })
            # get average polarity per word across the 50 units
                  pol.means <- lapply(polarities, function(f) rowMeans(f, na.rm=T)) # the average polarity for each word vector
            # put into dfs of HD & LD
                  pol.mean.dfs <- lapply(pol.means, function(f) f <- matrix(f, 2, 48, byrow=T))
            # summarize HD & LD
                  pol.summary <- as.data.frame(do.call(rbind, lapply(pol.mean.dfs, function(f) rowMeans(f))))
                  colnames(pol.summary) <- c("HD", "LD")
                  colMeans(pol.summary)
                  
            ######################################################
            # look at mean difference between HD & LD
                  boxplot(pol.summary)
                  t.test(pol.summary[,1], pol.summary[,2], var.equal=T)
            #########################################################
            
      # average activation in HF & LF in HD & LD
            # for each time point, average across the HF, high diversity (1-24), LF, high diversity (25-48), HF, low diversity (49-72) and LF, low diversity (73-96) words
                  mean.act.time.freq <- lapply(sem.sumz, function(f) rbind(HFHD=colMeans(f[1:24,]), LFHD=colMeans(f[25:48,]), HFLD=colMeans(f[49:72,]), LFLD=colMeans(f[73:96,])))
                  m.act.time.freq.df <- do.call(rbind, mean.act.time.freq)
                  m.act.time.freq.df <- m.act.time.freq.df[c(seq(1, nrow(m.act.time.freq.df), 4), seq(2, nrow(m.act.time.freq.df), 4), seq(3, nrow(m.act.time.freq.df), 4), seq(4, nrow(m.act.time.freq.df), 4)),]
            # average across all time points for HD and LD
                  mean.act.summary.freq <- do.call(rbind, lapply(mean.act.time.freq, function(f) rowMeans(f)))
                  m.a.s.f <- rbind(cbind.data.frame(Freq="HF", SemD="HD", mean.act=mean.act.summary.freq[,1]),
                                   cbind.data.frame(Freq="LF", SemD="HD", mean.act=mean.act.summary.freq[,2]),
                                   cbind.data.frame(Freq="HF", SemD="LD", mean.act=mean.act.summary.freq[,3]),
                                   cbind.data.frame(Freq="LF", SemD="LD", mean.act=mean.act.summary.freq[,4]))
                  mean.act.summary.freq.t <- rbind(colMeans(m.act.time.freq.df[1:10,]), colMeans(m.act.time.freq.df[11:20,]), colMeans(m.act.time.freq.df[21:30,]), colMeans(m.act.time.freq.df[31:40,]))
                  colnames(mean.act.summary.freq.t) <- paste0("t", 1:28)
                  mean.act.summary.freq.t <- cbind.data.frame(cat=c("HFHD", "LFHD", "HFLD", "LFLD"), mean.act.summary.freq.t)
                  library(tidyr)
                  m.a.s.f.t <- gather(mean.act.summary.freq.t, time, mean.act, 2:ncol(mean.act.summary.freq.t), factor_key=T)
            
      ##################################################
            # mean activations, freq x SemD 
                  
                  mean.act.sum <- round(cbind(
                        LD=mean(m.a.s.f[m.a.s.f$SemD=="LD",'mean.act']),
                        HD=mean(m.a.s.f[m.a.s.f$SemD=="HD",'mean.act']),
                        LF=mean(m.a.s.f[m.a.s.f$Freq=="LF",'mean.act']),
                        HF=mean(m.a.s.f[m.a.s.f$Freq=="HF",'mean.act']),
                        LDLF=mean(m.a.s.f[m.a.s.f$Freq=="LF" & m.a.s.f$SemD=="LD",'mean.act']),
                        LDHF=mean(m.a.s.f[m.a.s.f$Freq=="HF" & m.a.s.f$SemD=="LD",'mean.act']),
                        HDLF=mean(m.a.s.f[m.a.s.f$Freq=="LF" & m.a.s.f$SemD=="HD",'mean.act']),
                        HDHF=mean(m.a.s.f[m.a.s.f$Freq=="HF" & m.a.s.f$SemD=="HD",'mean.act']),
                        div.fx=mean(m.a.s.f[m.a.s.f$SemD=="HD", 'mean.act'])-mean(m.a.s.f[m.a.s.f$SemD=="LD", 'mean.act']),
                        freq.fx=mean(m.a.s.f[m.a.s.f$Freq=="HF", 'mean.act'])-mean(m.a.s.f[m.a.s.f$Freq=="LF", 'mean.act']),
                        HD.WFE=mean(m.a.s.f[m.a.s.f$Freq=="HF" & m.a.s.f$SemD=="HD", 'mean.act'])-mean(m.a.s.f[m.a.s.f$Freq=="LF" & m.a.s.f$SemD=="HD", 'mean.act']),
                        LD.WFE=mean(m.a.s.f[m.a.s.f$Freq=="HF" & m.a.s.f$SemD=="LD", 'mean.act'])-mean(m.a.s.f[m.a.s.f$Freq=="LF" & m.a.s.f$SemD=="LD", 'mean.act'])
                        ), 2)
                  mean.act.sum
                  
                  mean.act.summary.freq.2 <- cbind.data.frame(Freq=rep(c(rep("HF", 10), rep('LF', 10)), 2), SemD=rep(c('HD', 'LD'), each=20),
                                                              mean.act=c(mean.act.summary.freq[,1], mean.act.summary.freq[,2], mean.act.summary.freq[,3], mean.act.summary.freq[,4]))
                  mean.act.summary.freq.2$Freq <- factor(mean.act.summary.freq.2$Freq, levels=c("LF", "HF"))
                  mean.act.summary.freq.2$SemD <- factor(mean.act.summary.freq.2$SemD, levels=c("LD", "HD"))
                  
                  write.csv(mean.act.summary.freq.2, "")
                  
                  # analysis across models
                  
                        library(ggplot2)
                        theme_set(theme_gray(base_size = 16)) # change font size
                        
                        colMeans(mean.act.summary.freq)
                        boxplot(mean.act.summary.freq)
                        ggplot(mean.act.summary.freq.2, aes(x=Freq, y=mean.act, color=SemD)) +
                          geom_boxplot() + xlab("Frequency") + ylab("Mean Activation")
                          #labs(list(x="Frequency", y = "mean activation"))
                  
                          library(car)
                          SemD <- factor(c(rep("HD",2), rep("LD", 2)))
                          Freq <- factor(rep(rep(c("HF", "LF"), each=1), 2))
                          btw <- lm(cbind(m.a.s.f[m.a.s.f$Freq=="HF" & m.a.s.f$SemD=="HD",'mean.act'], 
                                          m.a.s.f[m.a.s.f$Freq=="LF" & m.a.s.f$SemD=="HD",'mean.act'],
                                          m.a.s.f[m.a.s.f$Freq=="HF" & m.a.s.f$SemD=="LD",'mean.act'],
                                          m.a.s.f[m.a.s.f$Freq=="LF" & m.a.s.f$SemD=="LD",'mean.act'])~1)
                          Anv <- Anova(btw, idata=data.frame(SemD, Freq), idesign=~SemD*Freq, type=3) # to get Type 3 SS
                          summary(Anv, multivariate=F)
                          
                          t.test(mean.act~Freq, data=m.a.s.f[m.a.s.f$SemD=="HD",], paired=T)
                          t.test(mean.act~Freq, data=m.a.s.f[m.a.s.f$SemD=="LD",], paired=T)
                          t.test(mean.act~SemD, data=m.a.s.f[m.a.s.f$Freq=="HF",], paired=T)
                          t.test(mean.act~SemD, data=m.a.s.f[m.a.s.f$Freq=="LF",], paired=T)
                  
                  # analysis across items
                          
                          library(ggplot2)
                          theme_set(theme_gray(base_size = 16)) # change font size
                          
                          c(mean(model.wd.mean.acts[model.wd.mean.acts$Freq=="HF" & model.wd.mean.acts$SemD=="HD",'mean.act']),
                            mean(model.wd.mean.acts[model.wd.mean.acts$Freq=="LF" & model.wd.mean.acts$SemD=="HD",'mean.act']),
                            mean(model.wd.mean.acts[model.wd.mean.acts$Freq=="HF" & model.wd.mean.acts$SemD=="LD",'mean.act']),
                            mean(model.wd.mean.acts[model.wd.mean.acts$Freq=="LF" & model.wd.mean.acts$SemD=="LD",'mean.act'])
                            )
                          boxplot(mean.act~Freq*SemD, data=model.wd.mean.acts)
                          ggplot(model.wd.mean.acts, aes(x=Freq, y=mean.act, color=SemD)) +
                            geom_boxplot() +
                            labs(list(x="Frequency", y = "mean activation"))
                          
                          SemD <- factor(c(rep("HD",2), rep("LD", 2)))
                          Freq <- factor(rep(rep(c("HF", "LF"), each=1), 2))
                          btw <- lm(cbind(model.wd.mean.acts[model.wd.mean.acts$Freq=="HF" & model.wd.mean.acts$SemD=="HD",'mean.act'], 
                                          model.wd.mean.acts[model.wd.mean.acts$Freq=="LF" & model.wd.mean.acts$SemD=="HD",'mean.act'],
                                          model.wd.mean.acts[model.wd.mean.acts$Freq=="HF" & model.wd.mean.acts$SemD=="LD",'mean.act'],
                                          model.wd.mean.acts[model.wd.mean.acts$Freq=="LF" & model.wd.mean.acts$SemD=="LD",'mean.act'])~1)
                          Anv2 <- Anova(btw, idata=data.frame(SemD, Freq), idesign=~SemD*Freq, type=3) # to get Type 3 SS
                          summary(Anv2, multivariate=F)
                          
                          t.test(mean.act~Freq, data=model.wd.mean.acts[model.wd.mean.acts$SemD=="HD",], paired=T)
                          t.test(mean.act~Freq, data=model.wd.mean.acts[model.wd.mean.acts$SemD=="LD",], paired=T)
                          t.test(mean.act~SemD, data=model.wd.mean.acts[model.wd.mean.acts$Freq=="HF",], paired=T)
                          t.test(mean.act~SemD, data=model.wd.mean.acts[model.wd.mean.acts$Freq=="LF",], paired=T)
                          
                          
                  # mixed effects model
                          model.wd.mean.acts$item <- 1:nrow(model.wd.mean.acts)
                          colnames(model.wd.mean.acts)[3:12] <- c('One', 'Two', 'Three', 'Four', 'Five', 'Six', 'Seven', 'Eight', 'Nine', 'Ten')
                          model.wd.mean.acts2 <- model.wd.mean.acts[,c(14,1:12)]
                          library(tidyr)
                          model.wd.mean.acts.long <- gather(model.wd.mean.acts2, model, mean.act, One:Ten, factor_key=T)
                          model.wd.mean.acts.long$item <- as.factor(model.wd.mean.acts.long$item)
                          
                          library(lme4)
                          m <- lmer(mean.act ~ Freq + SemD + (1|item) + (1|model), data=model.wd.mean.acts.long, REML=F)
                          summary(m)
                          m2 <- lmer(mean.act ~ Freq + SemD + Freq:SemD + (1|item) + (1|model), data=model.wd.mean.acts.long, REML=F)
                          summary(m2)
                          anova(m, m2) # Interaction adds to model
                        
                          
                  
      #########################################################
                  
      # look at polarity for HF vs. LF
            pol.means.df <- as.data.frame(do.call(cbind, pol.means))
            # avg. polarity across models
                  mod.pol.0 <- rowMeans(pol.means.df)
                  mod.pol <- cbind.data.frame(freq=rep(c('HF', 'LF', 'HF', 'LF'), each=24), mod.pol.0)
                  
                  t.test(mod.pol[,2]~mod.pol$freq)
                  
            # avg. polarity across items
                  item.pol <- rbind(apply(pol.means.df[c(1:24,49:72),],2,mean),
                                    apply(pol.means.df[c(25:48,73:96),],2,mean))
                  t.test(item.pol[1,], item.pol[2,])
                  
                  
                  
            # look at HD/LD difference across time
                  library(ggplot2)
                  ggplot(m.a.s.f.t, aes(x=time, y=mean.act, color=cat)) +
                    geom_point()
      # look at polarity for Freq & SemD
                  # put into dfs by Freq & SemD
            pol.mean.dfs.freq <- lapply(pol.means, function(f) f <- matrix(f, 4, 24, byrow=T))
      # summarize HD & LD
            # across words
                  pol.summary.freq <- as.data.frame(do.call(rbind, lapply(pol.mean.dfs.freq, function(f) rowMeans(f))))
                  colnames(pol.summary.freq) <- c("HFHD", "LFHD", "HFLD", "LFLD")
                  colMeans(pol.summary.freq)
                  p.s.f <- rbind(cbind.data.frame(Freq="HF", SemD="HD", mean.pol=pol.summary.freq[,1]),
                                 cbind.data.frame(Freq="LF", SemD="HD", mean.pol=pol.summary.freq[,2]),
                                 cbind.data.frame(Freq="HF", SemD="LD", mean.pol=pol.summary.freq[,3]),
                                 cbind.data.frame(Freq="LF", SemD="LD", mean.pol=pol.summary.freq[,4]))
      # across models
      pol.summary.freq.2 <- as.data.frame(do.call(rbind, pol.mean.dfs.freq))
      pol.summary.freq.2 <- pol.summary.freq.2[c(seq(1, nrow(pol.summary.freq.2), 4),
                                                 seq(2, nrow(pol.summary.freq.2), 4),
                                                 seq(3, nrow(pol.summary.freq.2), 4),
                                                 seq(4, nrow(pol.summary.freq.2), 4)),]
      pol.sum.freq.2 <- cbind(HFHD=colMeans(pol.summary.freq.2[1:10,]), LFHD=colMeans(pol.summary.freq.2[11:20,]), HFLD=colMeans(pol.summary.freq.2[21:30,]), LFLD=colMeans(pol.summary.freq.2[31:40,]))
      colMeans(pol.sum.freq.2)
      p.s.f.2 <- rbind(cbind.data.frame(Freq="HF", SemD="HD", mean.pol=pol.sum.freq.2[,1]),
                       cbind.data.frame(Freq="LF", SemD="HD", mean.pol=pol.sum.freq.2[,2]),
                       cbind.data.frame(Freq="HF", SemD="LD", mean.pol=pol.sum.freq.2[,3]),
                       cbind.data.frame(Freq="LF", SemD="LD", mean.pol=pol.sum.freq.2[,4]))
      # look at mean difference between HD & LD
            boxplot(pol.summary.freq)
            summary(aov(mean.pol~Freq*SemD, data=p.s.f))
            boxplot(pol.sum.freq.2)
            summary(aov(mean.pol~Freq*SemD, data=p.s.f.2))
            
            t.test(mean.pol~Freq, data=p.s.f[p.s.f$SemD=="HD",], paired=T)
            t.test(mean.pol~Freq, data=p.s.f[p.s.f$SemD=="LD",], paired=T)
            t.test(mean.pol~SemD, data=p.s.f[p.s.f$Freq=="HF",], paired=T)
            t.test(mean.pol~SemD, data=p.s.f[p.s.f$Freq=="LF",], paired=T)
            
            
  #     # polarity & total activation of starting representations
  #             #     # get polarities
  #             #     polarities.start <- as.data.frame(matrix(NA, 96, 50))
  #             #     for(i in 1:50){
  #             #       for(j in 1:96){
  #             #         a <- start[j,i]
  #             #         polarities.start[j,i] <- a*log2(a)+ (1-a)*log2(1-a) +1
  #             #       }
  #             # # get average polarity per word across the 50 units
  #             # pol.means.start <- rowMeans(polarities.start, na.rm=T) # the average polarity for each word vector
  #             # # put into dfs of HD & LD
  #             # pol.mean.dfs.start <- matrix(pol.means.start, 48, 2, byrow=F)
  #             # colnames(pol.mean.dfs.start) <- c("HD", "LD")
  #             # # summarize HD & LD
  #             # pol.summary.start <- colMeans(pol.mean.dfs.start)
  #             # # look at mean difference between HD & LD
  #             # boxplot(pol.mean.dfs.start)
  #             # t.test(pol.mean.dfs.start[,1], pol.mean.dfs.start[,2])
  #             # # put into dfs of Freq & SemD
  #             # pol.mean.dfs.start.freq <- matrix(pol.means.start, 24, 4, byrow=F)
  #             # colnames(pol.mean.dfs.start.freq) <- c("HFHD", "LFHD", "HFLD", "LFLD")
  #             # p.m.s.f <- rbind(cbind.data.frame(Freq="HF", SemD="HD", mean.pol=pol.mean.dfs.start.freq[,1]),
  #             #                  cbind.data.frame(Freq="LF", SemD="HD", mean.pol=pol.mean.dfs.start.freq[,2]),
  #             #                  cbind.data.frame(Freq="HF", SemD="LD", mean.pol=pol.mean.dfs.start.freq[,3]),
  #             #                  cbind.data.frame(Freq="LF", SemD="LD", mean.pol=pol.mean.dfs.start.freq[,4]))
  #             # # summarize Freq & SemD
  #             # pol.summary.start.freq <- colMeans(pol.mean.dfs.start.freq)
  #             # # look at mean difference between HD & LD
  #             # boxplot(mean.pol~Freq*SemD, data=p.m.s.f)
  #             # summary(aov(mean.pol~Freq*SemD, data=p.m.s.f))
  #             # t.test(mean.pol~Freq, data=p.m.s.f[p.m.s.f$SemD=="HD",])
  #             # t.test(mean.pol~Freq, data=p.m.s.f[p.m.s.f$SemD=="LD",])
  #             # # sum of activation per word across the 50 units
  #             # act.start <- rowSums(start, na.rm=T) # the average polarity for each word vector
  #             # # put into dfs of HD & LD
  #             # act.start.df <- matrix(act.start, 48, 2, byrow=F)
  #             # colnames(act.start.df) <- c("HD", "LD")
  #             # # summarize HD & LD
  #             # act.sum.start <- colMeans(act.start.df)
  #             # # look at mean difference between HD & LD
  #             # boxplot(act.start.df)
  #             # t.test(act.start.df[,1], act.start.df[,2])
  #             # # put into dfs of Freq & SemD
  #             # act.start.df.freq <- matrix(act.start, 24, 4, byrow=F)
  #             # colnames(act.start.df.freq) <- c("HFHD", "LFHD", "HFLD", "LFLD")
  #             # a.m.s.f <- rbind(cbind.data.frame(Freq="HF", SemD="HD", mean.pol=act.start.df.freq[,1]),
  #             #                  cbind.data.frame(Freq="LF", SemD="HD", mean.pol=act.start.df.freq[,2]),
  #             #                  cbind.data.frame(Freq="HF", SemD="LD", mean.pol=act.start.df.freq[,3]),
  #             #                  cbind.data.frame(Freq="LF", SemD="LD", mean.pol=act.start.df.freq[,4]))
  #             # # summarize Freq & SemD
  #             # act.start.df.freq <- colMeans(act.start.df.freq)
  #             # # look at mean difference between HD & LD
  #             # boxplot(mean.pol~Freq*SemD, data=a.m.s.f)
  #             # summary(aov(mean.pol~Freq*SemD, data=a.m.s.f))
  #             # t.test(mean.pol~Freq, data=a.m.s.f[a.m.s.f$SemD=="HD",])
  #             # t.test(mean.pol~Freq, data=a.m.s.f[a.m.s.f$SemD=="LD",])
  #             # act.start.df.freq
  #             # pol.summary.start.freq
  #       boxplot(pol.summary.freq)
  #       summary(aov(mean.pol~Freq*SemD, data=p.s.f))
  #       boxplot(pol.sum.freq.2)
  #       summary(aov(mean.pol~Freq*SemD, data=p.s.f.2))
  #       pol.mean.dfs.freq
  #       pol.summary.freq
  #       boxplot(mean.act~Freq*SemD, data=m.a.s.f)
  #       summary(aov(mean.act~Freq*SemD, data=m.a.s.f))
  #       t.test(mean.act~Freq, data=m.a.s.f[m.a.s.f$SemD=="HD",])
  #       t.test(mean.act~Freq, data=m.a.s.f[m.a.s.f$SemD=="LD",])
  #       mean.act.summary.freq
  #       act.start.df.freq
  # start <- read.csv("C:/Users/Curtiss Chapman/Box Sync/Rice/Projects/Dissertation Frequency project/Hoffman & Woollams 2015 model/Hoffman & Woollams 2015 model/Orig.reprepresentations.0.52.csv", header=F)
  # p <- "C:/cygwin/home/Curtiss Chapman/Lens/Models/Hoffman & Woollams 2015/Curtreps0.52_iterations/out/"
  # out.lists <- list.files(p, pattern=".out$")
  # out.lists
  #       # put the files into dfs
  #       out.dfs <- vector('list', length(out.lists))
  #       for(i in 1:length(out.lists)){
  #         out.dfs[[i]] <- read.table(paste0(p, out.lists[i]), header=F)
  #       }
  #       out.dfs <- lapply(out.dfs, function(f) f <- f[,1:(ncol(f)-1)]) # take off the last column, which is just "e"s
  #       # look at total activation of semantic units over each word on average across the 28 time steps
  #       # the way these data are structured are as follows:
  #       # each of the files read in is 2688 x 50
  #       # each column is a unit of the semantic layer
  #       # every 28 consecutive rows is a consecutive time set for 1 word presentation, and there are 96 words (28*96 = 2688)
  #       lapply(out.dfs, function(f) dim(f)) # check that the files have the right dimensions
  #       # average across the 50 units for every time point
  #       sem.sums <- lapply(out.dfs, function(f) rowSums(f)) # the sum of activation for each word vector
  #       # make it into a matrix, where it's words x time point (96 x 28)
  #       sem.sumz <- lapply(sem.sums, function(f) matrix(f, 96, 28, byrow=T))
  #       # just looking at HD/LD
  #       # for each time point, average across the high diversity (1-48) and low diversity (49-96) words
  #       mean.act.time <- lapply(sem.sumz, function(f) rbind(HD=colMeans(f[1:48,]), LD=colMeans(f[49:96,])))
  #       m.act.time.df <- do.call(rbind, mean.act.time)
  #       m.act.time.df <- m.act.time.df[c(seq(1, nrow(m.act.time.df), 2),seq(2, nrow(m.act.time.df), 2)),]
  #       # average across all time points for HD and LD
  #       mean.act.summary <- do.call(rbind, lapply(mean.act.time, function(f) rowMeans(f)))
  #       mean.act.summary.t <- rbind(colMeans(m.act.time.df[1:10,]), colMeans(m.act.time.df[11:20,]))
  #       colnames(mean.act.summary.t) <- paste0("t", 1:28)
  #       mean.act.summary.t <- cbind.data.frame(SemD=c("HD", "LD"), mean.act.summary.t)
  #       library(tidyr)
  #       m.a.s.t <- gather(mean.act.summary.t, time, mean.act, 2:ncol(mean.act.summary.t), factor_key=T)
  #       # look at mean difference between HD & LD
  #       boxplot(mean.act.summary)
  #       t.test(mean.act.summary[,1], mean.act.summary[,2])
  #       # look at HD/LD difference across time
  #       library(ggplot2)
  #       ggplot(m.a.s.t, aes(x=time, y=mean.act, color=SemD)) +
  #         geom_point()
  #       # look at polarity for HD & LD
  #       # get polarity for each unit across the models
  #       t28 <- lapply(out.dfs, function(f) f <- f[seq(28, nrow(f), 28),])
  #       polarities <- lapply(t28, function(f) {
  #         pol <- matrix(NA, 96, 50)
  #         for(i in 1:50){
  #           for(j in 1:96){
  #             a <- f[j,i]
  #             pol[j,i] <- a*log2(a)+ (1-a)*log2(1-a) +1
  #           }
  #           pol
  #         })
  #       # get average polarity per word across the 50 units
  #       pol.means <- lapply(polarities, function(f) rowMeans(f, na.rm=T)) # the average polarity for each word vector
  #       # put into dfs of HD & LD
  #       pol.mean.dfs <- lapply(pol.means, function(f) f <- matrix(f, 2, 48, byrow=T))
  #       # summarize HD & LD
  #       pol.summary <- as.data.frame(do.call(rbind, lapply(pol.mean.dfs, function(f) rowMeans(f))))
  #       colnames(pol.summary) <- c("HD", "LD")
  #       colMeans(pol.summary)
  #       # look at mean difference between HD & LD
  #       boxplot(pol.summary)
  #       t.test(pol.summary[,1], pol.summary[,2])
  #       # looking at HF & LF in HD & LD
  #       # for each time point, average across the HF, high diversity (1-24), LF, high diversity (25-48), HF, low diversity (49-72) and LF, low diversity (73-96) words
  #       mean.act.time.freq <- lapply(sem.sumz, function(f) rbind(HFHD=colMeans(f[1:24,]), LFHD=colMeans(f[25:48,]), HFLD=colMeans(f[49:72,]), LFLD=colMeans(f[73:96,])))
  #       m.act.time.freq.df <- do.call(rbind, mean.act.time.freq)
  #       m.act.time.freq.df <- m.act.time.freq.df[c(seq(1, nrow(m.act.time.freq.df), 4), seq(2, nrow(m.act.time.freq.df), 4), seq(3, nrow(m.act.time.freq.df), 4), seq(4, nrow(m.act.time.freq.df), 4)),]
  #       # average across all time points for HD and LD
  #       mean.act.summary.freq <- do.call(rbind, lapply(mean.act.time.freq, function(f) rowMeans(f)))
  #       m.a.s.f <- rbind(cbind.data.frame(Freq="HF", SemD="HD", mean.act=mean.act.summary.freq[,1]),
  #                        cbind.data.frame(Freq="LF", SemD="HD", mean.act=mean.act.summary.freq[,2]),
  #                        cbind.data.frame(Freq="HF", SemD="LD", mean.act=mean.act.summary.freq[,3]),
  #                        cbind.data.frame(Freq="LF", SemD="LD", mean.act=mean.act.summary.freq[,4]))
  #       mean.act.summary.freq.t <- rbind(colMeans(m.act.time.freq.df[1:10,]), colMeans(m.act.time.freq.df[11:20,]), colMeans(m.act.time.freq.df[21:30,]), colMeans(m.act.time.freq.df[31:40,]))
  #       colnames(mean.act.summary.freq.t) <- paste0("t", 1:28)
  #       mean.act.summary.freq.t <- cbind.data.frame(cat=c("HFHD", "LFHD", "HFLD", "LFLD"), mean.act.summary.freq.t)
  #       library(tidyr)
  #       m.a.s.f.t <- gather(mean.act.summary.freq.t, time, mean.act, 2:ncol(mean.act.summary.freq.t), factor_key=T)
  #       # look at mean difference between HD & LD
  #       boxplot(mean.act~Freq*SemD, data=m.a.s.f)
  #       summary(aov(mean.act~Freq*SemD, data=m.a.s.f))
  #       t.test(mean.act~Freq, data=m.a.s.f[m.a.s.f$SemD=="HD",])
  #       t.test(mean.act~Freq, data=m.a.s.f[m.a.s.f$SemD=="LD",])
  #       # look at HD/LD difference across time
  #       library(ggplot2)
  #       ggplot(m.a.s.f.t, aes(x=time, y=mean.act, color=cat)) +
  #         geom_point()
  #       # look at polarity for Freq & SemD
  #       # put into dfs by Freq & SemD
  #       pol.mean.dfs.freq <- lapply(pol.means, function(f) f <- matrix(f, 4, 24, byrow=T))
  #       # summarize HD & LD
  #       # across words
  #       pol.summary.freq <- as.data.frame(do.call(rbind, lapply(pol.mean.dfs.freq, function(f) rowMeans(f))))
  #       colnames(pol.summary.freq) <- c("HFHD", "LFHD", "HFLD", "LFLD")
  #       colMeans(pol.summary.freq)
  #       p.s.f <- rbind(cbind.data.frame(Freq="HF", SemD="HD", mean.pol=pol.summary.freq[,1]),
  #                      cbind.data.frame(Freq="LF", SemD="HD", mean.pol=pol.summary.freq[,2]),
  #                      cbind.data.frame(Freq="HF", SemD="LD", mean.pol=pol.summary.freq[,3]),
  #                      cbind.data.frame(Freq="LF", SemD="LD", mean.pol=pol.summary.freq[,4]))
  #       # across models
  #       pol.summary.freq.2 <- as.data.frame(do.call(rbind, pol.mean.dfs.freq))
  #       pol.summary.freq.2 <- pol.summary.freq.2[c(seq(1, nrow(pol.summary.freq.2), 4),
  #                                                  seq(2, nrow(pol.summary.freq.2), 4),
  #                                                  seq(3, nrow(pol.summary.freq.2), 4),
  #                                                  seq(4, nrow(pol.summary.freq.2), 4)),]
  #       pol.sum.freq.2 <- cbind(HFHD=colMeans(pol.summary.freq.2[1:10,]), LFHD=colMeans(pol.summary.freq.2[11:20,]), HFLD=colMeans(pol.summary.freq.2[21:30,]), LFLD=colMeans(pol.summary.freq.2[31:40,]))
  #       colMeans(pol.sum.freq.2)
  #       p.s.f.2 <- rbind(cbind.data.frame(Freq="HF", SemD="HD", mean.pol=pol.sum.freq.2[,1]),
  #                        cbind.data.frame(Freq="LF", SemD="HD", mean.pol=pol.sum.freq.2[,2]),
  #                        cbind.data.frame(Freq="HF", SemD="LD", mean.pol=pol.sum.freq.2[,3]),
  #                        cbind.data.frame(Freq="LF", SemD="LD", mean.pol=pol.sum.freq.2[,4]))
  #       # look at mean difference between HD & LD
  #       boxplot(pol.summary.freq)
  #       summary(aov(mean.pol~Freq*SemD, data=p.s.f))
  #       boxplot(pol.sum.freq.2)
  #       summary(aov(mean.pol~Freq*SemD, data=p.s.f.2))
  #       # polarity & total activation of starting representations
  #       # get polarities
  #       polarities.start <- as.data.frame(matrix(NA, 96, 50))
  #       for(i in 1:50){
  #         for(j in 1:96){
  #           a <- start[j,i]
  #           polarities.start[j,i] <- a*log2(a)+ (1-a)*log2(1-a) +1
  #         }
  #         # get average polarity per word across the 50 units
  #         pol.means.start <- rowMeans(polarities.start, na.rm=T) # the average polarity for each word vector
  #         # put into dfs of HD & LD
  #         pol.mean.dfs.start <- matrix(pol.means.start, 48, 2, byrow=F)
  #         colnames(pol.mean.dfs.start) <- c("HD", "LD")
  #         # summarize HD & LD
  #         pol.summary.start <- colMeans(pol.mean.dfs.start)
  #         # look at mean difference between HD & LD
  #         boxplot(pol.mean.dfs.start)
  #         t.test(pol.mean.dfs.start[,1], pol.mean.dfs.start[,2])
  #         # put into dfs of Freq & SemD
  #         pol.mean.dfs.start.freq <- matrix(pol.means.start, 24, 4, byrow=F)
  #         colnames(pol.mean.dfs.start.freq) <- c("HFHD", "LFHD", "HFLD", "LFLD")
  #         p.m.s.f <- rbind(cbind.data.frame(Freq="HF", SemD="HD", mean.pol=pol.mean.dfs.start.freq[,1]),
  #                          cbind.data.frame(Freq="LF", SemD="HD", mean.pol=pol.mean.dfs.start.freq[,2]),
  #                          cbind.data.frame(Freq="HF", SemD="LD", mean.pol=pol.mean.dfs.start.freq[,3]),
  #                          cbind.data.frame(Freq="LF", SemD="LD", mean.pol=pol.mean.dfs.start.freq[,4]))
  #         # summarize Freq & SemD
  #         pol.summary.start.freq <- colMeans(pol.mean.dfs.start.freq)
  #         # look at mean difference between HD & LD
  #         boxplot(mean.pol~Freq*SemD, data=p.m.s.f)
  #         summary(aov(mean.pol~Freq*SemD, data=p.m.s.f))
  #         t.test(mean.pol~Freq, data=p.m.s.f[p.m.s.f$SemD=="HD",])
  #         t.test(mean.pol~Freq, data=p.m.s.f[p.m.s.f$SemD=="LD",])
  #         # sum of activation per word across the 50 units
  #         act.start <- rowSums(start, na.rm=T) # the average polarity for each word vector
  #         # put into dfs of HD & LD
  #         act.start.df <- matrix(act.start, 48, 2, byrow=F)
  #         colnames(act.start.df) <- c("HD", "LD")
  #         # summarize HD & LD
  #         act.sum.start <- colMeans(act.start.df)
  #         # look at mean difference between HD & LD
  #         boxplot(act.start.df)
  #         t.test(act.start.df[,1], act.start.df[,2])
  #         # put into dfs of Freq & SemD
  #         act.start.df.freq <- matrix(act.start, 24, 4, byrow=F)
  #         colnames(act.start.df.freq) <- c("HFHD", "LFHD", "HFLD", "LFLD")
  #         a.m.s.f <- rbind(cbind.data.frame(Freq="HF", SemD="HD", mean.pol=act.start.df.freq[,1]),
  #                          cbind.data.frame(Freq="LF", SemD="HD", mean.pol=act.start.df.freq[,2]),
  #                          cbind.data.frame(Freq="HF", SemD="LD", mean.pol=act.start.df.freq[,3]),
  #                          cbind.data.frame(Freq="LF", SemD="LD", mean.pol=act.start.df.freq[,4]))
  #         # summarize Freq & SemD
  #         act.start.df.freq <- colMeans(act.start.df.freq)
  #         # look at mean difference between HD & LD
  #         boxplot(mean.pol~Freq*SemD, data=a.m.s.f)
  #         summary(aov(mean.pol~Freq*SemD, data=a.m.s.f))
  #         t.test(mean.pol~Freq, data=a.m.s.f[a.m.s.f$SemD=="HD",])
  #         t.test(mean.pol~Freq, data=a.m.s.f[a.m.s.f$SemD=="LD",])
  #         act.start.df.freq
  #         pol.summary.start.freq
  #         summary(aov(mean.pol~Freq*SemD, data=p.m.s.f))
  #         t.test(mean.pol~Freq, data=p.m.s.f[p.m.s.f$SemD=="HD",])
  #         t.test(mean.pol~Freq, data=p.m.s.f[p.m.s.f$SemD=="LD",])
  #         boxplot(pol.summary.freq)
  #         summary(aov(mean.pol~Freq*SemD, data=p.s.f))
  #         boxplot(pol.sum.freq.2)
  #         summary(aov(mean.pol~Freq*SemD, data=p.s.f.2))
  #         summary(aov(mean.act~Freq*SemD, data=m.a.s.f))
  #         t.test(mean.act~Freq, data=m.a.s.f[m.a.s.f$SemD=="HD",])
  #         t.test(mean.act~Freq, data=m.a.s.f[m.a.s.f$SemD=="LD",])
  #         mean.act.summary.freq
  #         mean.act.summary
  #         pol.summary
  #         pol.summary.freq
  #         