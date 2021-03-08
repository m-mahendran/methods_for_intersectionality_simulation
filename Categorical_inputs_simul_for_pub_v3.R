#############all categorical inputs simulation#############

Packages <- c("lsr", "foreach", "doParallel", "lme4", "parallel", "MASS", 
              "rpart", "partykit", "tuneRanger", "mlr", "truncnorm", "merTools", "plyr")
lapply(Packages, library, character.only = TRUE)



#############functions for simulation
    
    #bimodal dist for coeff
        Sample.negative.coeff <- function (n) {
          y0 <- rtruncnorm(n,a=-1.94, b=-0.06, mean=-1, sd = 1)
        }
          
        Sample.positive.coeff <- function (n) {
          y0 <- rtruncnorm(n,a=.06, b=1.94, mean=1, sd = 1)
        }

  
    #SET KNOWN TRUTH (NEEDS TO RESET FOR EACH ROUND)
        createTruth <- function(B1.1, B1.2, B1.3, B2, B3, B4, B5 ,Bint12, Bint13, Bint2) {
          
          x1 <- c(0, 1, 2, 3)
          x2 <- c(0, 1)
          x3 <- c(0, 1)
          x4 <- c(0, 1)
          x5 <- c(0, 1)
          x6 <- c(0, 1, 2)
          
          #create every combo
          Truth <- expand.grid(x1=x1,x2=x2,x3=x3, x4=x4, x5=x5, x6=x6)
          
          
          Truth$cluster <- factor(100000*(Truth$x1+1) + 10000*(Truth$x2+1) + 1000*(Truth$x3+1) + 100*(Truth$x4+1) + 10*(Truth$x5 + 1) + 1*(Truth$x6+1))
          
          
          Truth$int12 <- 0
          Truth$int12 [Truth$x1==2 & Truth$x2==1] <- 1  
          
          Truth$int13 <- 0
          Truth$int13 [Truth$x1==3 & Truth$x2==1] <- 1  
          
          Truth$x1.1 <- ifelse(Truth$x1==1, 1, 0)
          Truth$x1.2 <- ifelse(Truth$x1==2, 1, 0)
          Truth$x1.3 <- ifelse(Truth$x1==3, 1, 0)
          
          Truth$y <- B1.1*Truth$x1.1 + B1.2*Truth$x1.2 + B1.3*Truth$x1.3 + B2*Truth$x2 + B3*Truth$x3 + B4*Truth$x4 + B5*Truth$x5 +  
                     Bint12*Truth$int12 + Bint13*Truth$int13+ Bint2*Truth$x3*Truth$x4*Truth$x5
          
          Truth$x1<- as.factor(Truth$x1) 
          Truth$x2<- as.factor(Truth$x2) 
          Truth$x3<- as.factor(Truth$x3)
          Truth$x4<- as.factor(Truth$x4) 
          Truth$x5<- as.factor(Truth$x5) 
          Truth$x6 <- as.factor(Truth$x6)
          
          Truth
          }

#############looping simulation
  
  registerDoParallel()
  
  n_list = c(2000, 5000, 50000, 200000)
  
  for (n in n_list){
  
    MSEtable = NULL
    MSEtable <- matrix(nrow=1000, ncol=1558)
    
    results = NULL
    results <- matrix(nrow=1000, ncol=1558)
    
    results <- foreach (i=1:1000, .combine=rbind, .packages=c('rpart', 'partykit', 'tuneRanger', 'lme4', 'lsr', 'truncnorm', 'mlr', 'merTools', 'plyr')) %dopar% {
     
        set.seed (i)
      
      D <- data.frame (x1 = sample(0:3, n, replace=TRUE),
                       x2 = sample(c(0,1),n, prob=c(0.80, 0.20), replace = TRUE),
                       x3 = sample(c(0,1),n, replace = TRUE),
                       x5 = sample(c(0,1), n, prob=c(0.75, 0.25), replace=TRUE),
                       x6 = sample(0:2, n, replace=TRUE))
      
      #create x3 as x4 mediator
      D$prX4 <- ifelse(D$x3<1,0.40, 0.7)
      D$x4=rbinom(n,1,D$prX4)
      
      D$x1 <- as.numeric(D$x1)
      D$x2 <- as.numeric(D$x2)
      D$x3 <- as.numeric(D$x3)
      D$x4 <- as.numeric(D$x4)
      D$x5 <- as.numeric(D$x5)
      D$x6 <- as.numeric(D$x6)
      
      #create intersection labels
      D$cluster <- factor(100000*(D$x1+1) + 10000*(D$x2+1) + 1000*(D$x3+1) + 100*(D$x4+1) + 10*(D$x5 + 1) + 1*(D$x6+1))
      
      D$int12 <- 0
      D$int12 [D$x1==2 & D$x2==1] <- 1  
      
      D$int13 <- 0
      D$int13 [D$x1==3 & D$x2==1] <- 1  
      
      D$x1.1 <- ifelse(D$x1==1, 1, 0)
      D$x1.2 <- ifelse(D$x1==2, 1, 0)
      D$x1.3 <- ifelse(D$x1==3, 1, 0)
      
      # mediator x4 (b/w x3 and y), interaction between binary and categorical, interaction between binary and binary.
      #null effect categorical
    
      b1.1 <- Sample.positive.coeff(1)
      b1.2 <-  Sample.positive.coeff(1)
      b1.3 <-  Sample.positive.coeff(1)
      b2 <-  Sample.positive.coeff(1)
      b3 <- Sample.negative.coeff(1)
      b4 <- Sample.negative.coeff(1)
      b5 <- Sample.negative.coeff(1)
      b6 <- 0  
      bint12 <- Sample.negative.coeff(1)
      bint13 <- Sample.negative.coeff(1)
      bint2 <- Sample.positive.coeff(1)
      
      D$y <- b1.1*D$x1.1 + b1.2*D$x1.2 + b1.3*D$x1.3 + b2*D$x2 + b3*D$x3 + b4*D$x4 + b5*D$x5 + bint12*D$int12 + bint13*D$int13 + bint2*D$x3*D$x4*D$x5 + rnorm(n) 
      
      D$x1<- as.factor(D$x1) 
      D$x2<- as.factor(D$x2) 
      D$x3<- as.factor(D$x3)
      D$x4<- as.factor(D$x4) 
      D$x5<- as.factor(D$x5) 
      D$x6<- as.factor(D$x6) 
      
      
      Truth<- createTruth(b1.1, b1.2, b1.3, b2, b3, b4, b5, bint12,bint13, bint2)
    
      
      ###############Training ML methods
      #pruning method for rpart from: https://www.statmethods.net/advstats/cart.html
      
      CART <-rpart(y~x1+x2+x3+x4+x5+x6,D)
      CARTpruned<- prune(CART, cp=CART$cptable[which.min(CART$cptable[,"xerror"]),"CP"])
      
      # of leaves: https://github.com/AshwiniKV/obesity_decision_trees/blob/master/simulation_study/code/sce3_30.R
      leafCART <- sum(ifelse(CARTpruned$frame[,1] == "<leaf>", 1, 0))
      numsplitvarCART <- length(unique((CARTpruned$frame[,1])))-1
        #subtract 1 because uses "leaf"
      splitvarCART <- paste(unique((CARTpruned$frame[,1])), collapse=".")
      x6CART <- ifelse(grepl("x6", splitvarCART)=="TRUE", 1, 0)
      
    
      CTree <- ctree (y~x1+x2+x3+x4+x5+x6,data = D, control = ctree_control(mincriterion = 0.95)) 
      leafCTree<- length(nodeids(CTree, terminal=TRUE))
      splitvarCTree <- names(varimp(CTree))
      numsplitvarCTree <- length(splitvarCTree)
      splitvarCTree <- paste(names(varimp(CTree)), collapse=".") 
      x6CTree <- ifelse(grepl("x6", splitvarCTree)=="TRUE", 1, 0) 
    
     
      RFmodel <- tuneMtryFast(formula = y~x1+x2+x3+x4+x5+x6, data = D, stepFactor = 1, improve = 0.05,
                              trace = TRUE, plot = TRUE, doBest = T, importance = "impurity", seed=i)
      newRFimp <- importance(RFmodel)
      
      ##############regression coefficients
      #best-specified regression
      Lmbest <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x1*x2 + x3*x4*x5, data = D)
      
      #saturated regression
      Overfit <- lm(y ~ (x1 + x2 + x3 + x4 + x5 + x6)^6, data = D)
      
      #main effects regression (non-intersectional)
      Lm_main <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data = D)
      
      
      ###############Cross classified
      crossclass <- aggregate(D$y, list(by=D$cluster), mean)
      colnames(crossclass) <- c("cluster","y")
      Dcrossclass <- join(Truth["cluster"], crossclass, by="cluster", type="left", match="first")
      #only merges for clusters that exist in D(:. won't have rows for clusters missing)
      
      ###############MLM coefficients
      MLM<- lmer(y ~ x1 + x2 +x3 + + x4 + x5  + x6 + (1|cluster), data=D)
    
      
      ##############intersection predictions
      create_cts_predictions <- function(model, abbrev) {
       
       Ipredy <- (model- Truth$y)
       
       names2 <- c(1:192)
       names(Ipredy) <- c(paste(names2, rep(abbrev, 192), sep="."))
       
       MSE_y <- mean((Ipredy)^2, na.rm=TRUE)
       names(MSE_y) <- c(paste("MSE", abbrev, sep="."))
      
       return(c(MSE_y, Ipredy))
       
       }
     
      CART.predict <- predict(CARTpruned, Truth, type="vector")
      CART.predict.results <- create_cts_predictions(CART.predict, "CART")
      
      CTree.predict <- predict(CTree, Truth, type="response")
      CTree.predict.results <- create_cts_predictions(CTree.predict, "CTree")
      
      RF.predict <- predict(RFmodel, Truth)$predictions
      RF.predict.results <- create_cts_predictions(RF.predict, "RF")
      
      BF.predict <- predict(Lmbest,Truth, se.fit=TRUE, interval = NULL)$fit
      BF.predict.results <- create_cts_predictions(BF.predict, "BF")
      
      OF.predict <- predict(Overfit, Truth, se.fit=TRUE, interval = NULL)$fit
      OF.predict.results <- create_cts_predictions(OF.predict, "OF")
      
      MF.predict <- predict(Lm_main,Truth, se.fit=TRUE, interval = NULL)$fit
      MF.predict.results <- create_cts_predictions(MF.predict, "MF")
      
      CC.predict <- Dcrossclass[[2]]
      CC.predict.results <- create_cts_predictions(CC.predict, "CC")
      
      MLM.predict1<- predict (MLM, newdata=Truth, allow.new.levels = TRUE)
      # allow.new.levels statement for any clusters not included. Therefore remove any predictions made with clusters we don't have info on 
      MLM.predict2 <- ifelse(is.na(Dcrossclass$y)=="TRUE", NA, MLM.predict1)
      MLM.predict.results <- create_cts_predictions(MLM.predict2, "MLM")
      
      #create a sum of NA clusters... these clusters aren't calculated for cross.class and MLM
      NAcluster <- sum(is.na(Dcrossclass[2]))
      
      MSEtable = c(i,NAcluster, CART.predict.results, CTree.predict.results, RF.predict.results, 
                   BF.predict.results, OF.predict.results, MF.predict.results, CC.predict.results, MLM.predict.results,
                   leafCART, x6CART, splitvarCART,
                   leafCTree, x6CTree , splitvarCTree,
                   newRFimp)
  }
  
  
  #then save the table to working directory
  write.csv(results,paste("Cts_cat", n, "csv", sep = "."))   
  
  }

  

################# Primary outcome presented by boxplots
  results2000<- read.csv ("T:/Cts_cat.2000.csv")
  results5000<- read.csv ("T:/Cts_cat.5000.csv")
  results50000<- read.csv ("T:/Cts_cat.50000.csv")
  results200000<- read.csv ("T:/Cts_cat.2e+05.csv")
  
  create_long_MSE <- function(n, results) {
    
    a <- cbind.data.frame(rep(n, 1000), rep("CART", 1000), results$MSE.CART)
    colnames(a) <- c ("n", "Method", "MSE") 
    
    b <- cbind.data.frame(rep(n, 1000), rep("CTree", 1000), results$MSE.CTree)
    colnames(b) <- c ("n", "Method", "MSE") 
    
    c <- cbind.data.frame(rep(n, 1000), rep("RF", 1000), results$MSE.RF)
    colnames(c) <- c ("n", "Method", "MSE") 
    
    d <- cbind.data.frame(rep(n, 1000), rep("BF", 1000), results$MSE.BF)
    colnames(d) <- c ("n", "Method", "MSE") 
    
    e <- cbind.data.frame(rep(n, 1000), rep("OF", 1000), results$MSE.OF)
    colnames(e) <- c ("n", "Method", "MSE") 
    
    f <- cbind.data.frame(rep(n, 1000), rep("CC", 1000), results$MSE.CC)
    colnames(f) <- c ("n", "Method", "MSE") 
    
    g <- cbind.data.frame(rep(n, 1000), rep("MLM", 1000), results$MSE.MLM)
    colnames(g) <- c ("n", "Method", "MSE")
    
    h <- cbind.data.frame(rep(n, 1000), rep("MF", 1000), results$MSE.MF)
    colnames(h) <- c ("n", "Method", "MSE")
    
    final <- rbind.data.frame(a, b, c, d, e, f, g, h)
    
  }
  
  MSE_2000   <- create_long_MSE(2000, results2000)
  MSE_5000   <- create_long_MSE(5000, results5000)
  MSE_50000  <- create_long_MSE(50000, results50000)
  MSE_200000 <- create_long_MSE(200000, results200000)
  
  final_primary_outcome <- rbind.data.frame(MSE_2000, MSE_5000, MSE_50000, MSE_200000)
  
  
  library(ggplot2)
  
  
  p2 <- ggplot(final_primary_outcome, aes(x=n, y=MSE, group = interaction(n, Method), fill=Method)) + geom_boxplot(outlier.shape=NA, width=1) +
    scale_y_continuous(limits = c(0, 1.25)) + scale_x_continuous(trans='log10', breaks=c(2000, 5000, 50000, 200000), labels=scales::comma)
  
  p_colour <- p2 + scale_fill_manual( values=c("#56B4E9","#F0E442", "#E69F00",  
                                               "#009E73", "#999999","#D55E00", 
                                               "#CC79A7",  "#0072B2"),
                                      labels= c("Regression (correctly specified)", "CART", "Cross-classification", 
                                                "CTree", "Regression (main effects - non-intersectional)","MAIHDA", 
                                                "Regression (saturated)",  "Random forest"),
                                      guide = guide_legend(nrow=2, byrow=TRUE))
  
  p_final <- p_colour + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                              panel.background = element_blank(), axis.line = element_line(colour = "black"),
                              legend.direction = "horizontal", legend.position = c(0.5,0.95))
  
  
  
  png(filename = "Cat_inputs.png",width = 8, height = 4, units = 'in', res=300)
  p_final
  dev.off()
  
  
  #Number of NA cluster's
  estimateinterval <- function(varlist) {
    mean <- apply(varlist, 2, mean)
    LCI <- apply(varlist, 2,quantile, probs=c(0.025), na.rm=TRUE) 
    UCI <- apply(varlist, 2,quantile, probs=c(0.975), na.rm=TRUE)
    return(as.data.frame(rbind(mean, LCI, UCI)))
  }
  
  NAintersections <- cbind(estimateinterval(results2000[3]), estimateinterval(results5000[3]),
                           estimateinterval(results50000[3]), estimateinterval(results200000[3]))
  

################# Secondary outcome (variable relevance)
  
  a_list <- c( "T:/Cts_cat.2000.csv",
               "T:/Cts_cat.5000.csv",
               "T:/Cts_cat.50000.csv",
               "T:/Cts_cat.2e+05.csv")
  
  
  output_ML_final <- NULL
  
  for (a in a_list)  {
    
      results <- read.csv(a)
      
      estimateinterval <- function(varlist) {
        mean <- apply(varlist, 2, mean)
        LCI <- apply(varlist, 2,quantile, probs=c(0.025), na.rm=TRUE) 
        UCI <- apply(varlist, 2,quantile, probs=c(0.975), na.rm=TRUE)
        return(as.data.frame(rbind(mean, LCI, UCI)))
      }
    
      #CART
      CART  <- estimateinterval(results[c(1548,1549)])
      
      #To ID as true or FALSE the splitvarCART
      results$splitx1 <- ifelse(grepl("x1", results[,1550])==TRUE, 1, 0)
      results$splitx2 <- ifelse(grepl("x2", results[,1550])==TRUE, 1, 0)
      results$splitx3 <- ifelse(grepl("x3", results[,1550])==TRUE, 1, 0)
      results$splitx4 <- ifelse(grepl("x4", results[,1550])==TRUE, 1, 0)
      results$splitx5 <- ifelse(grepl("x5", results[,1550])==TRUE, 1, 0)
      
      CARTx1_to_x5 <- estimateinterval(results[,c("splitx1", "splitx2", "splitx3", "splitx4", "splitx5")])
      
      #CTree
      CTree  <- estimateinterval(results[c(1551,1552)])
      
      #To ID as true or FALSE the splitvarCTree
      results$splitx1 <- ifelse(grepl("x1", results[,1553])==TRUE, 1, 0)
      results$splitx2 <- ifelse(grepl("x2", results[,1553])==TRUE, 1, 0)
      results$splitx3 <- ifelse(grepl("x3", results[,1553])==TRUE, 1, 0)
      results$splitx4 <- ifelse(grepl("x4", results[,1553])==TRUE, 1, 0)
      results$splitx5 <- ifelse(grepl("x5", results[,1553])==TRUE, 1, 0)
      
      CTreex1_to_x5 <- estimateinterval(results[,c("splitx1", "splitx2", "splitx3", "splitx4", "splitx5")])
      
      #RF
      RF<- estimateinterval(results[c(1554:1559)])
      
    #save results
      output_ML <- cbind(CART, CARTx1_to_x5, CTree, CTreex1_to_x5 ,RF)
      output_ML_final <- rbind(output_ML_final, output_ML) 
    }
  
  #then save the table to working directory
  colnames(output_ML_final) <- c("CART_leaves", "CART_x6",
                                 "CART_x1", "CART_x2", "CART_x3", "CART_x4", "CART_x5",
                                 "CTree_leaves", "CTree_x6", 
                                 "CTree_x1", "CTree_x2", "CTree_x3", "CTree_x4", "CTree_x5",
                                 "RF_x1", "RF_x2", "RF_x3", "RF_x4", "RF_x5", "RF_x6") 
  write.csv(output_ML_final,paste("Cts_cat_ML_results", "csv", sep = "."))  
  
  
  
