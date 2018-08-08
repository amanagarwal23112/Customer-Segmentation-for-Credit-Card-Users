# Import File
customer <- read.csv("CC GENERAL.csv")
View(customer)
str(customer)
names(customer)

#Missing Values
customer$MINIMUM_PAYMENTS[is.na(customer$MINIMUM_PAYMENTS)] <- 864.2065423
customer$CREDIT_LIMIT [is.na(customer$CREDIT_LIMIT)] <- 4494.4494504

# Advanced KPIs

customer$monthly_avg_purchase_amt <- customer$PURCHASES/12
customer$monthly_avg_cash_amt <- customer$CASH_ADVANCE/12

customer$purchase_type <- as.numeric(ifelse(customer$ONEOFF_PURCHASES == 0 & customer$INSTALLMENTS_PURCHASES == 0,4,
                                  ifelse(customer$INSTALLMENTS_PURCHASES == 0,2,
                                         ifelse(customer$INSTALLMENTS_PURCHASES != 0 & customer$ONEOFF_PURCHASES != 0,3,
                                                ifelse(customer$ONEOFF_PURCHASES == 0,1,"")))))

customer$avg_amt_purchase <- ifelse(customer$PURCHASES_TRX == 0 ,0,customer$PURCHASES/customer$PURCHASES_TRX)

customer$avg_amt_cashadvance <- ifelse(customer$CASH_ADVANCE_TRX == 0,0,customer$CASH_ADVANCE/customer$CASH_ADVANCE_TRX)
customer$limit_usuage <- customer$BALANCE/customer$CREDIT_LIMIT
customer$payment_min_pay_ratio <- customer$PAYMENTS/customer$MINIMUM_PAYMENTS

#write.csv(customer,"new_cust.csv")
###############################################################################################

#Factor Analysis

corrm<- cor(customer[,c(2:17)])
View(corrm)

eigen(corrm)$values             # EIGEN VALUES
require(psych)
require(GPArotation)

scree(corrm, factors=T, pc=T, main="scree plot", hline=NULL, add=FALSE)         ### SCREE PLOT

require(dplyr)
eigen_values <- mutate(data.frame(eigen(corrm)$values)
                       ,cum_sum_eigen=cumsum(eigen.corrm..values)
                       , pct_var=eigen.corrm..values/sum(eigen.corrm..values)
                       , cum_pct_var=cum_sum_eigen/sum(eigen.corrm..values))

View(eigen_values)
write.csv(eigen_values,"eigen.csv")

FA<-fa(r=corrm, 6, rotate="varimax", fm="ml")               ### CONDUCTING FACTOR ANALYSIS
print(FA)
FA_SORT<-fa.sort(FA)
ls(FA_SORT)                                                  ### LISTING OUT THE OBJECTS
FA_SORT$loadings
Loadings<- data.frame(FA_SORT$loadings[,])                   ### CAPTURING ONLY LOADINGS INTO DATA FRAME
View(Loadings)
write.csv(Loadings,"loadings.csv")

###########################################################################################################

################################ Cluster Analysis##########################################################

#Descriptive Analysis
mystats <- function(x) {
  nmiss<-sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  n <- length(a)
  s <- sd(a)
  min <- min(a)
  uc = m + 3*s
  lc = m - 3*s
  pctls<-quantile(a,probs=c(0.01, 0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99))
  max <- max(a)
  return(c(n=n, nmiss=nmiss,  mean=m, stdev=s,min = min, pctls=pctls,max=max, uc=uc, lc=lc))
}

vars <- c("BALANCE","BALANCE_FREQUENCY","PURCHASES","ONEOFF_PURCHASES","INSTALLMENTS_PURCHASES","CASH_ADVANCE","PURCHASES_FREQUENCY","ONEOFF_PURCHASES_FREQUENCY","PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_FREQUENCY","CASH_ADVANCE_TRX","PURCHASES_TRX","CREDIT_LIMIT","PAYMENTS","MINIMUM_PAYMENTS","PRC_FULL_PAYMENT","TENURE","monthly_avg_purchase_amt","monthly_avg_cash_amt","avg_amt_purchase","avg_amt_cashadvance","limit_usuage","payment_min_pay_ratio","purchase_type")


customer_stats<-t(data.frame(apply(customer[vars], 2, mystats)))
View(customer_stats)
write.csv(customer_stats, "cust.csv")

#Outliers - Lower Limit
customer$BALANCE[customer$BALANCE < 8.81451835] <- 8.81451835
customer$BALANCE_FREQUENCY[customer$BALANCE_FREQUENCY < 0.272727] <- 0.272727
customer$CREDIT_LIMIT[customer$CREDIT_LIMIT < 1000] <- 1000
customer$PAYMENTS[customer$PAYMENTS < 89.98892395] <- 89.98892395
customer$MINIMUM_PAYMENTS[customer$MINIMUM_PAYMENTS < 73.2820058] <- 73.2820058
customer$TENURE[customer$TENURE < 8] <- 8
customer$limit_usuage[customer$limit_usuage < 0.0029429831875] <- 0.0029429831875
customer$payment_min_pay_ratio[customer$payment_min_pay_ratio < 0.257304343448113] <- 0.257304343448113

#Outliers - Upper Limit
customer$BALANCE[customer$BALANCE > 5909.11180785] <- 5909.11180785
customer$PURCHASES[customer$PURCHASES > 3998.6195] <- 3998.6195
customer$ONEOFF_PURCHASES[customer$ONEOFF_PURCHASES > 2671.09399999999] <- 2671.09399999999
customer$INSTALLMENTS_PURCHASES[customer$INSTALLMENTS_PURCHASES > 1750.0875] <- 1750.0875
customer$CASH_ADVANCE[customer$CASH_ADVANCE > 4647.16912199999] <- 4647.16912199999
customer$PAYMENTS[customer$PAYMENTS > 6082.09059525] <- 6082.09059525
customer$MINIMUM_PAYMENTS[customer$MINIMUM_PAYMENTS > 2766.56331] <- 2766.56331
customer$PURCHASES_TRX[customer$PURCHASES_TRX > 57] <- 57
customer$CREDIT_LIMIT[customer$CREDIT_LIMIT > 12000] <- 12000
customer$monthly_avg_purchase_amt[customer$monthly_avg_purchase_amt > 333.218291666667] <- 333.218291666667
customer$monthly_avg_cash_amt[customer$monthly_avg_cash_amt > 387.264093499999] <- 387.264093499999
customer$avg_amt_purchase[customer$avg_amt_purchase > 228.210785714285] <- 228.210785714285
customer$avg_amt_cashadvance[customer$avg_amt_cashadvance > 926.7271481125] <- 926.7271481125
customer$limit_usuage[customer$limit_usuage > 0.966685554172222] <- 0.966685554172222
customer$payment_min_pay_ratio[customer$payment_min_pay_ratio > 20.9041942630091] <- 20.9041942630091

vars1 <- c("BALANCE","ONEOFF_PURCHASES","CASH_ADVANCE","MINIMUM_PAYMENTS","PURCHASES_INSTALLMENTS_FREQUENCY","CASH_ADVANCE_TRX","CREDIT_LIMIT","PAYMENTS","ONEOFF_PURCHASES_FREQUENCY")

inputdata_final <-customer[vars1]
View(inputdata_final)

#Prepare final Data
#standardizing the data

inputdata_final = scale(inputdata_final)
View(inputdata_final)

#building clusters using k-means clustering 

cluster_three <- kmeans(inputdata_final,3)
cluster_four <- kmeans(inputdata_final,4)
cluster_five <- kmeans(inputdata_final,5)
cluster_six <- kmeans(inputdata_final,6)

customer_new<-cbind(customer,km_clust_3=cluster_three$cluster,km_clust_4=cluster_four$cluster,km_clust_5=cluster_five$cluster ,km_clust_6=cluster_six$cluster   )
View(customer_new)

#Graph based on k-means
require(cluster)

clusplot(inputdata_final, 
         cluster_five$cluster,
         color = TRUE,
         lines =6,
         labels = 2
)

###Profiling

#Converting into factors
customer_new$km_clust_3=factor(customer_new$km_clust_3)
customer_new$km_clust_4=factor(customer_new$km_clust_4)
customer_new$km_clust_5=factor(customer_new$km_clust_5)
customer_new$km_clust_6=factor(customer_new$km_clust_6)

require(tables)
profile<-tabular(1+BALANCE+BALANCE_FREQUENCY+PURCHASES+ONEOFF_PURCHASES+INSTALLMENTS_PURCHASES+CASH_ADVANCE+PURCHASES_FREQUENCY+ONEOFF_PURCHASES_FREQUENCY+PURCHASES_INSTALLMENTS_FREQUENCY+CASH_ADVANCE_FREQUENCY+CASH_ADVANCE_TRX+PURCHASES_TRX+CREDIT_LIMIT+PAYMENTS+MINIMUM_PAYMENTS+PRC_FULL_PAYMENT+TENURE+monthly_avg_purchase_amt+monthly_avg_cash_amt+avg_amt_purchase+avg_amt_cashadvance+limit_usuage+payment_min_pay_ratio+purchase_type~
                   mean+(mean*km_clust_3)+(mean*km_clust_4)+(mean*km_clust_5)+(mean*km_clust_6),
                 data=customer_new)
profile1<-as.matrix(profile)
profile1<-data.frame(profile1)
View(profile1)
write.csv(profile1,"profile1.csv")


profile<-tabular(1~length+(length*km_clust_3)+(length*km_clust_4)+(length*km_clust_5)+(length*km_clust_6),
                 data=customer_new)
profile2<-as.matrix(profile)
profile2<-data.frame(profile2)
View(profile2)
write.csv(profile2,"profile2.csv")

############################################################################################################################
