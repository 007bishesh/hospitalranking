rankall <- function(outcome, num = "best") {
        
        outcome_2<-tbl_df(out_come)
        
        states <-unique(outcome_2$state)
        type<-c("heart attack", "heart failure",  "pneumonia")
        
        if (length(grep(outcome,type))==0)
        {
                stop("invalid outcome")
                
        }
        else if (outcome=="heart failure")
        {
                fg<-select(outcome_2,hospital_name,state,hospital_30_day_death__mortality__rates_from_heart_failure)
                
                fg<-fg[complete.cases(fg),]
                
                z <- fg[with(fg, order(state, hospital_30_day_death__mortality__rates_from_heart_failure,hospital_name)),]
                
                zrank <- within(z , rank <-ave(hospital_30_day_death__mortality__rates_from_heart_failure, state, FUN=function(x)order(x)))
                
                zrank$rank<-as.numeric(zrank$rank)
                
                if (num=="best")
                {
                        bcd<-select(zrank[zrank$rank==1,],hospital_name,state)
                        
                        
                        for(i in states){
                                if (length(grep(i,bcd$state))==0){
                                        bcd<-rbind(bcd,c("<NA>",i))       
                                }
                                
                                
                        }
                        colnames(bcd) <- c("hospital","state")
                        arrange(bcd,state)
                }
                else if
                (num=="worst")
                {
                        val<-sqldf("select state,max(rank) as rank from zrank group by state")
                        
                        cde<-list()
                        for (i in 1:nrow(val)){
                                
                                check<-val[i,]
                                cde<-rbind(cde,select(zrank[zrank$rank==check$rank & zrank$state==check$state,],hospital_name,state))
                        }
                        
                        
                        
                        for(i in states){
                                if (length(grep(i,bcd$state))==0){
                                        cde<-rbind(cde,c("<NA>",i))       
                                }
                                
                                
                        }
                        
                        
                        colnames(cde) <- c("hospital","state")
                        arrange(cde,state)
                }
                else
                {
                        bcd<-select(zrank[zrank$rank==num,],hospital_name,state)
                        
                        
                        for(i in states){
                                if (length(grep(i,bcd$state))==0){
                                        bcd<-rbind(bcd,c("<NA>",i))       
                                }
                                
                                
                        }
                        colnames(bcd) <- c("hospital","state")
                        arrange(bcd,state)
                }
                
        }
        else if (outcome=="heart attack")
        {
                
                fg<-select(outcome_2,hospital_name,state,hospital_30_day_death__mortality__rates_from_heart_attack)
                
                z <- fg[with(fg, order(state, hospital_30_day_death__mortality__rates_from_heart_attack,hospital_name)),]
                
                zrank <- within(z , rank <-ave(hospital_30_day_death__mortality__rates_from_heart_attack, state, FUN=function(x)order(x)))
                
                
                
                
                if (num=="best")
                {
                        bcd<-select(zrank[zrank$rank==1,],hospital_name,state)
                        
                        
                        for(i in states){
                                if (length(grep(i,bcd$state))==0){
                                        bcd<-rbind(bcd,c("<NA>",i))       
                                }
                                
                                
                        }
                        colnames(bcd) <- c("hospital","state")
                        arrange(bcd,state)
                        
                }
                else if
                (num=="worst")
                {
                        val<-sqldf("select state,max(rank) as rank from zrank group by state")
                        
                        cde<-list()
                        for (i in 1:nrow(val)){
                                
                                check<-val[i,]
                                cde<-rbind(cde,select(zrank[zrank$rank==check$rank & zrank$state==check$state,],hospital_name,state))
                        }
                        
                        
                        
                        for(i in states){
                                if (length(grep(i,bcd$state))==0){
                                        cde<-rbind(cde,c("<NA>",i))       
                                }
                                
                                
                        }
                        
                        
                        colnames(cde) <- c("hospital","state")
                        arrange(cde,state)
                }
                else
                {
                        bcd<-select(zrank[zrank$rank==num,],hospital_name,state)
                        
                        
                        for(i in states){
                                if (length(grep(i,bcd$state))==0){
                                        bcd<-rbind(bcd,c("<NA>",i))       
                                }
                                
                                
                        }
                        colnames(bcd) <- c("hospital","state")
                        arrange(bcd,state)
                }
                
                
                
                
                
        }
        else if (outcome=="pneumonia")
        {
                
                fg<-select(outcome_2,hospital_name,state,hospital_30_day_death__mortality__rates_from_pneumonia)
                
                fg<-fg[complete.cases(fg),]
               
                z <- fg[with(fg, order(state, hospital_30_day_death__mortality__rates_from_pneumonia,hospital_name)),]
                
                zrank <- within(z , rank <-ave(hospital_30_day_death__mortality__rates_from_pneumonia, state, FUN=function(x)order(x)))
                
                zrank$rank<-as.numeric(zrank$rank)
                
                
                
                if (num=="best")
                {
                        bcd<-select(zrank[zrank$rank==1,],hospital_name,state)
                        
                        
                        for(i in states){
                                if (length(grep(i,bcd$state))==0){
                                        bcd<-rbind(bcd,c("<NA>",i))       
                                }
                                
                                
                        }
                        colnames(bcd) <- c("hospital","state")
                        arrange(bcd,state)
                        
                }
                else if
                (num=="worst")
                {
                        
                      
                        val<-sqldf("select state,max(rank) as rank from zrank group by state")
                        
                        cde<-list()
                        for (i in 1:nrow(val)){
                               
                             check<-val[i,]
                             cde<-rbind(cde,select(zrank[zrank$rank==check$rank & zrank$state==check$state,],hospital_name,state))
                        }
                        
                        
                       
                        for(i in states){
                                if (length(grep(i,bcd$state))==0){
                                        cde<-rbind(cde,c("<NA>",i))       
                                }
                                
                                
                        }
                        
                        
                        colnames(cde) <- c("hospital","state")
                        arrange(cde,state)
                }
                else
                {
                        bcd<-select(zrank[zrank$rank==num,],hospital_name,state)
                        
                        
                        for(i in states){
                                if (length(grep(i,bcd$state))==0){
                                        bcd<-rbind(bcd,c("<NA>",i))       
                                }
                                
                                
                        }
                        colnames(bcd) <- c("hospital","state")
                        arrange(bcd,state)
                }
               
        }
        
}

library(D:\Coursera\data\Rpackages\swirl)

