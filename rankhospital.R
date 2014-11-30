rankhospital <- function(state, outcome, num = "best") {
        outcome_2<-tbl_df(out_come)
        
        
        
        type<-c("heart attack", "heart failure",  "pneumonia")
        
       
        
        if (length(grep(state,outcome_2$state))==0 )
        {
                stop("invalid state")
        }
        else if (length(grep(outcome,type))==0)
        {
                stop("invalid outcome")
                
        }
        else if (outcome=="heart failure")
        {
                
                outcome_2<-outcome_2[ outcome_2$state==state,]
                
                if (num=="best")
                {
                        num=1
                }
                else if
                (num=="worst")
                {
                        num<-nrow(outcome_2[complete.cases(outcome_2),])
                }
                if (num>nrow(outcome_2[complete.cases(outcome_2),]))
                {
                        NA
                }
                else
                {
                arrange(outcome_2,hospital_30_day_death__mortality__rates_from_heart_failure,hospital_name)$hospital_name[[num]]
                }
        }
        else if (outcome=="heart attack")
        {
               
                outcome_2<-outcome_2[ outcome_2$state==state,]
                if (num=="best")
                {
                        num=1
                }
                else if
                (num=="worst")
                {
                        num<-nrow(outcome_2[complete.cases(outcome_2),])
                }
                
                if (num>nrow(outcome_2[complete.cases(outcome_2),]))
                {
                         NA
                }
                else
                {
                        arrange(outcome_2,hospital_30_day_death__mortality__rates_from_heart_attack,hospital_name)$hospital_name[[num]]
                        
                }
                
               
        }
        else if (outcome=="pneumonia")
        {
                
              
                
                outcome_2<-outcome_2[ outcome_2$state==state,]
                if (num=="best")
                {
                        num=1
                }
                else if
                (num=="worst")
                {
                        num<-nrow(outcome_2[complete.cases(outcome_2),])
                }
                if (num>nrow(outcome_2[complete.cases(outcome_2),]))
                {
                        NA
                }
                else
                {
                arrange(outcome_2,hospital_30_day_death__mortality__rates_from_pneumonia,hospital_name)$hospital_name[[num]]
                }
        }
        
}

