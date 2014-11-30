
best <- function(state, outcome) {
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
                
                outcome_2$lower_mortality_estimate___hospital_30_day_death__mortality__rates_from_heart_failure<-as.numeric(outcome_2$lower_mortality_estimate___hospital_30_day_death__mortality__rates_from_heart_failure)
                
                outcome_2<-outcome_2[ outcome_2$state==state,]
                
                arrange(outcome_2,lower_mortality_estimate___hospital_30_day_death__mortality__rates_from_heart_failure)$hospital_name[[1]]
        }
        else if (outcome=="heart attack")
        {
                
                
                outcome_2$lower_mortality_estimate___hospital_30_day_death__mortality__rates_from_heart_attack<-as.numeric(outcome_2$lower_mortality_estimate___hospital_30_day_death__mortality__rates_from_heart_attack)
                
                outcome_2<-outcome_2[ outcome_2$state==state,]
                
                arrange(outcome_2,lower_mortality_estimate___hospital_30_day_death__mortality__rates_from_heart_attack)$hospital_name[[1]]
        }
        else if (outcome=="pneumonia")
        {
                
                outcome_2$lower_mortality_estimate___hospital_30_day_death__mortality__rates_from_pneumonia<-as.numeric(outcome_2$lower_mortality_estimate___hospital_30_day_death__mortality__rates_from_pneumonia)
                
                outcome_2<-outcome_2[ outcome_2$state==state,]
                
                arrange(outcome_2,lower_mortality_estimate___hospital_30_day_death__mortality__rates_from_pneumonia)$hospital_name[[1]] 
        }
       
}