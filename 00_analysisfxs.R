# ------------------------------------------------------------------------------ #
#
#      Project: Transporting when trial activities impact adherence
#       Author: Rachael Ross
#
#      Purpose: Cleaning TEDS-A for sensitivity analysis project
# 
# ------------------------------------------------------------------------------ #

# Nuisance models ----

## To estimate model ----
estnuisance <- function(data,ml,vars,out){
  if(ml==TRUE){
    data |>
      dplyr::select(all_of(vars),all_of(out)) %>%
      mlr3superlearner(
        data = .,
        discrete = FALSE,
        target = out,
        library = lib,
        outcome_type = "binomial",
        info=FALSE)
  }else{
    glm(reformulate(vars, response = out), 
        data=data, family='binomial')
  }
}

## To filter data for stratified model ----
fitmodins <- function(data,ml,vars,out){
  df <- data %>%
    filter(s==1) 
  estnuisance(df,ml,vars,out)
}

fitmodins_strat_a <- function(data,anum,ml,vars,out){
  df <- data %>%
    filter(s==1,a==anum) 
  estnuisance(df,ml,vars,out)
}

fitmodins_strat_az <- function(data,anum,znum,ml,vars,out){
  df <- data %>%
    filter(s==1,a==anum,z==znum) 
  estnuisance(df,ml,vars,out)
}

# Predictions ----

## To get predictions ----
getpred <- function(data,model){
  data %>%
    mutate(pred = predict(model, newdat = ., type="response")) |>
    select(id,pred)
}

## To alter data for predictions from pooled model ----
getpredalta <- function(data,model,anum){
  df <- data %>%
    mutate(a=anum) 
  getpred(df,model)
}

getpredaltz <- function(data,model,znum){
  df <- data %>%
    mutate(z=znum) 
  getpred(df,model)
}

getpredaltaz <- function(data,model,anum,znum){
  df <- data %>%
    mutate(a=anum,z=znum) 
  getpred(df,model)
}


# To create sample splitting ----
createss <- function(data,seed,folds){
  
  set.seed(seed)
  n <- nrow(data) 
  
  # Create dataset with a variable identifying split
  splits <- tibble(id = data$id,
                   split = sample(seq(1:folds), n, replace=TRUE)) |>
    left_join(data, by="id")
  
  # Create splits
  tibble(
    formodfit = if(folds==1){ # data to be used for model fitting
      list(splits)
    }else{
      map(seq(1:folds), function(x,y) y[y$split!=x,], splits)},
    
    forpredict = if(folds==1){ # data to be used for prediction
      list(splits)
    }else{
      map(seq(1:folds), function(x,y) y[y$split==x,], splits)})
}


# To fit nuisance models ----
getpredictions <- function(datalist,
                           strat_a=TRUE,strat_z=TRUE,ml,
                           outvar_new,outvar_old,adhvar,trtvar,selvar){
  
  # Outcome models for new estimator
  if(strat_a==TRUE & strat_z==TRUE){ #stratified by both a and z
    outpreds <- datalist %>%
      mutate(
        outmod0_z1 = future_map(formodfit,fitmodins_strat_az,anum=0,znum=1,ml=ml,vars=outvar_new,out="y",.options=furrr_options(seed = TRUE)),
        outmod0_z0 = future_map(formodfit,fitmodins_strat_az,anum=0,znum=0,ml=ml,vars=outvar_new,out="y",.options=furrr_options(seed = TRUE)),
        outmod1_z1 = future_map(formodfit,fitmodins_strat_az,anum=1,znum=1,ml=ml,vars=outvar_new,out="y",.options=furrr_options(seed = TRUE)),
        outmod1_z0 = future_map(formodfit,fitmodins_strat_az,anum=1,znum=0,ml=ml,vars=outvar_new,out="y",.options=furrr_options(seed = TRUE)),
        yhat_01    = future_map2(forpredict, outmod0_z1, getpred,.options=furrr_options(seed = TRUE)),
        yhat_00    = future_map2(forpredict, outmod0_z0, getpred,.options=furrr_options(seed = TRUE)),
        yhat_11    = future_map2(forpredict, outmod1_z1, getpred,.options=furrr_options(seed = TRUE)),
        yhat_10    = future_map2(forpredict, outmod1_z0, getpred,.options=furrr_options(seed = TRUE))) %>%
      dplyr::select(contains("hat")) 
  }else if(strat_a==TRUE & strat_z==FALSE){ # stratified by a only
    outpreds <- datalist %>%
      mutate(
        outmod0    = future_map(formodfit,fitmodins_strat_a,anum=0,ml=ml,vars=outvar_new,out="y",.options=furrr_options(seed = TRUE)),
        outmod1    = future_map(formodfit,fitmodins_strat_a,anum=1,ml=ml,vars=outvar_new,out="y",.options=furrr_options(seed = TRUE)),
        yhat_01    = future_map2(forpredict, outmod0, getpredaltz,znum=1,.options=furrr_options(seed = TRUE)),
        yhat_00    = future_map2(forpredict, outmod0, getpredaltz,znum=0,.options=furrr_options(seed = TRUE)),
        yhat_11    = future_map2(forpredict, outmod1, getpredaltz,znum=1,.options=furrr_options(seed = TRUE)),
        yhat_10    = future_map2(forpredict, outmod1, getpredaltz,znum=0,.options=furrr_options(seed = TRUE))) %>%
      dplyr::select(contains("hat")) 
  }else{ # pooled outcome model
    outpreds <- datalist %>%
      mutate(
        outmod     = future_map(formodfit,estnuisance,ml=ml,vars=outvar_new,out="y",.options=furrr_options(seed = TRUE)),
        yhat_01    = future_map2(forpredict, outmod, getpredaltaz,anum=0,znum=1,.options=furrr_options(seed = TRUE)),
        yhat_00    = future_map2(forpredict, outmod, getpredaltaz,anum=0,znum=0,.options=furrr_options(seed = TRUE)),
        yhat_11    = future_map2(forpredict, outmod, getpredaltaz,anum=1,znum=1,.options=furrr_options(seed = TRUE)),
        yhat_10    = future_map2(forpredict, outmod, getpredaltaz,anum=1,znum=0,.options=furrr_options(seed = TRUE))) %>%
      dplyr::select(contains("hat")) 
  }
  print("Outmodels for new estimator done")
  
  yhat_01    = reduce(outpreds$yhat_01, rbind) |> mutate(pred=as.numeric(pred)) |> rename(yhat_01=pred)
  yhat_00    = reduce(outpreds$yhat_00, rbind) |> mutate(pred=as.numeric(pred)) |> rename(yhat_00=pred)
  yhat_11    = reduce(outpreds$yhat_11, rbind) |> mutate(pred=as.numeric(pred)) |> rename(yhat_11=pred)
  yhat_10    = reduce(outpreds$yhat_10, rbind) |> mutate(pred=as.numeric(pred)) |> rename(yhat_10=pred)
  print("Predictions done")
  
  # Outcome models for old estimator
  if(strat_a==TRUE){ # stratified by a 
    outpreds <- datalist %>%
      mutate(
        outmod0    = future_map(formodfit,fitmodins_strat_a,anum=0,ml=ml,vars=outvar_old,out="y",.options=furrr_options(seed = TRUE)),
        outmod1    = future_map(formodfit,fitmodins_strat_a,anum=1,ml=ml,vars=outvar_old,out="y",.options=furrr_options(seed = TRUE)),
        yhat_0    = future_map2(forpredict, outmod0, getpred,.options=furrr_options(seed = TRUE)),
        yhat_1    = future_map2(forpredict, outmod1, getpred,.options=furrr_options(seed = TRUE))) %>%
      dplyr::select(contains("hat")) 
  }else{ # pooled outcome model
    outpreds <- datalist %>%
      mutate(
        outmod    = future_map(formodfit,estnuisance,ml=ml,vars=outvar_old,out="y",.options=furrr_options(seed = TRUE)),
        yhat_0    = future_map2(forpredict, outmod, getpredalta,anum=0,.options=furrr_options(seed = TRUE)),
        yhat_1    = future_map2(forpredict, outmod, getpredalta,anum=1,.options=furrr_options(seed = TRUE))) %>%
      dplyr::select(contains("hat")) 
  }
  print("Outmodels for old estimator done")
  
  yhat_0    = reduce(outpreds$yhat_0, rbind) |> mutate(pred=as.numeric(pred)) |> rename(yhat_0=pred)
  yhat_1    = reduce(outpreds$yhat_1, rbind) |> mutate(pred=as.numeric(pred)) |> rename(yhat_1=pred)
  print("Predictions done")
  
  # Adherence models
  if(strat_a==TRUE){ # stratified by a
    adhpreds <- datalist %>%
      mutate(
        adhmod0    = future_map(formodfit,fitmodins_strat_a,anum=0,ml=ml,vars=adhvar,out="z",.options=furrr_options(seed = TRUE)),
        adhmod1    = future_map(formodfit,fitmodins_strat_a,anum=1,ml=ml,vars=adhvar,out="z",.options=furrr_options(seed = TRUE)),
        z1hat_0    = future_map2(forpredict, adhmod0, getpred,.options=furrr_options(seed = TRUE)),
        z1hat_1    = future_map2(forpredict, adhmod1, getpred,.options=furrr_options(seed = TRUE))) %>%
      dplyr::select(contains("hat"))
  }else{ # pooled
    adhpreds <- datalist %>%
      mutate(
        adhmod     = future_map(formodfit,estnuisance,ml=FALSE,vars=adhvar,out="z",.options=furrr_options(seed = TRUE)),
        z1hat_0    = future_map2(forpredict, adhmod, getpredalta,anum=0,.options=furrr_options(seed = TRUE)),
        z1hat_1    = future_map2(forpredict, adhmod, getpredalta,anum=1,.options=furrr_options(seed = TRUE))) %>%
      dplyr::select(contains("hat"))
  }
  print("Adherence models done")
  
  z1hat_0    = reduce(adhpreds$z1hat_0, rbind) |> mutate(pred=as.numeric(pred)) |> rename(z1hat_0=pred)
  z1hat_1    = reduce(adhpreds$z1hat_1, rbind) |> mutate(pred=as.numeric(pred)) |> rename(z1hat_1=pred)
  print("Predictions done")
  
  # Treatment and selection models
  othpreds <- datalist %>%
    mutate(trtmod     = future_map(formodfit,fitmodins,ml=FALSE,vars=trtvar,out="a",.options=furrr_options(seed = TRUE)),
           selectmod  = future_map(formodfit,estnuisance,ml=FALSE,vars=selvar,out="s",.options=furrr_options(seed = TRUE)),
           a1hat      = future_map2(forpredict, trtmod, getpred,.options=furrr_options(seed = TRUE)),
           s1hat      = future_map2(forpredict, selectmod, getpred,.options=furrr_options(seed = TRUE))) %>%
    dplyr::select(contains("hat")) 
  print("Treatment and selection models done")
  
  a1hat      = reduce(othpreds$a1hat, rbind) |> mutate(pred=as.numeric(pred)) |> rename(a1hat=pred)
  s1hat      = reduce(othpreds$s1hat, rbind) |> mutate(pred=as.numeric(pred)) |> rename(s1hat=pred)
  print("Predictions done")
  
  with <- reduce(list(yhat_01, yhat_00, yhat_11, yhat_10,
                      yhat_0, yhat_1,
                      z1hat_0, z1hat_1,
                      a1hat,
                      s1hat
  ), 
  ~left_join(.x,.y, by=c("id")))
  
  return(with) 
} 

# Estimators ----

newestimator <- function(data,delta0=1,delta1=1){
  data |>
    mutate(est0 = (1-a)*s*(1-s1hat)/((1-a1hat)*s1hat)*((z*delta0 +
                                                          (1-z)*(1-z1hat_0*delta0)/(1-z1hat_0))*(y - yhat_00) - 
                                                         z1hat_0*delta0*(yhat_01-yhat_00)) + 
             (1-s)*(yhat_01*z1hat_0*delta0 + yhat_00*(1-z1hat_0*delta0)),
           est1 = a*s*(1-s1hat)/(a1hat*s1hat)*((z*delta1 + 
                                                  (1-z)*(1-z1hat_1*delta1)/(1-z1hat_1))*(y - yhat_10) - 
                                                 z1hat_1*delta1*(yhat_11-yhat_10)) + 
             (1-s)*(yhat_11*z1hat_1*delta1 + yhat_10*(1-z1hat_1*delta1)))
}


oldestimator <- function(data){
  data |>
    mutate(est0 = (1-a)*s*(1-s1hat)/((1-a1hat)*s1hat)*(y - yhat_0) + (1-s)*yhat_0,
           est1 = a*s*(1-s1hat)/(a1hat*s1hat)*(y - yhat_1) + (1-s)*yhat_1)
}

trialestimator <- function(data){
  data |>
    filter(s==1) |>
    mutate(est0 = (1-a)/(1-a1hat)*(y - yhat_0) + yhat_0,
           est1 = a/(a1hat)*(y - yhat_1) + yhat_1)
}

 
## To get points estimates/CIs ----
getresults <- function(data){
  tibble(r1=sum(data$est1)/sum(1-data$s),
         r1_se=sqrt(var(data$est1/mean(1-data$s),na.rm=TRUE)/nrow(data)),
         r1_lcl = r1 - 1.96*r1_se,
         r1_ucl = r1 + 1.96*r1_se,
         r0=sum(data$est0)/sum(1-data$s), #=mean(data$est_0/mean(1-data$s)),
         r0_se=sqrt(var(data$est0/mean(1-data$s),na.rm=TRUE)/nrow(data)), #=sqrt(var(data$est_0,na.rm=TRUE)/nrow(data))/mean(1-data$s),
         r0_lcl = r0 - 1.96*r0_se,
         r0_ucl = r0 + 1.96*r0_se,
         rd=sum(data$est1-data$est0)/sum(1-data$s),
         rd_se=sqrt(var((data$est1 - data$est0)/mean(1-data$s),na.rm=TRUE)/nrow(data)),
         rd_lcl = rd - 1.96*rd_se,
         rd_ucl = rd + 1.96*rd_se) #|>
    #select(!contains("se"))
}

gettrialresults <- function(data){
  tibble(r1=mean(data$est1),
         r1_se=sqrt(var(data$est1,na.rm=TRUE)/nrow(data)),
         r1_lcl = r1 - 1.96*r1_se,
         r1_ucl = r1 + 1.96*r1_se,
         r0=mean(data$est0),
         r0_se=sqrt(var(data$est0,na.rm=TRUE)/nrow(data)), 
         r0_lcl = r0 - 1.96*r0_se,
         r0_ucl = r0 + 1.96*r0_se,
         rd=mean(data$est1-data$est0),
         rd_se=sqrt(var((data$est1 - data$est0),na.rm=TRUE)/nrow(data)),
         rd_lcl = rd - 1.96*rd_se,
         rd_ucl = rd + 1.96*rd_se) #|>
    #select(!contains("se"))
}

## Estimator and results fxs wrapped for mcsampling analysis
getresultsmc <- function(delta0,delta1,data){
  getresults(newestimator(data,delta0,delta1))
}














