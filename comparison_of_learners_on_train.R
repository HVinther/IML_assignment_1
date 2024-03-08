source("libraries.R")
source("utils.R")
source("extra_pipeops.R")

loadData()

train_task <- as_task_regr(train,target = "ClaimAmount")


## gam and ensemble learners
lrn_gam<-
  po_add_weighting%>>%
  po_RecBeg_num %>>%
  po_RecEnd_rc %>>%
  po("scale")%>>%
  po("encodeimpact") %>>%
  lrn("regr.gam")|>
  as_learner() |>
  set_id("gam")

lrn_ensemble<-
  po_add_weighting%>>%
  po_RecBeg_num %>>%
  po_RecEnd_rc %>>%
  po("scale")%>>%
  po("encodeimpact")%>>%
  gunion(list(
    po("learner_cv",
       learner = lrn("regr.gam"),
       id = "gam",
       resampling.folds = 2),
    po("learner_cv",
       learner = lrn("regr.xgboost"),
       id = "xgboost",
       resampling.folds = 2))
  ) %>>%
  po("featureunion") %>>%
  po("learner", learner = lrn("regr.lm")) |>
  as_learner()|>
  set_id("ensemble")

## xgboost 
lrn_obj = lrn("regr.xgboost",booster=to_tune(c("gbtree", "gblinear", "dart")),
              max_depth=to_tune(floor(seq(1,500,length.out=10))))

# xboost_mse <- 
#   po_add_weighting%>>%
#   po_VehAge_num %>>% 
#   po_VehPrice_int %>>% 
#   po_SocCat_int %>>% 
#   po("encode") %>>% 
#   po_RecBeg_num %>>% 
#   po_RecEnd_rc %>>% 
#   po("scale") %>>% 
#   lrn_obj |> 
#   at_create() |>
#   set_id("wi.num.adhoc.dummy_xgboost")

lrn_xgboost <- 
  po_add_weighting%>>%
  po_RecBeg_num %>>% 
  po_RecEnd_rc %>>% 
  po("encodeimpact") %>>% 
  po("scale") %>>% 
  lrn_obj |> 
  at_create() |>
  set_id("xgboost")

## ranger

lrn_ranger <- 
  po_add_weighting%>>%
  po_VehAge_num %>>% 
  po_VehPrice_int %>>% 
  po_SocCat_int %>>% 
  po("encodeimpact") %>>% 
  po_RecBeg_num %>>% 
  po_RecEnd_rc %>>% 
  po("scale") %>>% 
  lrn("regr.ranger") |> 
  as_learner() |>
  set_id("ranger")
# 
# ranger_<- 
#   po_VehAge_num %>>% 
#   po_VehPrice_int %>>% 
#   po_SocCat_int %>>% 
#   po("encodeimpact") %>>% 
#   po_RecBeg_num %>>% 
#   po_RecEnd_rc %>>% 
#   po("scale") %>>% 
#   lrn("regr.ranger") |> 
#   as_learner() |>
#   set_id("w0.num.adhoc.impact_ranger")

## design
design<-benchmark_grid(
  tasks = train_task,
  learners = list(
    lrn_gam,
    lrn_ensemble,
    lrn_xgboost,
    lrn_ranger),
  resamplings = rsmp("cv",folds = 5))

## benchmark

future::plan("multisession")

bm_comp<-benchmark(design)

future::plan("sequential")

scor<-
  bm_comp$score(msrs(c("regr.mse","regr.mse_inter","time_train")))|> 
  tibble() |> 
  select(-c("uhash","task","resampling","prediction","learner"))

aggr<-
  bm_comp$aggregate(msrs(c("regr.mse","regr.mse_inter","time_train")))|> 
  tibble() |> 
  select(-resample_result)


saveRDS(scor,"bw_train_comp_scor.rds")
saveRDS(aggr,"bw_train_comp_aggr.rds")

# train on train and test on test -----------------------------------

test_task <- as_task_regr(test,target = "ClaimAmount")

lrn_gam$train(train_task)
lrn_ensemble$train(train_task)
lrn_xgboost$train(train_task)
lrn_ranger$train(train_task)

p_gam<-lrn_gam$predict(test_task)
p_ens<-lrn_ensemble$predict(test_task)
p_xgb<-lrn_xgboost$predict(test_task)
p_ran<-lrn_ranger$predict(test_task)

res<-do.call(rbind,
        list(p_gam$score(msrs(c("regr.mse","regr.mse_inter","time_train")), task = test_task,learner = lrn_gam),
             p_ens$score(msrs(c("regr.mse","regr.mse_inter","time_train")), task = test_task,learner = lrn_ensemble),
             p_xgb$score(msrs(c("regr.mse","regr.mse_inter","time_train")), task = test_task,learner = lrn_xgboost),
             p_ran$score(msrs(c("regr.mse","regr.mse_inter","time_train")), task = test_task,learner = lrn_ranger)))

prediction_results<-
  res |> 
  as.data.frame() |>
  mutate(learner = c("gam","ensemble","xgboost","ranger"))

saveRDS(prediction_results,"prediction_results.rds")

specific_predictions<-
rbind(p_gam$response[c(11386, 12286, 2119, 2238, 27833, 27988)],
      p_ens$response[c(11386, 12286, 2119, 2238, 27833, 27988)],
      p_xgb$response[c(11386, 12286, 2119, 2238, 27833, 27988)],
      p_ran$response[c(11386, 12286, 2119, 2238, 27833, 27988)],
      p_gam$truth[c(11386, 12286, 2119, 2238, 27833, 27988)])|>
  as_tibble()|>
  rename("i11386" = V1,
         "i12286" = V2,
         "i2119" = V3,
         "i2238" = V4,
         "i27833" = V5,
         "i27988" = V6)|>
  mutate(learner = c("gam","ensemble","xgboost","ranger",NA),
         type = c(rep("prediction",4),"truth"))|>
  select("learner","type","i11386","i12286", "i2119", "i2238","i27833","i27988")|>
  mutate(mse = i11386^2/6+i12286^2/6+i2119^2/6+i2238^2/6+i27833^2/6+i27988^2/6)

saveRDS(specific_predictions,"specific_predictions.rds")



what_if<-
  rbind(
  test[c(11386, 12286, 2119, 2238, 27833, 27988),]|>
  mutate(Exposure = 1,
         RecordEnd = max(na.omit(test$RecordEnd))),
  test[c(11386, 12286, 2119, 2238, 27833, 27988),]|>
    mutate(Exposure = 1,
           RecordEnd = max(na.omit(test$RecordEnd)),
           ClaimInd = 1L)
    )|>
  as_task_regr(target = "ClaimAmount",id = "what_if")

preds<-rbind(lrn_gam$predict(what_if)$response,
      lrn_ensemble$predict(what_if)$response,
      lrn_xgboost$predict(what_if)$response,
      lrn_ranger$predict(what_if)$response)

what_if_pred<-rbind(
  preds[1:4,1:6],
  preds[1:4,7:12])|>
  as_tibble()|>
  rename("i11386" = V1,
         "i12286" = V2,
         "i2119" = V3,
         "i2238" = V4,
         "i27833" = V5,
         "i27988" = V6)|>
  mutate(learner = rep(c("gam","ensemble","xgboost","ranger"),times = 2),
         ClaimInd = rep(0:1,each = 4))|>
  select("learner","ClaimInd","i11386","i12286","i2119","i2238","i27833","i27988")

saveRDS(what_if_pred,"what_if.rds")
