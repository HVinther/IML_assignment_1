source("libraries.R")
source("utils.R")
source("extra_pipeops.R")

loadData()

train_task <- as_task_regr(train,target = "ClaimAmount")


## gam and ensemble learners
lrn_gam<-
  po_RecBeg_num %>>%
  po_RecEnd_rc %>>%
  po("scale")%>>%
  po("encodeimpact") %>>%
  lrn("regr.gam")|>
  as_learner() |>
  set_id("gam")

lrn_ensemble<-
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

Dummy_lrn_custom <- 
  po_VehAge_num %>>% 
  po_VehPrice_int %>>% 
  po_SocCat_int %>>% 
  po("encode") %>>% 
  po_RecBeg_num %>>% 
  po_RecEnd_rc %>>% 
  po("scale") %>>% 
  lrn_obj |> 
  at_create() |>
  set_id("xgboost")
## ranger

Rforest_lrn <- 
  lrn("regr.ranger",
      min.node.size = to_tune(1, 50))

Target_rforest_lrn_custom <- 
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

## design
design<-benchmark_grid(
  tasks = list(
    add_weight(train_task,"interest"),
    train_task
  ),
  learners = list(
    lrn_gam,
    lrn_ensemble,
    Dummy_lrn_custom,
    Target_rforest_lrn_custom),
  resamplings = list(
    rsmp("cv",folds = 5)))[c(1,2,3,8)]

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