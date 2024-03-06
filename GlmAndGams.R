source("libraries.R")
source("utils.R")
source("extra_pipeops.R")
loadData()

train_task <- as_task_regr(train,target = "ClaimAmount")

date_encodings<-list(
  "num" = 
    po_RecBeg_num %>>%
    po_RecEnd_rc,
  "df" =  
    po("missind") %>>%
    po("select",
       selector = selector_invert(selector_name("RecordEnd")))%>>%
    po("datefeatures")
)

adhoc<-list(
  "adhoc" = 
    po_SocCat_int%>>%
    po_VehAge_num%>>%
    po_VehPrice_int%>>%
    po("scale"),
  po("scale")
)

factor <- list(
  "dummy" = po("encode"),
  "impact" = po("encodeimpact")
)

grp_bind<-function(first,second){
  lapply(first, \(grp_f){
    lapply(second,\(grf_s){
      grp_f%>>%grf_s
    })
  }) |> 
    unlist()
}

graph_list<-grp_bind(date_encodings,adhoc) |>
  grp_bind(first = _,factor)

lrn_cv_glmnet<-set_id(
      lrn("regr.cv_glmnet"),
      id = "cv.glmnet")

lrn_glmnet<-
  auto_tuner(
    tuner = tnr("random_search"),
    learner = lrn("regr.glmnet",
                  s = to_tune(0,10)),
    resampling = rsmp("cv",folds = 5),
    term_evals = 20
  )|>
  set_id(id = "glmnet")

lrn_gam<-
  lrn("regr.gam")|>
  set_id("gam")

lrn_ensemble<-
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


# lrn_branched<-
#   po("targetmutate",
#      trafo = \(x){as.factor(sign(x))},
#      new_task_type = "classif") %>>%
#   lrn("classif.xgboost")

learner_list<-list(
  lrn_cv_glmnet,
  lrn_glmnet,
  lrn_gam,
  lrn_ensemble
)

future::plan("multisession")

bm<-benchmark(
  benchmark_grid(
    tasks = list(
      train_task,
      add_weight(train_task,"interest"),
      add_weight(train_task,"frequency"),
      add_weight(train_task)
    ),
    learners = append(
      combine_graphs_and_learners(
        graph_list,
        learner_list),
      lrn("regr.featureless")),
    resamplings = rsmp("cv",folds = 5)
  )
)

future::plan("sequential")

scor<-bm$score(msrs(c("regr.mse","regr.mse_inter","time_train")))

aggr<-bm$aggregate(msrs(c("regr.mse","regr.mse_inter","time_train")))

aggr <- aggr |>
  mutate(
    encoding = as.factor(stringr::str_replace(learner_id,"_.*","")),
    learner = as.factor(stringr::str_replace(learner_id,".*_","")))

scor <- scor |>
  mutate(
    encoding = as.factor(stringr::str_replace(learner_id,"_.*","")),
    learner = as.factor(stringr::str_replace(learner_id,".*_","")))

aggr <-aggr |> tibble() |> select(-resample_result)
scor<-scor |> tibble() |> select(-c("uhash","task","resampling","prediction"))

saveRDS(scor, "GnG_score_cv5.rds")
saveRDS(aggr, "GnG_aggr_cv5.rds")
