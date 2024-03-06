source("libraries.R")
source("utils.R")
source("extra_pipeops.R")
source("grph_lists.R")
loadData()

train_task <- as_task_regr(train,target = "ClaimAmount")


## gam and ensemble learners
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

lrn_list_ge<-list(
  lrn_gam,
  lrn_ensemble)

ge<-combine_graphs_and_learners(
  list("num.impact"= 
         po_RecBeg_num %>>%
         po_RecEnd_rc %>>%
         po("scale")%>>%
         po("encodeimpact")),
  lrn_list_ge
  )

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
  set_id("Dummy_numeric")
## ranger


## design
n_folds <- 5

design<-benchmark_grid(
  tasks = list(
    add_weight(task,"interest"),
    add_weight(task,"interest"),
    add_weight(task,"interest"),
    task
  ),
  learners =
    append(
      ge,
      list(
        "xgboost" = Dummy_lrn_custom,
        "ranger" = 
      ))
    ,
  resamplings = list(
    rsmp("cv",folds = n_folds),
    rsmp("cv",folds = n_folds),
    rsmp("cv",folds = n_folds),
    rsmp("cv",folds = n_folds)
  ),
  paired = TRUE)


## benchmark

future::plan("multisession")

bw_comp<-benchmark(design)

future::plan("sequential")
