source("libraries.R")
source("utils.R")
source("extra_pipeops.R")
loadData()

train_task <- as_task_regr(train,target = "ClaimAmount")

grph_full<-
  po_SocCat_int%>>%
  po_VehAge_num%>>%
  po_VehPrice_int%>>%
  po_RecBeg_num %>>%
  po_RecEnd_rc %>>%
  po("encode")%>>%
  po("scale")

grph_base<-
  po_RecBeg_num %>>%
  po_RecEnd_rc %>>%
  po("encode")%>>%
  po("scale")

lrn_glmnet<-set_id(
      lrn("regr.cv_glmnet"),
      id = "cv_glmnet")

lrn_xboost<-
  lrn("regr.xgboost") |>
  set_id("xgboost")

lrn_gam<-
  lrn("regr.gam")|>
  set_id("gam")

lrn_ensemble<-
  gunion(list(
    po("learner_cv",
       learner =lrn("regr.gam"),
       resampling.folds = 5,
       id = "gam"),
    po("learner_cv",
       learner =lrn("regr.xgboost"),
       resampling.folds = 5,
       id = "xgboost"))
    ) %>>%
  po("featureunion") %>>%
  po("learner", learner = lrn("regr.lm")) |>
  as_learner()|>
  set_id("ensemble")

graph_list<-list(
  "full" = grph_full,
  "base" = grph_base
)

learner_list<-list(
  lrn_glmnet,
  lrn_xboost,
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
    resamplings = rsmp("cv",folds = 2)
  )
)

future::plan("sequential")

bm$aggregate()

bm$aggregate(list(msr("regr.mse"),msr("time_train")))|>
  ggplot()+
  geom_hline(aes(yintercept = regr.mse, 
                 color = learner_id),
             linetype = "dashed",
             linewidth = 1,
             alpha = 0.8)+
  geom_point(aes(x= time_train,
                 y = regr.mse,
                 color = learner_id,
                 shape = task_id),
             size = 5)
