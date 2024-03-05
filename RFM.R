
future::plan("multisession")

train_task <- as_task_regr(train,target = "ClaimAmount")

test_task <- as_task_regr(test, target = "ClaimAmount")

Rforest_lrn <- lrn("regr.ranger",
                   min.node.size = to_tune(1, 50))

Dummy_rforest_lrn <- po("encode") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% po("scale") %>>% lrn("regr.ranger", id = "Dummy_rforest") |> as_learner()
Target_rforest_lrn <- po("encodeimpact") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% po("scale") %>>% lrn("regr.ranger") |> as_learner()
Dummy_rforest_lrn_custom <- po_VehAge_num %>>% po_VehPrice_int %>>% po_SocCat_int %>>% po("encode") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% po("scale") %>>% lrn("regr.ranger") |> as_learner()
Target_rforest_lrn_custom <- po_VehAge_num %>>% po_VehPrice_int %>>% po_SocCat_int %>>% po("encodeimpact") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% po("scale") %>>% lrn("regr.ranger") |> as_learner()

Dummy_rforest_lrn$id <- "dmy_Rfor"
Dummy_rforest_lrn_custom$id <- "dmy_Rfor_cus"
Target_rforest_lrn$id <- "Trgt_Rfor"
Target_rforest_lrn_custom$id <- "Trgt_Rfor_cus"

Dummy_rforest_lrn$train(train_task)
Dummy_rforest_lrn_custom$train(train_task)
Target_rforest_lrn$train(train_task)
Target_rforest_lrn_custom$train(train_task)

Dummy_rforest_lrn_at <- auto_tuner(
  tuner = tnr("grid_search",resolution = 5),
  learner = Dummy_rforest_lrn,
  resampling = rsmp("cv",folds = 5),
  measure = msr("regr.mse"),
  terminator = trm("evals",n_evals = 20)
)

Dummy_rforest_lrn_at$train(train_task)

Dummy_rforest_lrn_at$model$learner$param_set$values$regr.ranger.min.node.size

Target_rforest_lrn_at <- auto_tuner(
  tuner = tnr("grid_search",resolution = 5),
  learner = Target_rforest_lrn,
  resampling = rsmp("cv",folds = 5),
  measure = msr("regr.mse"),
  terminator = trm("evals",n_evals = 20)
)

Target_rforest_lrn_at$train(train_task)

Target_rforest_lrn_at$model$learner$param_set$values$regr.ranger.min.node.size

rforest_BM3 <- benchmark_grid(
  tasks = list(train_task, add_weight(train_task,weighting = "interest"),add_weight(train_task,weighting = "frequency"),add_weight(train_task)),
  learners = list(lrn("regr.featureless"), Dummy_rforest_lrn,Dummy_rforest_lrn_custom,Target_rforest_lrn,Target_rforest_lrn_custom),
  resamplings = rsmp("cv",folds=3)
)|> benchmark()

save(rforest_BM3, file = "RforestBM3.RData")

rforest_BM5 <- benchmark_grid(
  tasks = list(train_task, add_weight(train_task,weighting = "interest"),add_weight(train_task,weighting = "frequency"),add_weight(train_task)),
  learners = list(lrn("regr.featureless"), Dummy_rforest_lrn,Dummy_rforest_lrn_custom,Target_rforest_lrn,Target_rforest_lrn_custom),
  resamplings = rsmp("cv",folds=5)
)|> benchmark()

save(rforest_BM5, file = "RforestBM5.RData")

rforest_BM10 <- benchmark_grid(
  tasks = list(train_task, add_weight(train_task,weighting = "interest"),add_weight(train_task,weighting = "frequency"),add_weight(train_task)),
  learners = list(lrn("regr.featureless"), Dummy_rforest_lrn,Dummy_rforest_lrn_custom,Target_rforest_lrn,Target_rforest_lrn_custom),
  resamplings = rsmp("cv",folds=10)
)|> benchmark()

save(rforest_BM10, file = "RforestBM10.RData")

rforest_BM20 <- benchmark_grid(
  tasks = list(train_task, add_weight(train_task,weighting = "interest"),add_weight(train_task,weighting = "frequency"),add_weight(train_task)),
  learners = list(lrn("regr.featureless"), Dummy_rforest_lrn,Dummy_rforest_lrn_custom,Target_rforest_lrn,Target_rforest_lrn_custom),
  resamplings = rsmp("cv",folds=20)
)|> benchmark()

save(rforest_BM20, file = "RforestBM20.RData")

load("RforestBM20.RData")

rforest_BM$aggregate(list(msr("regr.mse"),
                          msr("time_train")))

ggplot(rforest_BM5$aggregate(list(msr("regr.mse"),
                                 msr("time_train"))))+
  geom_point(mapping = aes(x=time_train, y=regr.mse, color= learner_id, shape=task_id))+
  geom_hline(mapping = aes(yintercept = regr.mse, color = learner_id),
             linetype = "dashed")+
  xlab("time")+
  ylab("Mean Squared Error")

rforest_BM20$filter(learner_ids = list("dmy_Rfor","dmy_Rfor_cus","Trgt_Rfor","Trgt_Rfor_cus"))

rforest_BM$aggregate(list(msr("regr.mse"),msr("time_train")))
