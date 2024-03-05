
train_task = as_task_regr(train,target="ClaimAmount")
test_task =as_task_regr(train,target="ClaimAmount")

#We will train on some different graph encodings,
#and use nested cross validation and benchmarking to evaluate which has the lower
#mse using 3 outer folds and 5 inner folds for hyper-parameter tuning

graph1 = po_RecBeg_num%>>% po_RecEnd_rc %>>% po("encode") %>>% po("scale") %>>%
  lrn("regr.xgboost",booster=to_tune(c("gbtree", "gblinear", "dart")),
      max_depth=to_tune(floor(seq(1,500,length.out=5))))

graph2 = po_RecBeg_num%>>% po_RecEnd_rc %>>% po("encodeimpact") %>>% po("scale") %>>%
  lrn("regr.xgboost",booster=to_tune(c("gbtree", "gblinear", "dart")),
      max_depth=to_tune(floor(seq(1,500,length.out=5))))

#numeric social category
graph3 = po_SocCat_int %>>% po_RecBeg_num%>>% po_RecEnd_rc %>>% po("encode") %>>% po("scale") %>>%
  lrn("regr.xgboost",booster=to_tune(c("gbtree", "gblinear", "dart")),
      max_depth=to_tune(floor(seq(1,500,length.out=5))))

graph4 = po_SocCat_int %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% po("encodeimpact") %>>% po("scale") %>>%
  lrn("regr.xgboost",booster=to_tune(c("gbtree", "gblinear", "dart")),
      max_depth=to_tune(floor(seq(1,500,length.out=5))))

at_create <- function(graph){
  return(auto_tuner(tuner=tnr("grid_search",batch_size=10),
                    learner=as_learner(graph),
                    resampling=rsmp("cv",folds=5),
                    measure=msr("regr.mse"),
                    terminator=trm("evals",n_evals=20)))
}

reg.xgboost1 = at_create(graph1)
reg.xgboost2 = at_create(graph2)
reg.xgboost3 = at_create(graph3)
reg.xgboost4 = at_create(graph4)


learners = list(reg.xgboost1,reg.xgboost2,reg.xgboost3,reg.xgboost4)

saveRDS(learners,"xgboost_learners")

test_factors = benchmark_grid(train_task,learners,resampling=rsmp("cv",folds=5))

benchmark = benchmark(test_factors)

saveRDS(benchmark,"xgboost_benchmark_factors")

benchmark$aggregate(msr("regr.mse"))

benchmark$learners


nested_cv$aggregate(msr("regr.mse"))
nested_cv$score()


reg.xgboost$train(task)

reg.xgboost$tuning_instance
reg.xgboost$tuning_result





lrn_obj = lrn("regr.xgboost",booster=to_tune(c("gbtree", "gblinear", "dart")),
              max_depth=to_tune(floor(seq(1,500,length.out=5))))

Dummy_lrn <- po("encode") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% po("scale") %>>% lrn_obj |> as_learner()
Target_lrn <- po("encodeimpact") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% po("scale") %>>% lrn_obj |> as_learner()
Dummy_lrn_custom <- po_VehAge_num %>>% po_VehPrice_int %>>% po_SocCat_int %>>% po("encode") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% po("scale") %>>% lrn_obj |> as_learner()
Target_lrn_custom <- po_VehAge_num %>>% po_VehPrice_int %>>% po_SocCat_int %>>% po("encodeimpact") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% po("scale") %>>% lrn_obj |> as_learner()

Dummy_lrn$id="Dummy_direct"
Target_lrn$id="Target_direct"
Dummy_lrn_custom$id="Dummy_numeric"
Target_lrn_custom$id="Target_numeric"

task_interest = add_weight(train,weighting = c("interest")) %>% as_task_regr(target="ClaimAmount",id="task_interest_weight")
task_frequency = add_weight(train,weighting=c("frequency")) %>% as_task_regr(target="ClaimAmount",id="task_frequency_weight")

train_task$id="task_0weight"

xgbench = benchmark_grid(list(train_task,task_interest,task_frequency),list(Dummy_lrn,Target_lrn,Dummy_lrn_custom,Target_lrn_custom),rsmp("cv",folds=5))

