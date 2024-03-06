
train_task = as_task_regr(train,target="ClaimAmount")
test_task =as_task_regr(train,target="ClaimAmount")

#We will train on some different graph encodings,
#and use nested cross validation and benchmarking to evaluate which has the lower
#mse using 3 outer folds and 5 inner folds for hyper-parameter tuning


lrn_obj = lrn("regr.xgboost",booster=to_tune(c("gbtree", "gblinear", "dart")),
              max_depth=to_tune(floor(seq(1,500,length.out=10))))

Dummy_lrn <- po("encode") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% po("scale") %>>% lrn_obj |> at_create()
Target_lrn <- po("encodeimpact") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% po("scale") %>>% lrn_obj |> at_create()
Dummy_lrn_custom <- po_VehAge_num %>>% po_VehPrice_int %>>% po_SocCat_int %>>% po("encode") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% po("scale") %>>% lrn_obj |> at_create()
Target_lrn_custom <- po_VehAge_num %>>% po_VehPrice_int %>>% po_SocCat_int %>>% po("encodeimpact") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% po("scale") %>>% lrn_obj |> at_create()

Dummy_lrn$id="Dummy_direct"
Target_lrn$id="Target_direct"
Dummy_lrn_custom$id="Dummy_numeric"
Target_lrn_custom$id="Target_numeric"

task_interest = add_weight(train_task,weighting = c("interest"))
task_frequency = add_weight(train_task,weighting=c("frequency"))
task_interest_frequency = add_weight(train_task)

train_task$id="train_0weight"

future::plan("multisession")

xgbench = benchmark_grid(list(train_task,task_interest,task_frequency,task_interest_frequency),
                         list(Dummy_lrn,Target_lrn,Dummy_lrn_custom,Target_lrn_custom),
                         rsmp("cv",folds=10))


bench_mark = benchmark(xgbench)

future::plan(sequential)

#bench_mark = readRDS("xgbench")
plot(bench_mark)

ggplot(bench_mark$aggregate(list(msr("time_train"),msr("regr.mse"))))+
  geom_point(mapping=aes(x=time_train,y=regr.mse,colour=learner_id,shape=task_id))+
  geom_hline(mapping=aes(yintercept=regr.mse,colour=learner_id,linetype=task_id))
# saveRDS(bench_mark,"xgbench")
# autoplot(bench_mark)
