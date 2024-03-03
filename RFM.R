
train_task <- as_task_regr(train,target = "ClaimAmount")

test_task <- as_task_regr(test, target = "ClaimAmount")

Rforest_lrn <- lrn("regr.ranger",
                   min.node.size = to_tune(1, 50)) 

Rforest_lrn_part <- lrn("regr.ranger",
                   mtry.ratio = to_tune(0.1, 1),
                   min.node.size = to_tune(1, 50),
                   respect.unordered.factors = "partition"
                   )

Dummy_rforest <- po("encode") %>>% po_add_weighting %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% lrn("regr.ranger") |> as_learner()

Dummy_rforest$id <- "Dummy_rforest"

Target_rforest <- po("encodeimpact") %>>% po_add_weighting %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% lrn("regr.ranger") |> as_learner()

Target_rforest$id <- "Target_rforest"

glmm_rforest <- po("encodelmer", affect_columns = selector_type("factor")) %>>% po_add_weighting %>>%  po_RecBeg_num %>>% po_RecEnd_rc %>>% lrn("regr.ranger") |> as_learner() 

## Rforest_lrn_part <- po_add_weighting %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% lrn("regr.ranger",
                        ##respect.unordered.factors = "partition") |> as_learner()

## Har valgt ikke at implementere med native ranger target encoding (bruge partition) da det tager over 1 time at køre en enkelt gang.

Custom_rforest_dummy <- po_add_weighting %>>% po_SocCat_int %>>% po_VehAge_num %>>% po_VehPrice_int %>>% 
  po("encode") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% lrn("regr.ranger")|> as_learner()

Custom_rforest_dummy$id <- "Custom_rforest_dummy"

Custom_rforest_target <- po_add_weighting %>>% po_SocCat_int %>>% po_VehAge_num %>>% po_VehPrice_int %>>% 
  po("encodeimpact") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% lrn("regr.ranger")|> as_learner()

Custom_rforest_target$id <- "Custom_rforest_target"

Custom_rforest_glmm <- po_add_weighting %>>% po_SocCat_int %>>% po_VehAge_num %>>% po_VehPrice_int %>>% 
  po("encodelmer", affect_columns = selector_type("factor")) %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% lrn("regr.ranger")|> as_learner()

Custom_rforest_glmm$id <- "Custom_rforest_glmm"

rforest_BM <- benchmark_grid(
  tasks = train_task,
  learners = list(Dummy_rforest,Target_rforest,Custom_rforest_glmm, Custom_rforest_dummy,Custom_rforest_target),
  resamplings = rsmp("cv",folds = 5)
) |> benchmark()

benchmark_grid(
  tasks = train_task,
  learners = list(glmm_rforest),
  resamplings = rsmp("cv",folds = 2)
) |> benchmark()

save(rforest_BM, file = "RforestBM.RData")

load("RforestBM.RData")

rforest_BM$aggregate(list(msr("regr.mse"),
                          msr("time_train")))

ggplot(rforest_BM$aggregate(list(msr("regr.mse"),
                                 msr("time_train"))))+
  geom_point(mapping = aes(x=time_train, y=regr.mse, color= learner_id))+
  geom_hline(mapping = aes(yintercept = regr.mse, color = learner_id),
             linetype = "dashed")+
  xlab("time")+
  ylab("Mean Squared Error")

autoplot(rforest_BM, type = "boxplot")

autoplot(rforest_BM, type = "roc")
