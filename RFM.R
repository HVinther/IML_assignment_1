
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

Target_rforest <- po("encodeimpact") %>>% po_add_weighting %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% lrn("regr.ranger") |> as_learner()

glmm_rforest <- po("encodelmer", affect_columns = selector_type("factor")) %>>% po_add_weighting %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% lrn("regr.ranger") |> as_learner()

##Rforest_lrn_part <- po_add_weighting %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% lrn("regr.ranger",
                        ##respect.unordered.factors = "partition") |> as_learner()

## Har valgt ikke at implementere med native ranger target encoding (bruge partition) da det tager over 1 time at køre en enkelt gang.

Custom_rforest_dummy <- po_add_weighting %>>% po_SocCat_int %>>% po_VehAge_num %>>% po_VehPrice_int %>>% 
  po("encode") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% lrn("regr.ranger")|> as_learner()

Custom_rforest_target <- po_add_weighting %>>% po_SocCat_int %>>% po_VehAge_num %>>% po_VehPrice_int %>>% 
  po("encodeimpact") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% lrn("regr.ranger")|> as_learner()

Custom_rforest_glmm <- po_add_weighting %>>% po_SocCat_num %>>% po_VehAge_num %>>% po_VehPrice_int %>>% 
  po("encodelmer", affect_columns = selector_type("factor")) %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% lrn("regr.ranger")|> as_learner()

rforest_BM <- benchmark_grid(
  tasks = train_task,
  learners = list(Dummy_rforest,Target_rforest,glmm_rforest,Custom_rforest_dummy,Custom_rforest_target),
  resamplings = rsmp("cv",folds = 3)
) |> benchmark()

Custom_rforest_dummy$train(train_task)

Custom_rforest_target$train(train_task)

Custom_rforest_glmm$train(train_task)

Custom_rforest_dummy$predict(test_task)

Custom_rforest_target$predict(test_task)

Custom_rforest_glmm$predict(test_task)

AT_create <- function(lrn_object){
  return(auto_tuner(
    tuner = tnr("random_search"),
    learner = lrn_object,
    resampling = rsmp("cv", folds = 5),
    measure = msr("regr.mse"),
    terminator = trm("evals", n_evals = 20)
    )
  )
}

rforest_BM <- benchmark_grid(
  tasks = train_task,
  learners = list(Custom_rforest_glmm, Custom_rforest_dummy,Custom_rforest_target, Dummy_rforest,Target_rforest),
  resamplings = rsmp("cv",folds = 3)
) |> benchmark()

Dummy_rforest_at <- AT_create(po("encode") %>>% po_add_weighting %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% Rforest_lrn |> as_learner())

Target_rforest_at <- AT_create(po("encodeimpact") %>>% po_add_weighting %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% Rforest_lrn |> as_learner())

Dummy_rforest_at$train(train_task)
