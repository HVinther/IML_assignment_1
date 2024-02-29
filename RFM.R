
train_task <- as_task_regr(train,target = "ClaimAmount")

test_task <- as_task_regr(test, target = "ClaimAmount")

Rforest_lrn <- lrn("regr.ranger",
                   min.node.size = to_tune(1, 50)) 

Rforest_lrn_part <- lrn("regr.ranger",
                   mtry.ratio = to_tune(0.1, 1),
                   min.node.size = to_tune(1, 50),
                   respect.unordered.factors = "partition"
                   )

Dummy_rforest <- po("encode") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% Rforest_lrn |> as_learner()

Rforest_lrn_part_basic <- po_RecBeg_num %>>% po_RecEnd_rc %>>% lrn("regr.ranger",
                        respect.unordered.factors = "partition") |> as_learner()

Custom_rforest_basic <- po_SocCat_int %>>% po_VehAge_num %>>% po_VehPrice_int %>>% 
  po("encode") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% lrn("regr.ranger")|> as_learner()

Dummy_rforest_basic$train(train_task)


AT_create <- function(lrn_obj){
  return(auto_tuner(
    tuner = tnr("random_search"),
    learner = lrn_obj,
    resampling = rsmp("cv", folds = 5),
    measure = msr("regr.mse"),
    terminator = trm("evals",n_evals = 20)
  ))
}

Dummy_rforest_AT <- AT_create(Dummy_rforest)

Dummy_rforest_AT$train(train_task)
