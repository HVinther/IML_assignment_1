
train_task <- as_task_regr(train,target = "ClaimAmount")

test_task <- as_task_regr(test, target = "ClaimAmount")

Rforest_lrn <- lrn("regr.ranger",
                   mtry.ratio = to_tune(0.1, 1),
                   min.node.size = to_tune(1, 50)) 

Rforest_lrn_part <- lrn("regr.ranger",
                   mtry.ratio = to_tune(0.1, 1),
                   min.node.size = to_tune(1, 50),
                   )

Dummy_rforest_basic <- po("encode") %>>% po_RecBeg_num %>>% po_RecEnd_rc %>>% lrn("regr.ranger") |> as_learner()


