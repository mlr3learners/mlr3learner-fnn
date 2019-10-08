#' @title Regression fnn Learner
#'
#' @aliases mlr_learners_regr.fnn
#' @format [R6::R6Class] inheriting from [mlr3::LearnerRegr].
#'
#' @description
#' A [mlr3::LearnerRegr] for a regression fnn implemented in [FNN::knn()] in package \CRANpkg{FNN}.
#'
#' @export
LearnerRegrFNN <- R6Class("LearnerRegrFNN",
  inherit = LearnerRegr,
  public = list(
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamInt$new(id = "k", default = 1, lower = 1L, tags = "train"),
          ParamFct$new(id = "algorithm", default = "kd_tree", levels = c("kd_tree", "cover_tree", "brute"), tags = "train")
        )
      )

      super$initialize(
        id = "regr.fnn",
        packages = "FNN",
        feature_types = c("integer", "numeric"),
        predict_types = c("response"),
        param_set = ps,
      )
    },

    train_internal = function(task) {
      list(
        target = task$data(cols = task$target_names),
        data = task$data(cols = task$feature_names),
        pars = self$param_set$get_values(tags = "train")
      )
    },

    predict_internal = function(task) {
      model = self$model
      newdata = task$data(cols = task$feature_names)

      p = invoke(FNN::knn.reg, train = model$data, test = newdata, y = model$target[[1]], .args = model$pars)
      PredictionRegr$new(task = task, response = p$pred)
    }
  )
)
