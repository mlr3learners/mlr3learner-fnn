#' @title Regression fnn Learner
#'
#' @name mlr_learners_regr.fnn
#'
#' @description A [mlr3::LearnerRegr] for a regression fnn implemented in
#' [FNN::knn()] in package \CRANpkg{FNN}.
#'
#' @export
LearnerRegrFNN <- R6Class("LearnerRegrFNN",
  inherit = LearnerRegr,

  public = list(

    #' @description
    #' Create a `LearnerRegrFNN` object.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamInt$new(id = "k", default = 1, lower = 1L, tags = "train"),
          ParamFct$new(id = "algorithm", default = "kd_tree",
            levels = c("kd_tree", "cover_tree", "brute"), tags = "train")
        )
      )

      super$initialize(
        id = "regr.fnn",
        packages = "FNN",
        feature_types = c("integer", "numeric"),
        predict_types = "response",
        param_set = ps,
        man = "mlr3learners.fnn::mlr_learners_regr.fnn"
      )
    }),

  private = list(

    .train = function(task) {
      list(
        data = task$data(),
        pars = self$param_set$get_values(tags = "train")
      )
    },

    .predict = function(task) {
      model = self$model
      train = model$data[, task$feature_names, with = FALSE]
      target = model$data[, task$target_names, with = FALSE][[1]]
      newdata = task$data(cols = task$feature_names)

      p = invoke(FNN::knn.reg, train = train, test = newdata, y = target,
        .args = model$pars)
      PredictionRegr$new(task = task, response = p$pred)
    }
  )
)
