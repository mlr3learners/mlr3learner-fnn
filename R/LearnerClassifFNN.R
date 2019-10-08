#' @title Classification fnn Learner
#'
#' @aliases mlr_learners_classif.fnn
#' @format [R6::R6Class] inheriting from [mlr3::LearnerClassif].
#'
#' @description
#' A [mlr3::LearnerClassif] for a classification fnn implemented in [FNN::knn()] in package \CRANpkg{FNN}.
#'
#' @export
LearnerClassifFNN <- R6Class("LearnerClassifFNN",
  inherit = LearnerClassif,
  public = list(
    initialize = function() {
      ps <- ParamSet$new(
        params = list(
          ParamInt$new(id = "k", default = 1, lower = 1L, tags = "train"),
          ParamFct$new(id = "algorithm", default = "kd_tree", levels = c("kd_tree", "cover_tree", "brute"), tags = "train")
        )
      )

      super$initialize(
        id = "classif.fnn",
        packages = "FNN",
        feature_types = c("integer", "numeric"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("twoclass", "multiclass")
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

      if (self$predict_type == "response") {
        p = invoke(FNN::knn, train = model$data, test = newdata, cl = model$target[[1]], .args = model$pars)
        PredictionClassif$new(task = task, response = p)
      } else {
        if (task$properties != "twoclass") {
          stop("Probabilities are not available for multiclass")
        }
        p = invoke(FNN::knn, train = model$data, test = newdata, cl = model$target[[1]], prob = TRUE, .args = model$pars)

        # Predicted probabilities refer to the winning class
        prob = attr(p, "prob")
        p = ifelse(p == task$positive, prob, 1 - prob)
        p = matrix(c(p, 1 - p), ncol = 2L, nrow = length(p))
        colnames(p) = task$class_names
        PredictionClassif$new(task = task, prob = p)
      }
    }
  )
)
