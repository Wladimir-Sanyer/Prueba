box::use(
  R6[R6Class],
  cachem[cache_mem],
  dplyr[rows_upsert, filter],
  magrittr[`%>%`],
  rlang[expr, eval_tidy]
)

#' Cache en memoria para objectos R
#'
#' @export
class <- R6::R6Class(
  classname = "cache",
  public = list(
    #' @description Crea nuevo cache
    #' @param max.size Tamaño máximo en bytes del cache
    initialize = function(type = "mem", max.size = 200 * 1024^2, dir = NULL) {
      private$.catalog <- data.frame(
        id = character(0),
        category = character(0),
        description = character(0),
        time = as.POSIXlt(integer(0))
      )
      if (type == "mem") private$pool <- cache_mem(max.size)
      if (type == "disk") private$pool <- cache_disk(dir = dir, max.size = max.size)
      private$pool$reset()
    },
    #' @description Agrega objeto al cache
    #' @param object Objeto a guardar en el cache
    #' @param id Id para identificar el objeto
    #' @param category Categoria para ubicar el objeto facilmente
    #' @param description Descripcion del objeto siento guardado
    add = function(object, id, category = "", description = "") {
      new_item <- data.frame(
        id = id,
        category = category,
        description = description,
        time = Sys.time()
      )
      private$.catalog <- private$.catalog %>%
        dplyr::rows_upsert(new_item, by = c("id"))
      private$pool$set(id, object)
      private$validate()
      return(object)
    },
    #' @description Devuelve objeto en cache
    #' @param id Id del objeto a encontrar
    get = function(id) {
      private$pool$get(id, NULL)
    },
    #' @description Identifica o guarda un objeto en cache
    #' @param .expr Expresión que devuelve el objeto a guardar en el cache
    #' @param id Id para identificar el objeto
    #' @param category Categoria para ubicar el objeto facilmente
    #' @param description Descripcion del objeto siento guardado
    get_add = function(.expr, id, category, description) {
      lazy_expr <- rlang::expr({{ .expr }})
      if (!private$pool$exists(id)) {
        object <- rlang::eval_tidy(lazy_expr)
        return(self$add(object, id, category, description))
      }
      return(self$get(id))
    },
    #' @description Elimina todos los objetos de cache
    clear = function() {
      private$pool$reset()
      private$validate()
    }
  ),
  active = list(
    #' @field catalog Devuelve catalogo de objetos en el cache
    catalog = function(value) {
      if (missing(value)) {
        return(private$.catalog)
      }
      stop("Cache catalog is read only, use clear method to clear cache")
    }
  ),
  private = list(
    .catalog = NULL,
    pool = NULL,
    validate = function() {
      private$.catalog <- private$.catalog %>%
        dplyr::filter(id %in% private$pool$keys())
    }
  )
)

#' @export
new <- class$new
