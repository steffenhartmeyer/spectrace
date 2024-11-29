#' Process spectral clusters
#'
#' @param lightData The light data
#' @param datetimeVar The name of the datetime variable. Defaults to "datetime".
#' @param clusterVar The name of the variable that defines the cluster to process.
#' @param regularise Logical. Regularise the data?
 #'
#' @return
#' @export
#'
#' @examples
spectrace_process_clusters = function(lightData,
                                      datetimeVar = datetime,
                                      clusterVar = cluster_id,
                                      regularise = TRUE){

  stopifnot(
    "`clusterVar` cannot be a factor!" = !is.factor({{clusterVar}})
  )

  groups <- lightData %>% dplyr::group_vars()
  if(regularise){
    lightData <- lightData %>%
      spectrace_regularise_data({{datetimeVar}})
  }

  lightData <- lightData %>%
    dplyr::ungroup() %>%
    dplyr::nest_by(dplyr::pick(dplyr::all_of(groups))) %>%
    dplyr::mutate(
      data = list(
        mutate(data, find_cluster_timings({{clusterVar}}, {{datetimeVar}}))
      )
    ) %>%
    tidyr::unnest(cols = data) %>%
    dplyr::ungroup()
}
