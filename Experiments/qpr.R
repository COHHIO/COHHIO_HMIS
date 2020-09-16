
#TODO qpr reactive Take input an filter by Region & Between
qpr_between <- function() {
  
}
qpr_region <- function(.d, ProjectRegion, ProjectType, additional) {
  .pr <- rlang::ensym(ProjectRegion)
  reactive(dplyr::filter(.d, ProjectType %in% !!.pr, qpr_between()))
}
qpr_type <- function(.d, ProjectType, additional) {
  .pt <- rlang::ensym(ProjectType)
  reactive({
    if (!missing(additional)) {
      .call <- rlang::call2(dplyr::filter
                            , .data = .d
                            , ProjectType == !!.pt
                            , {{additional}}
      )
      
      
    } else {
      .call <- rlang::call2(dplyr::filter
                            , .data = .d
                            , ProjectType == !!.pt
      )
    }
    dplyr::group_by(eval(.call)
    , FriendlyProjectName
    , ProjectType
    , ProjectCounty
    , ProjectRegion)
  })
}
