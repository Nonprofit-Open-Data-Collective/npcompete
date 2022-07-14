#' @importFrom magrittr "%>%"
# @export
# magrittr::`%>%`

bottomcode <- function(x)
{
  x[ x < 0 ] <- 0
  return(x)
}

