
#' A first function in an example package
#'
#' @description This is the first function of the p1 package
#' written.
#' 
#' @param name the name of a person to greet.
#' @return A string greeting a person.
#' @examples
#'
#' print(hello("bis557"))
#'
#' @export
hello <- function(name) {
  paste0("Hello, ", name, "!")
}
