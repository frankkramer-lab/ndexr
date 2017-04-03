#' Expectation: contains the object all the expected names?
#'
#' @param object object to test
#' @param expected Expected names(object)
#' @param info extra information to be included in the message (useful when writing tests in loops).
#'
#' @return
#'
#' @examples
#' \dontrun{
#' expect_object_conains_names(list(fist='something', second='bla'), c('fist', 'second'))
#' }
expect_object_conains_names = function(object, expected, info = NULL) {
  comp = all(expected %in% names(object))
  mesg = ifelse(comp, paste0('objectect does not contain all names: ["', paste(object[!(expected %in% names(object))], collapse = '", "'), '"]\n',
                             'expected names: ["', paste(expected, collapse = '", "'), '"]\n',
                             'found names: ["', paste(names(object), collapse = '", "'), '"]\n'),
                '')
  expect( comp,
          mesg,
          info = info)
  invisible(object)
}