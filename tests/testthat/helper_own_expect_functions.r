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
    if("data.frame" %in% class(object)) {
        sel = expected %in% colnames(object)
    }else{
        sel = expected %in% names(object)
    }
    comp = all(sel)
    info = paste0(info,
                  'objectect does not contain all names: ["', paste(expected[!sel], collapse = '", "'), '"]\n',
                  'expected names: ["', paste(expected, collapse = '", "'), '"]\n',
                  'found names: ["', paste(names(object), collapse = '", "'), '"]\n')
    expect( comp,
            "",
            info = info)
    invisible(object)
}



