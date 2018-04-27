#' @details An <%= ifelse(exists("graphObject"), graphObject, "RCXgraph") %> object could look like this:
#' 
#'\preformatted{
#' ## Get some network...
#'> ndexcon = ndex_connect()
#'> rcx = ndex_get_network(ndexcon,"dd268e2f-fd4d-11e7-adc1-0ac135e8bacf")
#'
#'> summary(<%= ifelse(exists("graphObject"), tolower(graphObject), "rcxgraph") %>)
#' IGRAPH f99ed1e DN-- 30 218 -- 
#' + attr: metaData (g/x), numberVerification (g/x), ndexStatus (g/x), provenanceHistory (g/x), networkAttributes (g/x), cartesianLayout (g/x), cyVisualProperties
#' | (g/x), cyHiddenAttributes (g/x), status (g/x), name (v/c), @id (v/n), n (v/c), Basal (v/c), avg_PPR (v/c), LumA (v/c), LumB (v/c), ANOVA_FDR (v/c), ANOVA_p
#' | (v/c), Her2 (v/c), name (e/c), @id (e/n), i (e/c), NAME (e/c), strength (e/c), interaction (e/c)
#' 
#' ## Attributes marked with "(g/x)" are graph attributes, "(v/x)" and "(e/x)" correspond to vertex and edge attributes respectively
#' ## Accessing a graph attribute, e.g. "metaData"
#' <%= ifelse(exists("graphObject"), tolower(graphObject), "rcxgraph") %>$metaData
#'    consistencyGroup elementCount   lastUpdate               name properties version idCounter
#' 1                 1            1 1.516391e+12         ndexStatus       NULL     1.0        NA
#' 2                 1            1 1.516391e+12  provenanceHistory       NULL     1.0        NA
#' 3                 1           30           NA              nodes       NULL     1.0       826
#' 4                 1          218           NA              edges       NULL     1.0       827
#' 5                 1           11           NA  networkAttributes       NULL     1.0        NA
#' 6                 1          210           NA     nodeAttributes       NULL     1.0        NA
#' 7                 1          654           NA     edgeAttributes       NULL     1.0        NA
#' 8                 1           30           NA    cartesianLayout       NULL     1.0        NA
#' 9                 1            3           NA cyVisualProperties       NULL     1.0        NA
#' 10                1            1           NA cyHiddenAttributes       NULL     1.0        NA 
#' 
#' 
#' ## The vertices:
#'> V(<%= ifelse(exists("graphObject"), tolower(graphObject), "rcxgraph") %>)[[]]
#' + 30/30 vertices, named, from f99ed1e:
#'      name X.id      n            Basal          avg_PPR             LumA             LumB         ANOVA_FDR           ANOVA_p             Her2
#' 1    ABL1  332   ABL1 2.10282304283E-4 0.00153239525422 2.40855043132E-4 0.00243281556888   0.0126090186692  5.20659657795E-4  0.0085750597325
#' 2    JAK2  331   JAK2 0.00818834880039 0.00229451204468 0.00112294137386 5.02052428871E-4  2.16495388014E-4  5.05285465741E-6 0.00264438694116
#' 3  NOTCH4  330 NOTCH4 0.00271376234049 0.00146318626692 9.53762145731E-4 0.00122487574678  2.16495388014E-4  4.79793511798E-6 0.00258535935846
#' 4   KMT2C  329  KMT2C 0.00628341277798 0.00228045774193 0.00156001096343 0.00150488341005   0.0227077886547  9.78432545266E-4 9.99799190468E-4
#' 5  NOTCH1  328 NOTCH1 0.00839759047274  0.0185377425722  0.0238783637494  0.0174878270867 5.65380895701E-13 4.06018596554E-15  0.0104288971099 
#' 6 ...
#' 
#' ## Display the igraph vertex names
#'> V(<%= ifelse(exists("graphObject"), tolower(graphObject), "rcxgraph") %>)$name
#'  [1] "ABL1"   "JAK2"   "NOTCH4" "KMT2C"  "NOTCH1" "INPPL1" "SPOP"   "AKT1"   "MYC"    "GATA3"  "MET"    "PTPRM"  "ACVR1B" "PTK2"   "PLCB1"
#' [16] "IBSP"   "ERBB2"  "PIK3CA" "EGFR"   "TLN1"   "MYCN"   "KRAS"   "CDH1"   "TP53"   "CCNE1"  "CCND1"  "CARD11" "STK11"  "MSH6"   "PER1"  
#' 
#' ## The igraph vertex names equal to the ndex node names by default
#'> V(<%= ifelse(exists("graphObject"), tolower(graphObject), "rcxgraph") %>)$n
#'  [1] "ABL1"   "JAK2"   "NOTCH4" "KMT2C"  "NOTCH1" "INPPL1" "SPOP"   "AKT1"   "MYC"    "GATA3"  "MET"    "PTPRM"  "ACVR1B" "PTK2"   "PLCB1"
#' [16] "IBSP"   "ERBB2"  "PIK3CA" "EGFR"   "TLN1"   "MYCN"   "KRAS"   "CDH1"   "TP53"   "CCNE1"  "CCND1"  "CARD11" "STK11"  "MSH6"   "PER1"  
#' 
#' ## If in \link{rcx_toRCXgraph} or \link{rcxgraph_fromRCX} \quote{idAsVertexName} is set, the igraph vertex names equal to the ndex node IDs ("@id")
#' ## Note: "@id" is displayed in the vertex summary as "X.id", but can be accessed using "@id"
#'> V(<%= ifelse(exists("graphObject"), tolower(graphObject), "rcxgraph") %>)$'@id'
#' [1] 332 331 330 329 328 327 326 325 324 323 322 321 320 319 318 317 316 315 314 313 312 311 310 309 308 307 306 305 304 303
#' 
#' 
#' ## The edges:
#'> E(<%= ifelse(exists("graphObject"), tolower(graphObject), "rcxgraph") %>)[[]] 
#' + 218/218 edges from f99ed1e (vertex names):
#'       tail   head tid hid           name X.id              i                           NAME          strength    interaction
#' 1     ABL1   EGFR   1  19 interacts with  550 interacts with     ABL1 (interacts with) EGFR   2.6501871987E-8 interacts with
#' 2     ABL1  GATA3   1  10 interacts with  549 interacts with    ABL1 (interacts with) GATA3  0.00179423650681 interacts with
#' 3     ABL1 INPPL1   1   6 interacts with  548 interacts with   ABL1 (interacts with) INPPL1  0.00531125201389 interacts with
#' 4     ABL1   JAK2   1   2 interacts with  547 interacts with     ABL1 (interacts with) JAK2  0.00668830637406 interacts with
#' 5     ABL1    MET   1  11 interacts with  546 interacts with      ABL1 (interacts with) MET  2.37014815511E-5 interacts with
#' 6 ...
#' 
#' ## Display the igraph edge names
#'> E(<%= ifelse(exists("graphObject"), tolower(graphObject), "rcxgraph") %>)$name
#' [1] "interacts with" "interacts with" "interacts with" "interacts with" "interacts with" "interacts with" "interacts with"
#' [8] ...
#' 
#' ## The igraph edge names equal to the ndex edge "i"nteraction by default
#'> E(<%= ifelse(exists("graphObject"), tolower(graphObject), "rcxgraph") %>)$i
#' [1] "interacts with" "interacts with" "interacts with" "interacts with" "interacts with" "interacts with" "interacts with" 
#' [8] ...
#' 
#' ## If in \link{rcx_toRCXgraph} or \link{rcxgraph_fromRCX} \quote{idAsEdgeName} is set, the igraph edge names equal to the ndex edge IDs ("@id")
#' ## Note: "@id" is displayed in the edge summary as "X.id", but can be accessed using "@id"
#'> E(<%= ifelse(exists("graphObject"), tolower(graphObject), "rcxgraph") %>)$'@id'
#'  [1] 550 549 548 547 546 545 544 543 542 541 540 539 538 537 536 535 534 533 532 531 530 529 528 527 526 525 524 523 522 
#' [30] ...
#'}
