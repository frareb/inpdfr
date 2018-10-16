#' Copy of the identifyStructure function from Tad Dallas metacom package.
#'
#' Identifies structure (or quasi-structure) and outputs a classification.
#'
#' @param metacom.obj The result of the `Metacommunity` function, containing a 
#'   list of 4 elements; the empirical matrix being tested, and results for 
#'   coherence, turnover, and boundary clumping. 
#' @return Ouputs a classification of the metacommunity. 
#' @note Quasi structures, as well as 'random' and 'Gleasonian' structures, 
#'   may not strictly be discernable through the EMS approach, as they rely on 
#'   inferring a result from a non-significant test ('accepting the null'), 
#'   which is typically a bad idea. 
#' @details Tad Dallas <tdallas@uga.edu> identifyStructure function no longer 
#'   maintained in metacom package. see https://github.com/taddallas/metacom. 
#'   This function was copy-pasted from version 1.4.4 of package metacom with 
#'   minor modification (fix warning: the condition has length > 1 and only the 
#'   first element will be used).
#' @export
IdentifyStructure=function(metacom.obj) {
  #Coherence
  if(as.numeric(t(metacom.obj$Coherence)[,3][1]) >= 0.05) "Random" else
    if(as.numeric(t(metacom.obj$Coherence)[,1][1]) < as.numeric(t(metacom.obj$Coherence)[,4]) &
       as.numeric(t(metacom.obj$Coherence)[,3][1]) < 0.05) "Checkerboard (negative coherence)" else
         if(as.numeric(t(metacom.obj$Coherence)[,1][1]) >= as.numeric(t(metacom.obj$Coherence)[,4][1]) &
            as.numeric(t(metacom.obj$Coherence)[,3][1]) < 0.05) {
           print("Positive coherence...")
           #Significant positive turnover
           if(as.numeric(t(metacom.obj$Turnover)[,1][1]) >= as.numeric(t(metacom.obj$Turnover)[,4][1]) &
              as.numeric(t(metacom.obj$Turnover)[,3][1]) <= 0.05 &
              metacom.obj$Boundary[,1] >= 0 & metacom.obj$Boundary[,2][1] < 0.05) "Clementsian" else
                if(as.numeric(t(metacom.obj$Turnover)[,1][1]) >= as.numeric(t(metacom.obj$Turnover)[,4][1]) &
                   as.numeric(t(metacom.obj$Turnover)[,3][1]) <= 0.05 &
                   metacom.obj$Boundary[,1][1] < 0 & metacom.obj$Boundary[,2][1] >= 0.05) "Gleasonian" else
                     if(as.numeric(t(metacom.obj$Turnover)[,1][1]) >= as.numeric(t(metacom.obj$Turnover)[,4][1]) &
                        as.numeric(t(metacom.obj$Turnover)[,3][1]) <= 0.05 &
                        metacom.obj$Boundary[,1][1] < 0 & metacom.obj$Boundary[,2][1] < 0.05) "Evenly spaced" else
                          #Significant negative turnover
                          if(as.numeric(t(metacom.obj$Turnover)[,1][1]) < as.numeric(t(metacom.obj$Turnover)[,4]) &
                             as.numeric(t(metacom.obj$Turnover)[,3][1]) <= 0.05 &
                             metacom.obj$Boundary[,1][1] >=0 & metacom.obj$Boundary[,2][1] < 0.05) "Nested (clumped)" else
                               if(as.numeric(t(metacom.obj$Turnover)[,1][1]) < as.numeric(t(metacom.obj$Turnover)[,4][1]) &
                                  as.numeric(t(metacom.obj$Turnover)[,3][1]) <= 0.05 &
                                  metacom.obj$Boundary[,1][1] < 0 & metacom.obj$Boundary[,2][1] >= 0.05) "Nested (random)" else
                                    if(as.numeric(t(metacom.obj$Turnover)[,1][1]) < as.numeric(t(metacom.obj$Turnover)[,4][1]) &
                                       as.numeric(t(metacom.obj$Turnover)[,3][1]) <= 0.05 &
                                       metacom.obj$Boundary[,1][1] < 0 & metacom.obj$Boundary[,2][1] < 0.05) "Nested (hyperdispersed" else
                                         #Non-significant positive turnover
                                         if(as.numeric(t(metacom.obj$Turnover)[,1][1]) >= as.numeric(t(metacom.obj$Turnover)[,4][1]) &
                                            as.numeric(t(metacom.obj$Turnover)[,3][1]) > 0.05 &
                                            metacom.obj$Boundary[,1][1] >=0 & metacom.obj$Boundary[,2][1] < 0.05) "Quasi-clementsian" else
                                              if(as.numeric(t(metacom.obj$Turnover)[,1][1]) >= as.numeric(t(metacom.obj$Turnover)[,4][1]) &
                                                 as.numeric(t(metacom.obj$Turnover)[,3][1]) > 0.05 &
                                                 metacom.obj$Boundary[,1][1] < 0 & metacom.obj$Boundary[,2][1] >= 0.05) "Quasi-gleasonian" else
                                                   if(as.numeric(t(metacom.obj$Turnover)[,1][1]) >= as.numeric(t(metacom.obj$Turnover)[,4][1]) &
                                                      as.numeric(t(metacom.obj$Turnover)[,3][1]) > 0.05 &
                                                      metacom.obj$Boundary[,1][1] < 0 & metacom.obj$Boundary[,2][1] < 0.05) "Quasi-evenly spaced" else
                                                        #Non-significant negative turnover
                                                        if(as.numeric(t(metacom.obj$Turnover)[,1][1]) < as.numeric(t(metacom.obj$Turnover)[,4][1]) &
                                                           as.numeric(t(metacom.obj$Turnover)[,3][1]) > 0.05 &
                                                           metacom.obj$Boundary[,1][1] >=0 & metacom.obj$Boundary[,2][1] < 0.05) "Quasi-nested (clumped)" else
                                                             if(as.numeric(t(metacom.obj$Turnover)[,1][1]) < as.numeric(t(metacom.obj$Turnover)[,4][1]) &
                                                                as.numeric(t(metacom.obj$Turnover)[,3][1]) > 0.05 &
                                                                metacom.obj$Boundary[,1] < 0 & metacom.obj$Boundary[,2][1] >= 0.05) "Quasi-nested (random)" else
                                                                  if(as.numeric(t(metacom.obj$Turnover)[,1][1]) < as.numeric(t(metacom.obj$Turnover)[,4][1]) &
                                                                     as.numeric(t(metacom.obj$Turnover)[,3][1]) > 0.05 &
                                                                     metacom.obj$Boundary[,1][1] < 0 & metacom.obj$Boundary[,2][1] < 0.05) "Quasi-nested (hyperdispersed)" } }