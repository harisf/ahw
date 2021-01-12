#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
DataFrame calculateCumHazard(DataFrame Times) {
  NumericVector to = Times["to"];
  std::sort(to.begin(), to.end());
  to.erase(std::unique(to.begin(), to.end()), to.end());
  
  //for (double t : to)
    
  return to;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
Times <- data.frame(
  x = 1:8,
  to = c(0.1,0.3,0.3,0.4,1.2,1.2,1.2,0.3)
)
calculateCumHazard(Times)
*/
