#r-cran-rcpp
#r-cran-data.table
library(data.table)
Rcpp::sourceCpp(code = '
#include <Rcpp.h>
#include <fstream>      // <-- THIS WAS MISSING
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector CSV_Col_Means(std::string fname, int stepSize = 50009) {
  
  // R objects we will call from C++
  Function fread("fread");
  Function as_matrix("as.matrix");
  
  // open file
  std::ifstream file(fname.c_str());
  if (!file.is_open()) {
    stop("Unable to open file.");
  }

  std::string line;
  std::vector<std::string> buffer;
  buffer.reserve(stepSize);

  NumericVector sumHolder;  
  double nHolder = 0.0;
  bool firstChunk = true;

  while (true) {
    buffer.clear();
    
    // read up to stepSize lines
    for (int i = 0; i < stepSize && std::getline(file, line); i++) {
      buffer.push_back(line);
    }
    
    if (buffer.empty()) break;   // EOF
    
    // collapse lines into one string using \n
    std::string collapsed;
    collapsed.reserve(buffer.size() * 50); 
    for (const auto &s : buffer) {
      collapsed += s;
      collapsed += "\\n";
    }
    
    // call fread(text = collapsed)
    List chunkDT = fread(_["text"] = collapsed);
    NumericMatrix chunki = as_matrix(chunkDT);
    
    int nrow = chunki.nrow();
    int ncol = chunki.ncol();

    // initialize sumHolder on first chunk
    if (firstChunk) {
      sumHolder = NumericVector(ncol);
      firstChunk = false;
    }

    // accumulate column sums
    for (int j = 0; j < ncol; j++) {
      double s = 0.0;
      for (int r = 0; r < nrow; r++) {
        s += chunki(r, j);
      }
      sumHolder[j] += s;
    }
    
    // update nHolder
    nHolder += nrow;
  }

  // compute column means
  NumericVector means(sumHolder.size());
  for (int j = 0; j < sumHolder.size(); j++) {
    means[j] = sumHolder[j] / nHolder;
  }

  return means;
}
')
