// [[Rcpp::depends(Rcpp)]]
// [[Rcpp::plugins(cpp11)]]
#include <Rcpp.h>
#include <cmath>
#include <vector>
using namespace Rcpp;

// =====================================================================
// Grid / indexing helpers
// =====================================================================

// Column-major strides for a d-dimensional array with extents nvec.
std::vector<long> computeStrides(const std::vector<int>& nvec) {
  int d = nvec.size();
  std::vector<long> strides(d);
  strides[0] = 1;
  for (int i = 1; i < d; i++) strides[i] = strides[i - 1] * nvec[i - 1];
  return strides;
}

// Lag rows (original units) -> integer grid steps.
std::vector<std::vector<int> > computeLagSteps(const NumericMatrix& lagmat) {
  int nh = lagmat.nrow();
  int d = lagmat.ncol();
  std::vector<std::vector<int> > steps(nh, std::vector<int>(d));
  for (int h = 0; h < nh; h++)
    for (int c = 0; c < d; c++)
      steps[h][c] = (int) std::round(lagmat(h, c));
  return steps;
}

// A lag (in grid steps) -> single fixed linear offset into the flattened
// spatial grid. See explanation in the accompanying message: valid as long
// as the base point + lag stays within bounds along every dimension
// individually, which interiorRange() below guarantees.
std::vector<long> computeLagOffsets(const std::vector<std::vector<int> >& lagsteps,
                                    const std::vector<long>& strides) {
  int nh = lagsteps.size();
  int d = strides.size();
  std::vector<long> offsets(nh, 0);
  for (int h = 0; h < nh; h++)
    for (int c = 0; c < d; c++)
      offsets[h] += (long) lagsteps[h][c] * strides[c];
  return offsets;
}

// Max euclidean norm of the lag rows, in grid units.
double maxLagRadius(const NumericMatrix& lagmat) {
  int nh = lagmat.nrow();
  int d = lagmat.ncol();
  double Rmax = 0.0;
  for (int h = 0; h < nh; h++) {
    double ss = 0.0;
    for (int c = 0; c < d; c++) {
      double v = lagmat(h, c);
      ss += v * v;
    }
    double norm = std::sqrt(ss);
    if (norm > Rmax) Rmax = norm;
  }
  return Rmax;
}

// Interior index range [lo, hi] (0-based, inclusive) per dimension, mirroring
// ceiling(1+R):floor(n-R) in 1-based R indexing. Sets `empty` if any
// dimension's range is empty (unlike `lo:hi` in R, this never silently
// produces a reversed sequence).
void interiorRange(const std::vector<int>& nvec, double Rmax,
                   std::vector<int>& lo, std::vector<int>& hi, bool& empty) {
  int d = nvec.size();
  lo.resize(d); hi.resize(d);
  empty = false;
  for (int i = 0; i < d; i++) {
    int lo1 = (int) std::ceil(1.0 + Rmax);
    int hi1 = (int) std::floor((double) nvec[i] - Rmax);
    lo[i] = lo1 - 1;
    hi[i] = hi1 - 1;
    if (hi[i] < lo[i]) empty = true;
  }
}

// Advance idx (0-based, within [lo, hi] per dim) to the next point in
// column-major order. Returns false once all interior points are exhausted.
bool odometerNext(std::vector<int>& idx, const std::vector<int>& lo, const std::vector<int>& hi) {
  int d = idx.size();
  int dim0 = 0;
  while (dim0 < d) {
    idx[dim0]++;
    if (idx[dim0] > hi[dim0]) {
      idx[dim0] = lo[dim0];
      dim0++;
    } else {
      return true;
    }
  }
  return false; // wrapped past the last dimension -> done
}

// =====================================================================
// Extraction of z0 / zlagged at a grid point
// =====================================================================

void extractZ0(const NumericVector& spdata, long base0, long spatialSize, int k, NumericVector& z0) {
  for (int c = 0; c < k; c++) z0[c] = spdata[base0 + (long) c * spatialSize];
}

void extractZLagged(const NumericVector& spdata, long base0, const std::vector<long>& lagoffsets,
                    long spatialSize, int k, NumericMatrix& zlagged) {
  int nh = lagoffsets.size();
  for (int h = 0; h < nh; h++) {
    long lidx0 = base0 + lagoffsets[h];
    for (int c = 0; c < k; c++)
      zlagged(h, c) = spdata[lidx0 + (long) c * spatialSize];
  }
}

// =====================================================================
// Objective functions
// =====================================================================

// Squared euclidean-norm difference between z0 and each row of zlagged.
NumericVector sqDiffObjective(const NumericVector& z0, const NumericMatrix& zlagged) {
  int nh = zlagged.nrow();
  int k = zlagged.ncol();
  NumericVector obj(nh);
  for (int h = 0; h < nh; h++) {
    double ss = 0.0;
    for (int c = 0; c < k; c++) {
      double diff = z0[c] - zlagged(h, c);
      ss += diff * diff;
    }
    obj[h] = ss;
  }
  return obj;
}

// =====================================================================
// Best-index selection with random tie-break
// =====================================================================

int selectBestIndex(const NumericVector& obj_vals, bool minimize) {
  int nh = obj_vals.size();
  double best = obj_vals[0];
  for (int h = 1; h < nh; h++) {
    if ((minimize && obj_vals[h] < best) || (!minimize && obj_vals[h] > best)) best = obj_vals[h];
  }
  std::vector<int> ties;
  for (int h = 0; h < nh; h++) if (obj_vals[h] == best) ties.push_back(h + 1); // 1-based

  if (ties.size() > 1) {
    int r = (int) std::floor(R::unif_rand() * ties.size());
    return ties[r];
  }
  return ties[0];
}

// =====================================================================
// Shared grid-traversal driver
// =====================================================================

// Calls evalObjective(z0, zlagged) -> NumericVector at every interior grid
// point and records the best (tie-broken) lag index. Templated on the
// objective so the same traversal/indexing code serves both the R-callback
// path and the native fast path below.
template <typename ObjFun>
IntegerVector runGridSearch(const NumericVector& spdata,
                            const NumericMatrix& lagmat, bool minimize,
                            ObjFun evalObjective) {
  IntegerVector dimz = spdata.attr("dim");
  int ndim = dimz.size();
  int d = ndim - 1;
  int k = dimz[d];

  std::vector<int> nvec(d);
  for (int i = 0; i < d; i++) nvec[i] = dimz[i];

  int nh = lagmat.nrow();
  std::vector<long> strides = computeStrides(nvec);
  std::vector<std::vector<int> > lagsteps = computeLagSteps(lagmat);
  std::vector<long> lagoffsets = computeLagOffsets(lagsteps, strides);
  double Rmax = maxLagRadius(lagmat);

  // check that interior range is non-empty
  std::vector<int> lo, hi;
  bool empty;
  interiorRange(nvec, Rmax, lo, hi, empty);

  // set up output array
  long spatialSize = 1;
  for (int i = 0; i < d; i++) spatialSize *= nvec[i];

  IntegerVector spMaximizer(spatialSize, NA_INTEGER);
  spMaximizer.attr("dim") = dimz[Range(0, d - 1)];
  if (empty) return spMaximizer;

  NumericVector z0(k);
  NumericMatrix zlagged(nh, k);
  std::vector<int> idx = lo;

  RNGScope scope; // keep R's RNG state consistent for tie-break sampling

  bool more = true;
  while (more) {
    long base0 = 0;
    for (int i = 0; i < d; i++) base0 += (long) idx[i] * strides[i];

    // get coordinates
    extractZ0(spdata, base0, spatialSize, k, z0);
    extractZLagged(spdata, base0, lagoffsets, spatialSize, k, zlagged);

    // get values and compare
    NumericVector obj_vals = evalObjective(z0, zlagged);
    spMaximizer[base0] = selectBestIndex(obj_vals, minimize);

    // jump to next interior point
    more = odometerNext(idx, lo, hi);
  }

  return spMaximizer;
}

// =====================================================================
// Exported entry points
// =====================================================================

//' Finds maximizing lag at every point using a user-supplied R objective function
//'
//' @description
//' For each point of x spdata so that x+y is in spdata for every row y of lagmat,
//' computes the row index y maximizing the objective function at x. Uses
//' user-supplied R objective function which is evaluated at each point.
//'
//' @param spdata A numeric vector representing the spatial field values.
//' @param objfunc A user-supplied R function taking `(z0, zlagged)` and returning numeric scores.
//' @param lagmat A numeric matrix where each row represents a lag vector.
//' @param minimize Logical. Should the objective function be minimized? Default is `false`.
//'
//' @return An IntegerVector containing the optimal lag index for each spatial location.
//'
//' @name argmaxLagIndexCpp
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
IntegerVector argmaxLagIndexCpp(NumericVector spdata, Function objfunc,
                               NumericMatrix lagmat,
                               bool minimize = false) {
 return runGridSearch(spdata, lagmat, minimize,
                      [&](const NumericVector& z0, const NumericMatrix& zlagged) -> NumericVector {
                        return objfunc(z0, zlagged);
                      });
}

//' Finds maximizing lag at every point using norm of difference as objective function
//'
//' @description
//' For each point of x spdata so that x+y is in spdata for every row y of lagmat,
//' computes the row index y maximizing the objective function at x. Objective function
//' is squared norm of (multivariate) difference.
//' Bypasses R callbacks per grid point, making it substantially faster
//' and safe to parallelize.
//'
//' @param spdata A numeric vector representing the spatial field values.
//' @param lagmat A numeric matrix where each row represents a lag vector.
//' @param minimize Logical. Should the objective function be minimized? Default is `false`.
//'
//' @return An IntegerVector containing the optimal lag index for each spatial location.
//'
//' @name argmaxLagIndexSqDiffCpp
//' @keywords internal
//' @noRd
// [[Rcpp::export]]
IntegerVector argmaxLagIndexSqDiffCpp(NumericVector spdata,
                                     NumericMatrix lagmat, bool minimize = false) {
 return runGridSearch(spdata, lagmat, minimize,
                      [](const NumericVector& z0, const NumericMatrix& zlagged) -> NumericVector {
                        return sqDiffObjective(z0, zlagged);
                      });
}
