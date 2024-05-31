/**
 * @file src/test_r_binding.cpp
 *
 * This is an autogenerated file containing implementations of C++ functions to
 * be called by the R test_r_binding binding.
 */
#include <rcpp_mlpack.h>
#define BINDING_TYPE BINDING_TYPE_R
#include <mlpack/bindings/R/tests//test_r_binding_main.cpp>

#define Realloc(p,n,t) (t *) R_chk_realloc( (void *)(p), (R_SIZE_T)((n) * sizeof(t)) )
#define Free(p)        (R_chk_free( (void *)(p) ), (p) = NULL)

// [[Rcpp::export]]
void test_r_binding_call(SEXP params, SEXP timers)
{
  util::Params& p = *Rcpp::as<Rcpp::XPtr<util::Params>>(params);
  util::Timers& t = *Rcpp::as<Rcpp::XPtr<util::Timers>>(timers);

  if (p.Has("verbose"))
    Log::Info.ignoreInput = false;
  else
    Log::Info.ignoreInput = true;

  BINDING_FUNCTION(p, t);
}

// Any implementations of methods for dealing with model pointers will be put
// below this comment, if needed.

// Get the pointer to a GaussianKernel parameter.
// [[Rcpp::export]]
SEXP GetParamGaussianKernelPtr(SEXP params,
                                   const std::string& paramName,
                                   SEXP inputModels)
{
  util::Params& p = *Rcpp::as<Rcpp::XPtr<util::Params>>(params);
  Rcpp::List inputModelsList(inputModels);
  GaussianKernel* modelPtr = p.Get<GaussianKernel*>(paramName);
  for (int i = 0; i < inputModelsList.length(); ++i)
  {
    Rcpp::XPtr<GaussianKernel> inputModel =
        Rcpp::as<Rcpp::XPtr<GaussianKernel>>(inputModelsList[i]);
    // Don't create a new XPtr---just reuse the one given as input, so that we
    // don't end up deleting it twice.
    if (inputModel.get() == modelPtr)
      return inputModel;
  }

  return std::move((Rcpp::XPtr<GaussianKernel>) p.Get<GaussianKernel*>(paramName));
}

// Set the pointer to a GaussianKernel parameter.
// [[Rcpp::export]]
void SetParamGaussianKernelPtr(SEXP params, const std::string& paramName, SEXP ptr)
{
  util::Params& p = *Rcpp::as<Rcpp::XPtr<util::Params>>(params);
  p.Get<GaussianKernel*>(paramName) = Rcpp::as<Rcpp::XPtr<GaussianKernel>>(ptr);
  p.SetPassed(paramName);
}

// Serialize a GaussianKernel pointer.
// [[Rcpp::export]]
Rcpp::RawVector SerializeGaussianKernelPtr(SEXP ptr)
{
  std::ostringstream oss;
  {
    cereal::BinaryOutputArchive oa(oss);
    oa(cereal::make_nvp("GaussianKernel",
          *Rcpp::as<Rcpp::XPtr<GaussianKernel>>(ptr)));
  }

  Rcpp::RawVector raw_vec(oss.str().size());

  // Copy the string buffer so we can return one that won't get deallocated when
  // we exit this function.
  memcpy(&raw_vec[0], oss.str().c_str(), oss.str().size());
  raw_vec.attr("type") = "GaussianKernel";
  return raw_vec;
}

// Deserialize a GaussianKernel pointer.
// [[Rcpp::export]]
SEXP DeserializeGaussianKernelPtr(Rcpp::RawVector str)
{
  GaussianKernel* ptr = new GaussianKernel();

  std::istringstream iss(std::string((char *) &str[0], str.size()));
  {
    cereal::BinaryInputArchive ia(iss);
    ia(cereal::make_nvp("GaussianKernel", *ptr));
  }

  // R will be responsible for freeing this.
  return std::move((Rcpp::XPtr<GaussianKernel>)ptr);
}


