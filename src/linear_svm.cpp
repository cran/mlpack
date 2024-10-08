/**
 * @file src/linear_svm.cpp
 *
 * This is an autogenerated file containing implementations of C++ functions to
 * be called by the R linear_svm binding.
 */
#include <rcpp_mlpack.h>
#define BINDING_TYPE BINDING_TYPE_R
#include <mlpack/methods/linear_svm/linear_svm_main.cpp>

#define Realloc(p,n,t) (t *) R_chk_realloc( (void *)(p), (R_SIZE_T)((n) * sizeof(t)) )
#define Free(p)        (R_chk_free( (void *)(p) ), (p) = NULL)

// [[Rcpp::export]]
void linear_svm_call(SEXP params, SEXP timers)
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

// Get the pointer to a LinearSVMModel parameter.
// [[Rcpp::export]]
SEXP GetParamLinearSVMModelPtr(SEXP params,
                                   const std::string& paramName,
                                   SEXP inputModels)
{
  util::Params& p = *Rcpp::as<Rcpp::XPtr<util::Params>>(params);
  Rcpp::List inputModelsList(inputModels);
  LinearSVMModel* modelPtr = p.Get<LinearSVMModel*>(paramName);
  for (int i = 0; i < inputModelsList.length(); ++i)
  {
    Rcpp::XPtr<LinearSVMModel> inputModel =
        Rcpp::as<Rcpp::XPtr<LinearSVMModel>>(inputModelsList[i]);
    // Don't create a new XPtr---just reuse the one given as input, so that we
    // don't end up deleting it twice.
    if (inputModel.get() == modelPtr)
      return inputModel;
  }

  return std::move((Rcpp::XPtr<LinearSVMModel>) p.Get<LinearSVMModel*>(paramName));
}

// Set the pointer to a LinearSVMModel parameter.
// [[Rcpp::export]]
void SetParamLinearSVMModelPtr(SEXP params, const std::string& paramName, SEXP ptr)
{
  util::Params& p = *Rcpp::as<Rcpp::XPtr<util::Params>>(params);
  p.Get<LinearSVMModel*>(paramName) = Rcpp::as<Rcpp::XPtr<LinearSVMModel>>(ptr);
  p.SetPassed(paramName);
}

// Serialize a LinearSVMModel pointer.
// [[Rcpp::export]]
Rcpp::RawVector SerializeLinearSVMModelPtr(SEXP ptr)
{
  std::ostringstream oss;
  {
    cereal::BinaryOutputArchive oa(oss);
    oa(cereal::make_nvp("LinearSVMModel",
          *Rcpp::as<Rcpp::XPtr<LinearSVMModel>>(ptr)));
  }

  Rcpp::RawVector raw_vec(oss.str().size());

  // Copy the string buffer so we can return one that won't get deallocated when
  // we exit this function.
  memcpy(&raw_vec[0], oss.str().c_str(), oss.str().size());
  raw_vec.attr("type") = "LinearSVMModel";
  return raw_vec;
}

// Deserialize a LinearSVMModel pointer.
// [[Rcpp::export]]
SEXP DeserializeLinearSVMModelPtr(Rcpp::RawVector str)
{
  LinearSVMModel* ptr = new LinearSVMModel();

  std::istringstream iss(std::string((char *) &str[0], str.size()));
  {
    cereal::BinaryInputArchive ia(iss);
    ia(cereal::make_nvp("LinearSVMModel", *ptr));
  }

  // R will be responsible for freeing this.
  return std::move((Rcpp::XPtr<LinearSVMModel>)ptr);
}


