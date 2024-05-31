/**
 * @file src/gmm_train.cpp
 *
 * This is an autogenerated file containing implementations of C++ functions to
 * be called by the R gmm_train binding.
 */
#include <rcpp_mlpack.h>
#define BINDING_TYPE BINDING_TYPE_R
#include <mlpack/methods/gmm/gmm_train_main.cpp>

#define Realloc(p,n,t) (t *) R_chk_realloc( (void *)(p), (R_SIZE_T)((n) * sizeof(t)) )
#define Free(p)        (R_chk_free( (void *)(p) ), (p) = NULL)

// [[Rcpp::export]]
void gmm_train_call(SEXP params, SEXP timers)
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

// Get the pointer to a GMM parameter.
// [[Rcpp::export]]
SEXP GetParamGMMPtr(SEXP params,
                                   const std::string& paramName,
                                   SEXP inputModels)
{
  util::Params& p = *Rcpp::as<Rcpp::XPtr<util::Params>>(params);
  Rcpp::List inputModelsList(inputModels);
  GMM* modelPtr = p.Get<GMM*>(paramName);
  for (int i = 0; i < inputModelsList.length(); ++i)
  {
    Rcpp::XPtr<GMM> inputModel =
        Rcpp::as<Rcpp::XPtr<GMM>>(inputModelsList[i]);
    // Don't create a new XPtr---just reuse the one given as input, so that we
    // don't end up deleting it twice.
    if (inputModel.get() == modelPtr)
      return inputModel;
  }

  return std::move((Rcpp::XPtr<GMM>) p.Get<GMM*>(paramName));
}

// Set the pointer to a GMM parameter.
// [[Rcpp::export]]
void SetParamGMMPtr(SEXP params, const std::string& paramName, SEXP ptr)
{
  util::Params& p = *Rcpp::as<Rcpp::XPtr<util::Params>>(params);
  p.Get<GMM*>(paramName) = Rcpp::as<Rcpp::XPtr<GMM>>(ptr);
  p.SetPassed(paramName);
}

// Serialize a GMM pointer.
// [[Rcpp::export]]
Rcpp::RawVector SerializeGMMPtr(SEXP ptr)
{
  std::ostringstream oss;
  {
    cereal::BinaryOutputArchive oa(oss);
    oa(cereal::make_nvp("GMM",
          *Rcpp::as<Rcpp::XPtr<GMM>>(ptr)));
  }

  Rcpp::RawVector raw_vec(oss.str().size());

  // Copy the string buffer so we can return one that won't get deallocated when
  // we exit this function.
  memcpy(&raw_vec[0], oss.str().c_str(), oss.str().size());
  raw_vec.attr("type") = "GMM";
  return raw_vec;
}

// Deserialize a GMM pointer.
// [[Rcpp::export]]
SEXP DeserializeGMMPtr(Rcpp::RawVector str)
{
  GMM* ptr = new GMM();

  std::istringstream iss(std::string((char *) &str[0], str.size()));
  {
    cereal::BinaryInputArchive ia(iss);
    ia(cereal::make_nvp("GMM", *ptr));
  }

  // R will be responsible for freeing this.
  return std::move((Rcpp::XPtr<GMM>)ptr);
}


