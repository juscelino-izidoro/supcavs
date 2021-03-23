sampleComplexityPAC <-
function(nVar, maxRem, error=.1, signif=.05){
  rems=c(0:maxRem)
  rest=nVar-rems

  VC_Dim_MLRM = vcDimensionLinearRegression(rest)
  VC_Dim_ANN = vcDimensionArtificialNeuralNetwork(rest)
  VC_Dim_NB  = vcDimensionNaiveBayes(rest)

  SampleSize_PAC_ANN_Upper  = round( (1/error)*(4*log10(2/signif) + 8*VC_Dim_ANN*log10(13/error)) )
  SampleSize_PAC_MLRM_Upper = round( (1/error)*(4*log10(2/signif) + 8*VC_Dim_MLRM*log10(13/error)) )
  SampleSize_PAC_NB_Upper   = round( (1/error)*(4*log10(2/signif) + 8*VC_Dim_NB*log10(13/error)) )
  
tab = cbind(
    VariaveisRemovidas=rems,
    VariaveisRestantes=rest,
    SampleSize_PAC_ANN_Upper=SampleSize_PAC_ANN_Upper,
    SampleSize_PAC_MLRM_Upper=SampleSize_PAC_MLRM_Upper,
    SampleSize_PAC_NB_Upper=SampleSize_PAC_NB_Upper
  )
  
  return(tab)
}

