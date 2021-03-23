artificialNeuralNetworkModel <-
function(x, y, pcaMethod=NULL, pcaParams=NULL){
  library('RSNNS')

  # 33% for tests ; 66% for training
  disc = (y>mean(y))+0
  
  dados = splitForTrainingAndTest(x, disc, ratio=.33)

  dados = normTrainingAndTestSet(dados, dontNormTargets=T)


  m = mlp(x=dados$inputsTrain, y=dados$targetsTrain,
          inputsTest =dados$inputsTest, targetsTest=dados$targetsTest,
          #size=c(ceiling(length(s)*.15)),
          size=5,
          learnFuncParams=c(.3), maxit=50)

  resp = abs(m$IterativeTestError[length(m$IterativeTestError)])
  
  return(resp)
}

