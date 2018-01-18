
#' combine.samples
#' 
#' Takes two mcmc objects, and combines the samples together.  
#' @param sample1 An mcmc object
#' @param sample2 Another mcmc object
#' @return The two mcmc objects smushed together
#' @export
#' @examples
#' combine.samples(sample1, sample2) #pretty straightforward
combine.samples <- function(sample1, sample2) {
  if ( length(sample1) == 1 ) sample1 <- sample1[[1]]
  if ( length(sample2) == 1 ) sample2 <- sample2[[1]]
  sampleCombo <- rbind(sample1, sample2)
  class(sampleCombo) <- 'mcmc'
  attr(sampleCombo,'dimnames') <- attr(sample1,'dimnames')
  attr(sampleCombo,'mcpar') <- c(attr(sample1,'mcpar')[1],attr(sample2,'mcpar')[2],attr(sample1,'mcpar')[3])
  return(sampleCombo)
}



#' grabFromSamples
#' 
#' collects a parameter from an mcmc objects.  
#' @param samples the mcmc sample to collect samples from
#' @param pattern A naming pattern to be extracted from the mcmc object, or a vector of such names
#' @return if pattern is a single string, returns a vector. Otherwise, returns a tibble of tectors
#' @export
#' @examples
#' grabFromSamples(samples, 'muPrior') # returns vector
#' grabFromSamples(samples, c('muPrior', 'tauPrior'))  # returns tibble
grabFromSamples <- function(samples, pattern){
  if(length(pattern)==1){
    items <-grep(pattern, names(samples))
    if(length(items)==0){ stop("All pattern names must appear in samples")}
    return(samples[,grep(pattern, names(samples))])
  } else if(length(pattern) > 1){
      map_dfc(pattern, function(x) {
        enframe(grabFromSamples(samples, x), value=x)[x]
        })
    
  }
}


#' par.trace.samples
#' 
#' Composite function that handles creating, initing, and running jags models in parallel, then collects
#' the resulting samples. 
#' @param file A jags file
#' @param data Any data values, in a list, as though you were passing them to jags.model (you are)
#' @param n.adapt The number of adapts to do
#' @param inits Initial parameters
#' @param n.iter A number of iterations to run on the update cycle
#' @param n.samples The number of samples to collect. 
#' @param thin How much to thin
#' @param monitor What variables do we want to monitor?
#' @param n.breaks How often should we pause to update the user about progress?
#' @param predictionVariable If there is a prediction variable, that variable can be produced in a special cycle, to avoid generating too much data
#' @return a list of samples
#' @export
#' @examples
#' results <- par.trace.samples(file=paste(workingDir, jagsFile , sep="")
#'     , data=jagsData
#'     , monitor=monitor
#'     , n.iter = updateNum
#'     , n.adapt = n.adapt
#'     , n.samples = nSamplesCoda
#'     , n.breaks=5
#'     , thin=thinning
#'     )
par.trace.samples <- function(file,data,n.adapt=5e2, inits=NULL,
                              n.iter=1e3,n.samples=500, thin=1,monitor,n.breaks=c(10,2,5)
                              , predictionVariable=FALSE) {
  
  Log <- function(text, ..., file="junk.txt") { # listen with nc -l 4000
    msg <- sprintf(paste0(as.character(Sys.time()), ": ", text, "\n"), ...)
    cat(msg)
    if(exists("log.socket")){
      write.socket(log.socket, msg)
    } else {
      cat(msg, file=file, append=T)
    }
  }
  
  # inits: function(i) where i will be from 1 to getDoParWorkers()
  Log(paste("Starting to process jags file: ", file))
  n.breaks <- as.numeric(match.arg(as.character(n.breaks),c("10","2","5")))
  if ( is.null(inits) ) {
    inits <- function(i) list(.RNG.name='lecuyer::RngStream',.RNG.seed=i)
  } else if ( !is.function(inits) ) {
    warning("inits must be a function"); return(NA)
  }
  x <- system.time({result <- foreach(i=1:getDoParWorkers()) %dopar% {
    model <- jags.model(file=file,data=data,n.adapt=0,inits=inits(i),
                        n.chains=1,quiet=T)
    update(model,n.iter=n.adapt,progress.bar="none")
    Log(paste( "\nChain ",i," finished adapting.\n"))
    # Break up a single sampling call into multiple sampling calls.
    samples <- list()
    for ( j in 1:n.breaks ) {
      samples[[j]] <- coda.samples(model,monitor,n.iter=n.samples,thin=thin,progress.bar="none")[[1]]
      Log(paste("\nChain ", i, " is ", round(j/n.breaks*100,3), " Percent done.", sep=""))
      
    }
    # Combine all the mini-samples into a single sample.
    samples <- Reduce(combine.samples,samples)
    # Return coefs (values of unobserved nodes) & samples.
    return(list(coef=coef(model),samples=samples, model=model))
  }})
  # x : times ['user' 'system' 'elapsed'] in seconds
  samples <- as.mcmc.list(lapply(result,function(x)x[['samples']]))
  coef <- lapply(result,function(x)x[['coef']]) # Can be used as inits
  if(predictionVariable!=FALSE){
    model <- jags.model(file=file,data=data,n.adapt=0,inits=inits(1),
                        n.chains=1,quiet=T)
    update(model,n.iter=n.adapt,progress.bar="none")
    predSample <- coda.samples(model,c(monitor, predictionVariable),n.iter=1,thin=1,progress.bar="none")[[1]]
  } else {
    predSample <- 1
  }
  list(runtime=x['elapsed'],samples=samples,coef=coef, predSample=predSample)
}