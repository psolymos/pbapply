\name{NEWS}
\title{dclone News}
\encoding{UTF-8}

\section{Changes in version 1.3}{
  \subsection{GENERAL}{
    \itemize{
      \item 
    }
  } % end GENERAL
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item 
    }
  } % end NEW FUNCTIONS
  \subsection{NEW FEATURES}{
    \itemize{
      \item 
    }
  } % end NEW FEATURES
  \subsection{BUG FIXES}{
    \itemize{
      \item 
    }
  } % end BUG FIXES
  \subsection{ANALYSES}{
    \itemize{
      \item 
    }
  } % end ANALYSES
}% end VERSTION 1.3

\section{Changes in version 1.2}{
  \subsection{GENERAL}{
    \itemize{
      \item 
    }
  } % end GENERAL
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item 
    }
  } % end NEW FUNCTIONS
  \subsection{NEW FEATURES}{
    \itemize{
      \item 
    }
  } % end NEW FEATURES
  \subsection{BUG FIXES}{
    \itemize{
      \item 
    }
  } % end BUG FIXES
  \subsection{ANALYSES}{
    \itemize{
      \item 
    }
  } % end ANALYSES
}% end VERSTION 1.2

\section{Changes in version 1.1}{
  \subsection{GENERAL}{
    \itemize{
      \item 
    }
  } % end GENERAL
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item 
    }
  } % end NEW FUNCTIONS
  \subsection{NEW FEATURES}{
    \itemize{
      \item 
    }
  } % end NEW FEATURES
  \subsection{BUG FIXES}{
    \itemize{
      \item 
    }
  } % end BUG FIXES
  \subsection{ANALYSES}{
    \itemize{
      \item 
    }
  } % end ANALYSES
}% end VERSTION 1.1

\section{Changes in version 1.0}{
  \subsection{GENERAL}{
    \itemize{
      \item 

    }
  } % end GENERAL
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item 
    }
  } % end NEW FUNCTIONS
  \subsection{NEW FEATURES}{
    \itemize{
      \item Methods for \code{mcmc.list} objects added 
        (\code{coef}, \code{vcov}, \code{quantile}).
      \item Flavour argument added in dc.parfit.
      \item \code{plot.dcdiag} got \code{log.lambda.max} option.
    * 
    * 
    * log.var type of plot.dctable changed.
    }
  } % end NEW FEATURES
  \subsection{BUG FIXES}{
    \itemize{
      \item 
    }
  } % end BUG FIXES
  \subsection{ANALYSES}{
    \itemize{
      \item 
    }
  } % end ANALYSES
}% end VERSTION 1.0


Version 1.0-6, March 14, 2010
    * openbugs.seed removed.
    * bugs.fit fixed, several arguments removed.
    * dcpar package merged with dclone.
    * WinBUGS/OpenBUGS related revision done:
      as.mcmc.list.bugs and dc.fit now works properly.
    * dclone.ts removed, use dcdim instead.

Version 1.0-5, January 25, 2010
    * drop and perm arguments in dcdim.

Version 1.0-4, January 14, 2010
    * nclones.list: returns 'method' attributes.
    * vcov: got invfisher argument.
    * S3 methods added for diagnostic functions.
    * dclone options restructured.
    * dclone and dcpar split: parallet computing features
      in separate package.
    * dcdiag: diagnostic tools for data cloning.
    * chisq.diag: diagnostic tool added.

Version 1.0-3, December 14, 2009
    * lambdamax.diag: added back.
    * errlines: got col argument.
    * jags.fit: if n.iter=0, the (updated) JAGS model is returned
      instead of an MCMC object.
    * update.mcmc.list: function to perform automatic updates of an
      MCMC object, until a desired statistics value 
      (e.g. R_hat < 1.1) reached.

Version 1.0-2, December 4, 2009
    * updated.model attr in jags.fit output for further updating.
    * bg arg in errlines and box.bg in plot.dctable for background
      color of boxes.

Version 1.0-1, November 19, 2009
    * custommodel function added, fitting functions modified.

Version 1.0-0, November 16, 2009
    * Unload problem fixed (reported by Brian D Ripley).
    * dctable now can take more than one models as arguments.
    * Code annotated, documentation revised.

\section{Changes in version 0.9}{
  \subsection{GENERAL}{
    \itemize{
      \item First CRAN release.
        Initial functionality is stable.
    }
  } % end GENERAL
  \subsection{NEW FUNCTIONS}{
    \itemize{
      \item Inital set of core functions released.
    }
  } % end NEW FUNCTIONS
  \subsection{NEW FEATURES}{
    \itemize{
      \item Support for WinBUGS.
    }
  } % end NEW FEATURES
  \subsection{BUG FIXES}{
    \itemize{
      \item No bug fixes.
    }
  } % end BUG FIXES
  \subsection{ANALYSES}{
    \itemize{
      \item GLM and Poisson-Lognormal examples part of the release.
    }
  } % end ANALYSES
}% end VERSTION 0.9

dclone package version history

Version 1.3-5, August 23, 2011
    * n.update=1000 is now the default instead of 0
      to ensure independence after adaptation.
      Warning is issued if n.adapt>0 and n.update=0.
    * New argument partype added to dc.parfit,
      possible options (balancing, parchains, both)
      are described on the help page.
    * dc.parfit gained args (update, updatefun, initsfun)
      which are ignored if type is not "parchains".
    * dc.fit and dc.parfit (with "parchains" type)
      now call new function dcFit (former dc.fit reworked
      to allow parallel chains via jags.parfit).
      This function is not exported by namespace, thus
      not user visible and undocumented.
    * dclone is loaded to workers by snowWrapper
      only if .packages() does not contain it yet.

Version 1.3-4, July 9, 2011
    * dc.fit: updating fixed (only updated after 2nd iteration).
    * dc.fit: updatefun and initsfun now accepts 2 arguments,
      1st is model, 2nd is n.clones to allow for cloning
      latent variable vectors on the go. Example reworked accordingly.
    * snowWrapper: cleanup=TRUE cleans up after lib and dir args, too.
    * New option 'overwrite' in dcoptions (used by write.jags.model).
    * packageStartupMessage used in zzz.R .onAttach
    * ... is now passed to snowWrapper in *.parfit (thanks to
      Emmanuel Charpentier for reporting the bug).

Version 1.3-3, May 13, 2011
    * *.parfit: write model to hard drive only if
      cluster type is SOCK (shared memory)
    * snowWrapper: dir=NULL is set as the default.
    * mcmcapply functions simplified, FUN can be missing.
    * jags.parfit help notes that there is no update method
      for parallel mcmc.list objects.
    * snowWrapper got cleanup=TRUE argument to remove
      data after evaluating main function.

Version 1.3-2, March 2, 2011
    * Partial argument match fixed (NOTE from R CMD CHECK).
    * write.jags.model got new argument overwrite=FALSE.
    * Typo in rng.type argument of snowWrapper fixed.
    * plotClusterSize: col arg is reordered according to
      balancing type.

Version 1.3-1, January 4, 2011
    * JASA paper (Lele et al. 2010) citation added to Rd.
    * loading rjags changed: it is now only suggested.
      Reason: some dclone functionality might be useful
      without JAGS/rjags present, and now loading
      rjags won't cause error if JAGS shared library
      not found.
    * glm module not loading at startup because
      it cannot be nicely unloaded in parallel versions.
    * dcoptions RNG got "none" value as default,
      *.parfit functions use dcoptions for snowWrapper.
      As a result, dependencies changed a bit.
    * dc.parfit: none/both balancing fixed (were switched).

Version 1.3-0, December 31, 2010
    * preparing for major CRAN release.
    * final RJ citation added.

Version 1.2-3, December 16, 2010
    * bugs.parfit: migrated into the new dcextras package
      where experimental and not fully supported functions
      will reside in the future.

Version 1.2-2, October 21, 2010
    * expanded examples for *.parfit functions.
    * clusterSeed removed from package: use
      clusterSetupRNG in snow instead.

Version 1.2-1, August 26, 2010
    * experimenting with bugs.parfit:
      the different seeds approach is not satisfactory
      because it cannot garantee independence of the chains
    * seed arg added to bugs.fit
    * RNG option added to work with snowWrapper
    * set.rng arg added to snowWrapper that calls
      snow::clusterSetupRNG
    * options for dclone reworked: can be accessed via
      the dcoptions function, and it sets the dcoptions option
    * dclone.dcdim: dimnames bug fixed when drop=TRUE
    * examples added on inits specifications for *.parfit

Version 1.2-0, August 26, 2010
    * polishing plot methods for R Journal paper.
    * citation entry added.

Version 1.1-1, May 6, 2010
    * print.custommodel has deparse=FALSE argument.
    * glm module for JAGS >2.0 is loaded at startup
    * parallel RNG handling reworked.
    * dctable fix for one parameter mcmc.list.
    * dcdiag returns r.hat for one parameter models.

Version 1.1-0, May 3, 2010
    * dclone was tested with JAGS 2.0.
    * major release submitted to CRAN.

