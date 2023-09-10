# Troubleshooting Fisheries Case Studies

The fisheries case study is one of the easier ones to be able to troubleshoot as it's only additional software requirement outside of R and R packages is JAGS which is, relative to Stan, easy to get going with. 

The most common challenge is path issues with Windows machines.

## Windows Machines

If you're having trouble with paths in your install on a Windows machine, there are a number of somewhat easys solutions to try. 

First, if you're trying to use `Set.System()`, then you'll need to recall that Windows works in `\` backslashes, as opposed to the standard `/` forward slash of every other operating system. If you have been using `\`, first attempt to use double backslashes `\\` in the path direction. 

Also ensure that JAGS is actually installed in your `Programs` folder. This is a common mistake. 

## MacOS Machines

The most common issues during install on a Mac are during the package install phase where trying to install `rjags` in R fails. The first and easiest solution is to try installing it from binary with `install.packages("rjags", type = "binary")`. 

Also common is that if you have installed homebrew, Mac will try to use your `gcc` compiler, though R itself is compiled with `clang` typically. You need to ensure both R and JAGS are being compiled with the same compilers, so either set your `$PATH` to use clang as your compiler, or manually compile R and JAGs with `gcc`, usually `gcc-11` is the biggest culprit here. This is an unfortuante side-effect of Mac with R. See more details here: [https://mac.r-project.org/tools/:](https://mac.r-project.org/tools/:)