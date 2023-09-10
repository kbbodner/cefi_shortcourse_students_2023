# Troubleshooting the MacPan package

There are a number of known issues with the `MacPan` package that make a quick set-up sometimes difficult. 

The most important thing to note here, is that running the McMaster pandemic package will require you to a used docker with an R environment to launch a set of software that will run the package, or beer, install all of the required software on your own machine. Both options are imperfect, and we suggest you start with the first option. However, for those working on Windows machines, or very new versions of the Apple chip macOS, you might run into challenges with your our environment that lead on from missing the `gcc` compiler tools. Here, We try to link you to some resources for troubleshooting these issues.

## Troubleshooting Docker

If you have trouble installing and starting Docker, the most useful set of resources are the ones provided by Docker themselves. Please see this series of pages for dealing with challenges here [https://docs.docker.com/desktop/troubleshoot/overview/](https://docs.docker.com/desktop/troubleshoot/overview/). 

If you're having trouble with the `renv` inside the Docker container. There are a number of steps to take: 

### For MacOS users

There are occasional issues with Docker/`renv` being able to find the Fortran compiler on your machine. Please see this stack overflow post [https://stackoverflow.com/questions/29586487/still-cant-install-scipy-due-to-missing-fortran-compiler-after-brew-install-gcc](https://stackoverflow.com/questions/29586487/still-cant-install-scipy-due-to-missing-fortran-compiler-after-brew-install-gcc) that is not specific to Docker, but solves a problem if you are running into `gcc` issues. 

### For Windows users

During the in-person component of the shortcourse, the most successful approach for Windows users was to actually install the WSL for Windows and use that to run Docker, or alternatively, just run `renv` outside of the Docker container with Stan installed locally. 