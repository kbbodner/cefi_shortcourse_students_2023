## Installing and launching exercise using a Docker container

This project involves the use of Docker which is an open source platform that allows for all OS and application configuration to be stored in a container, which can easily be deployed on any machine. For this project we use `Rocker` which is a project to specifically set up R and Rstudio images. The project uses Stan, which requires a C++ toolchain to be set up. However, with Docker we can ensure that this is set-up correctly each time and can be used by other collaborators to avoid any "works on my machine" bugs.

First [install Docker](https://docs.docker.com/get-docker/). On Windows and Mac
the easiest way to get started is to install [Docker Desktop](https://www.docker.com/products/docker-desktop/).
Note, on Windows you will also need to [install Windows Subsystem for Linux (WSL2)](https://learn.microsoft.com/en-us/windows/wsl/install).

On Windows, make sure Docker Desktop is running and then go through the following steps:

1. Open a terminal (or in Windows open Powershell) and navigate to the parent directory `/Activity_01_STAN_primer` that
contains the file `Dockerfile`. i.e. run the command ```cd path/to/Activity_01_STAN_primer/```
1. Build with docker by running
   ```docker build -t stan_primer .```
   1. Note: You may run into an issue here where it says that the dockerfile isn't found. Can be resolved from this [issue](https://github.com/docker/buildx/issues/426#issuecomment-732980948)
1. Run the image with the following command by changing "apassword" to an alternative password. Alternatively if a password is not included one will be assigned (check logs). 
    ```docker run -d -e PASSWORD=apassword -p 8787:8787 -v ${PWD}:/home/rstudio/Documents stan_primer:latest```
1. Open your web browser (e.g. Chrome or Firefox) and type the following in the address bar `localhost:8787`. 
1. Log in with user/password `rstudio/apassword`.
1. At any time you can check what containers are running with the following
command ```docker ps```
1. When you're ready to close your session you can run
```docker stop container_name``` where `container_name` is an automatically 
generated name you can find under `NAMES` by running the ```docker ps``` command
1. To start-up a container again run ```docker start container_name```.
Note that you can see all available containers (including those not currently running)
by entering the following ```docker container ls --all```

**Note:** The `-v` flag sets the volume to share in the developer environment, make sure you're in the project directory before running the above command. This allows the docker environment to 
communicate with the working directory of the project so that any changes to
files while running Rstudio in the container will be made in the same directory
outside of the container. 
For more information see [The Rocker Project](https://www.rocker-project.org/). 

**Note:** There may be further complications with running Rstudio within docker if you're using `docker-ce` in `WSL2`. If you need to mount the drive that contains the git repo use the following command (e.g. if the drive name is `Z`)
```sudo mount -t drvfs Z: /mnt/z```


## Installing R libraries for exercise outside of Docker

If you encounter any errors while installing Docker above, as a fall back you can install `rstan` directly within your local `R` environment. 

1. Ensure that you have R version `4.0` or later installed
2. Follow the [rstan quick start guide](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started). You can follow the guide there or use the abridged version below
   1. You will first need to make sure that you can compile stan code using the C++ toolchain. This will vary depending on what your operating system is. Follow the separate insuctions below depending on which operating system you're using
        - [Windows](https://github.com/stan-dev/rstan/wiki/Configuring-C---Toolchain-for-Windows)
        - [Mac](https://github.com/stan-dev/rstan/wiki/Configuring-C---Toolchain-for-Mac)
        - [Linux](https://github.com/stan-dev/rstan/wiki/Configuring-C-Toolchain-for-Linux)
    1. Run the following commands in an `R` terminal
        ```r
        remove.packages("rstan")
        if (file.exists(".RData")) file.remove(".RData")
        ```
    1. Restart `R` and then run the following commands
        ```r
        Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1) # only necessary for Linux without the nodejs library / headers
        install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
        ```
    1. Verify that `rstan` has installed correctly by running the following command in `R`
        ```r
        example(stan_model, package = "rstan", run.dontrun = TRUE)
        ```
        You may see warnings related to the compiler, which is okay so long as the above command gives an output