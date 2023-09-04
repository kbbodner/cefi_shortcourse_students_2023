## Installing and launching Docker container

This project involves the use of Docker which is an open source platform that allows for all OS and application configuration to be stored in a container, which can easily be deployed on any machine. For this project we use `Rocker` which is a project to specifically set up R and Rstudio images. The project uses Stan, which requires a C++ toolchain to be set up. However, with Docker we can ensure that this is set-up correctly each time and can be used by other collaborators to avoid any "works on my machine" bugs.

First [install Docker](https://docs.docker.com/get-docker/). On Windows and Mac
the easiest way to get started is to install [Docker Desktop](https://www.docker.com/products/docker-desktop/).
Note, on Windows you will also need to [install Windows Subsystem for Linux (WSL2)](https://learn.microsoft.com/en-us/windows/wsl/install).

On Windows, make sure Docker Desktop is running and then go through the following steps:

1. Open a terminal (or in Windows open Powershell) and navigate to the parent directory `/covidseir` that
contains the file `Dockerfile`. i.e. run the command ```cd path/to/covidseir/```
1. Build with docker by running
   ```docker build -t covidseir .```
   1. Note: You may run into an issue here where it says that the dockerfile isn't found. Can be resolved from this [issue](https://github.com/docker/buildx/issues/426#issuecomment-732980948)
1. Run the image with the following command by changing "apassword" to an alternative password. Alternatively if a password is not included one will be assigned (check logs). 
    ```docker run -d -e PASSWORD=apassword -p 8787:8787 -v ${PWD}:/home/rstudio/Documents covidseir:latest```
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
