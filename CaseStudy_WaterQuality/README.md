# Forecasting short-course casestudy example

Vignette for running a WQ forecast using NEON data.

## Running the vignette
We will use a Docker container to ensure a consistent R environment when running the vignette. The docker container already contains the packages needed to run the vignette. 

### Installing Docker
Go to [https://docs.docker.com/get-docker/](https://docs.docker.com/get-docker/) to install the relevant install for your platform (available for PC, Mac and Linux). Also see [https://docs.docker.com/desktop/](https://docs.docker.com/desktop/).

NOTE: 
* If you're running Windows, you will need WSL (Windows Subsystem for Linux)
* If you're running a Linux distribution, you may have to enable Viritualization on your computer (see [here](https://stackoverflow.com/questions/76646465/unable-to-launch-docker-desktop-on-ubuntu/76655270#76655270))

### Running a docker container

1. Launch Docker Desktop (either from the Command Line or by starting the GUI) 
2. At the command line run the following command which tells docker to `run` the container with the name `eco4cast/rocker-neon4cast` that has all the packages and libraries installed already. The `PASSWORD=yourpassword` sets a simple password that you will use to open the container. The `-ti` option starts both a terminal and an interactive session. 
```
docker run --rm -ti -e PASSWORD=yourpassword -p 8787:8787 eco4cast/rocker-neon4cast
```
This can take a few minutes to download and install. It will be quicker the next time you launch it.  

3. Open up a web browser and navigate to `http://localhost:8787/`
4. Enter the username: `rstudio` and password: `yourpassword`
5. You should see a R Studio interface with all the packages etc. pre-installed and ready to go.

You can close this localhost window (and then come back to it) but if you close the container from Docker (turn off your computer etc.) any changes will be lost unless you push them to Github or exported to your local environment.

If you are _NOT_ running a Docker container then you will need to install the packages and the JAGS executable manually (see instructions in the vignette).

### Clone or fork the github repository

Cloning the repository to this container will not allow you to save and push any changes back and all modifications will be lost when the container is closed so it is recommended to fork and push to your own branch (see below). 

1. Navigate to the repository
2. Click the `Fork` button (top right), to make a copy in your own organisation.
3. Clone this repository using the `Code` button > Copy the `https` link
4. In the Docker R Studio session start a New Project > Version Control > Git > Paste the HTTPS link into the repository URL space > Create Project
5. Open up the relevant .Rmd file