#!/bin/bash


#SBATCH --nodes=1 #Number of nodes

# set the number of CPU cores per node
#SBATCH --ntasks=1 #How many tasks on each node

# How much memory is needed (per node)
#SBATCH --mem=1GB

# set a partition
#SBATCH --partition normal

# set max wallclock time
#SBATCH --time=1:00:00

# set name of job
#SBATCH --job-name=sessinfo

# mail alert at start, end and abortion of execution
#SBATCH --mail-type=ALL

# set an output file
#SBATCH --output sessinfo.dat

# send mail to this address
#SBATCH --mail-user=llezamav@uni-muenster.de

# run the application
module add palma/2020b
module add foss R GDAL
R CMD BATCH --vanilla session_info.R
