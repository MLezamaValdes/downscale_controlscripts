#!/bin/bash

# set the number of nodes
#SBATCH --nodes=1

# set the number of CPU cores per node
#SBATCH --ntasks-per-node 72

# How much memory is needed (per node)
#SBATCH --mem=160GB

# set a partition
#SBATCH --partition normal

# set max wallclock time
#SBATCH --time=120:00:00

# set name of job
#SBATCH --job-name=small_72_DI_log_par

# mail alert at start, end and abortion of execution
#SBATCH --mail-type=ALL

# set an output file
#SBATCH --output output_small_72_DI_log_par.dat

# send mail to this address
#SBATCH --mail-user=llezamav@uni-muenster.de

# run the application
module add GCC/8.2.0-2.31.1   OpenMPI/3.1.1
module add icc/2019.1.144-GCC-8.2.0-2.31.1  impi/2018.4.274
module add ifort/2019.1.144-GCC-8.2.0-2.31.1  impi/2018.4.274
module add R/3.6.0
module add foss R rgdal
R CMD BATCH --vanilla 10_DI_Palma_log_choosing_par_small_72_example.R