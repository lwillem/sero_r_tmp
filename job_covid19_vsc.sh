#!/bin/bash

#PBS -N job_covid19_vsc
#PBS -l walltime=00:05:00
#PBS -L tasks=1:lprocs=28
#PBS -m e
#PBS -M sereina.herzog@uantwerpen.be
#PBS -o job_covid19_log.txt
#PBS -e job_covid19_stderr.txt

# note: if you use e.g. 'parellel foreach', use
# -L tasks=1:lprocs=28

cd $VSC_DATA/covid19/

source activate science

# run all
Rscript ./vsc_00_main_v20200329.R

# run for each combination simulations 1 till 10
Rscript ./vsc_00_main_v20200329.R -start 1 -end 10

# run for combination 1, simulations 1 till 10
Rscript ./vsc_00_main_v20200329.R -start 1 -end 10 -comb 1


conda deactivate


