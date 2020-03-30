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

Rscript ./vsc_00_main_v20200329.R &



conda deactivate


