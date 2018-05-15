#!/bin/bash
#SBATCH --job-name="simulation_prediction"
#SBATCH --ntasks=1
#SBATCH --time=00:20:00
#SBATCH --mem=2G
#SBATCH --array=0-73
#SBATCH --output=./out/simulation_pred_%A_%a.out

echo "SLURM_JOBID: " $SLURM_JOBID
echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID

t="/data/skinneb1/geo/popcen/tract/2010/"
i="~/colchoice/estdat/"
o="~/colchoice/estdat/mostlikpred/"
n=${SLURM_ARRAY_TASK_ID}

typ=$1
est1=$2
est2stub=$3

Rscript --vanilla simulation_pred.R \
	${t}tract_split_${n}.txt $i $o $typ $est1 $est2stub

