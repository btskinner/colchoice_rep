#!/bin/bash

## attendance
sbatch simulation_pred.slurm attend asclogit_main_mat main_logit

## application
sbatch simulation_pred.slurm apply asclogit_main_apply_mat main_apply_logit
