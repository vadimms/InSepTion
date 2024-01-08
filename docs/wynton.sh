#! /usr/bin/env bash
#$ -cwd
#$ -pe smp 1
#$ -m beas

if [ "$#" -ne 1 ]; then
     echo "Illegal number of parameters"
     exit 1
fi

script=$1

module load CBI
module load r

echo "R modules loaded.  Running $script..."
echo "#########################################################################"
echo ""

Rscript "$script"


echo ""
echo "#########################################################################"
echo "Done.  Job stats:"
[[ -n "$JOB_ID" ]] && qstat -j "$JOB_ID"

I invoke running a job on Wynton by starting in the same directory as the run_R script like this:
qsub -pe smp 12 -l mem_free=16G -l h_rt=24:00:00 -N "myjobname" -j y -cwd run_R "path/to/script.R"

## command to submit job
# qsub -pe smp 12 -l mem_free=16G -l h_rt=24:00:00 -N "myjobname" -j y -cwd run_R "path/to/script.R"

# qsub: This is a command used to submit jobs to a job scheduling system.

# -pe smp 12: This specifies the parallel environment and the number of processors (or cores) to allocate for the job. In this case, it's using the "smp" (Symmetric MultiProcessing) parallel environment with 12 processors.  If you don't need parallel computing, omit this

# -l mem_free=16G: This specifies the amount of free memory required for the job per core. In this case, it's requesting 16 gigabytes of free memory per core so it's requesting 16*12 = 192 GB.

# -l h_rt=24:00:00: This sets the maximum running time for the job. The format is hours:minutes:seconds, so in this case, it's set to run for a maximum of 24 hours.

# -N "myjobname": This sets the name of the job to "myjobname". It's a user-friendly way to identify the job.

# -j y: This option specifies that the standard error and standard output streams of the job should be joined. This means that both the error and output messages will be directed to the same file.

# -cwd: This tells the job to execute in the current working directory. The job will start in the directory from which the qsub command is executed.

# run_R "path/to/script.R": This is the command that will be executed as the job. Note that there are two scripts:  there is the shell script "run_R" that I provided above, and the path to the actual R script.  

## Two other useful commands:

 #qstat: The qstat command is used to query the status of jobs in the job queue. When executed, it provides information about currently running jobs, pending jobs, and completed jobs. Users can check details such as job ID, job name, user, queue, status, and resource usage. This command is valuable for monitoring the progress and resource utilization of submitted jobs.

# qkill: The qkill command is used to terminate or cancel a running job. By providing the job ID as an argument to qkill, users can gracefully terminate a job before its natural completion. This command is useful in situations where a job needs to be stopped prematurely, either due to errors, changes in priorities, or other reasons. It allows users to free up resources and manage their workload effectively.
