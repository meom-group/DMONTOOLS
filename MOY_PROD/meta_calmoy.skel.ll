#!/bin/bash
#########################################################################
## title of the run
# @ job_name = calmoy

## Output listing location
# @ output = zz-$(job_name)-$(step_name).$(jobid)
# @ error  = $(output)
# @ notification = never

# @ step_name = monthlymeans
# @ job_type = parallel
# @ total_tasks = <NPROC>
## This is for ulam
# @ as_limit = 4.0gb
# @ wall_clock_limit = 2:00:00
## On vargas for large configs try
## @ data_limit = 1.0gb
## @ stack_limit = 2.2gb
## @ wall_clock_limit = 0:50:00
# @ executable = ./<CDFMOY>
# @ queue

# @ step_name = monthlyVT
# @ dependency = (monthlymeans ==0)
# @ job_type = parallel
# @ total_tasks = <NPROC> 
## This is for ulam
# @ as_limit = 4.0gb
# @ wall_clock_limit = 2:00:00
## On vargas for large configs try
## @ data_limit = 1.0gb
## @ stack_limit = 2.2gb
## @ wall_clock_limit = 0:50:00
# @ executable = ./<CDFVT>
# @ queue


