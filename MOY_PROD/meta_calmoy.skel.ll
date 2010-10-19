#!/bin/ksh
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
# @ as_limit = 2.0gb
# @ data_limit = 2.0gb
# @ wall_clock_limit = 0:10:00
# @ executable = ./<CDFMOY>
# @ queue

# @ step_name = monthlyVT
# @ job_type = parallel
# @ total_tasks = <NPROC> 
# @ as_limit = 2.0gb
# @ data_limit = 2.0gb
# @ wall_clock_limit = 0:10:00
# @ executable = ./<CDFVT>
# @ queue

# @ step_name = annual
# @ dependency = (monthlymeans == 0 && monthlyVT == 0 )
# @ job_type = serial
# @ wall_clock_limit = 2:00:00
# @ as_limit = 2.0gb
# @ data_limit = 2.0gb
# @ executable = ./<ANNUAL>
# @ queue

