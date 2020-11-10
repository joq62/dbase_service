-record(deployment,
	{
	  deployment_id,
	  deployment_spec_id,
	  deployment_spec_vsn,
	  date,
	  time,
	  host_id,
	  vm_id,
	  vm,
	  sd_list, %[{ServiceId,Vsn}] 
	  status  %started, stopped 
	}).
