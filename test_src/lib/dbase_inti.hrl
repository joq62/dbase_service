{tables,
 [{computer,[{attributes,[host_id,ssh_uid,ssh_passwd, ip_addr,port]},
	    {disc_copies,[mnesia@sthlm_1]}]},
  {service_def,[{attributes,[id,vsn,source]},
		{disc_copies,[mnesia@sthlm_1]}]},
  {deployment_spec,[{attributes,[id,vsn,sevices]},
		    {disc_copies,[mnesia@sthlm_1]}]},
  {deployment,[{attributes,[id,vsn,service_id,service_vsn,vm]},
	       {disc_copies,[mnesia@sthlm_1]}]},
  {service_discovery,[{attributes,[id,vm]},
		      {disc_copies,[mnesia@sthlm_1]},
		      {type,bag}]}
 ]
}.


