-module(erl_prism_export_test).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-define(TWOWORKERS,
    [{{1427,647972,725579},{node,worker,<<"helium_cache_pool_1_revolver@gen_server:loop">>,[{pid,pid_1},{reductions,10},{status,waiting},{dictionary,[{'$ancestors',[helium_cache_pool_1_revolver_sub,helium_sup,supervisor_pid]},{'$initial_call',{revolver,init,1}}]},{current_function,{gen_server,loop,6}},{heap_size,610},{initial_call,{proc_lib,init_p,5}},{memory,11568},{message_queue_len,0},{registered_name,helium_cache_pool_1_revolver}],{totals,0,11568,0},[]}},
     {{1427,647963,53985}, {node,worker,<<"helium_cache_pool_1_revolver@gen_server:loop">>,[{pid,pid_1},{reductions,15},{status,waiting},{dictionary,[{'$ancestors',[helium_cache_pool_1_revolver_sub,helium_sup,supervisor_pid]},{'$initial_call',{revolver,init,1}}]},{current_function,{gen_server,loop,6}},{heap_size,610},{initial_call,{proc_lib,init_p,5}},{memory,11568},{message_queue_len,0},{registered_name,helium_cache_pool_1_revolver}],{totals,0,11568,0},[]}}]
).

-define(TWOPOOLS,
    [{{1429,791109,722500},{node,pool,"helium_cache_sup_1_pool",undefined,{totals,48,21096,0},[{node,worker,<<"helium_cache:init@gen_server:loop">>,[{pid,pid_1},{reductions,0},{status,waiting},{dictionary,[{random_seed,{23496,5317,12683}},{'$ancestors',[helium_cache_sup_1,helium_sup,ancestor_pid]},{'$initial_call',{helium_cache,init,1}}]},{current_function,{gen_server,loop,6}},{heap_size,376},{initial_call,{proc_lib,init_p,5}},{memory,7032},{message_queue_len,0},{registered_name,[]}],{totals,0,7032,0},[]},{node,worker,<<"helium_cache:init@gen_server:loop">>,[{pid,pid_2},{reductions,0},{status,waiting},{dictionary,[{random_seed,{23496,5317,4694}},{'$ancestors',[helium_cache_sup_1,helium_sup,ancestor_2_pid]},{'$initial_call',{helium_cache,init,1}}]},{current_function,{gen_server,loop,6}},{heap_size,376},{initial_call,{proc_lib,init_p,5}},{memory,7032},{message_queue_len,0},{registered_name,[]}],{totals,0,7032,0},[]},{node,worker,<<"helium_cache:init@gen_server:loop">>,[{pid,pid_3},{reductions,48},{status,waiting},{dictionary,[{random_seed,{23496,5317,18279}},{'$ancestors',[helium_cache_sup_1,helium_sup,ancestor_2_pid]},{'$initial_call',{helium_cache,init,1}}]},{current_function,{gen_server,loop,6}},{heap_size,376},{initial_call,{proc_lib,init_p,5}},{memory,7032},{message_queue_len,0},{registered_name,[]}],{totals,48,7032,0},[]}]}},
     {{1429,791189,892920},{node,pool,"helium_cache_sup_1_pool",undefined,{totals,73964,454224,0},[{node,worker,<<"helium_cache:init@gen_server:loop">>,[{pid,pid_1},{reductions,23528},{status,waiting},{dictionary,[{random_seed,{23496,5317,12683}},{'$ancestors',[helium_cache_sup_1,helium_sup,ancestor_pid]},{'$initial_call',{helium_cache,init,1}}]},{current_function,{gen_server,loop,6}},{heap_size,6772},{initial_call,{proc_lib,init_p,5}},{memory,426568},{message_queue_len,0},{registered_name,[]}],{totals,23528,426568,0},[]},{node,worker,<<"helium_cache:init@gen_server:loop">>,[{pid,pid_2},{reductions,27536},{status,waiting},{dictionary,[{random_seed,{23496,5317,4694}},{'$ancestors',[helium_cache_sup_1,helium_sup,ancestor_2_pid]},{'$initial_call',{helium_cache,init,1}}]},{current_function,{gen_server,loop,6}},{heap_size,610},{initial_call,{proc_lib,init_p,5}},{memory,10848},{message_queue_len,0},{registered_name,[]}],{totals,27536,10848,0},[]},{node,worker,<<"helium_cache:init@gen_server:loop">>,[{pid,pid_4},{reductions,22900},{status,waiting},{dictionary,[{random_seed,{23496,5317,18279}},{'$ancestors',[helium_cache_sup_1,helium_sup,ancestor_2_pid]},{'$initial_call',{helium_cache,init,1}}]},{current_function,{gen_server,loop,6}},{heap_size,1598},{initial_call,{proc_lib,init_p,5}},{memory,16808},{message_queue_len,0},{registered_name,[]}],{totals,22900,16808,0},[]}]}}]
).

csv_worker_header_workers_test() ->
    Header   = erl_prism_export:csv_header(?TWOWORKERS),
    Expected = <<"time,helium_cache_pool_1_revolver\n">>,
    ?assertEqual(Expected, iolist_to_binary(Header)).

csv_worker_header_pool_test() ->
    Header   = erl_prism_export:csv_header(?TWOPOOLS),
    Expected = <<"time,pid_1,pid_2,pid_3,pid_4\n">>,
    ?assertEqual(Expected, iolist_to_binary(Header)).

csv_line_worker_test() ->
    [{Time, Node} | _] = ?TWOWORKERS,
    CSV = erl_prism_export:csv_line(worker, Time, Node, reductions),
    Expected = <<"2015-03-29 16:52:52,10\n">>,
    ?assertEqual(Expected, iolist_to_binary(CSV)).

csv_line_pool_test() ->
    [{Time, Node} | _] = ?TWOPOOLS,
    CSV = erl_prism_export:csv_line(pool, Time, Node, reductions),
    Expected = <<"2015-04-23 12:11:49,0,0,48\n">>,
    ?assertEqual(Expected, iolist_to_binary(CSV)).

