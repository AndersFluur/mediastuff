-record(state,{port}).

get_maxline() ->
	      255.

start(ExtProg) ->
	init(ExtProg).

init(ExtProg) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtProg}, [stream, {line, get_maxline()}]),
    {ok, #state{port = Port}}.

handle_call({echo, Msg}, _From, #state{port = Port} = State) ->
    port_command(Port, Msg),
    case collect_response(Port) of
        {response, Response} -> 
            {reply, Response, State};
        timeout -> 
            {stop, port_timeout, State}
    end.

collect_response(Port) ->
    collect_response(Port, [], []).

collect_response(Port, RespAcc, LineAcc) ->
    receive
        {Port, {data, {eol, "OK"}}} ->
            {response, lists:reverse(RespAcc)};

        {Port, {data, {eol, Result}}} ->
            Line = lists:reverse([Result | LineAcc]),
            collect_response(Port, [Line | RespAcc], []);

        {Port, {data, {noeol, Result}}} ->
            collect_response(Port, RespAcc, [Result | LineAcc])

{Port,{exit_status,Status}} ->
			    handle_exit
    %% Prevent the gen_server from hanging indefinitely in case the
    %% spawned process is taking too long processing the request.
    after get_timeout() -> 
            timeout
    end.

