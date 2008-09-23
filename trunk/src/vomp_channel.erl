-module(vomp_channel).
-export([probe_for_servers/1]).

-include("vomp_protocol.hrl").
-compile(export_all).

probe_for_servers(Timeout) ->
%%%
    Opts=[{reuseaddr,true},binary,{broadcast, true},{active,true}],
    Port=3011,
    {ok,Socket}=gen_udp:open(Port,Opts),
    ok = gen_udp:send(Socket,{255,255,255,255},3024,list_to_binary("VOMP")),
    Servers = wait_for_probe_replies(Timeout),   
    gen_udp:close(Socket),
    Servers.


wait_for_probe_replies(Timeout)->
    {ok, _TRef} = timer:send_after(Timeout*1000, _Message= probe_timeout) ,
    wait_for_probe_reply([]). 
wait_for_probe_reply(Servers)->
    receive 
	{udp, _Socket, IpAddress, InPortNo, Packet}  ->
	    io:format("Reply from Vompserver at ~p Name=~p~n",
		      [IpAddress,binary_to_list(Packet)]),
	    wait_for_probe_reply([{IpAddress, InPortNo, binary_to_list(Packet)}|Servers]);
	probe_timeout ->
	    Servers;
	AnythingElse -> 
	    io:format("probe_for_servers:received ~p~n",[AnythingElse]),
	    wait_for_probe_reply(Servers)
    end.


send(VompChannel, Message) ->
    VompChannel ! {send,  Message},
    ok.
    

-record(server_state,{req_resp_client,stream_client,keep_alive_client,socket,current_client,channel,remaining_length=0,received_data}).

create(DestAddress, DestPort) ->
    ServerState = #server_state{req_resp_client=self()},
    VompChannel = spawn(?MODULE,server_loop_init,[DestAddress, DestPort,
						 ServerState]),
    {ok, VompChannel}.

% active=true might overflow receiver
server_loop_init(DestAddress, DestPort,ServerState) ->
    {ok, Socket} = gen_tcp:connect(DestAddress,DestPort, 
                                 [binary, {packet, 0}]),
    server_loop(ServerState#server_state{socket=Socket}).

%%%   Request Response channel
%%% Received  chunk of data in a series of data with multiple headers
receive_data(ServerState,<<ChannelId:32,RequestId:32,Length:32,_Rest/binary>> = Data) when ServerState#server_state.remaining_length == 0,
           ChannelId == ?request_response_channel,
           Length < (size(Data)-12) ->
    {Binary1, Binary2} = split_binary(Data,Length+12),
    ServerState#server_state.req_resp_client ! Binary1,
    receive_data(ServerState,Binary2);

%%% Received first chunk of data in a series of data with header in this packet
receive_data(ServerState,<<ChannelId:32,RequestId:32,Length:32,_Rest/binary>> = Data) when ServerState#server_state.remaining_length == 0,
           ChannelId == ?request_response_channel,
           Length > (size(Data)-12) ->
    ServerState#server_state{
      remaining_length=Length - size(Data) + 12,
      received_data=Data,
      current_client=ServerState#server_state.req_resp_client,
      channel=?request_response_channel};

%%% Received first chunk of data in a complete message
receive_data(ServerState,<<ChannelId:32,RequestId:32,Length:32,_Rest/binary>> = Data) when ServerState#server_state.remaining_length == 0,
           ChannelId == ?request_response_channel,
Length == (size(Data)-12) ->
    ServerState#server_state.req_resp_client ! {response, Data},
    ServerState;
    
 
%%% Continuation of message and end of message 
receive_data(ServerState,Data) when 
        ServerState#server_state.remaining_length /= 0,
        ServerState#server_state.channel==?request_response_channel ->

    RemainingLength = ServerState#server_state.remaining_length - size(Data),
%%%	    io:format("Sz=~p, RemLen=~p~n",[size(Data),RemainingLength]),
    Data2 = concat_binary([ServerState#server_state.received_data, Data]),
    case RemainingLength of
	0 ->
	    ServerState#server_state.current_client ! {response,Data2},  
	    ServerState#server_state{remaining_length=0,
				     current_client=undefined,
				     channel=undefined};
	RemainingLength when RemainingLength > 0 ->
	    ServerState#server_state{remaining_length=RemainingLength,
				     received_data=Data2}
    end;


%%% Streaming packets
%%% Recived end of stream
receive_data(ServerState,<<ChannelId:32,RequestId:32,1:32,Length:32,Rest/binary>> = Data)
  when ServerState#server_state.remaining_length == 0,
       ChannelId == ?stream_channel ->
    
    io:format("Sz=~p, Len=~p  Rest=~p~n",[size(Data),Length,Rest]),
    ServerState#server_state.stream_client ! stream_end,
    ServerState#server_state{remaining_length=0,
			     current_client=undefined,
			     channel=undefined};
%%% Received stream packet with Header, more data to come in future packets
receive_data(ServerState,<<ChannelId:32,RequestId:32,0:32,Length:32,Rest/binary>> = Data)
	when ServerState#server_state.remaining_length == 0,
	     ChannelId == ?stream_channel,
             Length > (size(Data)-16) ->
 	    
%%%	    io:format("Sz=~p, Len=~p~n",[size(Data),Length]),
    ServerState#server_state.stream_client ! {stream, Rest},
    ServerState#server_state{remaining_length=Length-size(Data)+16,
			     current_client=ServerState#server_state.stream_client,
			     channel=?stream_channel};
		    
%%% Received stream packet with Header,no more data without a new header packet
receive_data(ServerState,<<ChannelId:32,RequestId:32,0:32,Length:32,Rest/binary>> = Data)
	when ServerState#server_state.remaining_length == 0,
	     ChannelId == ?stream_channel,
		Length == (size(Data)-16) ->
    ServerState#server_state.stream_client ! {stream, Rest},
    ServerState;

		    
receive_data(ServerState,Data)
when ServerState#server_state.remaining_length /= 0,
	ServerState#server_state.channel==?stream_channel ->
	    RemainingLength = ServerState#server_state.remaining_length - size(Data),
%	    io:format("Sz=~p, RemLen=~p~n",[size(Data),RemainingLength]),
	    case RemainingLength of
		0 ->
		    ServerState#server_state.current_client ! {stream,Data},  
		    ServerState#server_state{remaining_length=0,
						current_client=undefined,
						channel=undefined};
		RemainingLength when RemainingLength > 0 ->
		    ServerState#server_state.current_client ! {stream,Data},  
		    ServerState#server_state{remaining_length=RemainingLength};
		RemainingLength when RemainingLength < 0 ->
		    {B1,B2} = split_binary(Data,ServerState#server_state.remaining_length),
		    ServerState#server_state.current_client ! {stream,B1},  
		    receive_data(ServerState#server_state{
				   remaining_length=0,
				   current_client=undefined,
				   channel=undefined},
				 B2)
	    end;
%%% Received keep alive reponse in a separate message 
receive_data(ServerState,<<?keep_alive_channel:32,_Time:32>> = Data) when 
  size(Data) == 8 ->
    ServerState#server_state.keep_alive_client ! {response, Data},
    ServerState;
    
%%% Received keep alive reponse and other message 
receive_data(ServerState,<<?keep_alive_channel:32,_Time:32>> = Data) when 
  size(Data) > 8 ->
    {B1,B2} = split_binary(Data,8),
    ServerState#server_state.keep_alive_client ! {response, B1},
    receive_data(ServerState,B2);

receive_data(ServerState,Data) ->
    io:format("Unrecognised Data = ~p In server state =  ~p~n",[Data,ServerState]),
    exit(Data).
 
server_loop(ServerState) ->
    receive
	{exit,_Cause} ->
	    ok = gen_tcp:close(ServerState#server_state.socket);
	{send, Message} ->
	    ok = gen_tcp:send(ServerState#server_state.socket, Message),
	    server_loop(ServerState);

	{tcp, _Socket, Data} ->
	    ServerState2 = receive_data(ServerState,Data),
	    server_loop(ServerState2);
		    
 	{tcp_closed, _Socket}  ->
 	    {error, tcp_closed};
 	{tcp_error, _Socket, Reason} ->
 	    {error,Reason};
	{req_resp_client_add, ClientPid}->
	    server_loop(ServerState#server_state{req_resp_client=ClientPid});
	{req_resp_client_remove, _ClientPid} ->
	    server_loop(ServerState#server_state{req_resp_client=undefined});
	{stream_client_add, ClientPid}->
	    server_loop(ServerState#server_state{stream_client=ClientPid});
	{stream_client_remove, _ClientPid} ->
	    server_loop(ServerState#server_state{stream_client=undefined});
	{keep_alive_client_add, ClientPid}->
	    server_loop(ServerState#server_state{keep_alive_client=ClientPid});
	{keep_alive_client_remove, _ClientPid} ->
	    server_loop(ServerState#server_state{keep_alive_client=undefined})
    end.

delete(VompChannel) ->
    VompChannel ! {exit,undefined}.
    

req_resp_client_add(VompChannel) ->
    VompChannel ! {req_resp_client_add, self()}.
req_resp_client_remove(VompChannel) ->
    VompChannel ! {req_resp_client_remove, self()}.

stream_client_add(VompChannel) ->
    VompChannel ! {stream_client_add, self()}.
stream_client_remove(VompChannel) ->
    VompChannel ! {stream_client_remove, self()}.

keep_alive_client_add(VompChannel) ->
 VompChannel ! {keep_alive_client_add, self()}.
keep_alive_client_remove(VompChannel) ->
    VompChannel ! {keep_alive_client_remove, self()}.

%% receive_message(ExpectedResponse,RequestId) ->
%%     receive
%% 	{tcp, _Socket, Response} ->
%% 	    {ok, decode_response(ExpectedResponse,RequestId,Response)};	    
%% 	{tcp_closed, _Socket}  ->
%% 	    {error, tcp_closed};
%% 	{tcp_error, _Socket, Reason} ->
%% 	    {error,Reason} 
%%     end.
