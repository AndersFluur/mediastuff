%%% CODE-START

-module(vomp_protocol).
-export([code_message/1]).
-export([get_recordings/1]).
-export([delete_recording/2]).
-export([get_channels/1]).
-export([get_timers/1]).
-export([test_login/0]).
-export([unix_to_gregorian/1]).
-export([get_unix_timestamp/1]).
-compile(export_all).

-include("vomp_protocol.hrl").

    

%% inet:ifget("eth0", [hwaddr]).      
%% {ok,[{hwaddr,[0,12,110,150,212,42]}]}
%% inet:getiflist().
%% {ok,["lo","eth0"]}

receive_message(ExpectedResponse,RequestId) ->
    receive 
	{response,Response}  ->
	    decode_response(ExpectedResponse,RequestId,Response);
	Other ->
	    Other
     after 3000 ->
 	  no_response
    end.
 
code_message(Message) when list(Message) -> 
    concat_binary(Message);
code_message(Message) when record(Message,login_request) ->
    list_to_binary(Message#login_request.mac_address);
code_message(Message) when record(Message,timer_string) ->
    list_to_binary(Message#timer_string.string);
code_message(Message) when record(Message,channel) ->
    Number=Message#channel.number,
    <<Number:32>>;
code_message(Message) when binary(Message) ->
    Message;
    
code_message(_Message)-> % No message
   <<>>. 
%code_message(Message) when record(Message,) ->


code_request(Request) ->
    CodedMessage = code_message(Request#vomp_request.message),
    Size = size(CodedMessage),
    Opcode = Request#vomp_request.opcode,
    ChannelId = Request#vomp_request.channel_id,
    RequestId = Request#vomp_request.request_id,

   concat_binary([<<ChannelId:32,
    RequestId:32,
    Opcode:32,
    Size:32>>,
    CodedMessage]).

get_response_length(<<_ChannelId:32,_RequestId:32,Length:32,_Rest/binary>>) ->
    Length.

chop_off_long([L1,L2,L3,L4|List]) ->
    <<Long:32>> = <<L1,L2,L3,L4>>,
    { Long, List }.

extract_string(List) ->
    extract_string(List,[]).
extract_string([],Acc) ->
    {lists:reverse(Acc),[]};
extract_string([0|Tail],Acc) ->
    {lists:reverse(Acc),Tail};
extract_string([Head|Tail],Acc) ->
    extract_string(Tail,[Head|Acc]).

decode_recordings(Length,Recordings) ->
    decode_recordings(Length,Recordings,[]).
decode_recordings(Length,[],RecordingsRecords) ->
    RecordingsRecords;
decode_recordings(Length,Recordings,RecordingsRecords) ->
    [T1,T2,T3,T4|Rest] = Recordings,
    <<StartTime:32>> = <<T1,T2,T3,T4>>,
    {Name, Rest1} = extract_string(Rest),
    {FileName, Rest2} = extract_string(Rest1),
    Recording = #recording{start_time=StartTime,
	       name=Name,
	       file_name=FileName},
	decode_recordings(Length,Rest2,[Recording | RecordingsRecords]).
    
decode_channels(Length,Channels) ->
    decode_channels(Length,Channels,[]).
decode_channels(Length,[],ChannelRecords) ->
    ChannelRecords;
decode_channels(Length,Channels,ChannelRecords) ->
    [N1,N2,N3,N4,T1,T2,T3,T4|Rest] = Channels,
    <<Number:32>> = <<N1,N2,N3,N4>>,
    <<Type:32>> = <<T1,T2,T3,T4>>,
    {Name, Rest1} = extract_string(Rest),
    Channel = #channel{number=Number,
	       type=Type,
	       name=Name},
	decode_channels(Length,Rest1,[Channel | ChannelRecords]).

delete_recording_reply(4) ->
    {error, recording_not_found};
delete_recording_reply(3) ->
    {error, recording_control_found};
delete_recording_reply(2) ->
    {error, delete_failed};
delete_recording_reply(1) ->
    ok.

decode_timers(Length,Timers) ->
    decode_timers(Length,Timers,[]).
decode_timers(Length,[],TimerRecords) ->
    TimerRecords;
decode_timers(Length,Timers,TimerRecords) ->
    {Active,T1} = chop_off_long(Timers),
    {Recording,T2} = chop_off_long(T1),
    {Pending,T3} = chop_off_long(T2),
    {Priority,T4} = chop_off_long(T3),
    {Lifetime,T5} = chop_off_long(T4),
    {Channel_number,T6} = chop_off_long(T5),
    {Start_time,T7} = chop_off_long(T6),
    {Stop_time,T8} = chop_off_long(T7),
    {Day,T9} = chop_off_long(T8),
    {Week_days,T10} = chop_off_long(T9),
    {File_name,T11} = extract_string(T10),
    Timer = #timer{
      active=Active,
      recording=Recording,
      pending=Pending,
      priority=Priority,
      lifetime=Lifetime,
      channel_number=Channel_number,
      start_time=Start_time,
      stop_time=Stop_time,
      day=Day,
      week_days=Week_days,
      file_name=File_name
     },
    decode_timers(Length,T11,[Timer|TimerRecords]).
    
set_timer_reply(2) ->
    {error, bad_timerstring};
set_timer_reply(1) ->
    {error, timer_already_set};
set_timer_reply(0) ->
    ok.

set_start_streaming_reply(0) ->
    {error,no_reason};
set_start_streaming_reply(1) ->
    ok.

decode_response(Expected=?keep_alive_channel,RequestId=undefined,
		<<?keep_alive_channel:32,Time:32>>) ->
    {ok, Time};
decode_response(?get_block,RequestId,<<?request_response_channel:32,RequestId:32,Length:32,Reply:32>>) when Length == 4 ->
		       end_of_stream;
decode_response(?get_block,RequestId,<<?request_response_channel:32,RequestId:32,Length:32,Data/binary>>)  ->
		       {ok, Data};
decode_response(?login,RequestId,<<?request_response_channel:32,RequestId:32,Length:32,
			Time:32,GmtOffset:32>>) ->
		       {ok,#login_response{time=Time,gmt_offset=GmtOffset}};
decode_response(?get_recordings,RequestId,<<?request_response_channel:32,
					   RequestId:32,Length:32,
					   DiskSpaceTotal:32,
					   DiskSpaceFree:32,
					   DiskSpaceUtilisation:32,
					   Recordings/binary>>) ->
    RecordingsList = decode_recordings(Length,binary_to_list(Recordings)),
    io:format("received ~p~n",[RecordingsList]),

    {ok,#get_recordings_response{
		   disk_space_total=DiskSpaceTotal,
		   disk_space_free=DiskSpaceFree,
		   disk_space_utilisation=DiskSpaceUtilisation,
		   recordings=RecordingsList}};

decode_response(?get_channels,RequestId,<<?request_response_channel:32,
					   RequestId:32,Length:32,
					   Channels/binary>>) ->
    ChannelsList = decode_channels(Length,binary_to_list(Channels)),
    L = length(ChannelsList),
    io:format("received ~p~n~p~n",[L, ChannelsList]),
    {ok,ChannelsList};

decode_response(?delete_recording,RequestId,<<?request_response_channel:32,
					   RequestId:32,Length:32,
					   ReplyCode:32>>) ->
    delete_recording_reply(ReplyCode);
    
decode_response(?get_timers,RequestId,<<?request_response_channel:32,
				       RequestId:32,Length:32,
				       NumTimers:32,
				       Timers/binary>>) ->
    TimersList = case NumTimers of
		     0 ->
			 [];
		     NumTimers ->	 
			 decode_timers(Length,binary_to_list(Timers))
		 end,
    NumTimers = length(TimersList),
    io:format("received ~p~n~p~n",[NumTimers, TimersList]),
    {ok,TimersList};


decode_response(?set_timer,RequestId,<<?request_response_channel:32,
					   RequestId:32,Length:32,
					   ReplyCode:32>>) ->
    set_timer_reply(ReplyCode);

decode_response(?start_streaming_channel,RequestId,<<?request_response_channel:32,
					   RequestId:32,Length:32,
					   ReplyCode:32>>) ->
    set_start_streaming_reply(ReplyCode);

decode_response(?open_recording,RequestId,<<?request_response_channel:32,
					   RequestId:32,Length:32,
					   LengthBytes:64,LengthFrames:32>>) ->
    {ok,LengthBytes,LengthFrames}.

login(VompChannel) ->
    {ok,[{hwaddr,MacAddress}]} = inet:ifget("eth0", [hwaddr]),
    LoginMessage = #login_request{mac_address=MacAddress},
    LoginRequest = #vomp_request{channel_id=?request_response_channel,
				 request_id=1,
				 opcode=?login,
				 message=LoginMessage},
    LoginRequestCoded = code_request(LoginRequest),
    ok = vomp_channel:send(VompChannel, LoginRequestCoded),
    receive_message(?login,_RequestId=1).

get_recordings(VompChannel) ->
    Request = #vomp_request{request_id=1,
			    opcode=?get_recordings},
    RequestCoded = code_request(Request),
    ok = vomp_channel:send(VompChannel, RequestCoded),
    receive_message(?get_recordings,_RequestId=1).

get_channels(VompChannel) ->
    Request = #vomp_request{request_id=1,
				 opcode=?get_channels},
    RequestCoded = code_request(Request),
    ok = vomp_channel:send(VompChannel, RequestCoded),
    receive_message(?get_channels,_RequestId=1).

delete_recording(VompChannel,FileName) ->
    Message = lists:append(FileName,[0]),
    Request = #vomp_request{request_id=1,
				 opcode=?delete_recording,
				 message=Message},
    RequestCoded = code_request(Request),
    ok = vomp_channel:send(VompChannel, RequestCoded),
    receive_message(?delete_recording,_RequestId=1).

get_timers(VompChannel) ->
    Request = #vomp_request{request_id=1,
				 opcode=?get_timers},
    RequestCoded = code_request(Request),
    ok = vomp_channel:send(VompChannel, RequestCoded),
    receive_message(?get_timers,_RequestId=1).

convert_timer_flag(16#0) ->
    none;
convert_timer_flag(16#1) ->
    active;
convert_timer_flag(16#2) ->
    instant;
convert_timer_flag(16#4) ->
    vps;
convert_timer_flag(16#8) ->
    recording;
convert_timer_flag(16#ffff) ->
    all;
convert_timer_flag(none) ->
    16#0;
convert_timer_flag(active) ->
    16#1;
convert_timer_flag(instant) ->
    16#2;
convert_timer_flag(vps) ->
    16#4;
convert_timer_flag(recording) ->
    16#8;
convert_timer_flag(all) ->
    16#ffff.
convert_timer_flags(Flags) ->
    convert_timer_flags(Flags,0).
convert_timer_flags([],Bitmask) ->
    Bitmask;
convert_timer_flags([Flag|Flags],Bitmask) ->
    Bitmask2 = Bitmask bor convert_timer_flag(Flag),
    convert_timer_flags(Flags,Bitmask2).

% TODO: This is not complete
set_timer(VompChannel,Flags, Channel, Day, Start, Stop, Priority, Lifetime, FileName, Aux) ->

% Aux not handled
    TimerString = lists:flatten(
		io_lib:format("~p:~p:~p:~s:~s:~p:~p:~p:",
			      [convert_timer_flags(Flags),
			       Channel,
			       Day,
			       Start,Stop,Priority,Lifetime,FileName])),
    
     Message = #timer_string{string=TimerString},
     Request = #vomp_request{request_id=1,opcode=?set_timer, message=Message},
     RequestCoded = code_request(Request),
     io:format("~p~n",[RequestCoded]),
     ok = vomp_channel:send(VompChannel, RequestCoded),
     receive_message(?set_timer,_RequestId=1).


start_streaming_channel(VompChannel,ChannelNumber) ->
    Message = #channel{number=ChannelNumber},
    Request = #vomp_request{request_id=1,
				 opcode=?start_streaming_channel,
				 message=Message},
    RequestCoded = code_request(Request),
    ok = vomp_channel:send(VompChannel, RequestCoded),
    receive_message(?start_streaming_channel,_RequestId=1).

open_recording(VompChannel,FileName) ->
    Message = lists:append(FileName,[0]),
    Request = #vomp_request{request_id=1,
				 opcode=?open_recording,
				 message=Message},
    RequestCoded = code_request(Request),
    ok = vomp_channel:send(VompChannel, RequestCoded),
    receive_message(?open_recording,_RequestId=1).

get_block(VompChannel,Position,Amount) ->
    Request = #vomp_request{request_id=1,
				 opcode=?get_block,
				 message= <<Position:64,Amount:32>>},
    RequestCoded = code_request(Request),
    ok = vomp_channel:send(VompChannel, RequestCoded),
    receive_message(?get_block,_RequestId=1).

stop_streaming(VompChannel) ->
    Request = #vomp_request{request_id=3,% TODO request ID != start Id
				 opcode=?stop_streaming},
    RequestCoded = code_request(Request),
    ok = vomp_channel:send(VompChannel, RequestCoded),
    receive_message(?stop_streaming,_RequestId=3).

send_keep_alive(VompChannel) ->
    Time = get_unix_timestamp(erlang:now()),
    RequestCoded = <<?keep_alive_channel:32,Time:32>>,
    ok = vomp_channel:send(VompChannel, RequestCoded),
    receive_message(?keep_alive_channel,undefined).


%%% Test code
start_recording_receiver(VompChannel,FileName) ->
    Pid = spawn(?MODULE,recording_receiver_loop,[VompChannel,FileName]),
    {ok,Pid}.
recording_receiver_loop(VompChannel,FileName) ->   
    vomp_channel:req_resp_client_add(VompChannel),
    {ok,LengthBytes,LengthFrames} = open_recording(VompChannel,FileName),
    recording_receiver_loop1(VompChannel,0,LengthBytes).
recording_receiver_loop1(VompChannel,Counter,LengthBytes) ->
    io:format("Counter = ~p~n",[Counter]),
    {ok,Data} = get_block(VompChannel,Counter*1000,1000),
    recording_receiver_loop1(VompChannel,Counter+1,LengthBytes).



start_streaming_receiver(VompChannel) ->
    Pid = spawn(?MODULE,streaming_receiver_loop,[VompChannel]),
    {ok,Pid}.
streaming_receiver_loop(VompChannel) ->   
    vomp_channel:stream_client_add(VompChannel),
    streaming_receiver_loop1(VompChannel,0).
streaming_receiver_loop1(VompChannel,Counter) ->
    receive
	{stream, Data} ->
	    streaming_receiver_loop1(VompChannel,Counter+1);
	stream_end ->
	    io:format("sTREAM END Counter = ~p~n",[Counter]);
	counter ->
	    io:format("Counter = ~p~n",[Counter+1]),
	    streaming_receiver_loop1(VompChannel,Counter);
	stop_streaming  ->
	    stop_streaming(VompChannel)
	    
    after 3000 ->
	    io:format("Timeout in receiver~n",[])
	    
    end.
	    
start_keep_alive_sender(VompChannel) ->
    Pid = spawn(?MODULE,keep_alive_sender_loop,[VompChannel]),
    {ok,Pid}.
keep_alive_sender_loop(VompChannel) ->   
    vomp_channel:keep_alive_client_add(VompChannel),
    keep_alive_sender_loop1(VompChannel,0).
keep_alive_sender_loop1(VompChannel,Counter) ->
    send_keep_alive(VompChannel),
    receive
	after 15000 ->
		keep_alive_sender_loop1(VompChannel,Counter+1)    
	end.

test_login() ->
    Servers = vomp_channel:probe_for_servers(1),
    {Address,Port,_Name} = hd(Servers),
    {ok,VompChannel} = vomp_channel:create(Address,Port),
    {ok, _Resonse} = login(VompChannel),

    FileName = "/var/media/vdr/Oraklen/2008-08-18.11.55.99.99.rec",
    {ok, _Pid} = start_recording_receiver(VompChannel,FileName),  


%    start_keep_alive_sender(VompChannel),
%    start_streaming_channel(VompChannel,ChannelNumber=7),
%    {ok, _Pid} = start_streaming_receiver(VompChannel),  

    {ok, _Pid}.    

%%     {ok, RecordingsResp} = get_recordings(VompChannel),
%%     Recordings = RecordingsResp#get_recordings_response.recordings,
%%     Recording = hd(Recordings),
%%     io:format("~p",[Recording]).


%    {ok, _Resonse3} = get_channels(VompChannel),
%    {error, recording_not_found} = delete_recording(VompChannel,"PelleMaja"),
%    {error, recording_not_found} = delete_recording(VompChannel,"/var/media/vdr/Glada_huset/2008-08-17.21.05.99.99.rec").

% Create Timer
%%      {MSecs,Secs,Usecs} = erlang:now(),
%%     {{Year,Month,Day},{Hour,Minute,_Sec}} = calendar:local_time(),
%%     Start = lists:flatten(io_lib:format("~.2.0w~.2.0w",[Hour+10,Minute])),
%%     Stop = lists:flatten(io_lib:format("~.2.0w~.2.0w",[Hour+11,Minute])),
%%      ok = set_timer(VompChannel,[active,vps],1,Day, Start, Stop, 97,98,filename,aux),
%%     {ok, _Resonse3} = get_timers(VompChannel).

%% End test_login

%% http://schemecookbook.org/Erlang/TimeRFC1123
%% bash $ date +%s
%% 1204281713

%% (erl01@erllabs)>libs:get_unix_timestamp(now()).
%% 1204281715

%% get_unix_timestamp({_MegaSecs, Secs, MicroSecs}=TS) ->
%%     calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(TS) ) -

unix_to_gregorian(UnixSeconds) ->
    UnixSeconds + 
	62167219200.% from calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ).
%% %% @doc get_unix_timestamp
%% %% @spec
%% %% @output
get_unix_timestamp({_MegaSecs, _Secs, _MicroSecs}=TS) ->
    calendar:datetime_to_gregorian_seconds( calendar:now_to_universal_time(TS) ) -
	calendar:datetime_to_gregorian_seconds( {{1970,1,1},{0,0,0}} ). 
%%% CODE-END
