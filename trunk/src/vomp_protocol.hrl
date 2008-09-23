%%%----------------------------------------------------------------------
%%% File    : 
%%% Author  : Anders Fluur
%%% Purpose : 
%%% Created : 
%%%----------------------------------------------------------------------

-define(request_response_channel,1).
-define(stream_channel,2).
-define(keep_alive_channel,3).

-record(vomp_request, 
        {
	  channel_id=?request_response_channel,
	  request_id,
	  opcode,
	  message
}).

-record(vomp_response, 
        {
	  channel_id,
	  request_id,
	  message
}).

-define(login,1).
-record(login_request, 
        {
	  mac_address
}).
-record(login_response, 
        {
	  time,
	  gmt_offset
}).


-record(recording, 
        {
	  start_time,
	  name,
	  file_name
}).
-define(get_recordings,2).
-record(get_recordings_response, 
        {
	  disk_space_total,
	  disk_space_free,
	  disk_space_utilisation,
	  recordings
}).

-define(delete_recording,3).


-define(get_channels,5).
-record(channel, 
        {
	  number,
	  type,
	  name
	 }).
-define(start_streaming_channel,6).

-define(get_block,7).

-define(stop_streaming,8).

-define(open_recording,9).

-define(get_timers,14).
-record(timer, {
	  active,
	  recording,
	  pending,
	  priority,
	  lifetime,
	  channel_number,
	  start_time,
	  stop_time,
	  day,
	  week_days,
	  file_name
	 }).
-define(set_timer,15).
-record(timer_string, {
	  string
	 }).
%% -define(,).
%% -record(, 
%%         {
%% }).
%% -define(,).
%% -record(, 
%%         {
%% }).
%% -define(,).
%% -record(, 
%%         {
%% }).
%% -define(,).
%% -record(, 
%%         {
%% }).
%% -define(,).
%%      3:
%%       DeleteRecording();
%%       Request:
%% 	String RecordingName
%%       Response:
%% 	Reply { 4=recording_not_found, 3=recording_control_found,
%% 		2=delete_failed,1=ok }
%%      5:
%%       GetChannelsList();
%%       Request:
%%       Response:
%%       List {
%% 	   ULONG ChannelNumber
%% 	   ULONG ChannelType ??????????
%% 	   String ChannelName
%% 	   }
%%      6:
%%       StartStreamingChannel();
%% 	RequestId is used as handle for stream ??
%%       Request:
%% 	ULONG channelNumber
%%       Response:
%% 	Failure:
%% 		ULONG 0
%% 	Success:
%% 		ULONG 1
%%      7:
%%       GetBlock();
%%       Request:
%% 	ULONG Position
%% 	ULONG Amount
%%       Response:
%% 	Failure:
%% 		ULONG 0
%% 	Success:
%% 	Data Block
%%      8:
%%       StopStreaming();
%%       Request:
%% 	-
%%       Response:
%% 	ULONG 1
%%      9:
%%       StartStreamingRecording();
%%       Request:
%% 	String FileName
%%       Response:
%% 	Success:
%% 	Ulong LengthBytes
%% 	ULONG LengthFrames
%% 	Failure:
%% 		NO response !
	
%%      10:
%%       GetChannelSchedule();
%%       Request:
%% 	ULONG ChannelNumber
%% 	ULONG StartTime
%% 	ULONG Duration 
%%       Response:
%% 	Failure and no events found:
%% 		Ulong 0
%% 	List {
%% 	     ULONG   EventID
%% 	     ULONG   EventTime
%% 	     ULONG   EventDuration
%% 	     String  EventTitle
%% 	     String  EventSubTitle
%% 	     String  EventDescription
%% 	}


%%      11:
%%       ConfigSave();
%%       Request:
%% 	String Section
%% 	String Key
%% 	String Value
%%       Response:
%% 	ULONG	{ 1=ok, 0=error}
%%      12:
%%       ConfigLoad();
%%       Request:
%% 	String Section
%% 	String Key
%%       Response:
%% 	Failure:
%% 		Ulong 0
%% 	Success:
%% 		String Value
%%      13:
%%       ReScanRecording();         // FIXME obselete
%%      14:
%%       GetTimers();
%%       Request:
%%       Response:
%% 	ULONG	numTimers
%% 	List Timers{
%% 	    ULONG	Active
%% 	    ULONG	Recording
%% 	    ULONG	Pending
%% 	    ULONG	Priority
%% 	    ULONG	Lifetime
%% 	    ULONG	ChannelNumber
%% 	    ULONG	StartTime
%% 	    ULONG	StopTime
%% 	    ULONG	Day
%% 	    ULONG	WeekDays
%% 	    String	FileName
%% 	} 
%%      15:
%%       SetTimer();
%%       Request:
%% 	String timersString (YYYY-MM-DD + more parts ?? )
%%       Response:
%% 	ULONG { 3=bad_timerstring, 2=timer_already_set, 0=ok}
%%      16:
%%       PositionFromFrameNumber();
%%       Request:
%% 	ULONG frameNumber
%%       Response:
%% 	Ulong Position (0 in case of failure)
%%      17:
%%       FrameNumberFromPosition();
%%       Request:
%% 	ULONG Position 
%%       Response:
%% 	Ulong Frame (0 in case of failure)
%%      18:
%%       MoveRecording();
%%       Request:
%%       Response:
%%      19:
%%       GetIFrame();
%%       Request:
%%       ULONG frameNumber
%%       ULONG direction
%%       Response:
%% 	Failure:
%% 		Ulong 0
%% 	Success:
%% 		ULLONG filePosition 
%% 		ULONG frameNumber
%% 		ULONG frameLength
	
%%      20:
%%       GetRecInfo();
%%       Request:
%% 	String FileName
%%       Response:
%% 	Failure:
%% 		ULONG 0
%% 	Success:
%% 		ULONG	StartTime
%% 		ULONG	StopTime
%% 		ULONG	ResumePoint
%% 		String	Summary
%% 		ULONG	NumberOfChannels 
%% 		List Components {
%% 		     UCHAR	Stream
%% 		     UCHAR	Type
%% 		     String	Language
%% 		     String	Description
%% 		}
%%      21:
%%       GetMarks();
%%       Request:
%% 	String	FileName
%%       Response:
%% 	Failure:
%% 		ULONG 0
%% 	Success:
%% 	List Mark {
%% 	     ULONG	Position
%% 	}
%%      22:
%%       GetChannelPids();
%%       Request:
%% 	ULONG channelNumber
%%       Response:
%% 	Failure:
%% 		ULONG 0
%% 	Success:
%% 	ULONG vpid
%% 	ULONG numberOfApids
%% 	List {
%% 	     ULONG apid
%% 	     String language 
%% 	}
%% 	ULONG numberOfDpids
%% 	List {
%% 	     ULONG dpid
%% 	     String language 
%% 	}
%% 	ULONG Position
%% 	ULONG    numberOfSpids
%% 	List {
%% 	     ULONG spid
%% 	     String language 
%% 	     }
%% 	ULONG   tpid
		
%%      23:
%%       DeleteTimer();
%%       Request:
%% 	ULONG ChannelNumber
%% 	ULONG Weekdays
%% 	ULONG Day
%% 	ULONG StartTime 
%% 	ULONG StopTime

%%       Response:
%% 	ULONG { 10=ok,4=no_matching_timer, 3=unable_to_delete, 1=timers_being_edited }

%% 	ULONG Position
%% Media protocol extensions
%%      30:
%%       GetMediaList();
%%   * media List Request:
%%   * 4 length
%%   * 4 VDR_GETMEDIALIST
%%   * 4 flags (currently unused)
%%   * n dirname
%%   * n+1 0
%%   * Media List response:
%%   * 4 length
%%   * 4 VDR_
%%   * 4 numentries
%%   * per entry:
%%   * 4 media type
%%   * 4 time stamp
%%   * 4 flags
%%   * 4 strlen (incl. 0 Byte)
%%   * string
%%   * 0
%%      31:
%%       GetPicture();
%%   * get image Request: = GetPicture
%%   * 4 flags (currently unused)
%%   * 4 x size
%%   * 4 y size
%%   * n filename
%%   * n+1 0
%%   * get image response:
%%   * 4 length
%%   * 4 VDR_GETIMAGE
%%   * 4 len of image

%%      32:
%%       GetImageBlock();
%%       Request:
%% 	ULONG Position
%% 	ULONG Amount
%%       Response:
%% 	Failure:
%% 		ULONG 0
%% 	Success:
%% 	Data Block
%%      33:
%%       GetLanguageList();
%%      34:
%%       GetLanguageContent();
