> # Vomp Protocol for communication between Vomp Client and Server #
> > 

## Introduction ##


The Video Disk Recorder (VDR http://www.cadsoft.de/vdr/) supports a
plugin architecture.
Vomp (http://www.loggytronic.com/vomp.php) is a client application
implementing  set-top box functionality running on a Hauppauge MVP box.
The Vomp client accesses the TV functionality running on the VDR server via an
application specific protocol. The server part of the protocol is implemented
as a plugin on the VDR server.

This document specifies the Vomp protocol.
It is mostly compiled by reverse engineering of vompclientrrproc.c of Vomp
server, version 0.3.0.

The Vomp protocol consists of the following parts:

  * Server detection
  * Request-Response protocol
  * Streaming protocol
  * Keep Alive protocol

Server detection uses UDP messaging, whereas Request/Response, Streaming and Keep Alive messages are exchanged over TCP connection. The TCP connection is denoted a Vomp channel in subsequent text.

Multiple clients are supported by the protocol as well as the Vomp server implementation.

## Server detection ##


  1. client broadcasts UDP to Address 255.255.255.255. Source and destination ports are 3024. Payload is 4 octets "VOMP" = 0x56 0x4f 0x4d 0x50
  1. erver responds to Client Address UDP port 3024 (always regardless of source port in previous broadcast.) Payload is 12 octets containing server name. Padding is NULL.

NOTE!
Current Server (Version 0.3.0) does not support client and server on the same Linux
host because Server always replies to UDP port 3024, which is already bound to server. Solution is to respond to source UDP port.
Server patch:

> ds.send(ds.getFromIPA(),ds.getFromPort(), serverName, strlen(serverName));
instead of:
> ds.send(ds.getFromIPA(),3024, serverName, strlen(serverName));


  1. Client establishes a TCP connection to Server. Destination IP Address is taken from UDP Unicast packet from server. Destination TCP port is 3024.



## Request-Response protocol ##


```
/* Packet format for a request:

4 bytes = Channel ID = 1 (Request/Response channel)
4 bytes = Request ID
4 bytes = Opcode
4 bytes = Length of the rest of the packet
? bytes = rest of packet. depends on packet
*/

Packet format for a Response:

4 bytes = channel ID = 1 (request/response channel)
4 bytes = request ID
4 bytes = length of the rest of the packet
? bytes = rest of packet. depends on packet


The following lists the functional capabilities of the request/response protocol

    Opcode
     1: V
      Login();
      Login Request is 6 octets and contains the client MAC address
      Server responds with 4 octets Unix Time followed by 4 octets GMT offset in 
      Unix Time.
     2: V
      GetRecordingsList();
      Response:
      ULONG DiskspaceTotal[MB]
      ULONG DiskspaceFree[MB]
      ULONG DiskSpaceUtilisation[%]
      List Recordings {
	ULONG StartTime [Unix time]
	String Name
	String Filename
	}
     3: V
      DeleteRecording();
      Request:
	String FileName
      Response:
	Reply ULONG { 4=recording_not_found, 3=recording_control_found,
		2=delete_failed,1=ok }
     5: V
      GetChannelsList();
      Request:
      Response:
      List Channel {
	   ULONG Number
	   ULONG Type (What is type ?)
	   String Name
	   }
     6:
      StartStreamingChannel();
      Start live TV streaming for Channel number.
	RequestId is used as handle for stream.
	The stream will contain a header according to specification of streaming
	protocol below.
      Request:
	ULONG channelNumber
      Response:
	Failure:
		ULONG 0
	Success:
		ULONG 1
     7: V
      GetBlock();
      What is this for for ?
      Request:
	Ulonglong Position
	ULONG Amount
      Response:
	Failure:
		{ 0=end_of_stream }
	Success:
	Data Block
     8:
      StopStreaming();
      Stop current Streaming of Live TV .
      Request:
	-
      Response:
	ULONG 1
     9:
      OpenRecording();
      Open a recording identified by FileName.
      Request:
	String FileName
      Response:
	Success:
	ULongLong LengthBytes
	ULONG LengthFrames
	Failure:
		NO response !
	
     10:
      GetChannelSchedule();
      Request:
	ULONG ChannelNumber
	ULONG StartTime
	ULONG Duration 
      Response:
	Failure and no events found:
		Ulong 0
	List {
	     ULONG   EventID
	     ULONG   EventTime
	     ULONG   EventDuration
	     String  EventTitle
	     String  EventSubTitle
	     String  EventDescription
	}


     11:
      ConfigSave();
      Request:
	String Section
	String Key
	String Value
      Response:
	ULONG	{ 1=ok, 0=error}
     12:
      ConfigLoad();
      Request:
	String Section
	String Key
      Response:
	Failure:
		Ulong 0
	Success:
		String Value
     13:
      ReScanRecording();
      This Opcode is Obsolete
     14: V
      GetTimers();
      Request:
      Response:
	ULONG	numTimers
	List Timers{
	    ULONG	Active
	    ULONG	Recording
	    ULONG	Pending
	    ULONG	Priority
	    ULONG	Lifetime
	    ULONG	ChannelNumber
	    ULONG	StartTime
	    ULONG	StopTime
	    ULONG	Day
	    ULONG	WeekDays
Weekdays;       bitmask, lowest bits: SSFTWTM  (the 'M' is the LSB)

	    String	FileName
	} 
     15: V
      SetTimer();
      Request:
	String "Flags Channel Day Start Stop, Priority, Lifetime FileName Aux
Flags: unsigned integer
enum eTimerFlags { tfNone      = 0,
                   tfActive    = 1,
                   tfInstant   = 2,
                   tfVps       = 4,
                   tfRecording = 8,
                   tfAll       = 65535,
                 };
Channel:
	integer|String
Day:
  // possible formats are:
  // 19
  // 2005-03-19
  // MTWTFSS
  // MTWTFSS@19
  // MTWTFSS@2005-03-19
Start,Stop HHMM Local time
Priority: 0-99
Lifetime: 0-99
Aux: String Optional ( What is this for? )

      Response:
	ULONG { 2=bad_timerstring, 1=timer_already_set, 0=ok}
     16:
      PositionFromFrameNumber();
      Request:
	ULONG frameNumber
      Response:
	Ulong Position (0 in case of failure)
     17:
      FrameNumberFromPosition()
      Request:
	ULONG Position 
      Response:
	Ulong Frame (0 in case of failure)
     18:
      MoveRecording();
      Request:
      Response:
     19:
      GetIFrame();
      Request:
      ULONG frameNumber
      ULONG direction
      Response:
	Failure:
		Ulong 0
	Success:
		ULLONG filePosition 
		ULONG frameNumber
		ULONG frameLength
	
     20:
      GetRecInfo();
      Request:
	String FileName
      Response:
	Failure:
		ULONG 0
	Success:
		ULONG	StartTime
		ULONG	StopTime
		ULONG	ResumePoint
		String	Summary
		ULONG	NumberOfChannels 
		List Components {
		     UCHAR	Stream
		     UCHAR	Type
		     String	Language
		     String	Description
		}
     21:
      GetMarks();
      Request:
	String	FileName
      Response:
	Failure:
		ULONG 0
	Success:
	List Mark {
	     ULONG	Position
	}
     22:
      GetChannelPids();
      Request:
	ULONG channelNumber
      Response:
	Failure:
		ULONG 0
	Success:
	ULONG vpid
	ULONG numberOfApids
	List {
	     ULONG apid
	     String language 
	}
	ULONG numberOfDpids
	List {
	     ULONG dpid
	     String language 
	}
	ULONG    numberOfSpids
	List {
	     ULONG spid
	     String language
	     }
	ULONG   tpid
		
     23:
      DeleteTimer();
      Request:
	ULONG ChannelNumber
	ULONG Weekdays
	ULONG Day
	ULONG StartTime 
	ULONG StopTime

      Response:
	ULONG { 10=ok,4=no_matching_timer, 3=unable_to_delete, 1=timers_being_edited }

	ULONG Position


Media protocol extensions
     30:
      GetMediaList();
  * media List Request:
  * 4 length
  * 4 VDR_GETMEDIALIST
  * 4 flags (currently unused)
  * n dirname
  * n+1 0
  * Media List response:
  * 4 length
  * 4 VDR_
  * 4 numentries
  * per entry:
  * 4 media type
  * 4 time stamp
  * 4 flags
  * 4 strlen (incl. 0 Byte)
  * string
  * 0
     31:
      GetPicture();
  * get image Request: = GetPicture
  * 4 flags (currently unused)
  * 4 x size
  * 4 y size
  * n filename
  * n+1 0
  * get image response:
  * 4 length
  * 4 VDR_GETIMAGE
  * 4 len of image

     32:
      GetImageBlock();
      Request:
	ULONG Position
	ULONG Amount
      Response:
	Failure:
		ULONG 0
	Success:
	Data Block
     33:
      GetLanguageList();
     34:
      GetLanguageContent();
```

## Playing a recording ##

OpenRecording will will enable the server to send blocks of data of the opened recording. The client needs to repeatedly send GetBlock(Position,AmountOfData) to receive to entire Recording.

Mediainfo reports the following about the Recording

```
General #0
Complete name        : \\NAS1\Qdownload\recordings_play-8.yaws
Format               : MPEG-2 Program
Format/Family        : MPEG-2
File size            : 14.1 MiB

Video #0
Codec                : MPEG-2 Video
Codec profile        : Main@Main
Codec settings/Matri : Standard
Nominal bit rate     : 15 Mbps
Bit rate mode        : CBR
Width                : 720 pixels
Height               : 576 pixels
Display Aspect ratio : 16/9
Frame rate           : 25.000 fps
Standard             : PAL
Chroma               : 4:2:0
Interlacement        : Top Field First
```

## Streaming protocol ##


### Packet format for a stream packet ###

4 bytes = Channel ID = 2 (stream channel)
4 bytes = Stream ID (from requestID)
4 bytes = Code ( 0 = stream, 1= stream\_end)
4 bytes = Length of the stream data (=0 when stream\_end)
? bytes = stream data

Streaming live TV is accomplished by sending StartStreamingChannel with channelNumber as parameter. This will initiate a stream of packets over the Vomp channel.
The client needs to send a Keep Alive mesage every 15 seconds. The Vomp channel will be closed by the server 20 seconds after the last Keep Alive message.

The stream will stop when the client sends StopStreaming.

Mediainfo reports the following about the Live TV stream.
```
General #0
Complete name        : \\NAS1\Qdownload\output.mpg
Format               : MPEG-1 Transport
Format/Family        : MPEG-1
File size            : 71.4 MiB

```

## Keep Alive protocol ##

Keep Alive (KA) Channel

Request:
4 bytes = Channel ID = 3
4 Bytes Timestamp

Response:
4 bytes = channel ID = 3
4 Bytes Timestamp ( Copied from request )



Miscellaneous notes


For every video frame:
{
> File offset    4 bytes
> Picture type   1 byte
> File number    1 byte
> Zero           2 bytes
}

Picture types:

#define NO\_PICTURE 0
#define I\_FRAME    1
#define P\_FRAME    2
#define B\_FRAME    3