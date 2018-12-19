-module(grisp_802_15_4_mac).

-include("include/pmod_rf2.hrl").
-include("include/mac.hrl").
-import(pmod_rf2_register, [ read/2,
                             read_integer/2,
                             write/3 ]).
-export([force_transmit_mode/0, force_receive_mode/0]).
-export([load_frame_control/1]).
-export([read_rx_fifo/0]).

force_transmit_mode() ->
    write(short, ?RFCTL, 16#02).

force_receive_mode() ->
    write(short, ?RFCTL, 16#01).

%% with #{} passed - default data frame, intra PAN without ack, sec and pending frames
load_frame_control(Map) ->
    T = maps:get(t, Map, 1),
    S = maps:get(sec, Map, 0),
    P = maps:get(pen, Map, 0),
    A = maps:get(ack, Map, 0),
    I = maps:get(intra, Map, 1),
    DM = maps:get(dest_m, Map, 3),
    SM = maps:get(src_m, Map, 3),
    write(long, ?TX_FIFO+2, <<T:3, S:1, P:1, A:1, I:1, 0:1>>),
    write(long, ?TX_FIFO+3, <<0:2, DM:2, 0:2, SM:2>>),
    #fc{ t = translate_t(T),
         sec = S,
         pen = P,
         ack = A,
         intra = I,
         dst_m = DM,
         src_m = SM }.

read_rx_fifo() ->
   FrameLength = read_integer(long, ?RX_FIFO),
   <<T:3, Sec:1, Pen:1, Ack:1, Intra:1, _:1>> = read(long, ?RX_FIFO + 1),
   <<_:2, DMode:2, _:2, SMode:2>> = read(long, ?RX_FIFO + 2),
   <<S:8>> = read(long, ?RX_FIFO + 3),
   Type = translate_t(T),
   MHRLength  = mhr_length(Intra, DMode, SMode),
   MSDULength = FrameLength - MHRLength - 2,
   MPDU = #mpdu{ mhr = #mhr{ fc = #fc{ t = Type,
                                       sec = Sec,
                                       pen = Pen,
                                       ack = Ack,
                                       intra = Intra,
                                       dst_m = DMode,
                                       src_m = SMode },
                             sn = S,
                             addr_fields = read_addr_fields(Intra, DMode, SMode)},
                 msdu = read_msdu(Type, MHRLength, MSDULength)},
   #frame{ length = FrameLength, mpdu = MPDU }.

translate_t(0) ->
    beacon;
translate_t(1) ->
    data;
translate_t(2) ->
    ack;
translate_t(3) ->
    command.

r_translate_t(beacon) ->
    0;
r_translate_t(data) ->
    1;
r_translate_t(command) ->
    3.

read_addr_fields(1, 3, 3) ->
    Dest = read(long, ?RX_FIFO + 4, <<>>, 8),
    Src = read(long, ?RX_FIFO + 12, <<>>, 8),
    #addr_fields{ dst = #addr{ eadr = Dest },
                  src = #addr{ eadr = Src } }.

read_msdu(data, MHRLength, MSDULength) ->
    read(long, ?RX_FIFO + MHRLength, <<>>, MSDULength).

mhr_length(1, 3, 3) ->
    16.

read(_, _, Result, 0) ->
    Result;
read(Type, Addr, Read, Length) ->
    Part = read(Type, Addr),
    read(Type, Addr+1, <<Read/binary, Part/binary>>, Length-1).
