-module(grisp_802_15_4_mac).

-include("include/pmod_rf2.hrl").
-include("include/mac.hrl").
-import(pmod_rf2_register, [ read/2,
                             read_integer/2 ]).
-export([read_rx_fifo/0]).

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
