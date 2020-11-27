-module(grisp_802_15_4_mac).

-include("include/reg.hrl").
-include("include/mac.hrl").
-import(grisp_802_15_4_reg, [ read/2,
                              read/4,
                              read_integer/2,
                              write/3,
                              write/4]).
-export([load_tx_fifo/4, load_frame_control/1, load_addr_fields/1, load_msdu/3]).
-export([transmit/1]).
-export([read_tx_fifo/0]).
-export([read_addr_fields/3, read_msdu/3]).
-export([read_rx_fifo/0]).

transmit(_Frame = #frame{ mpdu = #mpdu{ mhr = #mhr{ fc = #fc{ ack = 1 }}}}) ->
    grisp_802_15_4_reg:write(short, 16#1B, 5);
transmit(_Frame) ->
    grisp_802_15_4_reg:set_bit(short, 16#1B, 0).

%% Acknowledged frame has frame length 0 (?!?), MSDULength has to be passed as part of payload
%% Payload = << MSDULength:8, Rest/binary >>
load_tx_fifo(FC = #{ intra := 0, dst_m := 2, src_m := 2 }, S, D, MSDU) ->
    load_tx_fifo(11, FC, S, D, MSDU);
load_tx_fifo(FC, S, D, MSDU) ->
    load_tx_fifo(19, FC, S, D, MSDU).

load_tx_fifo(MHRLength, FrameControlMap, Sequence, DestinationMap, MSDU) ->
    write(long, ?TX_FIFO, MHRLength),
    write(long, ?TX_FIFO + 1, MHRLength + size(MSDU)),
    FC = load_frame_control(FrameControlMap),
    write(long, ?TX_FIFO + 4, Sequence),
    AddrFields = load_addr_fields(DestinationMap),
    Payload = load_msdu(MHRLength, payload, MSDU),
    MPDU = #mpdu{ mhr = #mhr{ fc = FC,
                              sn = Sequence,
                              addr_fields = AddrFields },
                  msdu = Payload },
    #frame{ length = MHRLength + size(MSDU), mpdu = MPDU }.
%% with #{} passed - default data frame, intra PAN without ack, sec and pending frames
load_frame_control(Map) ->
    T = maps:get(t, Map, 1),
    S = maps:get(sec, Map, 0),
    P = maps:get(pen, Map, 0),
    A = maps:get(ack, Map, 0),
    I = maps:get(intra, Map, 1),
    DM = maps:get(dst_m, Map, 3),
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

%%-spec load_addr_fields(D :: addr(), S :: addr()) -> addr_fields().

load_addr_fields(#{ dest_panid := <<ID:16>>, dest_sadr := <<Addr:16>>}) ->
    <<PanID:16>> = grisp_802_15_4_phy:get_pan_id(),
    <<SSADR:16>> = grisp_802_15_4_phy:get_saddr(),
    io:format("~p~n", [PanID]),
    io:format("~p~n", [SSADR]),
    write(long, ?TX_FIFO+5, <<ID:16>>, <<>>),
    write(long, ?TX_FIFO+7, <<Addr:16>>, <<>>),
    write(long, ?TX_FIFO+9, <<PanID:16>>, <<>>),
    write(long, ?TX_FIFO+11, <<SSADR:16>>, <<>>),
    #addr_fields{ dst = #addr{pan_id = ID, sadr = Addr}, src = #addr{ pan_id = PanID, sadr = SSADR } };
load_addr_fields(DestinationMap) ->
    <<DEADR:64>> = maps:get(deadr, DestinationMap),
    <<SEADR:64>> = grisp_802_15_4_phy:get_eaddr(),
    write(long, ?TX_FIFO+5, <<DEADR:64, SEADR:64>>, <<>>),
    #addr_fields{ dst = #addr{eadr = <<DEADR:64>>}, src = #addr{ eadr = <<SEADR:64>> } }.

load_msdu(MHRLength, payload, Binary) ->
    write(long, ?TX_FIFO + MHRLength + 2, Binary, <<>>),
    Binary.

read_tx_fifo() ->
   HeaderLength = read_integer(long, ?TX_FIFO),
   FrameLength = read_integer(long, ?TX_FIFO + 1),
   <<T:3, Sec:1, Pen:1, Ack:1, Intra:1, _:1>> = read(long, ?TX_FIFO + 2),
   <<_:2, DMode:2, _:2, SMode:2>> = read(long, ?TX_FIFO + 3),
   <<S:8>> = read(long, ?TX_FIFO + 4),
   Type = translate_t(T),
   DEADR = read(long, ?TX_FIFO + 5, <<>>, 8),
   SEADR = read(long, ?TX_FIFO + 13, <<>>, 8),
   MPDU = #mpdu{ mhr = #mhr{ fc = #fc{ t = Type,
                                       sec = Sec,
                                       pen = Pen,
                                       ack = Ack,
                                       intra = Intra,
                                       dst_m = DMode,
                                       src_m = SMode },
                             sn = S,
                             addr_fields = #addr_fields{ dst = #addr{ eadr = DEADR }, src = #addr{ eadr = SEADR }}},
                 msdu = read(long, ?TX_FIFO + 21, <<>>, FrameLength - HeaderLength)},
   #frame{ length = FrameLength, mpdu = MPDU }.

recv_frame_length() ->
    read_integer(long, ?RX_FIFO).

read_rx_fifo() ->
   <<T:3, Sec:1, Pen:1, Ack:1, Intra:1, _:1>> = read(long, ?RX_FIFO + 1),
   <<_:2, DMode:2, _:2, SMode:2>> = read(long, ?RX_FIFO + 2),
   Type = translate_t(T),
   read_rx_fifo(#fc{ t = Type,
                     sec = Sec,
                     pen = Pen,
                     ack = Ack,
                     intra = Intra,
                     dst_m = DMode,
                     src_m = SMode }).
read_rx_fifo(FC = #fc{ t = data, ack = Ack, intra = Intra, dst_m = DMode, src_m = SMode }) ->
   MHRLength  = mhr_length(Intra, DMode, SMode),
   <<S:8>> = read(long, ?RX_FIFO + 3),
   {FrameLength, MSDULength} = case recv_frame_length() of
                                   0 ->
                                       M = read_integer(long, ?RX_FIFO + MHRLength + 1),
                                       {M + MHRLength + 2, M};
                                   N ->
                                       {N, N - MHRLength - 2}
                               end,
%%   MSDULength = FrameLength - MHRLength - 2,
   FCS = read(long, ?RX_FIFO + FrameLength - 1, <<>>, 2),
   LQI = read(long, ?RX_FIFO + FrameLength + 1),
   MPDU = #mpdu{ mhr = #mhr{ fc = FC,
                             sn = S,
                             addr_fields = read_addr_fields(Intra, DMode, SMode)},
                 msdu = read_msdu(data, MHRLength, MSDULength)},
   #frame{ length = FrameLength,
           mpdu = MPDU,
           fcs = grisp_802_15_4_reg:decode(FCS,10),
           lqi = grisp_802_15_4_reg:decode(LQI, 10)
         };
read_rx_fifo(_FC = #fc{ t = Type }) ->
    logger:error("MPDU of type ~p not supported", [Type]).

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

read_addr_fields(0, 2, 2) ->
    DestID = read(long, ?RX_FIFO + 4, <<>>, 2),
    DestAddr = read(long, ?RX_FIFO + 6, <<>>, 2),
    SrcID = read(long, ?RX_FIFO + 8, <<>>, 2),
    SrcAddr = read(long, ?RX_FIFO + 10, <<>>, 2),
    #addr_fields{ dst = #addr{pan_id = DestID, sadr = DestAddr}, src = #addr{ pan_id = SrcID, sadr = SrcAddr } };
read_addr_fields(1, 3, 3) ->
    Dest = read(long, ?RX_FIFO + 4, <<>>, 8),
    Src = read(long, ?RX_FIFO + 12, <<>>, 8),
    #addr_fields{ dst = #addr{ eadr = Dest },
                  src = #addr{ eadr = Src } }.

read_msdu(data, MHRLength, MSDULength) ->
    read(long, ?RX_FIFO + MHRLength + 1, <<>>, MSDULength).

mhr_length(0, 2, 2) ->
    7;
mhr_length(1, 3, 3) ->
    19.
