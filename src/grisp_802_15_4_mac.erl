-module(grisp_802_15_4_mac).

-include("include/reg.hrl").
-include("include/mac.hrl").
-import(grisp_802_15_4_reg, [ read/2,
                              read/4,
                              read_integer/2,
                              write/3,
                              write/4]).
-export([force_transmit_mode/0, force_receive_mode/0]).
-export([load_tx_fifo/4, load_frame_control/1, load_addr_fields/1, load_msdu/3]).
-export([transmit/1]).
-export([read_tx_fifo/0]).
-export([read_addr_fields/3, read_msdu/3]).
-export([read_rx_fifo/0]).

force_transmit_mode() ->
    write(short, ?RFCTL, 16#04),
    write(short, ?RFCTL, 16#02).

force_receive_mode() ->
    write(short, ?RFCTL, 16#04),
    write(short, ?RFCTL, 16#01).

transmit(_Frame = #frame{ mpdu = #mpdu{ mhr = #mhr{ fc = #fc{ ack = 1 }}}}) ->
    grisp_802_15_4_reg:write(short, 16#1B, 5);
transmit(_Frame) ->
    grisp_802_15_4_reg:set_bit(short, 16#1B, 0).

%% Acknowledged frame has frame length 0 (?!?), MSDULength has to be passed as part of payload
%% Payload = << MSDULength:8, Rest/binary >>
load_tx_fifo(FrameControlMap, Sequence, DestinationMap, MSDU) ->
    MHRLength = 19,
    write(long, ?TX_FIFO, 19),
    write(long, ?TX_FIFO + 1, 19 + size(MSDU)),
    FC = load_frame_control(FrameControlMap),
    write(long, ?TX_FIFO + 4, Sequence),
    AddrFields = load_addr_fields(DestinationMap),
    Payload = load_msdu(MHRLength, payload, MSDU),
    MPDU = #mpdu{ mhr = #mhr{ fc = FC,
                              sn = Sequence,
                              addr_fields = AddrFields },
                  msdu = Payload },
    #frame{ length = 19 + size(MSDU), mpdu = MPDU }.
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

%%-spec load_addr_fields(D :: addr(), S :: addr()) -> addr_fields().
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

read_addr_fields(1, 3, 3) ->
    Dest = read(long, ?RX_FIFO + 4, <<>>, 8),
    Src = read(long, ?RX_FIFO + 12, <<>>, 8),
    #addr_fields{ dst = #addr{ eadr = Dest },
                  src = #addr{ eadr = Src } }.

read_msdu(data, MHRLength, MSDULength) ->
    read(long, ?RX_FIFO + MHRLength + 1, <<>>, MSDULength).

mhr_length(1, 3, 3) ->
    19.
