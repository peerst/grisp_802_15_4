-module(grisp_802_15_4_phy).

-include("include/reg.hrl").

-import(grisp_802_15_4_reg,
        [read/2, write/3, read_integer/2, set_bit/3, unset_bit/3]).

% API
-export([initialize/0]).

-export([configure_pan_coordinator/1,
         configure_coordinator/0,
         configure_device/0]).

-export([get_eaddr/0,
         set_eaddr/1]).

-define(MODE, #{cpol => low, cpha => leading}).

%--- API -----------------------------------------------------------------------

initialize() ->
%% Hardware reset
    grisp_gpio:configure(spi1_pin8, output_0),
    timer:sleep(2500),
    grisp_gpio:configure(spi1_pin8, output_1),
    timer:sleep(2500),
%%TODO Extract to meaningful functions
    write(short, ?SOFTRST, 16#07),
%% Power Amplifier/ Low-noise amplifier
    write(short, ?PACON2, 16#98), %% enable FIFO bit<7>, transmitter on time bits <5:0>
    write(short, ?TXSTBL, 16#95), %% RF Stabilization Time bits<7:4>
%% Initialize RF Opt
    write(long, ?RFCON0, 16#03),
%% Initialize VCO Opt
    write(long, ?RFCON1, 16#01),
%% Enable Phase-Locked Loop - PLL must be enabled for reception/transmition
    write(long, ?RFCON2, 16#80),
%% TX Filter control bit<7>, recovery from sleep les than 1 ms bit<4>
    write(long, ?RFCON6, 16#90),
%% Use internal oscillator
    write(long, ?RFCON7, 16#80),
%%  VCO control
    write(long, ?RFCON8, 16#10),
%% Disable CLKOUT (recommended), Sleep Clock Divisor bit<4:0>
    write(long, ?SLPCON1, 16#21),
%% Configure CCA
    write(short, ?BBREG2, 16#80),
    write(short, ?CCAEDTH, 16#60),

%%    write(short, ?BBREG6, 16#40), appended RSSI value to RX FIFO bit <6>
%%    set bit<7> RSSIMODE1, wait for bit<0>  -> read 0x210

%% interrupts

%% Channel selection, RSSI ?
    write(long, ?RFCON0, 16#03),
%% Set transmitter power to  0dB - default 
    write(long, ?RFCON3, 16#00),
%% Reset RF State Machine
    write(short, ?RFCTL, 16#04),
    write(short, ?RFCTL, 16#00),
    timer:sleep(200). %% wait for min 192

set_pan_id(<<PanIDL:8, PanIDH:8>>) ->
    %% channel detection / pan_id detection can be done interchangeably
    write(short, ?PANIDL, PanIDL),
    write(short, ?PANIDH, PanIDH).

set_short_addr(<<SADRL:8, SADRH:8>>) ->
    write(short, ?SADRL, SADRL),
    write(short, ?SADRH, SADRH).

set_eaddr(<<EADR0:8, EADR1:8, EADR2:8, EADR3:8, EADR4:8, EADR5:8, EADR6:8, EADR7:8>>) ->
    write(short, ?EADR0, EADR0),
    write(short, ?EADR1, EADR1),
    write(short, ?EADR2, EADR2),
    write(short, ?EADR3, EADR3),
    write(short, ?EADR4, EADR4),
    write(short, ?EADR5, EADR5),
    write(short, ?EADR6, EADR6),
    write(short, ?EADR7, EADR7).

get_eaddr() ->
    <<(read(short, ?EADR0))/binary,
      (read(short, ?EADR1))/binary,
      (read(short, ?EADR2))/binary,
      (read(short, ?EADR3))/binary,
      (read(short, ?EADR4))/binary,
      (read(short, ?EADR5))/binary,
      (read(short, ?EADR6))/binary,
      (read(short, ?EADR7))/binary>>.


configure_device() ->
    unset_bit(short, ?RXMCR, 3).
    %% TXMCR keep default settings, <5> bit should be 0

configure_coordinator() ->
    ok.
configure_pan_coordinator(PanID) ->
    set_bit(short, ?RXMCR, 3),
    %% TXMCR keep default settings, <5> bit should be 0
    write(short, ?ORDER, 16#FF),
    set_pan_id(PanID),
    set_short_addr(<<16#0000:16>>).
