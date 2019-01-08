-module(grisp_802_15_4_phy).

-include("include/reg.hrl").

-import(grisp_802_15_4_reg,
        [read/2, write/3, read_integer/2, set_bit/3, unset_bit/3, poll/3, poll/4]).

% API
-export([initialize/0]).

-export([select_channel/1, calculate_rssi/0, append_rssi/0, configure_cca1/1, configure_cca2/1, configure_cca3/2]).

-export([configure_interrupts/1,
         enable_txnie/0,
         disable_txnie/0,
         enable_rxie/0,
         disable_rxie/0,
         check_and_reset_interrupts/0
         ]).

-export([rf_state/0, force_transmit_mode/0, force_receive_mode/0, rx_fifo_flush/0]).

-export([configure_pan_coordinator/1,
         configure_coordinator/0,
         configure_device/0]).

-export([get_eaddr/0,
         set_eaddr/1,
         set_pan_id/1,
         get_pan_id/0,
         get_saddr/0,
         set_saddr/1]).

-define(MODE, #{cpol => low, cpha => leading}).

%--- API -----------------------------------------------------------------------

initialize() ->
    hard_reset(),
    soft_reset(), 
%% Power Amplifier
    write(short, ?PACON2, 16#98), %% enable FIFO bit<7>, transmitter on time bits <5:0>
    write(short, ?TXSTBL, 16#95), %% RF Stabilization Time bits<7:4>
%% Initialize RF Opt
    write(long, ?RFCON0, 16#03),
%% Initialize VCO Opt
    write(long, ?RFCON1, 16#01),

    enable_pll(),
%% TX Filter control bit<7>, recovery from sleep les than 1 ms bit<4>
    write(long, ?RFCON6, 16#90),
%% Use internal oscillator
    write(long, ?RFCON7, 16#80),
%%  VCO control
    write(long, ?RFCON8, 16#10),
%% Disable CLKOUT (recommended), Sleep Clock Divisor bit<4:0>
    write(long, ?SLPCON1, 16#21),

    configure_cca1(16#60),
    select_channel(26),
    set_transmitter_power(0),
    rf_reset().

hard_reset() ->
%% Hardware reset
    grisp_gpio:configure(spi1_pin8, output_0),
    timer:sleep(2500),
    grisp_gpio:configure(spi1_pin8, output_1),
    timer:sleep(2500).

soft_reset() ->
    write(short, ?SOFTRST, 16#07).

enable_fifo() ->
    write(short, ?PACON2, 16#98).

enable_pll() ->  %% PLL must be enabled for reception/transmition
    write(long, ?RFCON2, 16#80).

set_transmitter_power(P) ->
    write(long, ?RFCON3, P).

select_channel(Channel) ->
    Val = (Channel  - 11) * 16 + 3,
    write(long, ?RFCON0, Val).

calculate_rssi() ->
    Initial = read_integer(short, ?BBREG6),
    Expected = Initial bor 1,
    set_bit(short, ?BBREG6, 7),
    poll(short, ?BBREG6, Expected),
    read_integer(long, ?RSSI).

append_rssi() ->
    set_bit(short, ?BBREG6, 6).

configure_cca1(ED) -> % energy detection from rssi
    write(short, ?BBREG2, 2#10001000),
    write(short, ?CCAEDTH, ED).

configure_cca2(CST) -> % carrier sense threshold
    MT = 2#010000 bor CST,
    write(short, ?BBREG2, MT*4).

configure_cca3(ED, CST) ->
    MT = 2#11000000 bor CST,
    write(short, ?BBREG2, MT*4),
    write(short, ?CCAEDTH, ED).


rx_fifo_flush() ->
    set_bit(short, ?RXFLUSH, 0).

rf_state() ->
    read(long, 16#20F).

rf_reset() ->
    write(short, ?RFCTL, 16#04),
    write(short, ?RFCTL, 16#00),
    timer:sleep(200).

force_transmit_mode() ->
    write(short, ?RFCTL, 16#04),
    write(short, ?RFCTL, 16#02).

force_receive_mode() ->
    write(short, ?RFCTL, 16#04),
    write(short, ?RFCTL, 16#01).

configure_interrupts(falling) ->
    unset_bit(long, ?SLPCON0, 1),
    grisp_gpio:configure(spi1_pin7, output_1);
configure_interrupts(raising) ->
    set_bit(long, ?SLPCON0, 1),
    grisp_gpio:configure(spi1_pin7, output_0).

enable_txnie() ->
    unset_bit(short, ?INTCON, 0).
disable_txnie() ->
    set_bit(short, ?INTCON, 0).

check_and_reset_interrupts() ->
    read(short, ?INTSTAT).

enable_rxie() ->
    unset_bit(short, ?INTCON, 3).
disable_rxie() ->
    set_bit(short, ?INTCON, 3).

set_pan_id(<<PanIDL:8, PanIDH:8>>) ->
    %% channel detection / pan_id detection can be done interchangeably
    write(short, ?PANIDL, PanIDL),
    write(short, ?PANIDH, PanIDH).

get_pan_id() ->
    %% channel detection / pan_id detection can be done interchangeably
    <<(read(short, ?PANIDL))/binary,
      (read(short, ?PANIDH))/binary>>.


set_saddr(<<SADRL:8, SADRH:8>>) ->
    write(short, ?SADRL, SADRL),
    write(short, ?SADRH, SADRH).

get_saddr() ->
    <<(read(short, ?SADRL))/binary,
      (read(short, ?SADRH))/binary>>.


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
    set_saddr(<<16#0000:16>>).
