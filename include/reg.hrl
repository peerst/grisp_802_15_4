-define(SHORT, 2#0).
-define(LONG, 2#1).

-define(W, 2#1).
-define(R, 2#0).

-define(Skip, 0).
%% REGISTERS
%%
-define(PANIDL, 16#01).
-define(PANIDH, 16#02).
-define(SADRL, 16#03).
-define(SADRH, 16#04).
-define(EADR0, 16#05).
-define(EADR1, 16#06).
-define(EADR2, 16#07).
-define(EADR3, 16#08).
-define(EADR4, 16#09).
-define(EADR5, 16#0A).
-define(EADR6, 16#0B).
-define(EADR7, 16#0C).
%% software reset
-define(SOFTRST, 16#2A).

%% POWER AMPLIFIER CONTROL REGISTERS
-define(PACON0, 16#16).
-define(PACON1, 16#17).
-define(PACON2, 16#18).

%% INTERFRAME SPACING
-define(TXPEND, 16#21).
-define(TXTIME, 16#27).
-define(TXSTBL, 16#2E). %% TX STABILIZATION REGISTER


%% table p.92 MRF24J40 Data Sheet

%% RF MODE CONTROL REGISTER
-define(RFCTL, 16#36).

%% RFCON[number] registers - RF CONTROL REGISTERS
%%
%% channel selection and frequency ( 2.4GHz band )
-define(RFCON0, 16#200). %%CHANNEL selection (bit 7-4), RF Optimize (bit 3-0)
-define(RFCON1, 16#201). %%Voltage-controlled oscillator optimization bits
-define(RFCON2, 16#202). %%PLL - Phase-locked loop (must be enabled for RF trans & rec)
-define(RFCON3, 16#203).
-define(RFCON6, 16#206). %% TX filter (7), 20MHx Clock Recovery (4), Batery monitor enable (3)
-define(RFCON7, 16#207). %% Sleep clock internal/external - selection (7-6)
-define(RFCON8, 16#208). %% Initialize VCO (4)
-define(RSSI, 16#210).
-define(SLPCON1, 16#220).
-define(BBREG2, 16#3A).
-define(CCAEDTH, 16#3F).
-define(BBREG6, 16#3E).

%%interrupts
-define(INTSTAT, 16#31).  %% flags
-define(INTCON, 16#32).   %% for enabling interrupts
-define(SLPCON0, 16#211). %% <1> bit determines the edge polarity of the INT pin
                          %% match the interrupt pin polarity of the host microcontroller!
                          %%
%% RECEIVE MAC CONTROL REGISTER reception modes, pcoord/coord/device
-define(RXMCR, 16#00). 
-define(RXFLUSH, 16#0D).
%% CSMA-CA MODE CONTROL REGISTER
-define(TXMCR, 16#11).
-define(ORDER, 16#10).

-define(RX_FIFO, 16#300).
-define(TX_FIFO, 16#000).
