-module(grisp_802_15_4_reg).

-include("include/reg.hrl").

%% R/W operations
-export([read/2,
         read/4,
         write/3,
         write/4,
         read_integer/2,
         set_bit/3,
         unset_bit/3]).

%% Helpers
-export([decode/2, fifo/2]).

-define(MODE, #{cpol => low, cpha => leading}).

-type register_addr() :: integer().
-type register_type() :: short | long.

%% --- R/W operations -----------------------------------------------------------------------
-spec read(Type :: register_type(), Addr :: register_addr() | [register_addr()]) -> binary().
read(Type, Registers) when is_list(Registers) ->
    Values = lists:map(fun(R) -> read_integer(Type, R) end, Registers),
    RegHex = lists:map(fun(R) -> integer_to_list(R, 16) end, Registers),
    lists:zip(RegHex, Values);
read(long, Addr) ->
    Bits = <<?LONG:1, Addr:10, ?R:1, ?Skip:4>>,
    grisp_spi:send_recv(spi1, ?MODE, Bits, 2, 1);
read(short, Addr) ->
    Bits = <<?SHORT:1, Addr:6, ?R:1>>,
    grisp_spi:send_recv(spi1, ?MODE, Bits, 1, 1).

-spec read_integer(Type :: register_type(), Addr :: register_addr()) -> integer().
read_integer(Type, Addr) ->
    Res = read(Type, Addr),
    <<Decimal:8>> = Res,
    Decimal.

read(_, _, Result, 0) ->
    Result;
read(Type, Addr, Read, Length) ->
    Part = read_integer(Type, Addr),
    read(Type, Addr+1, <<Read/binary, Part:8>>, Length-1).

-spec write(Type :: register_type(), Addr :: register_addr(), Command :: integer() | binary()) -> binary().
write(Type, Addr, Command) when is_integer(Command) ->
    write(Type, Addr, <<Command:8>>);
write(long, Addr, Command) ->
    Bits = <<?LONG:1, Addr:10, ?W:1, ?Skip:4, Command/binary>>,
    grisp_spi:send_recv(spi1, ?MODE, Bits);
write(short, Addr, Command) ->
    Bits = <<?SHORT:1, Addr:6, ?W:1, Command/binary>>,
    grisp_spi:send_recv(spi1, ?MODE, Bits).

write(_, _, <<>>, Addr) ->
    Addr;
write(Type, Addr, <<Part:8, Rest/binary>>, Result) ->
    write(Type, Addr, Part),
    write(Type, Addr+1, Rest, <<Part:8, Result/binary>>).

-spec set_bit(Type :: register_type(), Addr :: register_addr(), Pos :: integer()) -> binary().
set_bit(Type, Addr, Pos) ->
    <<Bit>> = <<0:(8 - Pos - 1), 1:1, 0:(Pos)>>,
    Resp = read_integer(Type, Addr),
    Cmd = Bit bor Resp,
    write(Type, Addr, Cmd).

-spec unset_bit(Type :: register_type(), Addr :: register_addr(), Pos :: integer()) -> binary().
unset_bit(Type, Addr, Pos) ->
    <<Bit>> = <<1:(8 - Pos - 1), 0:1, 1:(Pos)>>,
    Resp = read_integer(Type, Addr),
    Cmd = Bit band Resp,
    write(Type, Addr, Cmd).

%% --- Helpers -----------------------------------------------------------------------

fifo(From, Count) ->
    lists:seq(From, From + Count - 1).

decode(B, N) ->
    integer_to_list(binary:decode_unsigned(B), N).
