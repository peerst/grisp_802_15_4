% @doc grisp_802_15_4 public API.
% @end
-module(grisp_802_15_4).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) -> grisp_802_15_4_sup:start_link().

stop(_State) -> ok.
