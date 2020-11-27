-record(frame, {length :: integer(), mpdu :: mpdu(), fcs :: integer(), lqi :: integer()}).
-record(mpdu, {mhr :: mhr(), msdu :: msdu()}).
-record(mhr, {fc :: fc(), sn :: integer(), addr_fields :: addr_fields()}).
-record(fc, {t :: t(),
             sec :: integer(),
             pen :: integer(),
             ack :: integer(),
             intra :: integer(),
             dst_m :: mode(),
             src_m :: mode()}).
-record(addr_fields, {dst :: addr(), src :: addr()}).
-record(addr, {pan_id :: integer(), sadr :: binary(), eadr :: binary()}).


-type mpdu() :: #mpdu{}.

-type mhr() :: #mhr{}.

-type msdu() :: payload().

-type addr_fields() :: #addr_fields{}.

-type addr() :: #addr{}.

-type fc() :: #fc{}.

-type t() :: beacon | data | ack | command.

-type mode() :: integer().

-type payload() :: binary().
