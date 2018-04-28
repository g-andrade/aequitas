-module(aequitas_directory).
-behaviour(gen_statem).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start_link/0,
    register/1,
    whereis/1,
    insert_new_entry/2,
    delete_entry/1
   ]).

%% ------------------------------------------------------------------
%% gen_statem Function Exports
%% ------------------------------------------------------------------

-export([callback_mode/0]).
-export([init/1]).
-export([handle_event/4]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(CB_MODULE, ?MODULE).
-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-record(sdata, {
         }).

-record(entry, {
          name :: term(),
          pid :: pid()
         }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_statem:start_link({local,?SERVER}, ?CB_MODULE, [], []).

register(Name) ->
    case aequitas_directory_reg:start(Name, self()) of
        {ok, _Pid} -> ok;
        {error, {shutdown, already_started}} -> {error, already_started}
    end.

whereis(Name) ->
    case ets:lookup(?TABLE, Name) of
        [#entry{ pid = Pid }] ->
            Pid;
        [] ->
            undefined
    end.

insert_new_entry(Name, Pid) ->
    Entry = #entry{ name = Name, pid = Pid },
    ets:insert_new(?TABLE, Entry).

delete_entry(Name) ->
    ets:delete(?TABLE, Name).

%% ------------------------------------------------------------------
%% gen_statem Function Definitions
%% ------------------------------------------------------------------

callback_mode() -> 
    handle_event_function.

init([]) ->
    EtsOpts = [named_table, public, {keypos,#entry.name},
               {read_concurrency,true}, {write_concurrency,true}
              ],
    _ = ets:new(?TABLE, EtsOpts),
    {ok, active, #sdata{}}.

handle_event(EventType, EventContent, _State, _SData) ->
    {stop, {unexpected_event, EventType, EventContent}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
