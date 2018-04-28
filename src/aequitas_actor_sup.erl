-module(aequitas_actor_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start_link/0,
    start_child/1
   ]).

%% ------------------------------------------------------------------
%% supervisor Function Exports
%% ------------------------------------------------------------------

-export([init/1]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(SERVER, ?MODULE).
-define(CB_MODULE, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?CB_MODULE, []).

start_child(Args) ->
    supervisor:start_child(?SERVER, Args).

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    SupFlags =
        #{ strategy => simple_one_for_one,
           intensity => 10,
           period => 1
         },
    Children =
        [#{ id => actor,
            start => {aequitas_actor, start_link, []},
            restart => temporary
          }
        ],
    {ok, {SupFlags, Children}}.
