-module(aequitas_directory_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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

%% ------------------------------------------------------------------
%% supervisor Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    SupFlags =
        #{ strategy => one_for_all,
           intensity => 0,
           period => 1
         },
    Children =
        [#{ id => directory,
            start => {aequitas_directory, start_link, []}
          },
         #{ id => directory_reg_sup,
            start => {aequitas_directory_reg_sup, start_link, []},
            type => supervisor
          }
        ],
    {ok, {SupFlags, Children}}.
