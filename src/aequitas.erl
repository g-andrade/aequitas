%% Copyright (c) 2018-2022 Guilherme Andrade
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy  of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

-module(aequitas).

-ifdef(E48).
-moduledoc """
Fairness regulator for Erlang/OTP and Elixir, with optional rate limiting.

`aequitas` allows fair access to limited external resources (databases, web
services, ...) amongst distinct actors, by detecting outliers in ordinary
workload distributions. See the [README](readme.html) for an overview,
configuration and examples.
""".
-endif.

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export(
   [start/1,
    start/2,
    stop/1,
    ask/2,
    ask/3,
    async_ask/2,
    async_ask/3,
    reconfigure/2
   ]).

-ignore_xref(
   [start/1,
    start/2,
    stop/1,
    ask/2,
    ask/3,
    async_ask/2,
    async_ask/3,
    reconfigure/2
   ]).

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

-ifdef(E48).
-doc """
Like `start/2` but with default options.

See also `stop/1` and `reconfigure/2`.
""".
-endif.
-spec start(Category) -> ok | {error, Reason}
    when Category :: term(),
         Reason :: already_started.
start(Category) ->
    start(Category, []).

-ifdef(E48).
-doc """
Starts a handler for `Category`.

- `Category` can be any term.
- `Opts` must be a list of `t:aequitas_category:setting_opt/0` values.

Returns:

- `ok` in case of success
- `{error, Reason}` otherwise

See also `start/1`, `stop/1` and `reconfigure/2`.
""".
-endif.
-spec start(Category, Opts) -> ok | {error, Reason}
    when Category :: term(),
         Opts :: [aequitas_category:setting_opt()],
         Reason :: (already_started |
                    {invalid_setting_opt, term()} |
                    {invalid_setting_opts, term()}).
start(Category, Opts) ->
    case aequitas_category:start(Category, true, Opts) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _Pid}} ->
            {error, already_started};
        {error, Reason} ->
            {error, Reason}
    end.

-ifdef(E48).
-doc """
Stops a `Category` handler.

- `Category` must correspond to a started handler.

Returns:

- `ok` in case of success
- `{error, Reason}` otherwise

See also `start/1` and `reconfigure/2`.
""".
-endif.
-spec stop(Category) -> ok | {error, Reason}
    when Category :: term(),
         Reason :: not_started.
stop(Category) ->
    aequitas_category:stop(Category).

-ifdef(E48).
-doc """
Like `ask/3` but with default options.

See also `async_ask/2` and `async_ask/3`.
""".
-endif.
-spec ask(Category, ActorId) -> Status | {error, Reason}
    when Category :: term(),
         ActorId :: term(),
         Status :: accepted | {rejected, RejectionReason},
         RejectionReason :: outlier | rate_limited,
         Reason :: not_started.
ask(Category, ActorId) ->
    ask(Category, ActorId, []).

-ifdef(E48).
-doc """
Request permission to perform work, identified under `ActorId`, within `Category`.

- `Category` must refer to a started category handler.
- `ActorId` can be any term.
- `Opts` must be a list of `t:aequitas_category:ask_opt/0` values.

Returns:

- `accepted` if work execution was granted
- `{rejected, Reason}` if work execution was denied
- `{error, Reason}` if something went wrong

See also `ask/2`, `async_ask/2` and `async_ask/3`.
""".
-endif.
-spec ask(Category, ActorId, Opts) -> Status | {Status, Stats} |
                                      {error, Reason}
    when Category :: term(),
         ActorId :: term(),
         Opts :: [aequitas_category:ask_opt()],
         Status :: accepted | {rejected, RejectionReason},
         RejectionReason :: outlier | rate_limited,
         Stats :: aequitas_work_stats:t(),
         Reason :: not_started.
ask(Category, ActorId, Opts) ->
    aequitas_category:ask(Category, ActorId, Opts).

-ifdef(E48).
-doc """
Like `async_ask/3` but with default options.

See also `ask/2` and `ask/3`.
""".
-endif.
-spec async_ask(Category, ActorId) -> {Tag, Monitor}
    when Category :: term(),
         ActorId :: term(),
         Tag :: reference(),
         Monitor :: reference().
async_ask(Category, ActorId) ->
    async_ask(Category, ActorId, []).

-ifdef(E48).
-doc """
Like `ask/3` but the reply is sent asynchronously.

Returns a `{Tag, Monitor}` pair whose members can be used to pattern match
against the reply, which will be sent as a message to the calling process in
one of the following formats:

- `{Tag, accepted}` if work execution was granted
- `{Tag, {rejected, Reason}}` if work execution was denied
- `{Tag, {accepted, Stats}}` if work execution was granted and stats requested
- `{Tag, {{rejected, Reason}, Stats}}` if work execution was denied and stats requested
- `{'DOWN', Monitor, process, _Pid, _Reason}` if the handler stopped

In case of a successful reply, **don't forget to clean `Monitor` up**, which
can be done like this: `demonitor(Monitor, [flush])`.

See also `ask/2`, `ask/3` and `async_ask/2`.
""".
-endif.
-spec async_ask(Category, ActorId, Opts) -> {Tag, Monitor}
    when Category :: term(),
         ActorId :: term(),
         Opts :: [aequitas_category:ask_opt()],
         Tag :: reference(),
         Monitor :: reference().
async_ask(Category, ActorId, Opts) ->
    aequitas_category:async_ask(Category, ActorId, Opts).

-ifdef(E48).
-doc """
Tweak settings of work `Category`.

- `Category` must refer to a started category handler.
- `SettingOpts` must be a list of `t:aequitas_category:setting_opt/0` values.

Returns:

- `ok` in case of success
- `{error, Reason}` otherwise

See also `start/2` and `stop/1`.
""".
-endif.
-spec reconfigure(Category, SettingOpts)
        -> ok | {error, Reason}
    when Category :: term(),
         SettingOpts :: [aequitas_category:setting_opt()],
         Reason :: not_started | {invalid_setting_opt | invalid_setting_opts, term()}.
reconfigure(Category, SettingOpts) ->
    aequitas_category:update_settings(Category, SettingOpts).
