%% Copyright (c) 2018 Guilherme Andrade
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

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export(
   [ask/2,
    ask/3,
    configure/2
   ]).

-ignore_xref(
   [ask/1,
    ask/2,
    ask/3,
    configure/2
   ]).

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

%% @doc Like `:ask/3' but with defaults options
%%
%% <ul>
%% <li>`Group' must be an atom.</li>
%% <li>`Id' must be a term.</li>
%% </ul>
%%
%% Returns:
%% <ul>
%% <li>`accepted' if work execution was granted</li>
%% <li>`rejected' if work execution was denied</li>
%% </ul>
%% @see ask/1
%% @see ask/3
-spec ask(Group, Id)
        -> accepted | rejected
    when Group :: atom(),
         Id :: term().
ask(Group, Id) ->
    ask(Group, Id, []).

%% @doc Request permission to perform work identified by `Id' within `Group'
%%
%% <ul>
%% <li>`Group' must be an atom.</li>
%% <li>`Id' must be a term.</li>
%% <li>`Opts' must be a list of `aequitas_group:ask_opt()' values</li>
%% </ul>
%%
%% Returns:
%% <ul>
%% <li>`accepted' if work execution was granted</li>
%% <li>`rejected' if work execution was denied</li>
%% </ul>
%% @see ask/2
%% @see ask/3
-spec ask(Group, Id, Opts)
        -> accepted | rejected
    when Group :: atom(),
         Id :: term(),
         Opts :: [aequitas_group:ask_opt()].
ask(Group, Id, Opts) ->
    aequitas_group:ask(Group, Id, Opts).

%% @doc Tweak settings of work `Group'
%%
%% <ul>
%% <li>`Group' must be an atom.</li>
%% <li>`SettingOpts' must be a list of `aequitas_group:setting_opt()' values</li>
%% </ul>
%%
%% Returns:
%% <ul>
%% <li>`ok' in case of success</li>
%% <li>`{error, term()}' otherwise</li>
%% </ul>
-spec configure(Group, SettingOpts)
        -> ok | {error, bad_settings}
    when Group :: atom(),
         SettingOpts :: [aequitas_group:setting_opt()].
configure(Group, SettingOpts) ->
    aequitas_group:set_settings(Group, SettingOpts).
