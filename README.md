# aequitas

[![](https://img.shields.io/hexpm/v/aequitas.svg?style=flat)](https://hex.pm/packages/aequitas)
[![](https://travis-ci.org/g-andrade/aequitas.png?branch=master)](https://travis-ci.org/g-andrade/aequitas)

/ˈae̯.kʷi.taːs/

### <span id="aequitas_-_Fairness_Regulator">aequitas - Fairness Regulator</span>

`aequitas` is a fairness regulator library for Erlang/OTP and Elixir.

It intends on allowing fair access to limited external resources, like
databases and web services.

It does so by attempting to detect outliers amongst ordinary workloads
distributions.

#### <span id="Example">Example</span>

There's a web server handling HTTP requests. We want to ensure
misbehaving IP addresses don't steal (too much) system capacity from
benevolent clients.

As such, before we handle each HTTP request we ask `aequitas` whether an
IP address can be granted work. We'll get a reply that's based on the
statistical distribution of recent work allocations.

We'll name our category `http_requests`; any atom is allowed, and the
necessary processes are created on demand.

``` erlang
case aequitas:ask(http_requests, IPAddress) of
    accepted ->
        Reply = handle_request(...),
        {200, Reply};
    rejected ->
        % too many requests!
        {429, <<>>}
end.
```

Any term can represent a valid actor identifier.

#### <span id="Requirements">Requirements</span>

  - Erlang/OTP 18.0 or higher
  - rebar3

#### <span id="Documentation">Documentation</span>

Documentation is hosted on [HexDocs](https://hexdocs.pm/aequitas/).

#### <span id="Details">Details</span>

##### <span id="Work_Tracking">Work Tracking</span>

Each category is backed by a process that keeps a sliding window; this
sliding window helps keep track of how many work units were attributed
to each actor within the last `N` accepted requests; The maximum size of
the sliding window, `N`, is determined by constraints derived from the
following category settings:

  - `max_window_size`: Enforces a ceiling on how many acceptances we're
    allowed to track (defaults to 10000)
  - `max_window_duration`: Enforces a maximum age for tracked
    acceptances (defaults to 5000ms)

The first limit prevents excessive memory usage; the second, the
consideration of work events that are no longer relevant.

Whenever one of these limits is reached, old events will be dropped
until it is no longer so.

##### <span id="Rejection_Threshold">Rejection Threshold</span>

The [IQR](https://en.wikipedia.org/wiki/Interquartile_range)
(Interquartile range) of the work shares per actor - within the sliding
window - is calculated continuously (although asynchronously). Based on
this statistical measure, whenever there's a new request for work
execution we determine whether the actor's present work share is an
outlier to the right.

By default, the threshold of outlier categorization is set to `Q3 + (1.5
x IQR)`, with IQR being `Q3 - Q1`, and Q1 and Q3 being the median values
of the lower and higher halves of the samples, respectively.

The maximum IQR multiplier can be customized; picking the web server
example above, if we wanted to be stricter about upper quartile outliers
we could adjust `iqr_multiplier` from its default value of `1.5` down to
`0.5`.

``` erlang
case aequitas:ask(http_requests, IPAddress, [{iqr_multiplier, 0.5}]) of
    accepted ->
        Reply = handle_request(...),
        {200, Reply};
    rejected ->
        % too many requests!
        {429, <<>>}
end.
```

##### <span id="Work_Weight">Work Weight</span>

Work can be weighted by specifying the `weight` option when asking
permission to execute. It must be a positive integer; the default value
is `1`.

Picking the web server example above, if we were to weight our requests
using their body size, it would become something like this:

``` erlang
ReqBodySize = req_body_size(...),
WorkWeight = 1 + ReqBodySize,
case aequitas:ask(http_requests, IPAddress, [{weight, Weight}]) of
    accepted ->
        Reply = handle_request(...),
        {200, Reply};
    rejected ->
        % too many requests!
        {429, <<>>}
end.
```

This way, the work share of an IP address performing a few large
requests could of similar magnitude to the work share of an IP address
performing many small requests.

##### <span id="Rate_Limiting">Rate Limiting</span>

Total work performed under a category can be rate limited by use of the
`max_global_rate` option when asking permission to execute. It must be
either a non-negative integer or the atom `infinity` (the default.)

This rate limiting is independent of fairness enforcement but can be
combined with it in order to achieve *fair* rate limiting.

Once more picking the web server example above, if we were to rate limit
our requests to 1000 per second, it would become something like this:

``` erlang
ReqBodySize = req_body_size(...),
case aequitas:ask(http_requests, IPAddress, [{max_global_rate, 100}]) of
    accepted ->
        Reply = handle_request(...),
        {200, Reply};
    rejected ->
        % too many requests!
        {429, <<>>}
end.
```

##### <span id="Static_Configuration">Static Configuration</span>

The configuration of foreknown categories can be tweaked in `app.config`
/ `sys.config` by declaring the overriden settings, per category, in the
following fashion:

``` erlang
% ...
 {aequitas,
  [{{category, http_requests},
    [{max_window_duration, 10000} % Override default 5s to 10s
    ]},

   {{category, rare_ftp_requests},
    [{max_window_size, 100} % Track up to the last 100 acceptances
    ]}
  ]}
% ...
```

Proper app. configuration reloads that result in calls to the
application's internal `:config_change/3` callback will trigger a reload
of settings in each of the relevant category processes.

##### <span id="Dynamic_Configuration">Dynamic Configuration</span>

The configuration of specific categories can be tweaked in runtime by
calling `aequitas:configure/2`, e.g.:

``` erlang
ok = aequitas:configure(http_requests,
                        [{max_window_duration, 10000}]).
```

(Re)configuration performed this way will override the static category
configuration present in `app.config`, if any.

It will also trigger a reload of settings in the relevant category
process.

#### <span id="License">License</span>

MIT License

Copyright (c) 2018 Guilherme Andrade

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

-----

*Generated by EDoc, May 2 2018, 02:26:24.*
