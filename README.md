# aequitas

[![](https://img.shields.io/hexpm/v/aequitas.svg?style=flat)](https://hex.pm/packages/aequitas)
[![](https://travis-ci.org/g-andrade/aequitas.png?branch=master)](https://travis-ci.org/g-andrade/aequitas)

/ˈae̯.kʷi.taːs/

### aequitas - Fair regulator and rate limiter

`aequitas` is a fairness regulator library for Erlang/OTP and Elixir,
with optional rate limiting capabilities.

It intends on allowing fair access to limited external resources, like
databases and web services.

It does so by attempting to detect outliers amongst ordinary workloads
distributions.

#### Example

There's a web server handling HTTP requests. We want to ensure
misbehaving IP addresses don't steal (too much) system capacity from
benevolent clients.

As such, before we handle each HTTP request we ask `aequitas` whether an
IP address can be granted work. We'll get a reply that's based on the
statistical distribution of recent work allocations.

We'll name our category `http_requests`. Categories and actors can be
represented by any term, and the necessary processes are created on
demand.

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

Some more definitions of the `:ask` function exist:

  - `:ask(Category, ActorId, Opts)` - for
    [tweaking](#work-request-tweaking)
  - `:async_ask(Category, ActorId)` - analogous to `:ask/2` but replies
    asynchronously
  - `:async_ask(Category, ActorId, Opts)` - analogous to `:ask/3` but
    replies asynchronously

#### Documentation and Reference

Documentation and reference are hosted on
[HexDocs](https://hexdocs.pm/aequitas/).

#### Tested setup

  - Erlang/OTP 18.0 or higher
  - rebar3

#### Category Tweaking

The following settings can be used to tweak categories, both through
[static](#static-configuration) and [dynamic
configuration](#dynamic-configuration).

  - `max_window_size`: Enforces a ceiling on how many of the last work
    acceptances will be [tracked](#work-tracking) (default is 10000;
    must be a positive integer or infinity)
  - `max_window_duration`: Enforces an eventual expiration of tracked
    work acceptances (default is `{seconds,5}`; must be of type
    `aequitas_time_interval:t()` or infinity - see [type
    reference](#documentation-and-reference))
  - `iqr_factor`: IQR factor used to [detect outlying
    actors](#outlier-detection) among the workload distribution (default
    is 1.5; must be a non-negative number)
  - `max_collective_rate`: Enforces a [collective rate
    limit](#collective-rate-limit), per second, on work acceptances
    (default is infinity; must be a non-negative integer)

#### Work Request Tweaking

The following settings can be used to tweak individual work requests,
i.e. in calls to either the `:ask/3` or `:async_ask/3` functions.

  - `weight`: [Relative weight](#work-weighing) of the work request
    (default is 1; must be a positive integer)
  - `iqr_factor`: Overrides the `iqr_factor` configured globally for the
    category; its meaning remains the same but it only applies to the
    work request that overrode it.
  - `return_stats`: Returns the work stats used to detect outliers
    together with acceptance status (see [API
    reference](#documentation-and-reference))

#### Collective Rate Limiting

Collective rate limiting can be enabled in order to limit the total
amount of work performed, per second, within a category.

Once the configured limit is reached, its enforcement should tend to
homogenize the number of accepted work requests per actor, even in case
of very unbalanced workloads - within the reach of the configured
outlier detection, that is.

If the number of distinct actors tracked by the window is rather larger
than the rate limit, and if `iqr_factor` is set to a very strict value,
it should ultimately result in every actor performing at most one
request within the period covered by the sliding window.

This contrasts with impartial rate limiting which is the scope of many
other libraries, whereby the acception/rejection ratios per actor tend
to be the same, independently of how much work each actor is effectively
performing.

[Work weighing](#work-weighing) is taken into account when rate
limiting.

#### Outlier Detection

The outlier detection algorithm is based on [John Tukey's
fences](http://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_SummarizingData/BS704_SummarizingData7.html).

The [IQR](https://en.wikipedia.org/wiki/Interquartile_range)
(interquartile range) of the work shares per actor, encompassed by the
work tracker, is updated continuously (although asynchronously). Based
on this statistical measure, whenever there's a new request for work
execution we determine whether the actor's present work share is an
outlier to the right.

The default threshold of outlier categorization is set to `Q3 + (1.5 x
IQR)`, with IQR being `Q3 - Q1` and Q1 and Q3 being the median values of
the lower and higher halves of the samples, respectively.

The IQR factor can be customized, both per
[category](#category-tweaking) and per [work
request](#work-request-tweaking), using the `iqr_factor` setting, as
detailed previously.

Lower values will result in greater intolerance of high work-share
outliers; higher values, the opposite.

The reason for picking `1.5` as the default is rather down to convention
and might not be appropriate for your specific workloads. If necessary:
measure, adjust, repeat.

It's possible you'll conclude the IQR technique is not adequate to solve
your problem; a score of other approaches exist, with many of them being
computationally (much) more expensive, - it's a trade off between
correctness and availability.

#### Work Weighing

Work requests can be weighted by specifying the `weight` option when
asking permission to execute. It must be a positive integer; the default
value is `1`.

Picking the web server example above, if we were to weight our requests
based on their body size, it could become something similar to this:

``` erlang
ReqBodySize = req_body_size(...),
WorkWeight = 1 + ReqBodySize,
case aequitas:ask(http_requests, IPAddress, [{weight, Weight}]) of
% [...]
end.
```

This way, the work share of an IP address performing a few large
requests could of similar magnitude to the work share of an IP address
performing many small requests.

#### Work Tracking

Each category is backed by a process that keeps a sliding window; this
sliding window helps keep track of how many work units were attributed
to each actor within the last `N` accepted requests;

The size of the sliding window, `N`, is determined by constraints
derived from [category settings](#category-tweaking). Whenever it gets
excessively large, old events will be dropped until it is no longer so.

#### Static Configuration

The configuration of foreknown categories can be tweaked in `app.config`
/ `sys.config` by declaring the overriden settings, per category, in the
following fashion:

``` erlang
% ...
 {aequitas,
  [{{category, http_requests},
    [{max_window_duration, {seconds,10}} % Override default 5s to 10s
    ]},

   {{category, rare_ftp_requests},
    [{max_window_size, 100} % Only track up to the last 100 acceptances
    ]}
  ]}
% ...
```

Proper app. configuration reloads that result in calls to the
application's internal `:config_change/3` callback will trigger a reload
of settings in each of the relevant category processes.

#### Dynamic Configuration

The configuration of specific categories can be tweaked in runtime by
calling `aequitas:configure/2`, e.g.:

``` erlang
ok = aequitas:configure(http_requests,
                        [{max_window_duration, {seconds,10}}]).
```

(Re)configuration performed this way will override the static category
configuration present in `app.config`, if any.

It will also trigger a reload of settings in the relevant category
process.

#### License

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

*Generated by EDoc, May 6 2018, 19:23:43.*
