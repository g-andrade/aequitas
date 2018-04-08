# aequitas

[![](https://img.shields.io/hexpm/v/aequitas.svg?style=flat)](https://hex.pm/packages/aequitas)
[![](https://travis-ci.org/g-andrade/aequitas.png?branch=master)](https://travis-ci.org/g-andrade/aequitas)

/ˈae̯.kʷi.taːs/

### <span id="aequitas_-_Fairness_Regulator">aequitas - Fairness Regulator</span>

`aequitas` is a library for Erlang/OTP and Elixir that allows you to
fairly distribute system capacity within a group of actors.

#### <span id="Example">Example</span>

There's a web server handling HTTP requests. We want to ensure
misbehaving IP addresses don't steal (too much) system capacity from
benevolent clients.

As such, before we handle each HTTP request we ask `aequitas` whether an
IP address can be granted work based on the statistical distribution of
recent work allocations.

We'll name our group `http_requests` (any atom is allowed.)

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

#### <span id="Work_Tracking">Work Tracking</span>

Each group is a single process that keeps a sliding window; this sliding
window tracks how many work units were attributed to each actor within
the last `N` accepted requests;. The maximum size of the sliding window,
`N`, is determined by constraints derived from the following group
settings:

  - `max_window_size`: Enforces a ceiling on how many acceptances we're
    allowed to track (defaults to 10000)
  - `max_window_duration`: Enforces a maximum age for acceptances
    (defaults to 5000ms)

Whenever one these limits is reached, old events will be dropped until
the window is below both limits again.

#### <span id="Rationale">Rationale</span>

Both the mean of and the standard deviation from work share are
calculated, in a rolling fashion, for accepted work events within the
window.

Based on these statistical measures, whenever there's a new request for
work execution we calculate the present
[z-score](https://en.wikipedia.org/wiki/Standard_score) of the actor
within the overall work performed. The z-score, or standard score, is
the signed number of standard deviations above the mean.

Assuming that work distribution among actors approximates the [normal
distribution](https://en.wikipedia.org/wiki/Normal_distribution), we can
apply the [68–95–99.7
rule](https://en.wikipedia.org/wiki/68%E2%80%9395%E2%80%9399.7_rule) to
filter out outliers.

Taking the above into account, we can reject the ~2.3% upper quantile
(`(100 - 95.45) / 2`) of work requests - the outliers that are eating up
most system capacity - by rejecting work requests whose actors' z-scores
go above `2`. That is: rejecting actors whose work share is placed more
than two standard deviations above the work share
mean.

#### <span id="Custom_Rejection_Threshold">Custom Rejection Threshold</span>

The default work rejection threshold - how many standard deviations
above the mean is the work share of an actor allowed to be - is `2`.

This can be customized by specifying the `max_zscore` option when asking
permission to execute. It can be any number or `infinity`.

Picking the web server example above, if we wanted to be stricter about
upper quantile outliers, we could lower `max_zscore` to `1` - which
would cause rejection of the top '~16%' of work share culprits.

``` erlang
case aequitas:ask(http_requests, IPAddress, [{max_zscore, 1}]) of
    accepted ->
        Reply = handle_request(...),
        {200, Reply};
    rejected ->
        % too many requests!
        {429, <<>>}
end.
```

#### <span id="Work_Weight">Work Weight</span>

Work can be weighted by specifying the `weight` option when asking
permission to execute. It must be a positive integer and the default
value is '1'.

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

*Generated by EDoc, Apr 8 2018, 20:13:21.*
