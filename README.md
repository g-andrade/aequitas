# aequitas

[![](https://img.shields.io/hexpm/v/aequitas.svg?style=flat)](https://hex.pm/packages/aequitas)
[![](https://travis-ci.org/g-andrade/aequitas.png?branch=master)](https://travis-ci.org/g-andrade/aequitas)

### <span id="aequitas_-_Fairness_Regulator">aequitas - Fairness Regulator</span>

`aequitas` is a library for Erlang/OTP and Elixir that allows you to
fairly distribute system capacity to different identifiers within a
group.

#### <span id="Example">Example</span>

Let's say we've got a web server handling HTTP requests. We want to
ensure misbehaving IP addresses don't steal (too much) system capacity
from benevolent clients.

As such, before we handle each HTTP request we ask `aequitas` whether an
IP address can be granted work based on the statistical distribution of
recent work allocations.

We'll name our group `http_requests` (any atom will do.)

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

#### <span id="Event_Tracking">Event Tracking</span>

A group keeps track of how many work units were attributed to each
identifier within the last `N` events; the size of the event window,
`N`, is determined by constraints derived from from the group settings:

  - `max_window_size`: Enforces a ceiling on how many events we're
    allowed to track (defaults to 10000)
  - `max_window_duration`: Enforces a maximum age for events (defaults
    to 5000ms)

Whichever of these limits is reached first will trigger a drop of old
events until it is no longer so.

#### <span id="Rationale">Rationale</span>

Both the mean and the standard deviation of work share, per identifier -
for events within the event window - are calculated in a rolling
fashion.

Based on these statistical measures, whenever there's a new request for
work execution we calculate what would the [standard
score](https://en.wikipedia.org/wiki/Standard_score) of the identifier
become if the work were allowed to go through. The standard score is the
signed number of standard deviations above the mean.

Assuming that work distribution among identifiers approximates the
[normal
distribution](https://en.wikipedia.org/wiki/Normal_distribution), we can
apply the [68–95–99.7
rule](https://en.wikipedia.org/wiki/68%E2%80%9395%E2%80%9399.7_rule)
when there's enough samples.

Taking the above into account, we can reject the `~2.275%` higher
quantile (`(100 - 95.45) / 2`) of work requests - the outliers that are
eating up most work - by rejecting work requests whose identifiers would
end up with a standard score higher than `2`. That is: identifiers whose
work share would be placed more than two standard deviations from the
work share mean.

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

*Generated by EDoc, Apr 8 2018, 19:07:58.*
