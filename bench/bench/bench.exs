## Copyright (c) 2018 Guilherme Andrade
##
## Permission is hereby granted, free of charge, to any person obtaining a
## copy  of this software and associated documentation files (the "Software"),
## to deal in the Software without restriction, including without limitation
## the rights to use, copy, modify, merge, publish, distribute, sublicense,
## and/or sell copies of the Software, and to permit persons to whom the
## Software is furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
## FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
## DEALINGS IN THE SOFTWARE.

## Originally based on Dimagog's PersistentVector benchmarking code

only =
  case System.argv do
    ["--only", x | rest] -> System.argv(rest); String.upcase(x)
    _ -> nil
  end

full  = Enum.member?(System.argv, "full")
quick = Enum.member?(System.argv, "quick")
quickest = Enum.member?(System.argv, "quickest")
generate_html = Enum.member?(System.argv, "html")

defmodule Runner do
  @datetime DateTime.to_string(DateTime.utc_now())
  @generate_html generate_html

  @print_opts [benchmarking: false, fast_warning: false, configuration: false]

  @opts  [
    warmup: 2,
    time: (if quickest, do: 0.1, else: (if quick, do: 3, else: 10)),
    print: @print_opts
  ]

  IO.puts :stderr, "Time per test: #{Keyword.get(@opts, :time)} sec"

  @only only

  def suffixed_atom(atom, int) do
    :erlang.list_to_atom(
      :erlang.atom_to_list(atom)
      ++ '.'
      ++ :erlang.integer_to_list(int))
  end

  def bench(name, tests, inputs) do
    if should_run?(@only, name) do
      :lists.foreach(
        fn parallel ->
          overridden_name = "#{name} (#{parallel} workers)"
          bench(overridden_name, tests, inputs, parallel)
        end,
        parallel_values())
    end
  end

  defp bench(name, tests, inputs, parallel) do
    IO.puts ""
    IO.puts "#"
    IO.puts "# #{name}"
    IO.puts "#"

    opts =
      if @generate_html do
        report_path = :erlang.iolist_to_binary(
          :io_lib.format("html/~s/~ts/~ts.html", [@datetime, name, name]))

        @opts ++ [formatter_options: [html: [file: report_path]],
                  formatters: [
                    &Benchee.Formatters.HTML.output/1,
                    &Benchee.Formatters.Console.output/1
                  ]]
      else
        @opts
      end

    opts = opts ++ [inputs: inputs, parallel: parallel]
    :ok = :application.stop(:aequitas)
    :ok = :application.start(:aequitas)
    Benchee.run(tests, opts)
  end

  defp parallel_values() do
    :lists.usort(
      [1, 2]
      #:lists.seq(1, :erlang.system_info(:schedulers_online))
      #++ :lists.seq(1, 8)
      #++ [10, 50, 100, 500, 1000, 5000, 10000]
    )
  end

  defp should_run?(nil, _), do: true
  defp should_run?(only, this), do: only == String.upcase(this)
end

inputs =
  %{
    "1_actor": 1,
    "10_actors": 10,
    "100_actors": 100,
    "1000_actors": 1000,
    "10000_actors": 10000,
    "100000_actors": 100000,
   }

IO.puts :stderr, "Using #{Enum.count(inputs)} inputs"

Runner.bench(
  "Regular Calls",
  %{"ask" =>
      fn nr_of_actors ->
        :aequitas.ask(Runner.suffixed_atom(RegularCalls, nr_of_actors),
                      Integer.mod(:erlang.monotonic_time(), nr_of_actors))
      end
   },
  inputs)
