defmodule AequitasBench.Mixfile do
  use Mix.Project

  def project do
    [
      app: :aequitas_bench,
      version: "0.1.0",
      elixir: "~> 1.5",
      start_permanent: Mix.env == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: []  # [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:benchee, "0.13.0"},
      {:aequitas, path: ".."},
      {:benchee_html, "~> 0.5"}
    ]
  end
end
