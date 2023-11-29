defmodule LimitOffsetPaginator.MixProject do
  use Mix.Project

  def project do
    [
      app: :limit_offset_paginator,
      version: "1.0.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      description: description(),
      package: package(),
      name: "LimitOffsetPaginator",
      source_url: "https://github.com/DDoubleDee/limit_offset_paginator",
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    []
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, "~> 0.14", only: :dev, runtime: false},
      {:ecto_sql, "~> 3.6"}
    ]
  end

  defp description() do
    "A simple paginator for when you don't need an unlimited scroll."
  end

  defp package() do
    [
      name: "limit_offset_paginator",
      # These are the default files included in the package
      files: ~w(lib .formatter.exs mix.exs README.md LICENSE),
      licenses: ["CC0-1.0"],
      links: %{"GitHub" => "https://github.com/DDoubleDee/limit_offset_paginator"}
    ]
  end
end
