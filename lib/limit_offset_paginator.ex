defmodule LimitOffsetPaginator do
  @moduledoc """
  LimitOffsetPaginator adds one function to your Repo - paginate.
  To use this function, you need to make use of the `use` macro in your Repo module.
  It needs to receive the module itself as a `repo` parameter.
  Together with that, you can add your default limit, offset, and field by which the list will be ordered by.
  There are also two extra options:
  `enable_custom_field`, when set to true, enables the user to change the order by field by passing a `field` variable into the params. It is false by default;
  `enable_custom_limit`, when set to true, enables the user to change the limit in the query by passing a `limit` variable into the params. It is true by default;
  `enable_custom_offset`, when set to true, enables the user to change the offset in the query by passing an `offset` variable into the params. It is true by default;
  `show_count_by_default`, when set to true, makes it so that you must pass `show_count: false` in order to not receive the total counts, for more info on `show_count` see `paginate\2`.

  ## Exmple

      defmodule MyApp.Repo do
        use Ecto.Repo,
          otp_app: :me_app,
          adapter: Ecto.Adapters.Postgres

        use Paginator,
          repo: Nomenclature.Repo
      end
  """
  @moduledoc since: "1.0.0"
  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      import Ecto.Query
      @limit Keyword.get(opts, :limit, 10)
      @offset Keyword.get(opts, :offset, 0)
      @field Keyword.get(opts, :field, :id)
      @repo opts[:repo]

      @doc """
      Use this instead of Repo.all if you want to enable pagination.
      `params` is a map which can be (and frankly should be) passed directly from the controller.
      Parameter keys can be both strings and atoms, integer values can be strings and integers.
      What each parameter does:
      `paginate_clean` - if set to true, the function will return a list without the map.
      `paginate` - if set to false, no pagination occurs.
      `field` - when `enable_custom_field` is turned on, this parameter will be used for order by.
      `limit` - when `enable_custom_limit` is turned on, it will be used instead of the default limit set by default values.
      `page` - what page needs to be fetched.
      `offset` - when `enable_custom_offset` is turned on, it will be used instead of the default offset set by default values.
      `search_string` - when this value is passed, string fields from the `from` schema will be compared to the passed `search_string`. This directly alters the query, but keeps previous where clauses.
      `show_count` - when this is set to true, the function will have to fetch an unlimited query in order to count the total pages, which is why over-use of this parameter is discouraged.
      """
      @moduledoc since: "1.0.0"
      def paginate(query, params \\ %{})

      def paginate(query, %{"paginate_clean" => "true", "paginate" => "false"}),
        do: @repo.all(query)

      def paginate(query, %{"paginate_clean" => true, "paginate" => false}), do: @repo.all(query)
      def paginate(query, %{paginate_clean: true, paginate: false}), do: @repo.all(query)

      def paginate(query, %{"paginate" => "false"}), do: %{list: @repo.all(query)}
      def paginate(query, %{"paginate" => false}), do: %{list: @repo.all(query)}
      def paginate(query, %{paginate: false}), do: %{list: @repo.all(query)}

      def paginate(query, params), do: start(query, params)

      def start(query, params) do
        field = field_handler(params)
        limit = limit_handler(params)
        offset = offset_handler(params)

        query
        # TODO: add more ordering options
        |> order_by([n], asc: field(n, ^field))
        |> limit(^limit)
        |> offset(^offset)
        # TODO: add complex filters, custom search fields taken from a select query
        |> search_handler(params)
        |> count_handler(query, params, field, limit, offset)
      end

      if Keyword.get(opts, :enable_custom_field, false) do
        defp field_handler(%{"field" => field}) when is_bitstring(field),
          do: String.to_existing_atom(field)

        defp field_handler(%{field: field}) when is_bitstring(field),
          do: String.to_existing_atom(field)

        defp field_handler(%{"field" => field}), do: field
        defp field_handler(%{field: field}), do: field
      end

      defp field_handler(_), do: @field

      if Keyword.get(opts, :enable_custom_limit, true) do
        defp limit_handler(%{"limit" => limit}) when limit <= 0, do: 1
        defp limit_handler(%{limit: limit}) when limit <= 0, do: 1

        defp limit_handler(%{"limit" => limit}) when is_bitstring(limit),
          do: abs(String.to_integer(limit))

        defp limit_handler(%{"limit" => limit}), do: limit
        defp limit_handler(%{limit: limit}), do: limit
      end

      defp limit_handler(_), do: @limit

      defp offset_handler(%{"page" => page}) when page <= 0, do: 0
      defp offset_handler(%{page: page}) when page <= 0, do: 0

      defp offset_handler(%{"page" => page, "limit" => limit})
           when is_bitstring(page) and is_bitstring(limit),
           do: abs(String.to_integer(limit)) * abs(String.to_integer(page) - 1)

      defp offset_handler(%{"page" => page, "limit" => limit}), do: limit * (page - 1)
      defp offset_handler(%{page: page, limit: limit}), do: limit * (page - 1)

      defp offset_handler(%{"page" => page}) when is_bitstring(page),
        do: @limit * abs(String.to_integer(page) - 1)

      defp offset_handler(%{"page" => page}), do: @limit * (page - 1)
      defp offset_handler(%{page: page}), do: @limit * (page - 1)

      if Keyword.get(opts, :enable_custom_field, false) do
        defp offset_handler(%{"offset" => offset}) when is_bitstring(offset),
          do: abs(String.to_integer(offset))

        defp offset_handler(%{"offset" => offset}), do: offset
        defp offset_handler(%{offset: offset}), do: offset
      end

      defp offset_handler(_), do: @offset

      defp search_handler(query, %{"search_string" => string})
           when is_bitstring(string),
           do: search_helper(query, string)

      defp search_handler(query, %{search_string: string})
           when is_bitstring(string),
           do: search_helper(query, string)

      defp search_handler(query, _), do: query

      defp search_helper(query, string) do
        {fields, params} = field_helper(query, string)

        case length(fields) do
          0 ->
            query

          1 ->
            put_bool(query, hd(fields), params)

          _ ->
            [first | fields] = Enum.reverse(fields)

            put_bool(
              query,
              Enum.reduce(
                fields,
                first,
                &{:or, [],
                 [
                   &2,
                   &1
                 ]}
              ),
              params
            )
        end
      end

      defp put_bool(query, fields, params),
        do:
          Map.put(query, :wheres, [
            %Ecto.Query.BooleanExpr{
              expr: fields,
              file: "paginator",
              line: 109,
              op: :and,
              params: params,
              subqueries: []
            }
            | query.wheres
          ])

      defp field_helper(%{from: %{source: {_, s}}}, string),
        do:
          Enum.reduce(
            s.__schema__(:fields),
            {[], []},
            &if(s.__schema__(:type, &1) == :string,
              do:
                {[
                   {:ilike, [],
                    [{{:., [], [{:&, [], [0]}, &1]}, [], []}, {:^, [], [length(elem(&2, 0))]}]}
                   | elem(&2, 0)
                 ], [{"%#{string}%", :string} | elem(&2, 1)]},
              else: {elem(&2, 0), elem(&2, 1)}
            )
          )

      defp count_handler(
             query,
             _unlimited,
             %{"paginate_clean" => "true"},
             _field,
             _limit,
             _offset
           ),
           do: @repo.all(query)

      defp count_handler(query, _unlimited, %{"paginate_clean" => true}, _field, _limit, _offset),
        do: @repo.all(query)

      defp count_handler(query, _unlimited, %{paginate_clean: true}, _field, _limit, _offset),
        do: @repo.all(query)

      defp count_handler(query, unlimited, %{"show_count" => "true"}, field, limit, offset) do
        # I can't use aggregate because some queries can have filters or join clauses that limit the amount of values
        unlimited = length(@repo.all(unlimited))

        %{
          list: @repo.all(query),
          page: round(offset / limit) + 1,
          page_count: round(unlimited / limit) + 1,
          total_count: unlimited
        }
      end

      defp count_handler(query, unlimited, %{"show_count" => true}, field, limit, offset) do
        unlimited = length(@repo.all(unlimited))

        %{
          list: @repo.all(query),
          page: round(offset / limit) + 1,
          page_count: round(unlimited / limit) + 1,
          total_count: unlimited
        }
      end

      defp count_handler(query, unlimited, %{show_count: true}, field, limit, offset) do
        unlimited = length(@repo.all(unlimited))

        %{
          list: @repo.all(query),
          page: round(offset / limit) + 1,
          page_count: round(unlimited / limit) + 1,
          total_count: unlimited
        }
      end

      if Keyword.get(opts, :show_count_by_default, false) do
        defp count_handler(query, _unlimited, %{show_count: false}, _field, limit, offset),
          do: %{list: @repo.all(query), page: round(offset / limit) + 1}

        defp count_handler(query, unlimited, _, field, limit, offset) do
          unlimited = length(@repo.all(unlimited))

          %{
            list: @repo.all(query),
            page: round(offset / limit) + 1,
            page_count: round(unlimited / limit) + 1,
            total_count: unlimited
          }
        end
      else
        defp count_handler(query, _unlimited, _, _field, limit, offset),
          do: %{list: @repo.all(query), page: round(offset / limit) + 1}
      end
    end
  end
end
