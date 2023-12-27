defmodule LimitOffsetPaginator do
  @moduledoc """
  LimitOffsetPaginator adds one function to your Repo - paginate.
  To use this function, you need to make use of the `use` macro in your Repo module.
  It needs to receive the module itself as a `repo` parameter.
  Together with that, you can add your default limit, offset, and field by which the list will be ordered by.
  There are also four extra options:
  `enable_custom_field`, when set to true, enables the user to change the order by field by passing a `field` variable into the params. It is false by default;
  `enable_custom_limit`, when set to true, enables the user to change the limit in the query by passing a `limit` variable into the params. It is true by default;
  `enable_custom_offset`, when set to true, enables the user to change the offset in the query by passing an `offset` variable into the params. It is true by default;
  `show_count_by_default`, when set to true, makes it so that you must pass `show_count: false` in order to not receive the total counts, for more info on `show_count` see the doc on `paginate` from your Repo.

  ## Example

      defmodule MyApp.Repo do
        use Ecto.Repo,
          otp_app: :me_app,
          adapter: Ecto.Adapters.Postgres

        use LimitOffsetPaginator,
          repo: MyApp.Repo
      end
  """
  @moduledoc since: "1.1.0"
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
      `paginate_clean` or `pc` - if set to true, the function will return a list without the map.
      `paginate` - if set to false, no pagination occurs.
      `field` - when `enable_custom_field` is turned on, this parameter will be used for order by.
      `limit` or `l` - when `enable_custom_limit` is turned on, it will be used instead of the default limit set by default values.
      `page` or `p` - what page needs to be fetched.
      `filter` or `f` - receives a string in this format: "path.to.key:filter_value:filter_type|path.t...". Automatically converts `filter_value` to a required field type from the schema. `filter_type` can be one of "in", "inc", "includes" if you need an `ilike`; "ex", "exc", "excludes" for `not ilike`; "eq", "equal" for `==`; "dif", "different" for `!=`; "eql", "equallist" for `in`; "difl", "differentlist" for `not in` (for this and eql use , to separate multiple values); "mo", "more" for `>`; "le", "less" for `<`; "moq", "moreequal" for `>=`; "leq", "lessequal" for `<=`. Multiple filters can be added by dividing the string with a "|".
      `sort` or `s` - receives a string in this format: "path.to.key:sort_direction|path.t...". Like `filter`, the function can find fields present in maps in a `select` statement or main schema if `select` is nil. `sort_direction` can be "asc", "ascending" or "desc", "descending". Can sort many fields by dividing the string with a "|".
      `offset` - when `enable_custom_offset` is turned on, it will be used instead of the default offset set by default values.
      `search_string` or `search` or `ss` - when this value is passed, string fields from the `from` schema or the `select` statement if it is present will be compared to the passed `search_string`. This directly alters the query, but keeps previous where clauses.
      `show_count` or `sc` - when this is set to true, the function will have to fetch an unlimited query in order to count the total pages, which is why over-use of this parameter is discouraged.
      """
      @moduledoc since: "1.1.0"
      def paginate(query, params \\ %{})

      def paginate(query, %{"paginate_clean" => "true", "paginate" => "false"}),
        do: @repo.all(query)

      def paginate(query, %{"pc" => "true", "paginate" => "false"}),
        do: @repo.all(query)

      def paginate(query, %{"paginate_clean" => true, "paginate" => false}), do: @repo.all(query)
      def paginate(query, %{paginate_clean: true, paginate: false}), do: @repo.all(query)

      def paginate(query, %{"paginate" => "false"}), do: %{list: @repo.all(query)}
      def paginate(query, %{"paginate" => false}), do: %{list: @repo.all(query)}
      def paginate(query, %{paginate: false}), do: %{list: @repo.all(query)}

      def paginate(query, params), do: start(query, params)

      def start(query, params) do
        # Get default field to order by for limit and offset to work
        field = field_handler(params)
        # Get limit from params or if it's not there kee default
        limit = limit_handler(params)
        # Get offset from params by way of page or offset fields or if not from default
        offset = offset_handler(params)

        unlimited =
          query
          # add filters to where if the string is not empty
          |> filter_handler(params)
          # add where to all string fields if search_string is not empty
          |> search_handler(params)

        unlimited
        # DONE: add more ordering options
        # adds fields from select or schema to order_bys if string is not empty
        |> order_handler(params, field)
        # add limit to query
        |> limit(^limit)
        # add offset to query
        |> offset(^offset)
        # DONE: add complex filters, custom search fields taken from a select query
        # get list and if needed total counts
        |> count_handler(unlimited, params, field, limit, offset)
      end

      defp order_handler(query, %{sort: sort}, _) when is_bitstring(sort),
        do: order_handler(query, %{"sort" => sort}, "")

      defp order_handler(query, %{"s" => sort}, _) when is_bitstring(sort),
        do: order_handler(query, %{"sort" => sort}, "")

      defp order_handler(query, %{"sort" => sort}, _) when is_bitstring(sort) do
        # split string in case of multiple sorting fields
        String.split(sort, "|")
        # update query in a reduce
        |> Enum.reduce(query, fn sort, query ->
          # get path to field and direction of sorting
          [field, direction] = String.split(sort, ":")

          if is_nil(query.select) do
            # if no select add ordering to first element of query
            order_by(query, [n], {^direction_helper(direction), field(n, ^field)})
          else
            try do
              # get field expression from select
              expression = get_field(field, query)

              Map.put(query, :order_bys, [
                %Ecto.Query.QueryExpr{
                  expr: [{direction_helper(direction), expression}],
                  file: "paginator",
                  line: 100,
                  params: []
                }
                | query.order_bys
              ])
            rescue
              _ -> query
            end
          end
        end)
      end

      # if no order is chosen apply default ordering to keep a working pagination
      defp order_handler(query, _, field) do
        order_by(query, [n], {:asc, field(n, ^field)})
      end

      # convert strings and abbreviations to atoms
      defp direction_helper(direction) when direction in [:asc, :desc], do: direction

      defp direction_helper(direction) when direction in ["asc", "desc"],
        do: String.to_atom(direction)

      defp direction_helper("a"), do: :asc

      defp direction_helper("d"), do: :desc

      defp direction_helper(_), do: :asc

      # don't compile if disabled
      if Keyword.get(opts, :enable_custom_field, true) do
        # change default field for ordering
        defp field_handler(%{"field" => field}) when is_bitstring(field),
          do: String.to_existing_atom(field)

        defp field_handler(%{field: field}) when is_bitstring(field),
          do: String.to_existing_atom(field)

        defp field_handler(%{"field" => field}), do: field
        defp field_handler(%{field: field}), do: field
      end

      defp field_handler(_), do: @field

      # don't compile if disabled
      if Keyword.get(opts, :enable_custom_limit, true) do
        # set limits from a string or integer and disable negative or 0 values
        defp limit_handler(%{"limit" => limit}) when limit <= 0, do: 1
        defp limit_handler(%{"l" => limit}) when limit <= 0, do: 1
        defp limit_handler(%{limit: limit}) when limit <= 0, do: 1

        defp limit_handler(%{"limit" => limit}) when is_bitstring(limit),
          do: abs(String.to_integer(limit))

        defp limit_handler(%{"l" => limit}) when is_bitstring(limit),
          do: abs(String.to_integer(limit))

        defp limit_handler(%{"limit" => limit}), do: limit
        defp limit_handler(%{"l" => limit}), do: limit
        defp limit_handler(%{limit: limit}), do: limit
      end

      defp limit_handler(_), do: @limit

      # set offset from a page or custom offset, restrict negative values
      defp offset_handler(%{"page" => page}) when page <= 0, do: 0
      defp offset_handler(%{"p" => page}) when page <= 0, do: 0
      defp offset_handler(%{page: page}) when page <= 0, do: 0

      defp offset_handler(%{"page" => page, "limit" => limit})
           when is_bitstring(page) and is_bitstring(limit),
           do: abs(String.to_integer(limit)) * abs(String.to_integer(page) - 1)

      defp offset_handler(%{"p" => page, "l" => limit})
           when is_bitstring(page) and is_bitstring(limit),
           do: abs(String.to_integer(limit)) * abs(String.to_integer(page) - 1)

      defp offset_handler(%{"page" => page, "limit" => limit}), do: limit * (page - 1)
      defp offset_handler(%{"p" => page, "l" => limit}), do: limit * (page - 1)
      defp offset_handler(%{page: page, limit: limit}), do: limit * (page - 1)

      defp offset_handler(%{"page" => page}) when is_bitstring(page),
        do: @limit * abs(String.to_integer(page) - 1)

      defp offset_handler(%{"p" => page}) when is_bitstring(page),
        do: @limit * abs(String.to_integer(page) - 1)

      defp offset_handler(%{"page" => page}), do: @limit * (page - 1)
      defp offset_handler(%{"p" => page}), do: @limit * (page - 1)
      defp offset_handler(%{page: page}), do: @limit * (page - 1)

      if Keyword.get(opts, :enable_custom_offset, false) do
        defp offset_handler(%{"offset" => offset}) when is_bitstring(offset),
          do: abs(String.to_integer(offset))

        defp offset_handler(%{"offset" => offset}), do: offset
        defp offset_handler(%{offset: offset}), do: offset
      end

      defp offset_handler(_), do: @offset

      defp filter_handler(query, %{filter: filter}) when is_bitstring(filter),
        do: filter_handler(query, %{"filter" => filter})

      defp filter_handler(query, %{"f" => filter}) when is_bitstring(filter),
        do: filter_handler(query, %{"filter" => filter})

      defp filter_handler(query, %{"filter" => filter}) when is_bitstring(filter) do
        # split string in case of multiple sorting fields
        String.split(filter, "|")
        # update query in a reduce
        |> Enum.reduce(query, fn filter, query ->
          # get path to field, value for filtering and type of filter
          [field, value, t] = String.split(filter, ":")

          if is_nil(query.select) do
            # get type of field from main schema
            {expr, params} =
              elem(query.from.source, 1).__schema__(:type, field)
              # get correct expression for where insertion
              |> filter_case({{:., [], [{:&, [], [0]}, field]}, [], []}, value, t)

            Map.put(query, :wheres, [
              %Ecto.Query.BooleanExpr{
                expr: expr,
                file: "paginator",
                line: 212,
                op: :and,
                params: [params],
                subqueries: []
              }
              | query.wheres
            ])
          else
            try do
              # get field from select expression
              {expr, params} =
                get_field(field, query)
                |> case do
                  # if from main schema
                  {{:., [], [{:&, [], [0]}, field]}, [], []} = expression ->
                    # same as above
                    elem(query.from.source, 1).__schema__(:type, field)
                    # same as above
                    |> filter_case(expression, value, t)

                  # if from joins
                  {{:., [], [{:&, [], [number]}, field]}, [], []} = expression ->
                    # get type of field from join schema
                    elem(Enum.at(query.joins, number - 1).source, 1).__schema__(:type, field)
                    # same as above
                    |> filter_case(expression, value, t)
                end

              Map.put(query, :wheres, [
                %Ecto.Query.BooleanExpr{
                  expr: expr,
                  file: "paginator",
                  line: 237,
                  op: :and,
                  params: [params],
                  subqueries: []
                }
                | query.wheres
              ])
            rescue
              _ -> query
            end
          end
        end)
      end

      defp filter_handler(query, _), do: query

      defp filter_case(result, expression, value, t) do
        if t in ["eql", "equallist", "difl", "differentlist"] and not is_list(value) do
          value =
          String.split(value, ",", trim: true)
          |> Enum.map(fn value ->
            type_filter(result, value)
          end)
          filter_case(result, expression, value, t)
        else
          case result do
            :string ->
              filter_helper(expression, value, :string, t)

            type when type in [:id, :integer] ->
              filter_helper(expression, type_filter(result, value), type, t)

            :date_time ->
              filter_helper(expression, type_filter(result, value), :date_time, t)

            :naive_date_time ->
              filter_helper(expression, type_filter(result, value), :naive_date_time, t)

            :date ->
              filter_helper(expression, type_filter(result, value), :date, t)

            :time ->
              filter_helper(expression, type_filter(result, value), :time, t)

            :boolean ->
              filter_helper(expression, type_filter(result, value), :boolean, t)

            :float ->
              filter_helper(expression, type_filter(result, value), :float, t)

            type ->
              type.type
              |> filter_case(expression, type_filter(result, value), t)
          end
        end
      end

      defp type_filter(_result, value) when is_list(value) do
        value
      end

      defp type_filter(result, value) do
        case result do
          :string ->
            value

          type when type in [:id, :integer] ->
            String.to_integer(value)

          :date_time ->
            DateTime.from_iso8601(value)

          :naive_date_time ->
            NaiveDateTime.from_iso8601!(value)

          :date ->
            Date.from_iso8601!(value)

          :time ->
            Time.from_iso8601!(value)

          :boolean ->
            String.to_atom(value)

          :float ->
            String.to_float(value)

          type ->
            value
        end
      end

      defp filter_helper(expression, value, type, t) when t in ["in", "inc", "includes"],
        do: {{:ilike, [], [expression, {:^, [], [0]}]}, {"%#{value}%", type}}

      defp filter_helper(expression, value, type, t) when t in ["ex", "exc", "excludes"],
        do: {{:not, [], [{:ilike, [], [expression, {:^, [], [0]}]}]}, {"%#{value}%", type}}

      defp filter_helper(expression, value, type, t) when t in ["eq", "equal"],
        do: {{:==, [], [expression, {:^, [], [0]}]}, {value, type}}

      defp filter_helper(expression, value, type, t) when t in ["dif", "different"],
        do: {{:!=, [], [expression, {:^, [], [0]}]}, {value, type}}

      defp filter_helper(expression, value, type, t) when t in ["eql", "equallist"],
        do: {{:in, [], [expression, {:^, [], [0]}]}, {value, {:in, type}}}

      defp filter_helper(expression, value, type, t) when t in ["difl", "differentlist"],
        do: {{:not, [], [{:in, [], [expression, {:^, [], [0]}]}]}, {value, {:in, type}}}

      defp filter_helper(expression, value, type, t) when t in ["mo", "more"],
        do: {{:>, [], [expression, {:^, [], [0]}]}, {value, type}}

      defp filter_helper(expression, value, type, t) when t in ["le", "less"],
        do: {{:<, [], [expression, {:^, [], [0]}]}, {value, type}}

      defp filter_helper(expression, value, type, t) when t in ["moq", "moreequal"],
        do: {{:>=, [], [expression, {:^, [], [0]}]}, {value, type}}

      defp filter_helper(expression, value, type, t) when t in ["leq", "lessequal"],
        do: {{:<=, [], [expression, {:^, [], [0]}]}, {value, type}}

      defp search_handler(query, %{"search_string" => string})
           when is_bitstring(string),
           do: search_helper(query, string)

      defp search_handler(query, %{"search" => string})
           when is_bitstring(string),
           do: search_helper(query, string)

      defp search_handler(query, %{"ss" => string})
           when is_bitstring(string),
           do: search_helper(query, string)

      defp search_handler(query, %{search_string: string})
           when is_bitstring(string),
           do: search_helper(query, string)

      defp search_handler(query, %{search: string})
           when is_bitstring(string),
           do: search_helper(query, string)

      defp search_handler(query, _), do: query

      defp search_helper(query, string) do
        # get all string fields from select expression or from main schema if no select clause present
        {fields, params} = field_helper(query, string)

        case length(fields) do
          0 ->
            # if no string fields return unchanged
            query

          1 ->
            # if one insert into wheres as boolean
            put_bool(query, hd(fields), params)

          _ ->
            # if multiple, reverse the order
            [first | fields] = Enum.reverse(fields)

            # insert them into wheres with an OR statement between them
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
              line: 338,
              op: :and,
              params: params,
              subqueries: []
            }
            | query.wheres
          ])

      defp field_helper(%{from: %{source: {_, s}}, select: nil}, string),
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

      defp field_helper(
             %{select: %Ecto.Query.SelectExpr{expr: {:%{}, [], list}}} = query,
             string
           ),
           do:
             Enum.reduce(
               collect_fields(query, list),
               {[], []},
               &{[
                  {:ilike, [], [&1, {:^, [], [length(elem(&2, 0))]}]}
                  | elem(&2, 0)
                ], [{"%#{string}%", :string} | elem(&2, 1)]}
             )

      defp collect_fields(query, list, fields \\ []) do
        # recursively pull all string fields from select expression
        Enum.reduce(list, fields, fn
          {_, {:%{}, [], list}}, fields ->
            collect_fields(query, list, fields)

          {_, {:{}, [], list}}, fields ->
            collect_fields(query, list, fields)

          {_, {{:., [], [{:&, [], [0]}, field]}, [], []}}, fields ->
            collect_helper(query, field, fields, 0)

          {_, {{:., [], [{:&, [], [number]}, field]}, [], []}}, fields ->
            collect_helper(query, field, fields, number)

          {{:., [], [{:&, [], [0]}, field]}, [], []}, fields ->
            collect_helper(query, field, fields, 0)

          {{:., [], [{:&, [], [number]}, field]}, [], []}, fields ->
            collect_helper(query, field, fields, number)

          _, fields ->
            fields
        end)
      end

      # if from main schema
      defp collect_helper(%{from: %{source: {_, s}}} = query, field, fields, 0) do
        if s.__schema__(:type, field) == :string do
          [{{:., [], [{:&, [], [0]}, field]}, [], []} | fields]
        else
          fields
        end
      end

      # if from join schema
      defp collect_helper(query, field, fields, number) do
        try do
          if elem(Enum.at(query.joins, number - 1).source, 1).__schema__(:type, field) ==
               :string do
            [{{:., [], [{:&, [], [number]}, field]}, [], []} | fields]
          else
            fields
          end
        rescue
          _ -> fields
        end
      end

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

      defp count_handler(query, _unlimited, %{"pc" => true}, _field, _limit, _offset),
        do: @repo.all(query)

      defp count_handler(query, _unlimited, %{paginate_clean: true}, _field, _limit, _offset),
        do: @repo.all(query)

      defp count_handler(query, unlimited, %{"show_count" => "true"}, field, limit, offset) do
        count_helper(query, unlimited, field, limit, offset)
      end

      defp count_handler(query, unlimited, %{"sc" => "true"}, field, limit, offset) do
        count_helper(query, unlimited, field, limit, offset)
      end

      defp count_handler(query, unlimited, %{"show_count" => true}, field, limit, offset) do
        count_helper(query, unlimited, field, limit, offset)
      end

      defp count_handler(query, unlimited, %{show_count: true}, field, limit, offset) do
        count_helper(query, unlimited, field, limit, offset)
      end

      if Keyword.get(opts, :show_count_by_default, false) do
        defp count_handler(query, _unlimited, %{show_count: false}, _field, limit, offset),
          do: %{list: @repo.all(query), page: round(offset / limit) + 1}

        defp count_handler(query, unlimited, _, field, limit, offset) do
          count_helper(query, unlimited, field, limit, offset)
        end
      else
        defp count_handler(query, _unlimited, _, _field, limit, offset),
          do: %{list: @repo.all(query), page: round(offset / limit) + 1}
      end

      defp count_helper(query, unlimited, field, limit, offset) do
        # I can't use aggregate because some queries can have filters or join clauses that limit the amount of values
        unlimited = length(@repo.all(unlimited))

        %{
          list: @repo.all(query),
          page: floor(offset / limit) + 1,
          page_count: ceil(unlimited / limit),
          total_count: unlimited
        }
      end

      defp get_field(path, query) do
        String.split(path, ".")
        |> Enum.reduce(query.select.expr, fn
          field, {:%{}, [], list} ->
            Keyword.get(list, String.to_existing_atom(field))
        end)
      end
    end
  end
end
