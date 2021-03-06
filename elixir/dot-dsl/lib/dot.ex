defmodule Dot do
  defmacro graph(ast) do
    lines = case ast[:do] do
      {:__block__, _meta, l} -> l # a multiline block. Just unpack the lines
      x -> [x]                    # a single line block
    end

    sort_by_key = &(List.keysort(&1, 0))

    Enum.reduce(lines, %Graph{}, &handle_line/2)
    # sorting the lists only so they match the test data
    |> update(:attrs, sort_by_key)
    |> update(:edges, sort_by_key)
    |> update(:nodes, sort_by_key)
    |> Macro.escape()
  end

  defp assert_keyword_list(l, context) do
    if ! Keyword.keyword?(l), do:
      raise ArgumentError, message: "attributes '#{Macro.to_string(l)}' resulting from of #{Macro.to_string(context)} are not a keyword list"
  end

  defp handle_line({:graph, _meta, [vals]}, graph), do: add_list(graph, :attrs, vals)
  defp handle_line(line={:--, _m1, [ {a,_m2,nil}, {b,_m3,attrs} ]}, graph) do 
    attrs = case attrs do
      nil  -> [] # no attributes given
      [x|_] when is_tuple(x) -> attrs
      [a]  -> a  # attributes given as list
    end
    assert_keyword_list(attrs, line)
    add_single(graph, :edges, {a,b,attrs})
  end
  defp handle_line({n, _m, nil}, graph), do: add_single(graph, :nodes, {n, []})
  defp handle_line(line={n, _m, [attrs]}, graph) do
    assert_keyword_list(attrs, line)
    add_single(graph, :nodes, {n, attrs})
  end
  defp handle_line(l, _g), do: 
    raise ArgumentError, message: "'#{Macro.to_string(l)}' is not a valid dot expression"

  defp add_single(g, t, v), do: update(g, t, &( [v|&1] ))
  defp add_list(g, t, v), do: update(g, t, &( v ++ &1 ))

  defp update(g, t, f) do
    %{^t => old_list} = g
    new_list = f.(old_list)
    Map.put(g, t, new_list)
  end

end
