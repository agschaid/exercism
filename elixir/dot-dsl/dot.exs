defmodule Graph do
  defstruct attrs: [], nodes: [], edges: []
end

defmodule Dot do
  defmacro graph(ast) do
    lines = case ast[:do] do
      {:__block__, _meta, l} -> l
      x -> [x]
    end

    g = Enum.reduce(lines, %Graph{}, &handle_line/2)
    Macro.escape(g)
  end

  defp handle_line({:graph, _meta, [vals]}, graph), do: add_list(graph, :attrs, vals)

  defp handle_line(_line, graph) do
    graph
  end

  defp add_single(g, t, v), do: update(g, t, &( [v|&1] ))
  defp add_list(g, t, v), do: update(g, t, &( v ++ &1 ))

  defp update(g, t, f) do
    %{^t => old_list} = g
    new_list = f.(old_list)
    Map.put(g, t, new_list)
  end

end
