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

  defp handle_line(_line, graph) do
    graph
  end

  defp add_to(g, t, v) do 
    new_list = [v|g[t]]
    # %{g | t new_list}
  end
end
