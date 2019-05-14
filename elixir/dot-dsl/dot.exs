defmodule Graph do
  defstruct attrs: [], nodes: [], edges: []
end

defmodule Dot do
  defmacro graph(ast) do
    graph = command_list(ast)
            |> Enum.reduce(%Graph{}, &command/2)

    quote do
      %Graph{attrs: unquote(graph.attrs),
             nodes: unquote(graph.nodes),
             edges: unquote(graph.edges)
            }
    end
      

  end

  defp command_list({:__block__, [], list}) do
    list
  end

  defp command_list(single_command) do
    [single_command]
  end

  defp command([a, _, nil], acc) when is_atom(a) do
    %Graph{acc | nodes: [{a, []} | acc.nodes]}
  end

end
