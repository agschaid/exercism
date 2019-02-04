defmodule Words do
  @doc """
  Count the number of words in the sentence.

  Words are compared case-insensitively.
  """
  @spec count(String.t()) :: map
  def count(sentence) do

    Enum.reduce(word_list(sentence), %{}, &add_to_count/2)

  end

  defp add_to_count(word, dict) do
    Map.update(dict, word, 1, &(&1 + 1))
  end

  defp word_list(sentence) do
    sentence
    |> String.split(~r/[^[:alnum:]-]+/u) 
    |> Enum.map(fn x -> x |> String.trim() |> String.downcase() end) 
    |> Enum.filter(fn x -> x != "" end)
  end

end
