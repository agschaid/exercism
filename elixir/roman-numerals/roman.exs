defmodule Roman do

  # a keyword list from a roman numeral (atom) to a tuple, holding its integer
  # value and its allowed subtractor.
  # TODO is there a way to propperly document module attributes? @doc?
  @numerals_descending [
    M: {1000, :C},
    D: { 500, :C},
    C: { 100, :X},
    L: {  50, :X},
    X: {  10, :I},
    V: {   5, :I},
    I: {   1, nil}
  ]
  

  @doc """
  Convert the number to a roman number.
  """
  @spec numerals(pos_integer) :: String.t()
  def numerals(number) do
    numerals(number, @numerals_descending, "")
  end

  defp numerals(0, _, acc) do acc end

  defp numerals(number, numerals, acc) do
    [{numeral, {value, subtractor}} | numerals_tail] = numerals

    diminished_value = 
      case numerals[subtractor] do
        nil -> nil  # should only happen with numeral :I (so the diminished
                    # value is no going to be needed)
        {subtractor_value, _} -> value - subtractor_value
      end

    cond do
      number >= value -> 
        numerals(number-value, 
                 numerals, 
                 append(acc, numeral))

      number >= diminished_value -> 
        numerals(number-diminished_value, 
                 numerals_tail,
                 append(acc, subtractor, numeral))

      true -> numerals(number, numerals_tail, acc)
    end

  end

  defp append(acc, n) do acc <> Atom.to_string(n) end
  defp append(acc, n1, n2) do acc <> Atom.to_string(n1) <> Atom.to_string(n2) end

end
