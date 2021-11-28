defmodule LanguageList do
  @moduledoc """
  not sure about the scope of the assignment I opted to go for "no helper functions"
  """

  def new() do
    []
  end

  def add(list, language) do
    [language | list]
  end

  def remove([_x|xs]) do
    xs
  end

  def first([x|_xs]) do
    x
  end

  def count([]) do
    0
  end
  def count([_x|xs]) do
    1 + count(xs)
  end

  def exciting_list?([]) do
    false
  end
  def exciting_list?(["Elixir" | _xs]) do
    true
  end
  def exciting_list?([_x | xs]) do
    exciting_list?(xs)
  end
end
