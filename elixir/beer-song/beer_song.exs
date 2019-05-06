defmodule BeerSong do
  @doc """
  Get a single verse of the beer song
  """
  @spec verse(integer) :: String.t()
  def verse(number) do
    """
#{first_line(number)}
#{second_line(number)}
"""
  end

  @doc """
  Get the entire beer song for a given range of numbers of bottles.
  """
  @spec lyrics(Range.t()) :: String.t()
  def lyrics(range \\ 99..0) do
    verses = for n <- range, do: verse(n)
    Enum.join(verses, "\n")
  end

  @doc """
  Get the first line of a verse of the beer song.
  """
  @spec first_line(integer) :: String.t()
  defp first_line(verse) do
    beer_count = amount_of_beer(verse)
    "#{String.capitalize(beer_count)} of beer on the wall, #{beer_count} of beer."
  end

  @doc """
  Get the second line of a verse of the beer song.
  """
  @spec second_line(integer) :: String.t()
  def second_line(0) do
      "Go to the store and buy some more, 99 bottles of beer on the wall."
  end

  def second_line(verse) do
      what_to_take = if verse == 1 do "it" else "one" end
      "Take #{what_to_take} down and pass it around, #{amount_of_beer(verse-1)} of beer on the wall."
  end

  @doc """
  Get the text representation of an amount of beer as used in the beer song.
  """
  @spec amount_of_beer(integer) :: String.t()
  defp amount_of_beer(i) do
    case i do
      0 -> "no more bottles"
      1 -> "1 bottle"
      _ -> Integer.to_string(i) <> " bottles"  
    end
  end

end
