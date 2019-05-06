defmodule BeerSong do

  @doc """
  Get the entire beer song for a given range of numbers of bottles.
  """
  @spec lyrics(Range.t()) :: String.t()
  def lyrics(range \\ 99..0) do
    verses = for n <- range, do: verse(n)
    Enum.join(verses, "\n")
  end

  @doc """
  Get a single verse of the beer song
  """
  @spec verse(integer) :: String.t()
  def verse(0) do
    """
No more bottles of beer on the wall, no more bottles of beer.
Go to the store and buy some more, 99 bottles of beer on the wall.
"""
  end


  def verse(x) do
    bottles_initial = bottles_text(x)
    bottles_after = bottles_text(x-1)
    what_to_take = if x == 1 do "it" else "one" end

    """
#{bottles_initial} of beer on the wall, #{bottles_initial} of beer.
Take #{what_to_take} down and pass it around, #{bottles_after} of beer on the wall.
"""

  end

  defp bottles_text(0) do
    "no more bottles"
  end

  defp bottles_text(1) do
    "1 bottle"
  end

  defp bottles_text(x) do
    "#{x} bottles"
  end



end
