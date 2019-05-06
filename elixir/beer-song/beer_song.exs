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

  def verse(1) do
    """
1 bottle of beer on the wall, 1 bottle of beer.
Take it down and pass it around, no more bottles of beer on the wall.
"""
  end

  def verse(2) do
    """
2 bottles of beer on the wall, 2 bottles of beer.
Take one down and pass it around, 1 bottle of beer on the wall.
"""
  end

  def verse(x) do
  """
#{x} bottles of beer on the wall, #{x} bottles of beer.
Take one down and pass it around, #{x-1} bottles of beer on the wall.
"""
  end



end
