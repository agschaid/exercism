defmodule Lasagna do

  @moduledoc """
  API for creating a tasty dish made of stacked layers of lasgane and filling.
  """

  @doc """
  Returns the total time in minutes that a lasagna is expected to remain in the oven.

  ## Examples
    iex> Lasagna.expected_minutes_in_oven()
    40
  """
  @spec expected_minutes_in_oven() :: pos_integer()
  def expected_minutes_in_oven(), do: 40

  @doc """
  Returns the time in minutes that a lassagna has to remain in the oven based on the given
  `minutes_in_oven` that the lassagna has already been in the oven.

  `minutes_in_oven` might be a negative number if the lasgana has not yet been put into
  the oven.

  ## Examples
    iex> Lasagna.remaining_minutes_in_oven(0)
    40
    iex> Lasagna.remaining_minutes_in_oven(21)
    19
    iex> Lasagna.remaining_minutes_in_oven(-4)
    44
    iex> Lasagna.remaining_minutes_in_oven(42)
    -2
  """
  @spec remaining_minutes_in_oven(integer()) :: integer()
  def remaining_minutes_in_oven(minutes_in_oven) do
    expected_minutes_in_oven() - minutes_in_oven
  end

  @doc """
  Computes the expected preparation time based on the given `number_of_layers`.

  ## Examples
    iex> Lasagna.preparation_time_in_minutes(0)
    0
    iex> Lasagna.preparation_time_in_minutes(2)
    4
    iex> Lasagna.preparation_time_in_minutes(4319871)
    8639742
  """
  @spec preparation_time_in_minutes(pos_integer()) :: pos_integer()
  def preparation_time_in_minutes(number_of_layers), do: number_of_layers * 2

  @doc """
  Computes the total amount of minutes spent on preparing the lasagna based on the given
  `number_of_layers` and `minutes_in_oven`.

  If `minutes_in_oven` is negative it is disregarded (do something else while waiting to
  put the lasagna in the oven).

  ## Examples
  iex> Lasagna.total_time_in_minutes(1, 10)
  12
  iex> Lasagna.total_time_in_minutes(2, 10)
  14
  iex> Lasagna.total_time_in_minutes(1, 20)
  22
  iex> Lasagna.total_time_in_minutes(1, 0)
  2
  iex> Lasagna.total_time_in_minutes(1, -4)
  2
  """
  @spec total_time_in_minutes(pos_integer(), pos_integer()) :: pos_integer()
  def total_time_in_minutes(number_of_layers, minutes_in_oven) when minutes_in_oven >= 0 do
    preparation_time_in_minutes(number_of_layers) + minutes_in_oven
  end
  def total_time_in_minutes(number_of_layers, _minutes_in_oven) do
    preparation_time_in_minutes(number_of_layers)
  end

  @doc """
  Uses sophisticated magic to alert you.

  ## Examples
  iex> Lasagna.alarm()
  "Ding!"
  """
  @spec alarm() :: String.t()
  def alarm(), do: "Ding!"
end
