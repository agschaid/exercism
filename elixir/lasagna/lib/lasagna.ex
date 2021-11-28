defmodule Lasagna do
  # Please define the 'expected_minutes_in_oven/0' function
  @spec expected_minutes_in_oven() :: pos_integer()
  def expected_minutes_in_oven(), do: 40

  # Please define the 'remaining_minutes_in_oven/1' function
  @spec remaining_minutes_in_oven(integer()) :: integer()
  def remaining_minutes_in_oven(minutes_in_oven) do
    expected_minutes_in_oven() - minutes_in_oven
  end

  # Please define the 'preparation_time_in_minutes/1' function
  @spec preparation_time_in_minutes(pos_integer()) :: pos_integer()
  def preparation_time_in_minutes(number_of_layers), do: number_of_layers * 2

  # Please define the 'total_time_in_minutes/2' function
  @spec total_time_in_minutes(pos_integer(), pos_integer()) :: pos_integer()
  def total_time_in_minutes(number_of_layers, minutes_in_oven) do
    preparation_time_in_minutes(number_of_layers) + minutes_in_oven
  end

  # Please define the 'alarm/0' function
  @spec alarm() :: String.t()
  def alarm(), do: "Ding!"
end
