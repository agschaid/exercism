defmodule KitchenCalculator do
  @milliliter_per_unit %{
    :milliliter => 1.0,
    :cup => 240.0,
    :fluid_ounce => 30.0,
    :teaspoon => 5.0,
    :tablespoon => 15.0
  }

  def get_volume({_unit, volume}), do: volume

  def to_milliliter({unit, volume}) do
    milliliter_volume = Map.fetch!(@milliliter_per_unit, unit) * volume
    {:milliliter, milliliter_volume}
  end

  def from_milliliter({:milliliter, volume}, unit) do
    converted_volume = volume / Map.fetch!(@milliliter_per_unit, unit)
    {unit, converted_volume}
  end

  def convert({unit, volume}, unit), do: {unit, volume}
  def convert(volume_pair, unit) do
    volume_pair |> to_milliliter() |> from_milliliter(unit)
  end
end
