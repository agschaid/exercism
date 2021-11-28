defmodule LogLevel do

  @legacy_codes %{ 1 => :debug, 2 => :info, 3 => :warning, 4 => :error }

  @new_codes Map.merge( @legacy_codes, %{0 => :trace, 5 => :fatal} )

  def to_label(level, true) do
    Map.get(@legacy_codes, level, :unknown)
  end
  def to_label(level, false) do
    Map.get(@new_codes, level, :unknown)
  end

  def alert_recipient(level, legacy?) do
    case {to_label(level, legacy?), legacy?} do
      {:error, _} -> :ops
      {:fatal, _} -> :ops
      {:unknown, true} -> :dev1
      {:unknown, false} -> :dev2
      _ -> false
    end
  end
end
