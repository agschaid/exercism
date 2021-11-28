defmodule LogLevel do

  @legacy_codes %{ 1 => :debug, 2 => :info, 3 => :warning, 4 => :error }

  @new_codes Map.merge( @legacy_codes, %{0 => :trace, 5 => :fatal} )

  def to_label(level, legacy?) do

    # OK. The creators of this exercise want me to learn about cond. And I respect
    # that. But those long lists of low-level clauses smell. So this is all you
    # get. Sorry. I mean well ;)
    supported_codes = cond do
      legacy? -> @legacy_codes
      true -> @new_codes
    end

    Map.get(supported_codes, level, :unknown)
  end

  def alert_recipient(level, legacy?) do

    label = to_label(level, legacy?)

    cond do
      label in [:error, :fatal] -> :ops
      label == :unknown -> if legacy? do :dev1 else :dev2 end
      true -> false
    end

  end
end
