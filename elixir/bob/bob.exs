defmodule Bob do
  def hey(input) do
    trimmed = String.trim(input)

    case trimmed do
      "" -> "Fine. Be that way!"
      _  -> case {yelling(trimmed), asking(trimmed)} do
        {:not_yelling, :asking    } -> "Sure."
        {:yelling    , :not_asking} -> "Whoa, chill out!"
        {:yelling    , :asking    } -> "Calm down, I know what I'm doing!"
        _                           -> "Whatever."
      end
    end
  end

  def yelling(s) do
    case {String.downcase(s), String.upcase(s)} do
      # no uppercase character present.
      {^s, _} -> :not_yelling
      {_, ^s} -> :yelling
       _      -> :not_yelling
    end
  end

  def asking(s) do
    case String.at(s, -1) do
      "?" -> :asking
      _   -> :not_asking
    end
  end

end
