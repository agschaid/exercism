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
    cond do
      String.upcase(s) == s && String.downcase(s) != s -> :yelling
      true -> :not_yelling
    end
  end

  def asking(s) do
    cond do
      String.at(s, -1) == "?" -> :asking
      true -> :not_asking
    end
  end

end
