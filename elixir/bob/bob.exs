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

  defp yelling(s) do
    
    cond do
      # there is no uppercase character (or no alphabetical character at all?)
      String.downcase(s) == s -> :not_yelling
      # there is at least one upper- and no lowercase character
      String.upcase(s)   == s -> :yelling
      # mixed
      true                    -> :not_yelling
    end

  end

  defp asking(s) do
    if String.ends_with?(s, "?") do
      :asking
    else
      :not_asking
    end
  end

end
