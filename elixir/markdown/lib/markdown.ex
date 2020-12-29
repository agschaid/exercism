defmodule Markdown do
  @doc """
    Parses a given string with Markdown syntax and returns the associated HTML for that string.

    How I refactored:
      * rewrote parse/1 with pipes
      * rewrote process/1 with pattern matching
      * made html generation much easier by introducing enclose_with/2
      * rewrote the heading code to use pattern matching too (making it slower but simpler)
      * unsatisfied with list code being spread out to multiple places I refactored process/1 
        to be a tail recursive function with an accumulator. This allowed for explicit context
        switches when lists start and end. By the way: the new code now supports multiple lists.
      * refactored italic and bold support to a rather simple splitter alogrithm

    ## Examples

    iex> Markdown.parse("This is a paragraph")
    "<p>This is a paragraph</p>"

    iex> Markdown.parse("#Header!\n* __Bold Item__\n* _Italic Item_")
    "<h1>Header!</h1><ul><li><em>Bold Item</em></li><li><i>Italic Item</i></li></ul>"
  """
  @spec parse(String.t()) :: String.t()
  def parse(m) do
    String.split(m, "\n") |> process("")
  end

  defp process([], acc), do: acc
  defp process([line|lines], acc) do

    case line do
      "#" <> _rest  -> process_heading(line, lines, acc)
      "* " <> text -> 
        new_acc = acc <> "<ul>" <> list_item(text)
        process_list_context(lines, new_acc)
      _ -> 
        new_acc = acc <> handle_strong_and_italic(line) |> enclose_with("p")
        process(lines, new_acc)
    end
  end

  defp process_heading(current_line, lines, acc) do
    handle_heading = fn(tag, text) ->
      new_acc = acc <> enclose_with(text, tag)
      process(lines, new_acc)
    end

    case current_line do
      # this might not be the most performant way. But it's very explicit and clear
      "# " <> text -> handle_heading.("h1", text)
      "## " <> text -> handle_heading.("h2", text)
      "### " <> text -> handle_heading.("h3", text)
      "#### " <> text -> handle_heading.("h4", text)
      "##### " <> text -> handle_heading.("h5", text)
      "###### " <> text -> handle_heading.("h6", text)
      # html only supports up to <h6>
    end
  end

  defp list_item(text), do: handle_strong_and_italic(text) |> enclose_with("li")

  defp process_list_context(["* " <> text | rest], acc) do 
    new_acc = acc <> list_item(text)
    process_list_context(rest, new_acc)
  end
  defp process_list_context(l, acc), do: process(l, acc <> "</ul>")

  @spec parse(String.t()) :: String.t()
  defp enclose_with(text, tag), do: "<#{tag}>#{text}</#{tag}>"

  @spec parse(String.t()) :: String.t()
  defp handle_strong_and_italic(t) do 
    t
    |> replace_all_embracing("__", "strong")
    |> replace_all_embracing("_", "em")
  end

  defp replace_all_embracing(t, md_text, tag) do
    String.split(t, md_text)
    |> enclose_every_second(tag, "")
  end

  defp enclose_every_second([last], _tag, acc), do: acc <> last
  defp enclose_every_second([t1, t2 | ts], tag , acc) do
    new_acc = acc <> t1 <> enclose_with(t2, tag)
    enclose_every_second(ts, tag, new_acc)
  end

end
