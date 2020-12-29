defmodule Markdown do
  @doc """
    Parses a given string with Markdown syntax and returns the associated HTML for that string.

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
  defp process([line|ls], acc) do

    case line do
      "#" <> _rest  -> process_heading(line, ls, acc)
      "* " <> text -> 
        {list_rest, list_acc} = splice_list(ls, single_list_text(text))
        new_acc = acc <> enclose_with(list_acc, "ul")
        process(list_rest, new_acc)
      _ -> 
        new_acc = acc <> handle_strong_and_italic(line) |> enclose_with("p")
        process(ls, new_acc)
    end
  end

  defp process_heading(current_line, lines, acc) do
    handle_heading = fn(tag, text) ->
      new_acc = acc <> enclose_with(text, tag)
      process(lines, new_acc)
    end

    case current_line do
      "# " <> text -> handle_heading.("h1", text)
      "## " <> text -> handle_heading.("h2", text)
      "### " <> text -> handle_heading.("h3", text)
      "#### " <> text -> handle_heading.("h4", text)
      "##### " <> text -> handle_heading.("h5", text)
      "###### " <> text -> handle_heading.("h6", text)
    end
  end

  defp single_list_text(text), do: handle_strong_and_italic(text) |> enclose_with("li")

  defp splice_list(["* " <> text | rest], acc) do 
    new_acc = acc <> single_list_text(text)
    splice_list(rest, new_acc)
  end
  defp splice_list(l, acc), do: {l, acc}

  @spec parse(String.t()) :: String.t()
  defp enclose_with(text, tag), do: "<#{tag}>#{text}</#{tag}>"

  @spec parse(String.t()) :: String.t()
  defp handle_strong_and_italic(t) do 
    String.split(t)
    |> Enum.map(&replace_md_with_tag/1)
    |> Enum.join(" ")
  end

  defp replace_md_with_tag(w) do
    replace_suffix_md(replace_prefix_md(w))
  end

  defp replace_prefix_md(w) do
    cond do
      w =~ ~r/^#{"__"}{1}/ -> String.replace(w, ~r/^#{"__"}{1}/, "<strong>", global: false)
      w =~ ~r/^[#{"_"}{1}][^#{"_"}+]/ -> String.replace(w, ~r/_/, "<em>", global: false)
      true -> w
    end
  end

  defp replace_suffix_md(w) do
    cond do
      w =~ ~r/#{"__"}{1}$/ -> String.replace(w, ~r/#{"__"}{1}$/, "</strong>")
      w =~ ~r/[^#{"_"}{1}]/ -> String.replace(w, ~r/_/, "</em>")
      true -> w
    end
  end

  defp patch(l) do
    String.replace_suffix(
      String.replace(l, "<li>", "<ul>" <> "<li>", global: false),
      "</li>",
      "</li>" <> "</ul>"
    )
  end
end
