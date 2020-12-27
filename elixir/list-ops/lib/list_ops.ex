defmodule ListOps do
  # Please don't use any external modules (especially List or Enum) in your
  # implementation. The point of this exercise is to create these basic
  # functions yourself. You may use basic Kernel functions (like `Kernel.+/2`
  # for adding numbers), but please do not use Kernel functions for Lists like
  # `++`, `--`, `hd`, `tl`, `in`, and `length`.

  @spec count(list) :: non_neg_integer
  def count(l), do: count_tail_rec(l, 0)

  @spec count_tail_rec(list, non_neg_integer) :: non_neg_integer
  defp count_tail_rec([], acc), do: acc
  defp count_tail_rec([_|rest], acc), do: count_tail_rec(rest, acc+1)

  @spec reverse(list) :: list
  def reverse(l), do: reverse_tail_rec(l, [])

  @spec reverse_tail_rec(list, list) :: list
  defp reverse_tail_rec([], acc), do: acc
  defp reverse_tail_rec([x|rest], acc), do: reverse_tail_rec(rest, [x|acc])

  @spec map(list, (any -> any)) :: list
  def map(l, f), do: map_tail_rec(l, [], f)

  @spec map_tail_rec(list, list, (any -> any)) :: list
  defp map_tail_rec([], rev_acc, _f), do: reverse(rev_acc) 
  defp map_tail_rec([x|rest], rev_acc, f), do: map_tail_rec(rest, [f.(x) | rev_acc], f )


  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter(l, f), do: filter_tail_rec(l, [], f)

  @spec filter_tail_rec(list, list, (any -> as_boolean(term))) :: list
  defp filter_tail_rec([], rev_acc, _f), do: reverse(rev_acc)
  defp filter_tail_rec([x|rest], rev_acc, f) do
    if f.(x) do
      filter_tail_rec(rest, [x | rev_acc], f)
    else
      filter_tail_rec(rest, rev_acc, f)
    end

  end

  @type acc :: any
  @spec reduce(list, acc, (any, acc -> acc)) :: acc
  def reduce(l, acc, f) do
  end

  @spec append(list, list) :: list
  def append(a, b) do
  end

  @spec concat([[any]]) :: [any]
  def concat(ll) do
  end
end
