defmodule ListOps do
  # Please don't use any external modules (especially List or Enum) in your
  # implementation. The point of this exercise is to create these basic
  # functions yourself. You may use basic Kernel functions (like `Kernel.+/2`
  # for adding numbers), but please do not use Kernel functions for Lists like
  # `++`, `--`, `hd`, `tl`, `in`, and `length`.

  @spec count(list) :: non_neg_integer
  def count(l), do: reduce(l, 0, fn (_x, acc) -> acc + 1 end)

  @spec reverse(list) :: list
  def reverse(l), do: reduce(l, [], &([&1|&2]))

  @spec map(list, (any -> any)) :: list
  def map(l, f), do: reverse(l) |> reduce([], &([f.(&1)|&2]))

  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter(l, f) do
    acc_fn = fn (x, acc) -> if f.(x) do [x | acc] else acc end end
    
    reverse(l) |> reduce([], acc_fn)
  end

  @type acc :: any
  @spec reduce(list, acc, (any, acc -> acc)) :: acc
  def reduce([], acc, _f), do: acc
  def reduce([x|rest], acc, f), do: reduce(rest, f.(x, acc), f)

  @spec append(list, list) :: list
  def append(a, b), do: reverse(a) |> reduce(b, fn (x, l) -> [x|l] end )

  @spec concat([[any]]) :: [any]
  def concat(ll), do: reverse(ll) |> reduce([], &append/2)

end
