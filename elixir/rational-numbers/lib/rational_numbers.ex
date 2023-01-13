defmodule RationalNumbers do
  @type rational :: {integer, integer}

  @doc """
  Add two rational numbers
  """
  @spec add(a :: rational, b :: rational) :: rational
  def add({num_a, denom_a}, {num_b, denom_b}) do
    {
      (num_a * denom_b) + (num_b * denom_a),
      denom_a * denom_b
    }
    |> reduce()
  end

  @doc """
  Subtract two rational numbers
  """
  @spec subtract(a :: rational, b :: rational) :: rational
  def subtract({num_a, denom_a}, {num_b, denom_b}) do
    # TODO almost identical to addition
    {
      (num_a * denom_b) - (num_b * denom_a),
      denom_a * denom_b
    }
    |> reduce()
  end

  @doc """
  Multiply two rational numbers
  """
  @spec multiply(a :: rational, b :: rational) :: rational
  def multiply({num_a, denom_a}, {num_b, denom_b}) do
    { num_a * num_b, denom_a * denom_b }
    |> reduce()
  end

  @doc """
  Divide two rational numbers
  """
  @spec divide_by(num :: rational, den :: rational) :: rational
  def divide_by({num_a, denom_a}, {num_b, denom_b}) when num_b != 0 do
    { num_a * denom_b , num_b * denom_a }
    |> reduce()
  end

  @doc """
  Absolute value of a rational number
  """
  @spec abs(a :: rational) :: rational
  def abs({num, denom}), do: {Kernel.abs(num), Kernel.abs(denom)} |> reduce()

  @doc """
  Exponentiation of a rational number by an integer
  """
  @spec pow_rational(a :: rational, n :: integer) :: rational
  def pow_rational(a, n) when n < 0, do: pow_rational(a, Kernel.abs(n))
  def pow_rational({num, denom}, n) do
    pow = fn(x) -> :math.pow(x, n) |> round() end

    {pow.(num), pow.(denom)} |> round()
  end

  @doc """
  Exponentiation of a real number by a rational number
  """
  @spec pow_real(x :: integer, n :: rational) :: float
  def pow_real(x, n) do
  end

  @doc """
  Reduce a rational number to its lowest terms
  """
  @spec reduce(a :: rational) :: rational
  def reduce({num, denom}) do
    gcd = gcd(num, denom)
    { num / gcd, denom / gcd }
  end

  @spec gcd(x :: integer, y :: integer) :: integer
  defp gcd(x, 0), do: x
  defp gcd(x, y), do: gcd(y, rem(x, y))
end
