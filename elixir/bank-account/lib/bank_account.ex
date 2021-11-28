defmodule BankAccount do
  @moduledoc """
  A bank account that supports access from multiple processes.
  """

  @typedoc """
  An account handle.
  """
  @opaque account :: pid

  defp manage_account({:closed, balance}, _any_command) do 
    {{:error, :account_closed}, {:closed, balance}}
  end
  defp manage_account({:open, balance}, :close) do
    {:ok, {:closed, balance}}
  end
  defp manage_account({:open, balance}, :balance) do
    {balance, {:open, balance}}
  end
  defp manage_account({:open, balance}, {:update, amount}) do
    {:ok, {:open, balance + amount }}
  end

  @doc """
  Open the bank. Makes the account available.
  """
  @spec open_bank() :: account
  def open_bank() do
    {:ok, pid} = Agent.start(fn -> {:open, 0} end)
    pid
  end

  @doc """
  Close the bank. Makes the account unavailable.
  """
  @spec close_bank(account) :: none
  def close_bank(account) do
    Agent.get_and_update(account, fn s -> manage_account(s, :close) end)
  end

  @doc """
  Get the account's balance.
  """
  @spec balance(account) :: integer
  def balance(account) do
    Agent.get_and_update(account, fn s -> manage_account(s, :balance) end)
  end

  @doc """
  Update the account's balance by adding the given amount which may be negative.
  """
  @spec update(account, integer) :: any
  def update(account, amount) do
    Agent.get_and_update(account, fn s -> manage_account(s, {:update, amount}) end)
  end
end
