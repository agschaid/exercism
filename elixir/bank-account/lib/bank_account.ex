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
  defp manage_account({:open, balance}, command) do
    case command do
      :close   -> {nil, {:closed, balance}}
      :balance -> {balance, {:open, balance}}
      {:update, amount} -> {:ok, {:open, balance + amount}}
    end
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

  Be aware that this does NOT terminate the underlying process. This helps to
  differentiate non-existing/deleted accounts from closed ones.
  """
  @spec close_bank(account) :: none
  def close_bank(account) do
    Agent.get_and_update(account, fn s -> manage_account(s, :close) end)
  end

  @doc """
  Deletes the bank.

  Be aware that this frees the PID of the bank to be (theoretically) reused in
  the future. Client code is responsible for sweeping all references to this account.
  """
  def delete_bank(account) do
    Agent.stop(account)
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
