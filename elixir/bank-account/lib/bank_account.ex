defmodule BankAccount do
  @moduledoc """
  A bank account that supports access from multiple processes.
  """

  @typedoc """
  An account handle.
  """
  @opaque account :: pid


  def manage_account({:closed, balance}, _any_command) do 
    {{:error, :account_closed}, {:closed, balance}}
  end
  def manage_account({:open, balance}, :close) do
    {nil, {:closed, balance}}
  end
  def manage_account({:open, balance}, :balance) do
    {balance, {:open, balance}}
  end
  def manage_account({:open, balance}, {:update, amount}) do
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
    Agent.get_and_update(account, __MODULE__, :manage_account, [:close])
  end

  @doc """
  Get the account's balance.
  """
  @spec balance(account) :: integer
  def balance(account) do
    Agent.get_and_update(account, __MODULE__, :manage_account, [:balance])
  end

  @doc """
  Update the account's balance by adding the given amount which may be negative.
  """
  @spec update(account, integer) :: any
  def update(account, amount) do
    Agent.get_and_update(account, __MODULE__, :manage_account, [{:update, amount}])
  end
end
