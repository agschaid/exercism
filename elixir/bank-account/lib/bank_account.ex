defmodule BankAccount do
  @moduledoc """
  A bank account that supports access from multiple processes.
  """

  use GenServer

  @typedoc """
  An account handle.
  """
  @opaque account :: pid

  @doc """
  GenServer callback.
  """
  def init(state), do: {:ok, state}

  # handles all calls for closed bank accounts
  def handle_call(_anyCall, _from, {:closed, balance}) do
    {:reply, {:error, :account_closed}, {:closed, balance} }
  end
  def handle_call(:balance, _from, {:open, balance}) do
    {:reply, balance, {:open, balance}}
  end
  def handle_call({:update, amount}, _from, {:open, balance}) do
    {:reply, :ok, {:open, balance + amount}}
  end
  def handle_call(:close, _from, {:open, balance}) do 
    {:reply, nil, {:closed, balance}}
  end

  @doc """
  Open the bank. Makes the account available.
  """
  @spec open_bank() :: account
  def open_bank() do
    {:ok, pid} = GenServer.start_link(__MODULE__, {:open, 0})
    pid
  end

  @doc """
  Close the bank. Makes the account unavailable.
  """
  @spec close_bank(account) :: none
  def close_bank(account) do
    GenServer.call(account, :close)
  end

  @doc """
  Get the account's balance.
  """
  @spec balance(account) :: integer
  def balance(account) do
    GenServer.call(account, :balance)
  end

  @doc """
  Update the account's balance by adding the given amount which may be negative.
  """
  @spec update(account, integer) :: any
  def update(account, amount) do
    GenServer.call(account, {:update, amount})
  end
end
