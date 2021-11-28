defmodule GuessingGame do
  def compare(x, x) do
    "Correct"
  end
  def compare(secret_number, guess \\ :no_guess) when guess == :no_guess do
    "Make a guess"
  end
  def compare(secret_number, guess) when abs(secret_number-guess) == 1 do
    "So close"
  end
  def compare(secret_number, guess) when guess < secret_number do
    "Too low"
  end
  def compare(secret_number, guess) when guess > secret_number do
    "Too high"
  end
end
